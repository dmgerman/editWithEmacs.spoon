---

local obj={}

obj.__index = obj

-- metadata for all spoons
obj.name = "editWithEmacs"
obj.version = "0.4"
obj.author = "Daniel German <dmg@uvic.ca> and  Jeremy Friesen <emacs@jeremyfriesen.com>"
obj.homepage = "https://github.com/dmgerman/editWithEmacs.spoon"
obj.license = "MIT - https://opensource.org/licenses/MIT"

obj.logger = hs.logger.new("editWithEmacs")

-- The name of the Emacs application
obj.emacsAppName = "Emacs"

-- Path to emacsclient. Override in configuration if needed.
obj.emacsClient = "/opt/homebrew/bin/emacsclient"

-- Cached Emacs server socket path, populated on first socket-not-found failure
-- by querying the running Emacs process with lsof. Invalidated on subsequent
-- socket failures so a restarted server is re-discovered.
obj.socketPath = nil

require ("hs.ipc")

if not hs.ipc.cliStatus() then
   hs.alert("hs is not installed.. Installing in default location.. This might not work for M1 emacs")
   hs.ipc.cliInstall()
   if not hs.ipc.cliStatus() then
      hs.alert("Unable to install ipc module in /usr/local. editWithEmacs will not function.")
      obj.logger.e("unable to install ipc module. You might have to do it manually. This works for M1 macs. "
                .. "Make sure you can execute hs from command line. See documentation of hs.ipc. "
                .. "For example: at /usr/local/bin do "
                .. "sudo ln -s /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs .")
      return obj
   end
end

-- Return geometry and app info for a window by ID, for Emacs to query.
-- Format: x||y||w||h||appName||winTitle
function obj:getWindowInfo(windowId)
   local w = hs.window.get(windowId)
   if not w then
      return "0||0||0||0||unknown||unknown"
   end
   local f = w:frame()
   return math.floor(f.x) .. "||" .. math.floor(f.y) .. "||" ..
          math.floor(f.w) .. "||" .. math.floor(f.h) .. "||" ..
          w:application():name() .. "||" .. w:title()
end

-- Discover the Emacs server socket path via lsof on the running Emacs process.
-- Calls onDone(socketPath) on success, onDone(nil) on failure (after alerting).
-- lsof completes in ~100ms on a typical Emacs process; we use hs.execute
-- (synchronous) for simplicity and to avoid hs.task buffering quirks with the
-- large lsof output.
function obj:discoverSocketAsync(onDone)
   local app = hs.application.find(self.emacsAppName)
   if not app then
      hs.alert(self.emacsAppName .. " is not running")
      onDone(nil)
      return
   end
   local pid = app:pid()
   if not pid then
      hs.alert("Could not get pid for " .. self.emacsAppName)
      onDone(nil)
      return
   end
   self.logger.f("discovering socket via lsof -p %d", pid)
   local cmd = string.format("/usr/sbin/lsof -p %d 2>&1 | /usr/bin/grep -Eo '/[^[:space:]]*/emacs[0-9]+/[^[:space:]/]+' | /usr/bin/head -1",
                             pid)
   local out, ok = hs.execute(cmd)
   if not ok or not out or out == "" then
      local msg = "could not find " .. self.emacsAppName .. " server socket"
      self.logger.e(msg)
      hs.alert(msg)
      onDone(nil)
      return
   end
   local socket = out:gsub("%s+$", "")
   self.logger.f("discovered socket %s", socket)
   self.socketPath = socket
   onDone(socket)
end

-- Build the argv for emacsclient, prepending --socket-name when cached.
function obj:emacsClientArgv(args)
   local argv = {}
   if self.socketPath then
      table.insert(argv, "--socket-name=" .. self.socketPath)
   end
   for _, a in ipairs(args) do
      table.insert(argv, a)
   end
   return argv
end

-- Invoke emacsclient asynchronously. On a "can't find socket" failure, discover
-- the socket via lsof and retry once. Other failures alert and stop.
function obj:runEmacsClient(args, attempted, callback)
   local argv = self:emacsClientArgv(args)
   self.logger.f("%s %s", self.emacsClient, table.concat(argv, " "))
   -- Retain the hs.task in self._pendingTasks until its callback fires; without
   -- a live Lua reference the task can be garbage-collected mid-flight and the
   -- callback never runs.
   self._pendingTasks = self._pendingTasks or {}
   local task
   task = hs.task.new(self.emacsClient, function(rc, stdOut, stdErr)
      self._pendingTasks[task] = nil
      if rc == 0 then
         if callback then callback(rc, stdOut, stdErr) end
         return
      end

      local isSocketErr = stdErr and stdErr:find("can't find socket", 1, true) ~= nil
      if isSocketErr and not attempted then
         self.socketPath = nil
         self:discoverSocketAsync(function(path)
            if path then
               self:runEmacsClient(args, true, callback)
            elseif callback then
               callback(rc, stdOut, stdErr)
            end
         end)
         return
      end

      local msg = string.format("emacsclient failed (rc=%d): %s", rc, tostring(stdErr))
      self.logger.e(msg)
      if callback then
         callback(rc, stdOut, stdErr)
      else
         hs.alert(msg)
      end
   end, argv)
   self._pendingTasks[task] = true
   task:start()
end

-- Execute Emacs Lisp code asynchronously via emacsclient. The optional callback
-- receives (rc, stdOut, stdErr) once the call resolves (after any auto-retry).
-- When a callback is supplied, default failure alerts are suppressed so the
-- caller can handle errors.
function obj:emacsExecute(elispCode, callback)
   self:runEmacsClient({"-e", elispCode}, false, callback)
end

-- Round-trip test for the Hammerspoon console. Sends a (message ...) form to
-- Emacs; on success it appears in Emacs's *Messages* buffer, on failure
-- runEmacsClient alerts the user.
function obj:test()
   self:emacsExecute(string.format('(message "editWithEmacs test %s")',
                                   os.date("%H:%M:%S")))
end

-- Call emacs-everywhere on the current window. Uses hs.task (non-blocking) so
-- Hammerspoon remains free to answer the getWindowInfo() callback from
-- dmg-emacs-everywhere-app-info.
function obj:openEditor()
   if not self.currentEmacs then
      hs.alert("No " .. self.emacsAppName .. " window found")
      return
   end
   self:emacsExecute("(emacs-everywhere)")
   self.currentEmacs:activate()
end

-- Read the focused element's text via macOS accessibility. Returns the text or
-- nil when the focused element does not expose it (Electron, terminals, custom
-- canvas widgets). For everything=true returns AXValue (full content); for
-- everything=false returns AXSelectedText (current selection).
function obj:readFocusedText(everything)
   local ok, focused = pcall(function()
      return hs.axuielement.systemWideElement().AXFocusedUIElement
   end)
   if not ok or not focused then return nil end
   local val = everything and focused.AXValue or focused.AXSelectedText
   -- Treat empty string as a miss. Some apps (kitty, iTerm2) expose
   -- AXTextArea with an always-empty AXValue because their content lives in
   -- a GPU-rendered surface, not in an NSText element. Falling through to
   -- the keystroke fallback gives a correct "could not capture" alert when
   -- the app also overrides Cmd+A; the cost is that editing a genuinely
   -- empty native field also takes the slow path.
   if type(val) ~= "string" or val == "" then return nil end
   return val
end

-- Sentinel placed on the pasteboard before the keystroke fallback. If the app
-- respects Cmd+A,Cmd+C (or Cmd+X) the sentinel gets overwritten; if it ignores
-- them, the sentinel stays and surfaces in the Emacs buffer so the user is
-- warned instead of silently editing stale clipboard content.
obj.captureFailureSentinel = "(application did not allow clipboard extraction)"

-- Populate the system pasteboard with the text to edit. Tries accessibility
-- first; on miss, plants the sentinel and runs the keystroke fallback
-- (Cmd+A,Cmd+C for everything or Cmd+X for selection). After a short delay
-- checks pasteboard changeCount: if the app did not update the clipboard, the
-- sentinel remains and the user is alerted. Either way onDone() is invoked so
-- editing proceeds.
function obj:prepareClipboard(everything, onDone)
   local text = self:readFocusedText(everything)
   if text then
      hs.pasteboard.setContents(text)
      onDone()
      return
   end

   hs.pasteboard.setContents(self.captureFailureSentinel)
   local before = hs.pasteboard.changeCount()
   if everything then
      hs.eventtap.keyStroke({"cmd"}, "a")
      hs.eventtap.keyStroke({"cmd"}, "c")
   else
      hs.eventtap.keyStroke({"cmd"}, "x")
   end

   hs.timer.doAfter(0.2, function()
      if hs.pasteboard.changeCount() == before then
         local key = everything and "Cmd+A" or "Cmd+X"
         local msg = "Could not capture text: the app appears to override " .. key
                  .. ". Editing the sentinel string instead."
         self.logger.e(msg)
         hs.alert(msg)
      end
      onDone()
   end)
end

-- Begin the edit with Emacs experience
function obj:beginEditing(everything)
   -- everything: if true, edit the full content of the focused field
   local w = hs.window.focusedWindow()
   if not w then
      hs.alert("No focused window found. Ignoring request")
      return
   end
   if w:title():sub(1, 5) == self.emacsAppName then
      hs.alert("🤔 already in " .. self.emacsAppName .. ". Ignoring request")
      return
   end
   self.currentEmacs = hs.application.find(self.emacsAppName)
   if not self.currentEmacs then
      hs.alert("No " .. self.emacsAppName .. " window found. Ignoring request")
      return
   end

   local windowId = w:id()
   local appName = w:application():name()
   local winTitle = w:title()
   local frame = w:frame()

   -- Write window info to temp file for Emacs to read (avoids IPC round-trip).
   -- Format: windowId||x||y||w||h||appName||winTitle
   local info = windowId .. "||" ..
                math.floor(frame.x) .. "||" .. math.floor(frame.y) .. "||" ..
                math.floor(frame.w) .. "||" .. math.floor(frame.h) .. "||" ..
                appName .. "||" .. winTitle
   local f = io.open("/tmp/emacs-everywhere.txt", "w")
   if f then f:write(info) ; f:close() end

   self:prepareClipboard(everything, function()
      hs.notify.new({title=appName, informativeText="«" .. winTitle .. "»", subTitle="Editing in " .. self.emacsAppName}):send()
      self:openEditor()
   end)
end

function obj:bindHotkeys(mapping)
   local def = {
      edit_selection = function() self:beginEditing(false) end,
      edit_all       = function() self:beginEditing(true) end
   }
   local descriptions = {
      edit_selection = "Edit selection with Emacs [Emacs]",
      edit_all       = "Edit all text with Emacs [Emacs]"
   }

   for name, spec in pairs(mapping) do
      if def[name] then
         hs.hotkey.bind(spec[1], spec[2], descriptions[name] or ("Edit with Emacs [Emacs]"), def[name])
      end
   end
end

function obj:endEditing(windowId, everything)
   assert(windowId ~= nil, "endEditing: windowId is nil")
   assert(type(windowId) == "number", "endEditing: windowId is not a number, got " .. type(windowId))

   local w = hs.window.get(windowId)
   assert(w ~= nil, "endEditing: no window found for id " .. windowId)

   self.logger.f("%s is sending back the text", self.emacsAppName)

   if w:focus() then
      if everything then
         hs.eventtap.keyStroke({"cmd"}, "a")
      end
      hs.eventtap.keyStroke({"cmd"}, "v")
   else
      hs.alert("Could not focus window " .. windowId)
   end
end

obj.logger.i("Finished loading editWithEmacs.spoon")

return obj
