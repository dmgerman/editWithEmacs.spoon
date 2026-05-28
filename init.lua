---

local obj={}

obj.__index = obj

-- metadata for all spoons
obj.name = "editWithEmacs"
obj.version = "0.3"
obj.author = "Daniel German <dmg@uvic.ca> and  Jeremy Friesen <emacs@jeremyfriesen.com>"
obj.homepage = "https://github.com/dmgerman/editWithEmacs.spoon"
obj.license = "MIT - https://opensource.org/licenses/MIT"

-- The name of the Emacs application
obj.emacsAppName = "Emacs"

require ("hs.ipc")

if not hs.ipc.cliStatus() then
   hs.alert("hs is not installed.. Installing in default location.. This might not work for M1 emacs")
   hs.ipc.cliInstall()
   if not hs.ipc.cliStatus() then
      hs.alert("Unable to install ipc module in /usr/local. editWithEmacs will not function.")
      print("\n\neditWithEmacs: unable to install ipc module. You might have to do it manually. This works for M1 macs.",
            "Make sure you can execute hs from command line. See documentation of hs.ipc\n",
            "For example: at /usr/local/bin do\n",
            "sudo ln -s /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs .\n",
            "\n")
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

-- Call emacs-everywhere for the given window ID.
-- Uses hs.task (non-blocking) so Hammerspoon remains free to answer
-- the getWindowInfo() callback from dmg-emacs-everywhere-app-info.
function obj:openEditor()
   if not self.currentEmacs then
      hs.alert("No " .. self.emacsAppName .. " window found")
      return
   end
   print("editWithEmacs: emacsclient -e '(emacs-everywhere)'")
   local task = hs.task.new("/Users/dmg/bin/osx/emacsclient", function(exitCode, stdOut, stdErr)
      if exitCode ~= 0 then
         local msg = "editWithEmacs: emacsclient failed (rc=" .. tostring(exitCode) .. "): " .. stdErr
         print(msg)
         hs.alert(msg)
      end
   end, {"-e", "(emacs-everywhere)"})
   task:start()
   self.currentEmacs:activate()
end

-- Begin the edit with Emacs experience
function obj:beginEditing(everything)
   -- everything: if true, do the equivalent of Ctrl-A select everything
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

   if everything then
      hs.eventtap.keyStroke({"cmd"}, "a")
      hs.eventtap.keyStroke({"cmd"}, "c")
   else
      hs.eventtap.keyStroke({"cmd"}, "x")
   end

   hs.notify.new({title=appName, informativeText="«" .. winTitle .. "»", subTitle="Editing in " .. self.emacsAppName}):send()
   self:openEditor()
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

   print(self.emacsAppName .. " is sending back the text")

   if w:focus() then
      if everything then
         hs.eventtap.keyStroke({"cmd"}, "a")
      end
      hs.eventtap.keyStroke({"cmd"}, "v")
   else
      hs.alert("Could not focus window " .. windowId)
   end
end

print("Finished loading editWithEmacs.spoon" )

return obj
