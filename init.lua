
---
--- dmg hammerspoon
---

local obj={}

obj.__index = obj

-- metadata

obj.name = "editWithEmacs"
obj.version = "0.1"
obj.author = "dmg <dmg@uvic.ca>"
obj.homepage = "https://github.com/dmgerman/hs-edit-with-emacs"
obj.license = "MIT - https://opensource.org/licenses/MIT"


obj.emacs = nil         -- the application itselt
obj.current_win = nil   -- the current emacs window
obj.emacsAppName = "Emacs"
obj.emacsClientAppName = "EmacsClient"

require ("hs.ipc")

if not hs.ipc.cliStatus() then
   hs.alert("hs is not installed.. Installing in default location /usr/local")
   -- if this fails, try to install to a different location
   -- e.g. hs.ipc.cliInstall('/Users/<yourusername>/bin') and
   -- add the directory to your path
   if not hs.ipc.cliInstall() then
      hs.alert("Unable to install ipc module in /usr/local. editWithEmacs will not function.")
      return obj
   end
end

function do_emacs()
   -- this is a callback to wait until other keys are consumed
   -- this can probably be done more reliably with emacsclient
   if obj.emacs then
      obj.emacs:activate()
      -- We want to call `M-x' (e.g., `execute-extended-command')
      hs.eventtap.keyStroke({"alt"}, "x")
      -- command to execute in emacs
      hs.eventtap.keyStrokes("hammerspoon-edit-begin")
      hs.eventtap.keyStrokes("\n")
-- this does not seem to work reliable
--      hs.execute("emacsclient -e '(hammerspoon-edit-begin)", true)
   else
      -- this should not be executed
      hs.alert("No emacs window found")
   end
end

function edit_in_emacs(everything)
   -- everything: if true, do the equivalent of Ctrl-A
   ---            select everything
   w = hs.window.focusedWindow()
   if w:title():sub(1, 5) == obj.emacsAppName then
      hs.alert("🤔 already in emacs. Ignoring request")
      return
   end
   obj.emacs = hs.application.find(obj.emacsAppName)

   if not obj.emacs then
      hs.alert("No Emacs window found. Ignoring request")
      return
   end

   obj.current_win = w

   -- use the selection as the text to send to emacs
   -- we use the clipboard to communicate both ways with emacs...
   -- there could be other ways, but this is simple and effective
   if everything then
      -- this basically says, ignore current selection and select current text
      hs.eventtap.keyStroke({"cmd"}, "a")
      -- copy selection into the clipboard
      hs.eventtap.keyStroke({"cmd"}, "c")
   else
      -- otherwise we have to cut,
      hs.eventtap.keyStroke({"cmd"}, "x")
   end

   hs.timer.doAfter(0.5,do_emacs)
end

function emacs_sends_back(everything)
   -- the text is in the clipboard
   -- enable the original window and see what happens
   -- this is usually run by emacs using hs
   -- hs -c "emacs_sends_back()"

   print("emacs is sending back the text")

   if not obj.current_win then
      hs.alert("No current window active")
   else
      if (obj.current_win:focus()) then
         if everything then
            hs.eventtap.keyStroke({"cmd"}, "a")
         end
         hs.eventtap.keyStroke({"cmd"}, "v")
      else
         hs.alert("Window to send back text does not exist any more")
      end
   end

end

-- make sure that emacs is brought to the front when EmacsClient is executed

function emacsclientWatcher(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      if (appName == obj.emacsClientAppName) then
         -- Bring Emacs to Front
         hs.osascript.applescript('tell application "Emacs" to activate')
      end
   end
end
appWatcher = hs.application.watcher.new(emacsclientWatcher)
appWatcher:start()


-- edit by selecting everything
hs.hotkey.bind({"alt"}, '2', nil, function()
      edit_in_emacs(true)
end)

-- edit by using current selection
hs.hotkey.bind({"alt"}, '3', nil, function()
      edit_in_emacs(false)
end)

print("Finished loading editWithEmacs spoon" )



return obj
