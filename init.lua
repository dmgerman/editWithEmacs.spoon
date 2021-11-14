---
--- dmg hammerspoon
---
print("Starting loading editWithEmacs spoon")

local obj={}

obj.__index = obj

-- metadata for all spoons
obj.name = "editWithEmacs"
obj.version = "0.2"
obj.author = "Jeremy Friesen <emacs@jeremyfriesen.com>"
obj.homepage = "https://github.com/jeremyf/editWithEmacs"
obj.license = "MIT - https://opensource.org/licenses/MIT"

-- Paying homage to the prior work
obj.derivedFrom = "http://github.com/dmgerman/editWithEmacs"

-- Additional local variables for managing the state of editing.

-- the current instance of Emacs
obj.currentEmacs = nil

-- the current non-Emacs window from which we will begin editing
obj.currentWindow = nil

-- The command to invoke
obj.beginEditShellCommand = "emacsclient -e '(hammerspoon-edit-begin)' --create-frame"

-- The name of the Emacs application
obj.emacsAppName = "Emacs"

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

-- Open the editor and give it focus.
function obj:openEditor()
   -- this is a callback to wait until other keys are consumed
   -- this can probably be done more reliably with emacsclient
   if self.currentEmacs then
      -- Prior comments indicate that the emacsclient approach does not reliably work.
      hs.execute(self.beginEditShellCommand, true)
      self.currentEmacs:activate()

      -- Commented out in case the emacsclient stops reliably working.
      -- What is happening below is that with the above obj.currentEmacs:activate() we
      -- are in the emacs editor.  We then want to open the M-x minibuffer, type
      -- hammerspoon-edit-begin and then hit return

      -- hs.eventtap.keyStroke({"alt"}, "x")
      -- hs.eventtap.keyStrokes("hammerspoon-edit-begin")
      -- hs.eventtap.keyStrokes("\n")
   else
      -- this should not be executed
      hs.alert("No " .. self.emacsAppName .. " window found")
   end
end

-- Begin the edit with Emacs experience
function obj:beginEditing(everything)
   -- everything: if true, do the equivalent of Ctrl-A
   ---            select everything
   w = hs.window.focusedWindow()
   if w:title():sub(1, 5) == self.emacsAppName then
      hs.alert("🤔 already in " .. self.emacsAppName .. ". Ignoring request")
      return
   end
   self.currentEmacs = hs.application.find(self.emacsAppName)

   if not self.currentEmacs then
      hs.alert("No " .. self.emacsAppName .. " window found. Ignoring request")
      return
   end

   self.currentWindow = w

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

   hs.notify.new({title=w:application():title(), informativeText="«" .. w:title() .. "»", subTitle="Editing in " .. self.emacsAppName}):send()
   self:openEditor()
end

function obj:bindHotkeys(mapping)
   hs.inspect(mapping)
   print("Bind Hotkeys for editWithEmacs")
   hs.hotkey.bind(mapping.selection[1], mapping.selection[2], function ()
      self:beginEditing(false)
   end)
   hs.hotkey.bind(mapping.all[1], mapping.all[2], function ()
      self:beginEditing(true)
   end)
end

function obj:endEditing(everything)
   -- the text is in the clipboard
   -- enable the original window and see what happens
   -- this is usually run by emacs using hs

   print(self.emacsAppName .. " is sending back the text")

   if not self.currentWindow then
      hs.alert("No current window active")
   else
      if (self.currentWindow:focus()) then
         if everything then
            hs.eventtap.keyStroke({"cmd"}, "a")
         end
         hs.eventtap.keyStroke({"cmd"}, "v")
      else
         hs.alert("Window to send back text does not exist any more")
      end
   end

end

print("Finished loading editWithEmacs.spoon" )

return obj
