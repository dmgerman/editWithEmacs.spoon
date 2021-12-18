---

local obj={}

obj.__index = obj

-- metadata for all spoons
obj.name = "editWithEmacs"
obj.version = "0.2"
obj.author = "Daniel German <dmg@uvic.ca> and  Jeremy Friesen <emacs@jeremyfriesen.com>"
obj.homepage = "https://github.com/dmgerman/editWithEmacs.spoon"
obj.license = "MIT - https://opensource.org/licenses/MIT"

-- Additional local variables for managing the state of editing.

-- the current instance of Emacs
obj.currentEmacs = nil

-- the current non-Emacs window from which we will begin editing
obj.currentWindow = nil

-- The command to invoke
-- make it non-blocking
obj.openEditorShellCommand = "emacsclient -e '(hammerspoon-edit-begin)' --create-frame -n"

-- The name of the Emacs application
obj.emacsAppName = "Emacs"

require ("hs.ipc")

if not hs.ipc.cliStatus() then
   hs.alert("hs is not installed.. Installing in default location.. This might not work for M1 emacs")
   -- if this fails, try to install to a different location
   -- e.g. hs.ipc.cliInstall('/Users/<yourusername>/bin') and
   -- add the directory to your path
   hs.ipc.cliInstall()
   if not hs.ipc.cliStatus() then
      hs.alert("Unable to install ipc module in /usr/local. editWithEmacs will not function.")
      print("\n\neditWithEmacs: unable to install ipc module. You might have to do it manually. This works for M1 macs.",
            "Make sure you can execute /usr/local/bin/hs from command line. See documentation of hs.ipc\n",
            "For example: at /usr/local do\n",
            "sudo ln -s /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs .\n",
            "\n")
      return obj
   end
end

-- Open the editor and give it focus.
function obj:openEditor()
   if self.currentEmacs then
      hs.execute(self.openEditorShellCommand, true)
      self.currentEmacs:activate()
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
   local def = {
      edit_selection = function() self:beginEditing(false) end,
      edit_all       = function() self:beginEditing(true) end
   }
   hs.spoons.bindHotkeysToSpec(def, mapping)
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
