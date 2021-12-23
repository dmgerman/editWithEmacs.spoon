;;; editWithEmacs.el --- communicate with hammerspoon to editWithEmacs anywhere

;; Copyright (C) 2021 Daniel M. German <dmg@turingmachine.org>
;;                             Jeremy Friesen <emacs@jeremyfriesen.com>
;;

;; Author: Daniel M. German <dmg@turingmachine.org>
;;         Jeremy Friesen <emacs@jeremyfriesen.com>
;; 
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;;
;; Keywords: hammerspoon, os x
;; Homepage: https://github.com/dmgerman/editWithEmacs.spoon

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use emacs and hammerspoon to edit text in any input box in os x
;; See: https://github.com/dmgerman/editWithEmacs.spoon
;;

;;; Code:


(defvar hammerspoon-buffer-mode 'markdown-mode
  "Name of major mode for hammerspoon editing")

(defvar hammerspoon-buffer-name "*hammerspoon_edit*"
  "Name of the buffer used to edit in emacs.")

(defvar hammerspoon-edit-minor-map nil
  "Keymap used in hammer-edit-minor-mode.")

(unless hammerspoon-edit-minor-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "C-c C-c") 'hammerspoon-edit-end)
    (define-key map (kbd "C-c m")   'hammerspoon-toggle-mode)
    (define-key map (kbd "C-c h")   'hammerspoon-test) ;; for testing

    (setq hammerspoon-edit-minor-map map)))

(define-minor-mode hammerspoon-edit-minor-mode
  "Minor mode to help with editing with hammerspoon"

  :global nil
  :lighter   "_hs-edit_"
  :keymap hammerspoon-edit-minor-map

  ;; if disabling `undo-tree-mode', rebuild `buffer-undo-list' from tree so
  ;; Emacs undo can work
  )

(defun hammerspoon-toggle-mode ()
  "Toggle from Markdown Mode to Org Mode."
  (interactive)
  (if (string-equal "markdown-mode" (format "%s" major-mode))
      (org-mode)
    (markdown-mode))
  (hammerspoon-edit-minor-mode))

(defun hammerspoon-do (command)
  "Send Hammerspoon the given COMMAND."
  (interactive "sHammerspoon Command:")
  (setq hs-binary (executable-find "hs"))
  (if hs-binary
      (call-process hs-binary
                    nil 0 nil
                    "-c"
                    command)
    (message "Hammerspoon hs executable not found. Make sure you hammerspoon has loaded the ipc module")))

(defun hammerspoon-alert (message)
  "Show given MESSAGE via Hammerspoon's alert system."
  (hammerspoon-do (concat "hs.alert.show('" message "', 1)")))

(defun hammerspoon-test ()
  "Show a test message via Hammerspoon's alert system.

If you see a message, Hammerspoon is working correctly."
  (interactive)
  (hammerspoon-alert "Hammerspoon test message..."))

(defun hammerspoon-edit-end ()
  "Send, via Hammerspoon, contents of buffer back to originating window."
  (interactive)
  (mark-whole-buffer)
  (call-interactively 'kill-ring-save)
  (hammerspoon-do (concat "spoon.editWithEmacs:endEditing(False)"))
  (previous-buffer))

(defun hammerspoon-edit-begin ()
  "Receive, from Hammerspoon, text to edit in Emacs"
  (interactive)
  (let ((hs-edit-buffer (get-buffer-create hammerspoon-buffer-name)))
    (switch-to-buffer hs-edit-buffer)
    (erase-buffer) ; Ensure we have a clean buffer
    (yank)
    (funcall hammerspoon-buffer-mode)
    (hammerspoon-edit-minor-mode)
    (message "Type C-c C-c to send back to originating window")
    (exchange-point-and-mark)))


(provide 'hammerspoon)
