;;; hammerspoon.el --- communicate with Hammerspoon for editWithEmacs

;; Copyright (C) 2021-26 Daniel M. German <dmg@turingmachine.org>
;; Copyright (C) 2021 Jeremy Friesen <emacs@jeremyfriesen.com>

;; Author: Daniel M. German <dmg@turingmachine.org>
;;         Jeremy Friesen <emacs@jeremyfriesen.com>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Keywords: hammerspoon, os x
;; Homepage: https://github.com/dmgerman/editWithEmacs.spoon

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(defun hammerspoon-do (command)
  "Send Hammerspoon the given COMMAND."
  (interactive "sHammerspoon Command:")
  (let ((hs-binary (executable-find "hs")))
    (if hs-binary
        (condition-case err
            (call-process hs-binary nil 0 nil "-q" "-c" command)
          (error (message "hammerspoon-do error: %s" (error-message-string err))))
      (message "Hammerspoon hs executable not found. Make sure hammerspoon has loaded the ipc module"))))

(defun hammerspoon-alert (message &optional duration)
  "Show given MESSAGE via Hammerspoon's alert system for DURATION seconds (default 5)."
  (hammerspoon-do (format "hs.alert.show('%s', %d)" message (or duration 5))))

(defun hammerspoon-alert-window (message &optional duration)
  "Show given MESSAGE via Hammerspoon's alert on the current window for DURATION seconds (default 5)."
  (hammerspoon-do (format "dmg_alert_on_window('%s', %d)" message (or duration 5))))

(defun hammerspoon-test ()
  "Show a test message via Hammerspoon's alert system."
  (interactive)
  (hammerspoon-alert "Hammerspoon test message..."))

(defun hammerspoon-do-capture (command)
  "Send Hammerspoon COMMAND and return its output as a string."
  (let ((hs-binary (executable-find "hs")))
    (if hs-binary
        (with-temp-buffer
          (call-process hs-binary nil t nil "-q" "-c" command)
          (string-trim (buffer-string)))
      (error "Hammerspoon hs executable not found"))))

(defun hammerspoon-emacs-everywhere-app-info ()
  "Return an emacs-everywhere-app struct by reading /tmp/emacs-everywhere.txt."
  (unless (file-exists-p "/tmp/emacs-everywhere.txt")
    (error "emacs-everywhere: /tmp/emacs-everywhere.txt not found — Hammerspoon did not write window info"))
  (let* ((raw (with-temp-buffer
                (insert-file-contents "/tmp/emacs-everywhere.txt")
                (string-trim (buffer-string))))
         (parts (split-string raw (regexp-quote "||") t)))
    (unless (>= (length parts) 6)
      (error "emacs-everywhere: malformed window info: %S" raw))
    (let ((win-id (nth 0 parts))
          (x      (string-to-number (nth 1 parts)))
          (y      (string-to-number (nth 2 parts)))
          (w      (string-to-number (nth 3 parts)))
          (h      (string-to-number (nth 4 parts)))
          (app    (nth 5 parts))
          (title  (string-join (nthcdr 6 parts) "||")))
      (message "emacs-everywhere: editing \"%s\" in %s (window %s)" title app win-id)
      (make-emacs-everywhere-app
       :id       win-id
       :class    app
       :title    title
       :geometry (list x y w h)))))

(defun hammerspoon-emacs-everywhere-finish ()
  "Send buffer contents back to Hammerspoon and close the emacs-everywhere frame.
Sets clipboard via native Emacs (no AppleScript), then calls Hammerspoon endEditing."
  (interactive)
  (unless emacs-everywhere-mode
    (error "emacs-everywhere-mode is not active in this buffer"))
  (let* ((text (buffer-string))
         (window-id (emacs-everywhere-app-id emacs-everywhere-current-app)))
    (unless (equal text emacs-everywhere--contents)
      (kill-new text)
      (gui-select-text text)
      (hammerspoon-do (format "spoon.editWithEmacs:endEditing(%s, false)" window-id)))
    (set-buffer-modified-p nil)
    (emacs-everywhere-mode -1)
    (server-buffer-done (current-buffer))))

(defun hammerspoon-emacs-everywhere-compositor (result)
  "Redirect emacs-everywhere's system detection to the hammerspoon compositor."
  (if (eq (car result) 'quartz)
      '(hammerspoon . nil)
    result))

(provide 'hammerspoon)
