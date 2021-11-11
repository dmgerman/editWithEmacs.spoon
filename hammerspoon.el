;; Emacs code required to communicate with Lisp

(defvar hammerspoon-buffer "_hs_edit")


(defun hammerspoon-do (command)
  (interactive "sHammerspoon Command:")
  "execute given command in hammerspoon"
  (setq hs-binary (executable-find "hs"))
  (if hs-binary 
      (call-process hs-binary
       nil 0 nil
       "-c"
       command)
    (message "hammerspoon hs executable not found. Make sure you hammerspoon has loaded the ipc module")
    ))


(defun hammerspoon-alert (message)
  "shows hammerspoon alert popup with a message"
  (hammerspoon-do (concat "hs.alert.show('" message "', 1)")))

;; use this to test that hammerspoon responds to emacs
(defun hammerspoon-test ()
    (interactive)
  (hammerspoon-alert "This is the end of the world..."))

(defun hammerspoon-edit-end ()
  "Util function to use to send edited text back to hammerspoon."
  (interactive)
   ;; kill the buffer to clipboard
   ;; since we do not kill the buffer, we can always 
   ;; undo to recover the text
   (mark-whole-buffer)
   (call-interactively 'kill-ring-save)
   (hammerspoon-do (concat "emacs_sends_back(False)"))
   (previous-buffer)
  )

(defvar hammerspoon-edit-minor-map nil
  "Keymap used in hammer-edit-minor-mode.")

(unless hammerspoon-edit-minor-map
(let ((map (make-sparse-keymap)))

(define-key map (kbd "C-c C-c")   'hammerspoon-edit-end)
(define-key map (kbd "C-c h")     'hammerspoon-test) ;; for testing

(setq hammerspoon-edit-minor-map map)))

(define-minor-mode hammerspoon-edit-minor-mode
  "my minor mode to help edit with hammerspoon"
  
  :global nil
  :lighter   "_hs-edit_"    ; lighter
  :keymap hammerspoon-edit-minor-map             ; keymap
  
  ;; if disabling `undo-tree-mode', rebuild `buffer-undo-list' from tree so
  ;; Emacs undo can work
  )

(defun hammerspoon-edit-begin ()
  "Util function for use with hammerspoon to edit text. This function is expected to be used by hammerspoon."
  (interactive)
  (let ((hs-edit-buffer (get-buffer-create hammerspoon-buffer))
        )
    (switch-to-buffer hs-edit-buffer)
    (erase-buffer)
    (yank)
    (org-mode)
    (hammerspoon-edit-minor-mode)
    (exchange-point-and-mark)
    ))
