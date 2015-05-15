;;; Using the modified jslint from http://whereisandy.com/code/jslint/
;;; and spidermonkey, easy linting of js files,
;;;
;;; M-j calls jslint and outputs to a split window. Within that
;;; window M-j will find the next lint issue and jump to that
;;; line in the file
(setq *jslint-target-buffer* nil)
(defun jslint ()
  (interactive)
  (let ((jsbuffer-name "*jslint-minibuffer*"))
    (if (string= (buffer-name) jsbuffer-name)
        (progn
          (if (re-search-forward "Lint at line \\([0-9]+\\)")
              (let ((num (match-string 1)))
                (goto-line (string-to-number num) *jslint-target-buffer*))))
      (let* ((filename (buffer-file-name))
             (suffix (downcase (file-name-extension filename))))
        (setq *jslint-target-buffer* (current-buffer))
        (if (and filename (string= suffix "js"))
              (let ((result (shell-command-to-string (concat (substitute-in-file-name "$HOME") "/.emacs.d/jslint-custom < " filename))))
                (if (string= result "jslint: No problems found.\n")
                    (progn
                      (message "jslint: No problems found.")
                      (if (get-buffer jsbuffer-name)
                          (kill-buffer (get-buffer jsbuffer-name))))
                  (let ((buff (get-buffer-create jsbuffer-name)))
                    (switch-to-buffer-other-window buff)
                    (erase-buffer)
                    (princ result buff)
                    (goto-char 0)
                    ))))
          (message "Warning: Not a file buffer or a .js file.")))))

(define-key global-map "\M-j" 'jslint)

;;;auto check on save of a javascript file
(defun jslint-hook ()
  (let* ((filename (buffer-file-name))
         (suffix (downcase (file-name-extension filename))))
    (if (and filename (string= suffix "js"))
        (jslint))))
(add-hook 'after-save-hook 'jslint-hook)

(provide 'jslint)