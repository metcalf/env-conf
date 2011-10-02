(require 'flymake)

(defun flymake-js-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
         (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "~/.emacs.d/flymake-jslint" (list local-file))
    )
  )

(add-to-list 'flymake-allowed-file-name-masks
                '("\\.js\\'" flymake-js-init))
(add-to-list 'flymake-allowed-file-name-masks
                '("\\.json\\'" flymake-js-init))

(setq flymake-err-line-patterns (cons '("^\\([0-9]+\\),\\([0-9]+\\),\\(.+\\)$"  
					nil 1 2 3) flymake-err-line-patterns))

(provide 'flymake-jslint)