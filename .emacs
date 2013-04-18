(setq buffer-auto-save-file-name nil)
(setq make-backup-files nil)
(setq-default truncate-lines t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq js-indent-level 4)
(setq scroll-preserve-screen-position t)
(setq c-basic-offset 4)

(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/code/.emacs_autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/code/.emacs_backups/"))))

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path "~/.emacs.d")

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(when (file-exists-p "/etc/paths")
  (defun read-system-path ()
    (with-temp-buffer
      (insert-file-contents "/etc/paths")
      (goto-char (point-min))
      (replace-regexp "\n" ":")
      (thing-at-point 'line)))
  (setenv "PATH" (read-system-path)))
(setq exec-path (split-string (getenv "PATH") path-separator))

(when window-system
  (set-scroll-bar-mode 'right)
  (tool-bar-mode -1)

  (add-to-list 'load-path "~/.emacs.d/autocomplete")
  (require 'auto-complete-config)		
  (add-to-list 'ac-dictionary-directories "/home/andrew/.emacs.d/autocomplete/ac-dict")
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil)

  (require 'tabbar)
  (tabbar-mode t)
  (defun tabbar-buffer-groups ()
    "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
    (list
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs Buffer"
       )
      ((and 
	(> (length (buffer-name)) 23)
	(or
	 (string-equal "mumamo-fetch-major-mode" (substring (buffer-name) 0 23))
	 (string-equal "template-indent-buffer" (substring (buffer-name) -22)))
	"Emacs Buffer"
	))
      ((eq major-mode 'dired-mode)
       "Dired"
       )
      (t
       "User Buffer"
       )
      ))) 


  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

  (setq speedbar-mode-hook '(lambda ()
			      (interactive)
			      (other-frame 0)))
;  (speedbar 1)


)

(defvar prelude-packages
  '(clojure-mode paredit nrepl)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;(require 'jslint)

;(require 'autopair)
;(autopair-global-mode)

;(require 'auto-pair+)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;(autoload 'python-mode "python-mode" "Python Mode." t)
;(setq python-mode-hook
;      '(lambda () (progn
;                    (set-variable 'py-indent-offset 4)
;                    (set-variable 'py-smart-indentation nil)
;                    (set-variable 'indent-tabs-mode nil) )))
(add-hook 'python-mode-hook
          '(lambda ()
                   (flymake-mode)))

(require 'whitespace)

; For chef
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;(load-file "~/.emacs.d/cedet-1.0/common/cedet.el")
;(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;(load "~/.emacs.d/nxhtml/autostart.el")
(setq mumamo-background-colors nil)
;(require 'jinja)
(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))

;; Mumamo is making emacs 23.3 freak out:
(when (and (equal emacs-major-version 23)
           (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function)))

;(ido-mode t)
;(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

(require 'textmate)
(textmate-mode)

;(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
;(require 'ecb)

(require 'less-css-mode)
(setq less-css-lessc-command (expand-file-name "~/node_modules/less/bin/lessc"))
(setq less-css-compile-at-save t)

(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)

(setq coffee-tab-width 2)
(setq coffee-js-mode 'js-mode)
(setq coffee-args-compile '("-c")) ; Send output up one directory

(defun coffee-custom ()
  "coffee-mode-hook"

  ;; Compile '.coffee' files on every save
  (and (file-exists-p (buffer-file-name))
       (coffee-cos-mode t))
)

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(require 'flymake-jslint)
;(defun js-mode-flymake-hook ()
;  (cond 
;   ((string-equal "ml" (substring (buffer-file-name) -2 nil)) nil )
;   (t (flymake-mode t)) )
;  )
;(add-hook 'js-mode-hook 'js-mode-flymake-hook)

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list temp-file))))
  
   (add-to-list 'flymake-allowed-file-name-masks
                '("\\.py\\'" flymake-pylint-init))
  
 )

(add-to-list 'load-path "~/.emacs.d/ess/lisp")
(require 'ess-site)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;'(ecb-display-default-dir-after-start ~/code)
 ;'(ecb-layout-window-sizes nil)
 ;'(ecb-options-version "2.40")
 ;'(ecb-source-path (quote (("/home/andrew/code/nimbus" "nimbus"))))
 '(kill-whole-line t)  
 '(x-select-enable-clipboard t)
 '(frame-background-mode (quote dark))
 '(inhibit-startup-screen t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
)

(when window-system
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#070606" :foreground "#eadbcd" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "Inconsolata"))))
 '(cursor ((t (:background "#ff0099"))))
 '(font-lock-comment-face ((t (:foreground "#5f7189"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ff9966"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#6699ff"))))
 '(font-lock-string-face ((t (:foreground "#92d170"))))
 '(link ((nil (:foreground "blue"))))
 '(region ((nil (:background "#550022"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "pink" :foreground "black")))))
)

;(defadvice isearch-search (after isearch-no-fail activate)
;  (unless isearch-success
;    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
;    (ad-activate 'isearch-search)
;    (isearch-repeat (if isearch-forward 'forward))
;    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
;    (ad-activate 'isearch-search)))

(global-set-key (kbd "C-S-z") 'undo)
(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-v") 'yank)
(global-set-key (kbd "C-q") 'query-replace)
(put 'downcase-region 'disabled nil)
