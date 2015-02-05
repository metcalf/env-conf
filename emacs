(setq buffer-auto-save-file-name nil)
(setq make-backup-files nil)
(setq-default truncate-lines t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq js-indent-level 2)
(setq scroll-preserve-screen-position t)
(setq c-basic-offset 4)
(setq ruby-deep-indent-paren nil)
(setq tab-width 4)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "linux")
(setq whitespace-style '(tab-mark))
(setq column-marker-1-face '((t (:background "gray15"))))
(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-block-padding 2)
(setq require-final-newline t)
(setq use-dialog-box nil)

(global-auto-revert-mode t)

(define-coding-system-alias 'ascii-8bit 'iso-latin-1)

(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs_autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs_backups/"))))

(defalias 'yes-or-no-p 'y-or-n-p)

(setenv "GOPATH" (concat (getenv "HOME") "/code/go"))

(setq load-path
      (append
       '("~/.emacs.d"
         "/usr/local/go/misc/emacs/"
         (concat (getenv "GOPATH") "/src/github.com/nsf/gocode/emacs/"))
       load-path))

(require 'cl)
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defvar prelude-packages
  '(coffee-mode clojure-mode paredit nrepl scala-mode2 web-mode flycheck
                auto-complete exec-path-from-shell puppet-mode markdown-mode)
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


(let ((ensime-path "/usr/local/share/ensime/elisp"))
  (if (file-accessible-directory-p ensime-path)
      (progn
        (add-to-list 'load-path ensime-path)
        (require 'ensime)
        (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))
    (warn "Please download ensime to %s" ensime-path)))

(require 'rbenv)
(global-rbenv-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-highlighting-mode 'lines)

(autoload 'octave-mode "octave-mod" nil t)

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(require 'yaml-mode)

(require 'whitespace)
(global-whitespace-mode)

(require 'column-marker)
(add-hook 'after-change-major-mode-hook (lambda () (interactive) (column-marker-1 80)))

(require 'textmate)
(textmate-mode)

(require 'less-css-mode)
;(setq less-css-lessc-command (expand-file-name "~/node_modules/less/bin/lessc"))
(add-to-list 'less-css-lessc-options (concatenate 'string "--include-path=" (expand-file-name "~/code/bootstrap/less")))
(setq less-css-compile-at-save t)

(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)

(require 'go-autocomplete)

(add-hook 'go-mode-hook
  (function (lambda ()
              (set (make-local-variable 'whitespace-style) '(empty)))))

;(load "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
;(add-hook 'go-mode-hook 'go-oracle-mode)

(setq coffee-tab-width 2)
(setq coffee-js-mode 'js-mode)

(defun coffee-custom ()
  "coffee-mode-hook"
  ;; Compile '.coffee' files on every save
  (and (file-exists-p (buffer-file-name))
       (coffee-cos-mode t))
)

;(add-hook 'coffee-mode-hook
;  '(lambda() (coffee-custom)))

(setq auto-mode-alist
      (append
       '(("\\.txt$" . text-mode)
         ("\\.text$" . text-mode)
         ("\\.rdoc$" . text-mode)
         ("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)
         ("\\.?emacs$" . emacs-lisp-mode)
         ("Vagrantfile$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode) ; Rack
         ("\\.m$" . octave-mode)
         ("\\.erb$" . web-mode)
         ("\\.ejs$" . web-mode))
       auto-mode-alist))

(setq web-mode-engines-alist
      '(("underscorejs"    . "\\.ejs\\'")))

(add-hook `text-mode-hook 'turn-on-visual-line-mode)

(defun mode-aware-delete-trailing-whitespace ()
  (unless (member major-mode '(markdown-mode))
    (delete-trailing-whitespace)
  ))

(add-hook 'before-save-hook 'mode-aware-delete-trailing-whitespace)

(defun strip-trailing-newlines (str)
  (replace-regexp-in-string "\n$" "" str))

(defun sh-cwd (working-directory command)
  (shell-command-to-string (concat "cd " working-directory " && " command)))

(defun am-git-open (repo-path)
  (interactive (list (ido-read-directory-name "GIT Repo: ")))
  (let* ((repo-base-path (file-name-as-directory
			  (strip-trailing-newlines
			   (sh-cwd repo-path "git rev-parse --show-toplevel"))))
	 (changed-files (mapcar (lambda (file) (concat repo-base-path (strip-trailing-newlines file)))
				(split-string
				 (strip-trailing-newlines
				  (sh-cwd repo-base-path "git diff $(git merge-base master HEAD) HEAD --name-only"))
				 "[\r\n]+"))))
    (progn
      (dolist (path (cdr changed-files))
	(find-file-noselect path))
      (unless (string= repo-base-path (car changed-files))
	(find-file (car changed-files))))))


(defun kill-all-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc
   (lambda (x) (if (buffer-file-name x)
		 (kill-buffer x)))
   (buffer-list)))

;(add-to-list 'load-path "~/.emacs.d/ess/lisp")
;(require 'ess-site)

(when window-system
  (set-scroll-bar-mode 'right)
  (tool-bar-mode -1)
  (require 'auto-complete-config)
  (ac-config-default)
  (setq create-lockfiles nil)
)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(kill-whole-line t)
 '(x-select-enable-clipboard t)
 '(frame-background-mode (quote dark))
 '(inhibit-startup-screen t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil
			 :stipple nil
			 :background "#070606"
			 :foreground "#eadbcd"
			 :inverse-video nil
			 :box nil
			 :strike-through nil
			 :overline nil
			 :underline nil
			 :slant normal
			 :weight normal
			 :height 140
			 :width normal
			 :family "Source Code Pro"))))
 '(cursor ((t (:background "#ff0099"))))
 '(font-lock-comment-face ((t (:foreground "#5f7189"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ff9966"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#6699ff"))))
 '(font-lock-string-face ((t (:foreground "#92d170"))))
 '(link ((nil (:foreground "blue"))))
 '(region ((nil (:background "#550022"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "pink" :foreground "black")))))

(global-set-key (kbd "C-S-z") 'undo)
(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-v") 'yank)
(global-set-key (kbd "C-q") 'query-replace)
(put 'downcase-region 'disabled nil)

(load "server")
(unless (server-running-p) (server-start))
