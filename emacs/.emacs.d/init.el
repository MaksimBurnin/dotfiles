(provide 'init)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa")
  (require 'use-package))

;; Generated
(use-package bind-key)
(use-package drag-stuff)
(use-package exec-path-from-shell)
(use-package expand-region)
(use-package flycheck)
(use-package idle-highlight-mode)
(use-package popwin)
(use-package smex)

;;  Neat stuff
(use-package nyan-mode)

;; Code style
(use-package editorconfig)
(use-package fill-column-indicator)

;; System-related
(use-package exec-path-from-shell)
(use-package with-editor)

;; Auto-compleate, search
(use-package auto-complete)
(use-package ac-helm)
(use-package helm)
(use-package helm-core)
(use-package helm-projectile)
(use-package projectile)
(use-package smartparens
  :config
  (smartparens-global-mode 0)
  (show-smartparens-global-mode t)
  )

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

;; Language modes
(use-package web-mode)
(use-package fish-mode)
(use-package ghci-completion)
(use-package gnuplot-mode)
(use-package haml-mode)
(use-package handlebars-mode)
(use-package haskell-mode)
(use-package less-css-mode)
(use-package lua-mode)
(use-package ruby-mode)
(use-package ruby-tools)
(use-package scss-mode)
(use-package sass-mode)
(use-package slim-mode)
(use-package yaml-mode)
(use-package jade-mode)
(use-package php-mode)
(use-package typescript-mode)
(use-package rust-mode)

;; Checkers-linters
(use-package flymake-cursor)
(use-package flymake-easy)
(use-package flymake-haml)
(use-package flymake-haskell-multi)
(use-package flymake-lua)
(use-package flymake-ruby)
(use-package flymake-sass)



;; Mac-OS Env variables
(when (memq window-system '(mac N's))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-en-vs '("PS1" "LANG" "PATH")))
(setq exec-path
  (append exec-path
    '("~/.rbenv/shims" "/usr/local/bin" "/bin" "/bin" "/usr/bin")))

;; Backups and lockfiles
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; Useful stuffs
(require 'whitespace)

(editorconfig-mode 1)
(projectile-global-mode)
(helm-mode 1)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(ac-config-default)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Smex. Upgraded M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Startup
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(nyan-mode t)
(setq-default left-margin-width 0 right-margin-width 0)
(set-window-margins nil 0 0)
(set-fringe-mode 10)

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(setq-default header-line-format
              '("%e"
                mode-line-front-space
                mode-line-buffer-identification
                mode-line-end-spaces))


;; Keybindings
(global-set-key (kbd "C-x /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; For erb, less, css
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))


(defun web-mode-indents-hook ()
  (setq web-mode-block-padding 0)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing nil)

  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

  ;; (setq web-mode-markup-indent-offset 2)
  ;; (setq web-mode-code-indent-offset 2)
  ;; (setq web-mode-css-indent-offset 2)
)

(add-hook 'web-mode-hook  'web-mode-indents-hook)

(add-hook 'editorconfig-custom-hooks
          (lambda (hash) (setq web-mode-block-padding 0)))

;; (setq css-indent-offset 2)
(setq scss-compile-at-save nil)


;; Ruby
(add-to-list 'auto-mode-alist '("\\.god\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.spec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))

;; Some other shit
(add-to-list 'auto-mode-alist '("\\.coffee.erb\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.slim\\'" . slim-mode))

;; Haskell
(setq haskell-process-type 'ghci)

;; PHP
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;; Flymake
(require 'flymake)
  (require 'flymake-haskell-multi)

  (add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

  ;; XSL
  (push '(".+\\.xsl$" flymake-xml-init) flymake-allowed-file-name-masks)
  (add-hook 'xsl-mode-hook
                  (lambda () (flymake-mode t)))
  ;; PHP
  (push '(".+\\.php$" flymake-php-init) flymake-allowed-file-name-masks)
  (add-hook 'php-mode-hook
                  (lambda () (flymake-mode t)))
  ;; RUBY
  ;; (push '(".+\\.rb$" flymake-ruby-load) flymake-allowed-file-name-masks)
  ;; (add-hook 'ruby-mode-hook
  ;;                 (lambda () (flymake-mode t)))
  ;; SCSS
  (push '(".+\\.scss$" flymake-scss-init) flymake-allowed-file-name-masks)
  (add-hook 'less-css-mode-hook
                  (lambda () (flymake-mode t)))


;;tabs

(setq wraped-modes '(shell-mode))

(defun decide-on-wrap ()
  "Decide if we need word-wrap in current mode"
  (let ((truncated (member major-mode wraped-modes)))
    (setq truncate-lines (not truncated))))

;; (add-hook 'after-change-major-mode-hook 'decide-on-tabs)
(add-hook 'after-change-major-mode-hook 'decide-on-wrap)
(run-mode-hooks)

;;highlight tabs and trailing whitespaces
(global-whitespace-newline-mode 1)
(setq whitespace-display-mappings
      '(
        ;;(space-mark   ?\     [?.]) ;; use space not dot
        (space-mark   ?\xA0  [?\u00A4]     [?_])
        (space-mark   ?\x8A0 [?\x8A4]      [?_])
        (space-mark   ?\x920 [?\x924]      [?_])
        (space-mark   ?\xE20 [?\xE24]      [?_])
        (space-mark   ?\xF20 [?\xF24]      [?_])
        ;;(newline-mark ?\n    [?$ ?\n])
        (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-source-header ((t (:background "#e9e2cb" :foreground "#52676f" :height 1.3 :family "Sans Serif"))))
 '(whitespace-space ((((class color) (background dark)) (:background "red" :foreground "white")) (((class color) (background light)) (:background "yellow" :foreground "black")) (t (:inverse-video t))))
 '(whitespace-tab ((((class color) (background dark)) (:background "red" :foreground "white")) (((class color) (background light)) (:background "yellow" :foreground "black")) (t (:inverse-video t))))
 '(whitespace-trailing ((((class color) (background dark)) (:background "red" :foreground "white")) (((class color) (background light)) (:background "yellow" :foreground "black")) (t (:inverse-video t)))))

(setq whitespace-style (quote (trailing face tabs newline space-mark tab-mark newline-mark)))
(setq show-trailing-whitespace t)
(global-whitespace-mode 1)

;;save all temp files to sys tmp directory
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
