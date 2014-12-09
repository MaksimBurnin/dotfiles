;; Packages
(require 'package)
(add-to-list 'package-archives
       '("marmalade" .
         "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Load-path
(add-to-list 'load-path "~/.emacs.d/")

;; Useful stuffs
(require 'nav)
(require 'whitespace)

;; Startup
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq-default left-margin-width 2 right-margin-width 2)
(set-window-margins nil 2 2)
(set-fringe-mode 0)

(setq-default header-line-format
              '("%e"
                mode-line-front-space
                mode-line-buffer-identification
                mode-line-end-spaces))


;; Keybindings
(global-set-key (kbd "C-x /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Color Theme
(defun decide-on-theme (frame)
  (select-frame frame)
  ;;(load-theme 'solarized-light t)
  (load-theme 'solarized-dark t))

(add-hook 'after-make-frame-functions 'decide-on-theme)


;; For erb, less, css
(require 'web-mode)
(require 'less-css-mode)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))

;; Ruby
(add-to-list 'auto-mode-alist '("\\.god\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.spec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))

;; Some other shit
(add-to-list 'auto-mode-alist '("\\.coffee.erb\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.slim\\'" . slim-mode))



;; Flymake

(require 'flymake)
  (require 'flymake-haskell-multi)
  (require 'flymake-less)

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
  (push '(".+\\.rb$" flymake-ruby-load) flymake-allowed-file-name-masks)
  (add-hook 'ruby-mode-hook
                  (lambda () (flymake-mode t)))
  ;; LESS
  (push '(".+\\.less$" flymake-less-load) flymake-allowed-file-name-masks)
  (add-hook 'less-css-mode-hook
                  (lambda () (flymake-mode t)))


;;tabs

  ;;(setq indent-line-function 'insert-space)

(setq tabed-modes  '(makefile-gmake-mode makefile-mode))
(setq wraped-modes '(shell-mode))
(defun set-tabs (usetabs)
  "get rid of tabs. indent with 2 spaces"
  (if (not usetabs)
      (progn (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (add-hook 'before-change-functions
                       (lambda (&rest args)
                         (if (not (buffer-modified-p))
                             (untabify (point-min) (point-max))))))
      (progn (setq indent-tabs-mode t)
             (setq tab-width 4)
             (setq before-change-functions ()))
    )
  )
(defun decide-on-tabs ()
  "Decide if we need tabs in current mode"
  (if (member major-mode tabed-modes)
      (progn (set-tabs t)
             (message "%s %s" major-mode "Tabbed mode"))
      (progn (set-tabs nil)
             (message "%s %s" major-mode "No-Tabs mode"))))

(defun decide-on-wrap ()
  "Decide if we need word-wrap in current mode"
  (let ((truncated (member major-mode wraped-modes)))
    (setq truncate-lines (not truncated))))

(add-hook 'after-change-major-mode-hook 'decide-on-tabs)
(add-hook 'after-change-major-mode-hook 'decide-on-wrap)
(run-mode-hooks)

;;highlight tabs and trailing whitespaces
(global-whitespace-newline-mode 1)
(setq whitespace-display-mappings
      '(
        ;;  (space-mark   ?\     [?.]) ;; use space not dot
        (space-mark   ?\xA0  [?\u00A4]     [?_])
        (space-mark   ?\x8A0 [?\x8A4]      [?_])
        (space-mark   ?\x920 [?\x924]      [?_])
        (space-mark   ?\xE20 [?\xE24]      [?_])
        (space-mark   ?\xF20 [?\xF24]      [?_])
        ;;  (newline-mark ?\n    [?$ ?\n])
        (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-space ((((class color) (background dark)) (:background "red" :foreground "white")) (((class color) (background light)) (:background "yellow" :foreground "black")) (t (:inverse-video t))))
 '(whitespace-tab ((((class color) (background dark)) (:background "red" :foreground "white")) (((class color) (background light)) (:background "yellow" :foreground "black")) (t (:inverse-video t))))
 '(whitespace-trailing ((((class color) (background dark)) (:background "red" :foreground "white")) (((class color) (background light)) (:background "yellow" :foreground "black")) (t (:inverse-video t)))))

(setq whitespace-style (quote (trailing face tabs newline space-mark tab-mark newline-mark lines-tail)))
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
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
