(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; uncomment this line for melpa stable, if done, make sure to comment line above
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;;; P A C K A G E S ;;;;

;; use-package installs plugins automatically (M-x package-install use-package)
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; evil-mode (VI layer)
(use-package evil
  :ensure t
  :init ;; these two for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

;; useful bindings for evil mode (vi bindings for helm)
(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

;; helm
(use-package helm
  :ensure t
  :config
  (helm-mode 1))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'arrow 'arrow))
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

;; telephone-line (like vim powerline)
(use-package telephone-line
  :ensure t
  :init
  (setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))
  (setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))
  (setq telephone-line-primary-left-separator 'telephone-line-identity-left
      telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
      telephone-line-primary-right-separator 'telephone-line-identity-right
      telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)
  (setq telephone-line-height 25)
  (telephone-line-mode 1))

;; replace with general???
(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'helm-M-x
    "f" 'helm-find-files
    "b" 'helm-buffers-list
    "/" 'helm-occur
    "n" 'neotree-toggle
    "k" 'kill-buffer
    "w" 'save-buffer
    "t" 'evil-buffer-new
    ";" 'shell-pop))

;; open shell in split window
;; M-x customize-variable RET shell-pop-shell-type RET <- get to group
(use-package shell-pop
  :ensure t)

;; colorscheme
(use-package srcery-theme 
  :ensure t)

;; language specific modes
(use-package python-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; set c indentation to 8
(setq-default c-basic-offset 8)
(setq-default go-basic-offset 8)
(setq-default java-basic-offset 4)
(setq-default python-basic-offset 4)
(setq-default javascript-basic-offset 4)

;;;; O T H E R   S T U F F ;;;;

;; no annoying startup screen
(setq inhibit-startup-screen t)

;; make emacs use tabs instead of brainlet spaces
;;(setq tab-to-tab-stop)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(setq default-tab-width 8)

;; vim like scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; complete parens, curly braces, quotation marks, etc.
(electric-pair-mode t)

;; show matching ( { [ < etc.
(show-paren-mode t)

;; highlight current line
(global-hl-line-mode)

;; line numbers
(global-display-line-numbers-mode)

;; utf-8 stuff
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (shell-pop use-package)))
 '(scroll-bar-mode nil)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 45)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1C1B19" :foreground "#FCE8C3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "ADBO" :family "Inconsolata")))))
