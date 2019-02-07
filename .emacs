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

;; install all plugins in this file
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; evil-mode (VI layer)
(use-package evil
  :ensure t
  :config
  (evil-mode t))

;; ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-height 15)
  (setq use-ivy-virtual-buffers t)
  (setq ivy-count-format "(%d/%d)"))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t)

;; perty icons
;; run M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
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
  (setq telephone-line-height 60)
  (telephone-line-mode 1))

;; replace with general???
(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'counsel-M-x
    "f" 'counsel-find-file
    "b" 'list-buffers
    "n" 'neotree-toggle
    "k" 'kill-buffer
    "w" 'save-buffer
    "t" 'evil-buffer-new
    ";" 'shell-pop))

;; open shell in split window
;; M-x customize-variable RET shell-pop-shell-type RET <- get to group
(use-package shell-pop
  :ensure t)

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

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
 '(package-selected-packages (quote (all-the-icons-dired shell-pop use-package)))
 '(scroll-bar-mode nil)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-window-position "right")
 '(shell-pop-window-size 45)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1C1B19" :foreground "#FCE8C3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 240 :width normal :foundry "CYRE" :family "Source Code Pro")))))
