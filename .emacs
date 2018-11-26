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
  (telephone-line-mode 1))

;; File explorer menu (like NERDTree)
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-show-hidden-files t)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

;; perty icons for neo-tree
;; run M-x all-the-icons-install-fonts on fresh emacs installation
(use-package all-the-icons
  :ensure t)

;; replace with general???
(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b" 'buffer-menu
    "k" 'kill-buffer
    "t" 'evil-buffer-new
    "n" 'neotree-toggle
    ";" 'shell-pop))

;; open shell in split window
;; M-x customize-variable RET shell-pop-shell-type RET <- get to group
(use-package shell-pop
  :ensure t)

;; colorscheme (monokai)
(use-package monokai-theme
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
(setq default-tab-width 7)

;; vim like scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; complete parens, curly braces, quotation marks, etc.
(electric-pair-mode t)

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
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (shell-pop use-package)))
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
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 124 :width normal :foundry "SRC" :family "Fira Code")))))
