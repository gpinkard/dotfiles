(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; P A C K A G E S ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; evil-mode (VI layer)
(use-package evil
  :ensure t
  :config
  (evil-mode t))

;;(use-package evil-org
;;  :ensure t
;;  :after org
;;  :config
;;  (add-hook 'org-mode-hook 'evil-org-mode)
;;  (add-hook 'evil-org-mode-hook
;;            (lambda ()
;;              (evil-org-set-key-theme)))
;;  (require 'evil-org-agenda)
;;  (evil-org-agenda-set-keys))

;; vim-like tabs
(use-package evil-tabs
  :ensure t
  :config
  (global-evil-tabs-mode t))

;; Ivy (completion interface thingy)
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  (setq ivy-count-format "(%d/%d) "))

;; replace with general???
(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f" 'counsel-grep-or-swiper
    "b" 'switch-to-buffer
    "k" 'kill-buffer
    "t" 'evil-buffer-new
    ";" 'shell-pop))

;; line numbers
(use-package nlinum
  :ensure t
  :init
  (global-nlinum-mode))

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

;; magit (git plugin)
;;(use-package evil-magit
;;  :ensure t)

;; shell-pop (open shell in minibuffer)
(use-package shell-pop
  :ensure t)

(use-package python-mode
  :ensure t)

(use-package go-mode
  :ensure t)

;;(use-package markdown-mode
;;  :ensure t)

;;;; OTHER SETTINGS ;;;;

;; no annoying welcome to emacs start up screen
(setq inhibit-startup-screen t)

;; vim like scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; finish parens, qutes, curly braces, etc.
(electric-pair-mode t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("8a9be13b2353a51d61cffed5123b157000da0347c252a7a308ebc43e16662de7" default)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (org-evil markdown-mode gruvbox-theme evil-magit helm use-package telephone-line shell-pop org-bullets nlinum neotree evil-tabs all-the-icons)))
 '(scroll-bar-mode nil)
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-window-position "right")
 '(shell-pop-window-size 40)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 218 :width normal :foundry "PfEd" :family "Monaco for Powerline")))))
