(require 'package)

;; repos
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;P A C K A G E S ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use-package
(eval-when-compile
  (require 'use-package))
;; make sure everything is installed on startup
(setq use-package-always-ensure t)

;; evil-mode (VI in Emacs :o)
(use-package evil
  :ensure t
  :config
  (evil-mode t))

;; vim-like tabs
(use-package evil-tabs
  :ensure t
  :config
  (global-evil-tabs-mode t))

;; pretty bullet points for org mode
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; powerline bar (telephone line)
(use-package telephone-line
  :ensure t
  :init
  (setq telephone-line-height 22)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-erc-modified-channels-segment))
	  (nil    . (telephone-line-major-mode-segment
		     telephone-line-minor-mode-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment
		     telephone-line-buffer-segment))
          (accent . (telephone-line-vc-segment
		     telephone-line-process-segment))
          (evil   . (telephone-line-airline-position-segment))))
  ;; shapes go here
  (setq telephone-line-primary-left-separator 'telephone-line-tan-left
      telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
      telephone-line-primary-right-separator 'telephone-line-tan-right
      telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)
  (telephone-line-mode 1))

;; neo-tree (file tree)
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-show-hidden-files t)
  :init
  (global-set-key [f8] 'neotree-toggle)
  (add-hook 'neotree-mode-hook (lambda ()
	    (define-key evil-normal-state-local-map (kbd "C-n") 'neotree-toggle)
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "+") 'neotree-hidden-file-toggle))))

;;(require 'all-the-icons)
(use-package all-the-icons
  :ensure t)

;; shell-pop (shell in emacs)
(use-package shell-pop
  :ensure t
  :init
  (global-set-key [f9] 'shell-pop))

;; line numbers (nlinum package)
(use-package nlinum
  :ensure t
  :init
  (global-nlinum-mode))

;; auto-complete
;; (ac-config-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; O T H E R  C O N F I G U R A T I O N S ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; finish parens, quotes, curly braces etc.
(electric-pair-mode t)

;; no startup screen
(setq inhibit-startup-screen t)

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("78496062ff095da640c6bb59711973c7c66f392e3ac0127e611221d541850de2" "6a23db7bccf6288fd7c80475dc35804c73f9c9769ad527306d2e0eada1f8b466" "081d0f8a263358308245355f0bb242c7a6726fc85f0397d65b18902ea95da591" "021720af46e6e78e2be7875b2b5b05344f4e21fad70d17af7acfd6922386b61e")))
 '(horizontal-scroll-bar-mode nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (go-mode atom-one-dark-theme evil-tabs use-package all-the-icons shell-pop neotree auto-complete nlinum telephone-line org-bullets org-link-minor-mode evil-visual-mark-mode)))
 '(scroll-bar-mode nil)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-size 40)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "UW" :family "UW Ttyp0"))))
 '(mode-line-buffer-id-inactive ((t (:inherit mode-line-buffer-id :height 1.0)))))
