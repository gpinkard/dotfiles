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

;; flycheck (linter)
(use-package flycheck
	:ensure t
	:init
	(global-flycheck-mode))

;; autocomplete
(use-package company
        :ensure t
        :config
        (add-hook 'after-init-hook 'global-company-mode))

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
        (setq telephone-line-primary-left-separator 'telephone-line-halfcos-left
                telephone-line-secondary-left-separator 'telephone-line-halfcos-hollow-left
                telephone-line-primary-right-separator 'telephone-line-halfcos-right
                telephone-line-secondary-right-separator 'telephone-line-halfcos-hollow-right)
        (setq telephone-line-height 50)
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
                "r" 'replace-string
                "E" 'eval-last-sexp
                "TAB" 'evil-buffer-new
                "t" 'ansi-term
                ";" 'shell-pop))

;; open shell in split window
;; M-x customize-variable RET shell-pop-shell-type RET <- get to group
(use-package shell-pop
        :ensure t)

;; relative line numbers (like in vim)
(use-package linum-relative
        :ensure t
        :init
        (global-linum-mode 1)
        (linum-relative-mode 1))

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

;;;; O T H E R   S T U F F ;;;;

;; --> TABS --> ;;
(setq correct-tab-width 8)
(setq bad-tab-width 4)
(setq stoopid-tab-width 2)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
        (local-set-key (kbd "TAB") 'tab-to-tab-stop)
        (setq indent-tabs-mode t)
        (setq tab-width correct-tab-width))

;; Hooks to Enable Tabs
(add-hook 'prog-mode-hook 'enable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'enable-tabs)
(add-hook 'emacs-lisp-mode-hook 'enable-tabs)

;; Language-Specific Tweaks
;; Python
(setq-default python-indent-offset bad-tab-width)

;;(setq-default c-basic-offset)
(setq-default c-basic-offset 8
        tab-width 8
        indent-tabs-mode t)

;; Javascript
(setq-default js-indent-level bad-tab-width)

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; Shift width for evil
(setq-default evil-shift-width correct-tab-width)

;; Visualize tabs with a '|'
(setq whitespace-style '(face tabs tab-mark trailing))

;; stop emacs from making annoying backups everywhere
(setq make-backup-files nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1C1B19" :foreground "#FCE8C3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :foundry "CYEL" :family "Iosevka"))))
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
        '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere
; END TABS CONFIG

;; no annoying startup screen
(setq inhibit-startup-screen t)

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
;; (global-display-line-numbers-mode)

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
 '(package-selected-packages (quote (flycheck shell-pop use-package)))
 '(scroll-bar-mode nil)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 45)
 '(tool-bar-mode nil))
