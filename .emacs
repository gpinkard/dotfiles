(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "This version of emacs does not support ssl..."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;P A C K A G E S;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extensible vi layer
(use-package evil
  :ensure t
  :init ;; these two for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

;; usefule bindings for evil mode (helm etc)
(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-startup-minibuffer t)
  :init
  (evil-collection-init))

;; helm
(use-package helm
  :ensure t
  :config
  (helm-mode 1))

;; replace with general???
;; vim leader functionality
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
    "k" 'kill-buffer
    "n" 'evil-buffer-new
    "w" 'save-buffer
    "c" 'split-window-horizontally
    "v" 'split-window-vertically
    "r" 'replace-string
    "E" 'eval-last-sexp
    "R" 'reload-emacs-file
    ";" 'shell-pop))

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

;; open a shell in a minibuffer
;; M-x customize-variable RET shell-pop-shell-type RET <- get to group
(use-package shell-pop
  :ensure t)

;; relative line numbers
(use-package linum-relative
  :ensure t
  :init
  (global-linum-mode 1)
  (linum-relative-mode 1))

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

;; color themes
(use-package srcery-theme
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; L A N G U A G E  S P E C I F I C ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python-mode
  :ensure t)

(use-package go-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; ---> T A B S ---> ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq correct-tab-width 8)
(setq bad-tab-width 4)
(setq brainlet-tab-width 2)

;; enable tabs
(defun disable-tabs () (setq indent-tabs-mode nil))
;; disable tabs
(defun enable-tabs ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width correct-tab-width))

;; hook to enable tabs
(add-hook 'prog-mode-hook 'enable-tabs)
;; hook to disable tabs
(add-hook 'lisp-mode-hook 'enable-tabs)
(add-hook 'emacs-lisp-mode-hook 'enable-tabs)

;; language specific tweeks
(setq-default python-indent-offset bad-tab-width)
(setq-default c-bassic-offset 8
	      tab-width 8
	      indent-tabs-mode t)
(setq-default js-indent-level bad-tab-width)

;; make electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; make backspace properly erase tab
(setq backward-delete-char-untabify-method 'hungry)

;; shift width for evil-mode
(setq-default evil-shift-width correct-tab-width)

;; visualize tabs with a '|'
(setq whitespace-style '(face tabs tab-mark trailing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; O T H E R  S T U F F ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-emacs-file ()
  (interactive)
  (load-file "~/.emacs"))

;; stop making backups everywhere
(setq make-backup-files nil)

;; no annoying startup screen
(setq inhibit-startup-screen t)

;; complete parens, curly braces, quotation marks, etc.
(electric-pair-mode t)

;; show matching ( { [ " etc.
(show-paren-mode t)

;; until zsh output gets fixed...
(setq explicit-shell-file-name "/bin/bash")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (srcery)))
 '(custom-safe-themes
   (quote
    ("9d36e7cbea9ab075fa1920275cbde349f5b80c9b901500d296856142b32c7516" default)))
 '(evil-collection-startup-minibuffer t t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (srcery-theme go-mode python-mode sublime-themes company flycheck linum-relative shell-pop telephone-line evil-leader helm evil-collection evil use-package)))
 '(scroll-bar-mode nil)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-window-size 35)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "DAMA" :family "Inconsolata")))))
