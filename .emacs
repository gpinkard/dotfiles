(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
SSL not enabled! Vulnerable to man-in-the-middle attacks!"))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; U S E   P A C K A G E ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; P A C K A G E S ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-mini-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 15)
  :bind (
         ("C-s" . swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x l" . counsel-locate)))

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; open a shell in a minibuffer
;; M-x customize-variable RET shell-pop-shell-type RET <- get to group customization
(use-package shell-pop
  :ensure t
  :config
  (setq shell-pop-shell-type "ansi-term")
  (setq shell-pop-window-height 40)
  :bind (("C-;" . shell-pop)))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode 1)
  (nyan-start-animation))

;; color theme
(use-package dracula-theme
  :ensure t)

;; language specific
(use-package python-mode
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package jsx-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B A S I C   S T U F F;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; complete [ ( , " etc.
(electric-pair-mode t)

;; show matching parens, qoutes, braces, etc.
(show-paren-mode t)

;; highlight current line
(global-hl-line-mode)

;; no tool bar
(tool-bar-mode 0)

;; no menu bar
(menu-bar-mode 0)

;; no scroll bar
(scroll-bar-mode -1)

;; no obnoxious cursor blink
(blink-cursor-mode -1)

;; default shell
(setq-default explicit-shell-file-name "/usr/bin/zsh")

;; enable line numbers when we find a file
(when (version<= "26.0.50" emacs-version )
  (add-hook 'find-file-hook (lambda () (display-line-numbers-mode))))
;; comment this line out if emacs version > 26.0.50
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; B I N D S ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x <return>") 'ansi-term)

;;;;;;;;;;;;;;;;;;;;;;;
;; ---> T A B S ---> ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq correct-tab-width 8)
(setq bad-tab-width 4)
(setq brainlet-tab-width 2)

;; function to disable tabs
(defun disable-tabs () (setq indent-tabs-mode nil))

;; function to enable tabs
(defun enable-tabs ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width correct-tab-width))

;; hook to enable tabs
(add-hook 'prog-mode-hook 'enable-tabs)

;; hook to disable tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; language specific tab 
(setq-default python-indent-offset bad-tab-width) ;; python
(setq-default js-indent-level bad-tab-width) ;; javascrip
(setq-default c-basic-offset correct-tab-width ;; c
  tab-width correct-tab-width
  ;; tab-width 8
  indent-tabs-mode t)
;; fix electric-indent
(setq-default electric-indent-inhibit t)

;; backspace erase tab correctly (instead of one space at a time)
(setq backward-delete-char-untabify-method 'hungry)

;; visualize tabs with "|" character
(setq whitespace-style '(face tabs tab-mark trainling))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "PfEd" :family "Menlo for Powerline"))))
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-desplay-mappings
      '((tab-mark 9 [124 9] [92 9])))
;; (global-whitespace-mode) ;; enable whitespace mode everywhere

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("947190b4f17f78c39b0ab1ea95b1e6097cc9202d55c73a702395fc817f899393" default)))
 '(package-selected-packages (quote (company-mode dracula-theme)))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))))

