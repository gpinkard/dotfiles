(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "SSL not enabled! Vulnerable to man-in-the-middle attacks!"))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; load actual config file
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (doom-oceanic-next)))
 '(custom-safe-themes
   (quote
    ("7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" default)))
 '(package-selected-packages
   (quote
    (graphql-mode all-the-icons json-mode dashboard dmenu evil linenum-relative linum-relative docker magit spaceline rainbow-mode avy org-bullets go-mode beacon ellocate company-mode)))
 '(powerline-height 21)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "SRC" :family "Hack"))))
 '(org-block ((t (:background "#1b2b34"))))
 '(org-block-begin-line ((t (:background "#1B2B34" :foreground "#FAC863"))))
 '(org-quote ((t (:background "#1B2B34" :slant italic))))
 '(spaceline-highlight-face ((t (:background "#5FB3B3")))))
