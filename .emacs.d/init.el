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
 '(custom-enabled-themes (quote (doom-solarized-light)))
 '(custom-safe-themes
   (quote
    ("08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" default)))
 '(package-selected-packages
   (quote
    (dired-rainbow dockerfile-mode elfeed vterm kubernetes rust-mode doom-modeline exwm csv-mode evil-mode crystal-mode symon dired-collapse dired-subtree markdown-mode counsel-projectile projectile exec-path-from-shell doom-themes pretty-mode popup-kill-ring kill-ring toml-mode lua-mode sudo-edit graphql-mode all-the-icons json-mode dashboard dmenu linenum-relative linum-relative docker magit rainbow-mode avy org-bullets go-mode beacon ellocate company-mode)))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))))

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#FDF6E3" :foreground "#556b72" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :foundry "nil" :family "Terminus (TTF)")))))
