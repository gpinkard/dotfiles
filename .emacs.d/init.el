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
 '(custom-enabled-themes (quote (doom-oceanic-next)))
 '(custom-safe-themes
   (quote
    ("7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "285efd6352377e0e3b68c71ab12c43d2b72072f64d436584f9159a58c4ff545a" default)))
 '(org-agenda-files (quote ("~/.emacs.d/config.new.org")))
 '(package-selected-packages
   (quote
    (doom-modeline exwm csv-mode evil-mode crystal-mode symon dired-collapse dired-subtree markdown-mode counsel-projectile projectile exec-path-from-shell doom-themes gruvbox-theme ample-theme neotree pretty-mode srcery-theme popup-kill-ring kill-ring toml-mode lua-mode sudo-edit graphql-mode all-the-icons json-mode dashboard dmenu evil linenum-relative linum-relative docker magit spaceline rainbow-mode avy org-bullets go-mode beacon ellocate company-mode)))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))))
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#1B2B34" :foreground "#D8DEE9" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(org-block ((t (:background "#1B2B34"))))
 '(org-block-begin-line ((t (:background "#1B2B34" :foreground "#65737E"))))
 '(org-quote ((t (:background "#1B2B34" :slant italic))))
 '(region ((t (:background "gray30"))))
 '(secondary-selection ((t (:background "#1B2B34")))))
