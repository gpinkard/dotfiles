;; load actual config file

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/.emacs.d/config.org"))
 '(package-selected-packages
   '(multi-vterm vterm bart-mode cider srcery-theme lsp-pyright kaolin-themes magit minions moody ivy-rich yaml-mode which-key use-package sudo-edit rainbow-mode python-mode projectile org-superstar lsp-ui lsp-ivy go-mode flycheck exec-path-from-shell elfeed dockerfile-mode dashboard counsel company clojure-mode all-the-icons-ivy-rich all-the-icons-dired ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
