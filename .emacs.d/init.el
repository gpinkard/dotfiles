;; load actual config file

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages
   '(solarized-theme brutalist-theme dockerfile-mode kaolin-themes symon symon-mode ace-window org-superstar all-the-icons-ivy-rich ivy-rich all-the-icons-dired all-the-icons rustic flycheck-rust rust-mode flycheck python-mode company lsp-ui lsp-ivy lsp-mode clojure-mode counsel swiper ivy tao-theme dashboard projectile rainbow-mode sudo-edit which-key exec-path-from-shell use-package)))
