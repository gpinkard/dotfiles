;; load actual config file

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#14141e" "#e84c58" "#35BF88" "#dbac66" "#4ca6e8" "#c79af4" "#6bd9db" "#e6e6e8"])
 '(package-selected-packages
   '(clojure-mode lsp-metals elixir-mode org-superstar lsp-jedi company-box company lsp javascript-mode rego-mode lsp-ivy lsp-ui lsp-mode k8s-mode ivy-avy bart-mode info-colors telephone-line all-the-icons-dired all-the-icons-ivy all-the-icons scala-mode highlight-symbol cargo dired-rainbow dockerfile-mode elfeed vterm kubernetes rust-mode exwm csv-mode crystal-mode symon dired-collapse dired-subtree markdown-mode counsel-projectile projectile exec-path-from-shell popup-kill-ring kill-ring toml-mode lua-mode sudo-edit graphql-mode json-mode dashboard dmenu linenum-relative linum-relative docker magit rainbow-mode avy go-mode ellocate company-mode))
 '(shell-pop-shell-type
   '("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
