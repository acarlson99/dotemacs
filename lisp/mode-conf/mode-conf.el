;; load mode configs

(require 'prog-config)
(require 'c-config)
(require 'c++-config)
(require 'ruby-config)
(require 'term-config)
(require 'web-config)
(require 'php-config)
(require 'js-config)
(require 'sql-config)
(require 'go-config)
(require 'org-config)
(require 'package-conf)

;; Set mode hooks
;; (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-2 80)))
;; commented out in favor of fci-mode
(add-hook 'prog-mode-hook 'prog-config)
(add-hook 'c-mode-hook 'c-config)
(add-hook 'c++-mode-hook 'c++-config)
(add-hook 'ruby-mode-hook 'ruby-config)
(add-hook 'term-mode-hook 'term-config)
(add-hook 'web-mode-hook 'web-config)
(add-hook 'php-mode-hook 'php-config)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'js-config)
(add-hook 'sql-mode-hook 'sql-config)
(add-hook 'go-mode-hook 'go-config)
;; (add-hook 'org-mode-hook 'font-lock-mode)
(add-hook 'org-mode-hook 'org-config)
(add-hook 'elisp-byte-code-mode 'hs-minor-mode)

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
;; TODO: haskell-mode disable fci-mode bc it messes with LSP
