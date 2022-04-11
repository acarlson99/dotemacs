;; load lang configs

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

;; Set mode hooks
;; (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-2 80)))
;; commented out in favor of fci-mode
(add-hook 'prog-mode-hook 'prog-config)
(add-hook 'c-mode-hook 'c-config)
(add-hook 'c++-mode-hook 'c++-config)
(add-hook 'ruby-mode-hook 'ruby-config)
(add-hook 'org-mode-hook 'font-lock-mode)
(add-hook 'term-mode-hook 'term-config)
(add-hook 'web-mode-hook 'web-config)
(add-hook 'php-mode-hook 'php-config)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'js-config)
(add-hook 'sql-mode-hook 'sql-config)
(add-hook 'go-mode-hook 'go-config)
