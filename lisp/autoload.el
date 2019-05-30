;; glsl
(when (require 'glsl-mode nil 'noerror)
  (autoload 'glsl-mode "glsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

;; Assembly
(when (require 'nasm-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.s\\'" . nasm-mode)))

;; Web stuff
(when (require 'web-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
