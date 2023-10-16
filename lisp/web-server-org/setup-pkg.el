;;; setup-pkg.el --- setup deps for web-server-org       -*- lexical-binding: t; -*-

(package-initialize)
(unless (require 'web-server nil :noerror)
  (package-install 'web-server)
  (require 'web-server))
