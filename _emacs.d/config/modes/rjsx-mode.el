(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))

(setq js-indent-level 2)
(setq js-switch-indent-offset 2)
;; (setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
