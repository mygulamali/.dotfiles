;; .emacs

;; disable loading of "default.el" at startup
;(setq inhibit-default-init t)

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/plugins/")

;; remove that menu bar
(menu-bar-mode -1)

;; turn on font-lock mode
(when
  (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
)

;; line numbering
(global-linum-mode 1)
(setq linum-format "%d ")
(global-set-key [f3] 'linum-mode)

;; enable deletion of entire regions
(setq delete-selection-mode t)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
  (concat "%b - emacs@" (system-name))
)

;; default to unified diffs
(setq diff-switches "-u")

;; set tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;(setq-default indent-line-function 'insert-tab)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(setq-default tab-stop-list (number-sequence 4 256 4))
(define-key function-key-map [iso-lefttab] [backtab])

;; always end a file with a newline
(setq require-final-newline 'query)

;; refresh using F5
;; http://tsengf.blogspot.co.uk/2011/06/bind-f5-to-revert-buffer-in-emacs.html
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;; ido
(require 'ido)
(ido-mode t)

;; python-mode
;(add-to-list 'load-path "~/.emacs.d/plugins/python-mode.el-6.0.7/") 
;(setq py-install-directory "~/.emacs.d/plugins/python-mode.el-6.0.7/")
;(require 'python-mode)
;(setq-default py-indent-offset 4)

;; powerline
;; https://github.com/milkypostman/powerline
(require 'powerline)
(powerline-default)

;; http://www.emacswiki.org/emacs/SupportBiDi
(setq-default bidi-display-reordering t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-24hr-format t)
 '(display-time-format nil)
 '(display-time-mode t nil (time))
 '(fill-column 78)
 '(ido-enable-flex-matching t)
 '(make-backup-files nil)
)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
)
