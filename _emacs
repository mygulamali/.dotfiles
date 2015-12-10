;; .emacs

;; disable loading of "default.el" at startup
;(setq inhibit-default-init t)

;; setup ELPA
(setq package-archives
  '(
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.org/packages/")
  )
)
(package-initialize)
(setq url-http-attempt-keepalives nil)

;; my packages
(setq my-packages
  '(
    haml-mode
    magit
    markdown-mode
    markdown-mode+
    rspec-mode
    ruby-test-mode
    ruby-tools
    sass-mode
    web-mode
   )
)

;; load my-packages (http://stackoverflow.com/questions/14836958/updating-packages-in-emacs)
(when (not package-archive-contents) (package-refresh-contents))
(dolist (pkg my-packages)
  (when (and (not (package-installed-p pkg)) (assoc pkg package-archive-contents))
    (package-install pkg)))

;; sensible defaults (https://github.com/hrs/sensible-defaults.el)
(load-file "~/.emacs.d/sensible-defaults/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

;; remove that menu bar
(menu-bar-mode -1)

;; line numbering
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%4d ")
(global-set-key [f3] 'linum-mode)

;; default to better frame titles
(setq frame-title-format
  (concat "%b - emacs@" (system-name))
)

;; default to unified diffs
(setq diff-switches "-u")

;; set tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default indent-line-function 'insert-tab)
;(global-set-key (kbd "TAB") 'tab-to-tab-stop)
;(setq-default tab-stop-list (number-sequence 4 256 4))
(define-key function-key-map [iso-lefttab] [backtab])

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; ido
(require 'ido)
(ido-mode t)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; http://www.emacswiki.org/emacs/SupportBiDi
(setq-default bidi-display-reordering t)

;; python-mode
;(require 'python-mode)
;(setq-default py-indent-offset 4)

;; ruby-mode for most ruby files
(add-to-list 'auto-mode-alist
  '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
  '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

;; setup web-mode
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; setup html-mode
(setq html-mode-markup-indent-offset 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-24hr-format t)
 '(display-time-format nil)
 '(display-time-mode t nil (time))
 '(global-hl-line-mode t)
 '(ido-enable-flex-matching t)
 '(js-indent-level 2)
 '(make-backup-files nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#222")))))
