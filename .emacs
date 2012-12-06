;; .emacs

;; uncomment this line to disable loading of "default.el" at startup
;(setq inhibit-default-init t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
	(global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
	(concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
(setq require-final-newline 'query)

;; Murtaza's own settings for emacs...
(custom-set-variables
	'(column-number-mode t)
	'(display-time-24hr-format t)
	'(display-time-format nil)
	'(display-time-mode t nil (time))
	'(fill-column 78)
	'(make-backup-files nil)
)
