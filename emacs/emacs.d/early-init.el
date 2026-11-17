;; -*- lexical-binding: t; -*-

;; Keep package.el from activating packages before init.el runs.
(setq package-enable-at-startup nil)

;; Reduce startup noise from files that intentionally omit a lexical-binding cookie.
(setq warning-inhibit-types '((files missing-lexbind-cookie)))

;; Disable native compilation at startup if desired.
;; (setq native-comp-speed -1)
