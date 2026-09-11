;; -*- lexical-binding: t; -*-

;; prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)

;; Warnings
(setq warning-inhibit-types '((files missing-lexbind-cookie)))

;; disable native compilation
;; (setq native-comp-speed -1)
