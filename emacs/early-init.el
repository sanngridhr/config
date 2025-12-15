;; -*- lexical-binding: t; -*-

;; optimise GC on startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (run-with-idle-timer 1 nil
                                          (lambda () (setq gc-cons-threshold 16777216))))) ; 16mb
;; start maximised
(setq default-frame-alist '((fullscreen . maximized)))
;; disable default package manager
(setq package-enable-at-startup nil)
