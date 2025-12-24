;;; twidget.el --- Vue3-style Reactive Widget System for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Twidget Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (dash "2.19.1"))
;; Keywords: reactive, components, ui, widgets
;; URL: https://github.com/Kinneyzhang/twidget

;;; Commentary:

;; Twidget is a Vue3-style reactive widget system for Emacs.
;; It provides a Composition API for creating reactive UI components
;; that automatically update when their data changes.
;;
;; Key features:
;; - Vue3-style Composition API with `twidget-define-component'
;; - Reactive data binding with `twidget-ref' and `twidget-reactive'
;; - Computed properties with `twidget-computed'
;; - Watch functionality with `twidget-watch'
;; - Text property reactivity via tp.el integration
;; - Built-in components: button, text, input
;;
;; See README.md for detailed documentation and examples.

;;; Code:

(require 'twidget-reactive)
(require 'twidget-component)

(provide 'twidget)
