;;; home-manager-options.el --- Interface for browsing and completing Home-Manager options.

;; Copyright (C) 2015 Diego Berrocal and Travis B. Hartwell

;; Author: Diego Berrocal <cestdiego@gmail.com>
;;      Travis B. Hartwell <nafai@travishartwell.net>
;; Created: 18 July 2015

;; Keywords: unix
;; Homepage: http://www.github.com/travisbhartwell/nix-emacs/
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for exploring the Home-Manager options.  Inspired by
;; https://home-manager.org/home-manager/options.html.

;;; Code:

(require 'json)

(defvar home-manager-options-name-indent-amount 0
  "Indent by the maximum length, plus a colon, plus two spaces.")

;; Macros for defining constants and functions for working with options
(defmacro define-home-manager-options-item (item long-name)
  (let* ((name-const (intern (concat "home-manager-options-" item)))
         (long-name-const (intern (concat "home-manager-options-" item "-long-name")))
         (long-name-length-plus-padding (+ 3 (length long-name)))
         (long-name-docstring (format "The long description for %s." item))
         (item-getter (intern (concat "home-manager-options-get-" item)))
         (item-getter-docstring
          (format "Get the value of %s from OPTION." item))
         (item-display (intern (concat "home-manager-options-display-" item)))
         (item-display-docstring
          (format "Display the value for %s from OPTION." item)))
    `(progn
       (defconst ,name-const ,item)
       (defconst ,long-name-const ,long-name ,long-name-docstring)
       (if (> ,long-name-length-plus-padding home-manager-options-name-indent-amount)
           (setq home-manager-options-name-indent-amount
                 ,long-name-length-plus-padding))
       (defun ,item-getter (option)
         ,item-getter-docstring
         (cdr (assoc ,name-const option)))
       (defun ,item-display (option)
         ,item-display-docstring
         (let ((item (,item-getter option))
               (format-string
                (format "%%-%ds %%s\n" home-manager-options-name-indent-amount)))
           (if (not (null item))
               (format format-string (concat ,long-name-const ":") item)
             ""))))))

(define-home-manager-options-item "name" "Name")
(define-home-manager-options-item "type" "Type")
(define-home-manager-options-item "description" "Description")
(define-home-manager-options-item "default" "Default value")
(define-home-manager-options-item "example" "Example value")
(define-home-manager-options-item "declarations" "Declared in")

(defvar home-manager-options-json-file
  (let*
      (expand-file-name "/etc/profiles/per-user/${USER}/share/doc/home-manager/options.json")
    "Location of the options file.")

  (defun home-manager-options--boolean-string (value)
    "Return the string representation of the boolean VALUE.
Returns VALUE unchanged if not a boolean."
    (cond ((eq value 't) "true")
          ((eq value :json-false) "false")
          (t value)))

  (defun home-manager-options--make-alist (option)
    (let ((name (car option))
          (data (cdr option))
          (default (home-manager-options-get-default option))
          (example (home-manager-options-get-example option)))
      (progn
        (if (not (null default))
            (setcdr (assoc home-manager-options-default option)
                    (home-manager-options--boolean-string default)))
        (if (not (null example))
            (setcdr (assoc home-manager-options-example option)
                    (home-manager-options--boolean-string example)))
        (add-to-list 'data `(,home-manager-options-name . ,name))
        `(,name . ,data))))

  (defvar home-manager-options
    (if (file-exists-p home-manager-options-json-file)
        (let* ((json-key-type 'string)
               (raw-options (json-read-file home-manager-options-json-file)))
          (mapcar 'home-manager-options--make-alist raw-options))
      (message "Warning: Cannot find home-manager option file.")))

  (defun home-manager-options-get-documentation-for-option (option)
    (concat (home-manager-options-display-name option)
            (home-manager-options-display-type option)
            (home-manager-options-display-description option)
            (home-manager-options-display-default option)
            (home-manager-options-display-example option)
            (home-manager-options-display-declarations option)))

  ;; Borrowed from anaconda-mode
  (defun home-manager-options-doc-buffer (doc)
    "Display documentation buffer with contents DOC."
    (let ((buf (get-buffer-create "*home-manager-options-doc*")))
      (with-current-buffer buf
        (view-mode -1)
        (erase-buffer)
        (insert doc)
        (goto-char (point-min))
        (view-mode 1)
        buf)))

  (defun home-manager-options-get-option-by-name (name)
    (assoc name home-manager-options))

  (provide 'home-manager-options)
;;; home-manager-options.el ends here
