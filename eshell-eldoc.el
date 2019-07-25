;;; eshell-eldoc.el --- Eldoc support for Eshell

;; Copyright (C) 2016 modula t.

;; Author: modula t. <defaultxr@gmail.com>
;; Homepage: https://github.com/defaultxr/eshell-eldoc
;; Version: 0.5
;; Package-Requires: ((osc "0.1") (emacs "24.4"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; Eshell-Eldoc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Foobar is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Eshell-Eldoc.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides `eldoc-mode' support for `eshell'.
;;
;; Eshell-Eldoc first checks for elisp functions named by eshell's input and
;; uses Emacs' built in eldoc functionality to display documentation if one is
;; found. If not, the whatis(1) command is used to get a description of a
;; shell command of the same name. Caching is built in to the whatis lookup, so
;; slowdown should be minimal for repeatedly-typed inputs.

;;; Code:

;; custom

(defgroup eshell-eldoc nil
  "Eldoc support for Eshell."
  :group 'external
  :prefix "eshell-eldoc-")

;; (defcustom eshell-eldoc-use-whatis t ;; FIX: implement
;;   "Whether or not to get external command descriptions using whatis(1)."
;;   :type '(boolean))

;; (defcustom eshell-eldoc-use-whatis-cache t ;; FIX: implement
;;   "Whether or not to cache results from whatis(1)."
;;   :type '(boolean))

(require 's)
(require 'eldoc)
(require 'eshell)

(defvar eshell-eldoc-whatis-cache (make-hash-table :test #'equal)
  "The cache of results from the whatis(1) utility for eshell-eldoc.")

(defun eshell-eldoc-whatis (command)
  "Lookup the \"whatis\" description of COMMAND, using the `eshell-eldoc-whatis-cache' if possible."
  (let ((cache-result (gethash command eshell-eldoc-whatis-cache)))
    (if cache-result
        (unless (eql 'none cache-result)
          cache-result)
      (let ((whatis (shell-command-to-string (concat "whatis " command))))
        (if (s-ends-with-p "nothing appropriate.\n" whatis) ;; FIX: use exit status instead...
            (puthash command 'none eshell-eldoc-whatis-cache)
          (puthash command (s-replace-regexp "\s+" " " (substring whatis 0 (position ?\n whatis))) eshell-eldoc-whatis-cache))))))

;;;###autoload
(defun eshell-eldoc-documentation-function () ;; FIX: finish this...
  "`eldoc-documentation-function' for Eshell."
  ;; (symbol-value (intern-soft (upcase (thing-at-point 'symbol))
  ;;                            tal-eldoc-obarray))
  (let* ((input (buffer-substring eshell-last-output-end (point-max)))
         (command (split-string input)))
    (when (plusp (length input)) ;; FIX: #'length is slow...
      (cond ((string-equal (substring input 0 1) "(")
             (elisp-eldoc-documentation-function))
            ((or (fboundp (intern (elt command 0))) ;; FIX
                 (fboundp (intern (concat "eshell/" (elt command 0)))))
             (with-temp-buffer
               (insert "(")
               (insert input)
               (insert ")")
               (elisp-eldoc-documentation-function))
             "Not done yet...")
            (t
             (eshell-eldoc-whatis (elt command 0)))))))

(add-function :before-until (local 'eldoc-documentation-function)
              #'eshell-eldoc-documentation-function)

(provide 'eshell-eldoc)

;;; eshell-eldoc.el ends here
