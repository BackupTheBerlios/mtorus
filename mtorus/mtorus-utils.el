;;; mtorus-utils.el --- auxiliary stuff used
;; $Id: mtorus-utils.el,v 1.1 2004/07/28 01:44:24 hroptatyr Exp $
;; Copyright (C) 2004 by Stefan Kamphausen
;;           (C) 2004 by Sebastian Freundt
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;;         Sebastian Freundt <hroptatyr@users.berlios.de>
;; Created: 2004/07/28
;; Keywords: bookmarks, navigation, tools, extensions, user

;; This file is not (yet) part of XEmacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


;;; Commentary:
;;

;; *** ToDo:
;; - <add some> ;)


;;; History


;;; Code:

(require 'cl-extra)
;;; sorry about that, but i use (every ...) and (some ...) constructions


(defun mtorus-utils-keyword->symbol (key)
  ":keyword -> 'keyword"
  (intern (substring (format "%s" key) 1)))
(defun mtorus-utils-symbol->keyword (sym)
  "'keyword -> :keyword"
  (intern (format ":%s" key)))
(defun mtorus-utils-keyval->cons (key val)
  ":keyword value -> '(keyword . value)"
  (cons (mtorus-utils-keyword->symbol key)
        val))
(defun mtorus-utils-cons->keyval (cons)
  "'(keyword . value) -> :keyword value"
  (list (mtorus-utils-symbol->keyword (car cons))
        (cdr cons)))
(defun mtorus-utils-symbol-conc (&rest symbols)
  "Concatenate SYMBOLS (which should be strings or symbols
and make the result a symbol."
  (intern
   (mapconcat #'(lambda (sym)
                  (cond ((symbolp sym)
                         (format "%s" sym))
                        ((stringp sym)
                         sym)
                        (t nil)))
              symbols "-")))
;;(mtorus-utils-symbol-conc 'a 'b 'c)

(defun mtorus-utils-parse-key (keyword spec &optional default)
  "Parses SPEC for keyword KEY and returns a cons cell of key and value."
  (let ((keyword
         (cond ((keywordp keyword)
                keyword)
               ((symbolp keyword)
                (mtorus-utils-symbol->keyword keyword))
               (t nil))))
    (mtorus-utils-keyval->cons keyword (plist-get spec keyword default))))
(defun mtorus-utils-parse-spec (spec &optional spec-keywords parse-unsupported)
  "Parses SPEC and returns a list '((key . value) ...).
The optional SPEC-KEYWORDS list ensures that any keyword from there
will be in the result list."
  (cond (parse-unsupported
         (mapcar #'(lambda (cc)
                     (mtorus-utils-keyval->cons (car cc) (cdr cc)))
                 (plist-to-alist spec)))
        (t
         (mapcar #'(lambda (key)
                     (cond ((listp key)
                            (eval key))
                           (t (mtorus-utils-parse-key key spec))))
                 spec-keywords))))
;;(mtorus-utils-parse-spec '(:type a :nother b) '(:type :name))



(provide 'mtorus-utils)

;;; mtorus-utils.el ends here
