;;; mtorus-utils.el --- auxiliary stuff used
;; $Id: mtorus-utils.el,v 1.4 2004/08/09 01:11:44 hroptatyr Exp $
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

(defun mtorus-utils-keyword->symbol (key)
  ":keyword -> 'keyword"
  (intern (replace-regexp-in-string ":" "" (format "%s" key))))
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

(defun mtorus-utils-plist-get (plist property &optional default)
  "Just like `plist-get'."
  (or (plist-get plist property)
      default))
;;(mtorus-utils-plist-get '(:test test1 :test test2) ':test)

(defun mtorus-utils-parse-key (keyword spec &optional default)
  "Parses SPEC for keyword KEY and returns a cons cell of key and value."
  (let ((keyword
         (cond ((keywordp keyword)
                keyword)
               ((symbolp keyword)
                (mtorus-utils-symbol->keyword keyword))
               (t nil))))
    (mtorus-utils-keyval->cons keyword
                               (mtorus-utils-plist-get spec keyword default))))
(defun mtorus-utils-parse-key-cdr (keyword spec &optional default)
  "Parses SPEC for keyword KEY and returns its value."
  (cdr-safe (mtorus-utils-parse-key keyword spec default)))

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



;;; Compatibilty:
;; This code was written in and for XEmacs but I hope that these lines
;; make a good and working GNU compatibility
(if (featurep 'xemacs)
    (progn ;; XEmacs code:
      (defun mtorus-message (msg)
        (display-message 'no-log msg))
      (defalias 'mtorus-make-extent         'make-extent)
      (defalias 'mtorus-set-extent-property 'set-extent-property)
      (defalias 'mtorus-delete-extent       'delete-extent)
      )
  (progn ;; GNU Emacs code:
    (defun mtorus-message (msg)
      (message msg))
    (defalias 'mtorus-make-extent 'make-overlay)
    (defalias 'mtorus-set-extent-property 'overlay-put)
    (defalias 'mtorus-delete-extent 'delete-overlay)

    (defun plist-to-alist (plist)
      "Convert property list PLIST into the equivalent association-list form.
The alist is returned.
The original plist is not modified."
      (let (alist)
        (while plist
          (setq alist (cons (cons (car plist) (cadr plist)) alist))
          (setq plist (cddr plist)))
        (nreverse alist)))
    )
  )
(defun mtorus-set-extent-face (extent face)
  (mtorus-set-extent-property extent 'face face))

;; from matlab.el:
(cond ((fboundp 'point-at-bol)
       (defalias 'mtorus-point-at-bol 'point-at-bol)
       (defalias 'mtorus-point-at-eol 'point-at-eol))
      ((fboundp 'line-beginning-position)
       (defalias 'mtorus-point-at-bol 'line-beginning-position)
       (defalias 'mtorus-point-at-eol 'line-end-position))
      (t
       (defmacro mtorus-point-at-bol ()
         (save-excursion (beginning-of-line) (point)))
       (defmacro mtorus-point-at-eol ()
         (save-excursion (end-of-line) (point)))))



;;; keyword handling


(defun mtorus-utils-keyword-type-replace-keyword-1 (list-elem prop-ht)
  ""
  (cond ((keywordp list-elem)
         `',(mtorus-element-keyword-type-get list-elem prop-ht))
        (t list-elem)))
(defun mtorus-utils-keyword-type-replace-keyword (list prop-ht)
  ""
  (mapcar #'(lambda (l-elem)
              (mtorus-utils-keyword-type-replace-keyword-1 l-elem prop-ht))
        list))


(defmacro define-mtorus-element-keyword-action (name keyword &rest specs)
  "Defines an mtorus-element-KEYWORD and some ACTION for it using NAME."
  (or (boundp 'mtorus-element)
      (defvar mtorus-element nil
        "DOCUMENT ME!"))
  (setq mtorus-element (del-alist keyword mtorus-element)
        mtorus-element
        (append mtorus-element (list (cons keyword (append `(:type ,name) specs)))))
  (let* ((type-specs (cdr-safe (assoc name mtorus-element-keyword-types)))
         (get-fun (mtorus-utils-parse-key-cdr :get type-specs))
         (put-fun (mtorus-utils-parse-key-cdr :put type-specs)))
    (mapc
     #'eval
     `((defun ,(mtorus-utils-symbol-conc 'mtorus-element-keyword-type 'get
                                         (mtorus-utils-keyword->symbol keyword))
         (el-p-ht &optional default)
         ,(format "Gets the %s %s from ELEMENT." keyword name)
         (funcall ',get-fun ',keyword el-p-ht default))
       (defun ,(mtorus-utils-symbol-conc 'mtorus-element-keyword-type 'put
                                         (mtorus-utils-keyword->symbol keyword))
         (el-p-ht value)
         ,(format "Puts VALUE to the %s %s of ELEMENT." keyword name)
         (funcall ',put-fun ',keyword el-p-ht value))
       )))
  `',keyword)

(defmacro define-mtorus-element-keyword-type (name &rest specs)
  "Installs `define-mtorus-element-<NAME>' funs by using name."
  (or (boundp 'mtorus-element-keyword-types)
      (defvar mtorus-element-keyword-types nil
        "DOCUMENT ME!"))
  (set-alist 'mtorus-element-keyword-types name specs)
  (let ((bouncer-name (mtorus-utils-symbol-conc 'define-mtorus-element name)))
    (mapc
     #'eval
     `((defmacro ,bouncer-name
         (keyword &rest specs)
         ,(format "Defines an mtorus-element %s-KEYWORD and some ACTION for it."
                  name)
         (eval
          ,(list
            'backquote
            `(define-mtorus-element-keyword-action ,name ,'(\, keyword) ,'(\,@ specs)))))
       ))
    `',bouncer-name))

(defun mtorus-element-keyword-get-type (keyword)
  "Return the type of KEYWORD."
  (mtorus-utils-parse-key-cdr ':type (cdr-safe (assoc keyword mtorus-element))))

(defun mtorus-element-property-keywords ()
  ""
  (mapcar #'car
          (remove-if-not
           #'(lambda (kw-type)
               (mtorus-element-keyword-get-type (car kw-type)))
           mtorus-element)))


;; element specific operations
(defun mtorus-element-keyword-type-put (keyword element value)
  "Puts VALUE of ELEMENT to KEYWORD of TYPE."
  (funcall
   (mtorus-utils-symbol-conc
    'mtorus-element-keyword-type 'put
    (mtorus-utils-keyword->symbol keyword))
   element value))

(defun mtorus-element-keyword-type-get (keyword element &optional default)
  "Gets the KEYWORD of TYPE from ELEMENT."
  (funcall
   (mtorus-utils-symbol-conc
    'mtorus-element-keyword-type 'get
    (mtorus-utils-keyword->symbol keyword))
   element default))





(provide 'mtorus-utils)

;;; mtorus-utils.el ends here
