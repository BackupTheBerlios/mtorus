;;; mtorus-utils.el --- auxiliary stuff used
;; $Id: mtorus-utils.el,v 1.8 2004/09/04 02:37:32 hroptatyr Exp $
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

;;; Compatibilty:
;; This code was written in and for XEmacs but I hope that these lines
;; make a good and working GNU compatibility
(if (featurep 'xemacs)
    (progn ;; XEmacs code:
      (defun mtorus-message (msg)
        (display-message 'no-log msg))
      (defalias 'mtorus-make-extent 'make-extent)
      (defalias 'mtorus-set-extent-property 'set-extent-property)
      (defalias 'mtorus-delete-extent 'delete-extent))
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

(defun set-alist (alist item value)
  (let ((alis (symbol-value alist)))
    (or (boundp alist)
        (set alist nil))
    (set alist 
         (let ((pair (assoc item alis)))
           (if pair
               (progn
                 (setcdr pair value)
                 alis)
             (cons
              (cons item value)
              alis))))))
(defun del-alist (item alist)
  "If there is a pair whose key is ITEM, delete it from ALIST.
\[tomo's ELIS emulating function]"
  (if (equal item (car (car alist)))
      (cdr alist)
    (let ((pr alist)
          (r (cdr alist)))
      (catch 'tag
        (while (not (null r))
          (if (equal item (car (car r)))
              (progn
                (rplacd pr (cdr r))
                (throw 'tag alist)))
          (setq pr r)
          (setq r (cdr r)))
        alist))))
    ))

(cond ((fboundp 'replace-regexp-in-string)
       (defalias 'mtorus-utils-replace-regexp-in-string 'replace-regexp-in-string))
      ((fboundp 'replace-in-string)
       (defun mtorus-utils-replace-regexp-in-string
         (regexp rep string &optional fixedcase literal subexp start)
         (replace-in-string string regexp rep literal))))

(cond ((fboundp 'time-less-p)
       (defalias 'mtorus-utils-time-less-p 'time-less-p))
      (t (defun mtorus-utils-time-less-p (t1 t2)
                 (or (< (car t1) (car t2))
                     (and (= (car t1) (car t2))
                          (< (nth 1 t1) (nth 1 t2)))))))

;;;
(defun mtorus-utils-keyword->symbol (key)
  ":keyword -> 'keyword"
  (intern (mtorus-utils-replace-regexp-in-string ":" "" (format "%s" key))))
(defun mtorus-utils-symbol->keyword (sym)
  "'keyword -> :keyword"
  (cond ((keywordp sym)
         sym)
        (t (intern (format ":%s" sym)))))
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
(defun mtorus-utils-namespace-conc (&rest symbols)
  "Concatenate SYMBOLS (which should be strings or symbols
and make the result a symbol."
  (intern
   (format
    "%s:%s"
    (car symbols)
    (apply #'mtorus-utils-symbol-conc (cdr symbols)))))
;;(mtorus-utils-namespace-conc 'a 'b 'c)

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

(defun mtorus-utils-parse-spec (spec &optional spec-keywords parse-unsupported default)
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
                           (t (mtorus-utils-parse-key key spec default))))
                 spec-keywords))))
;;(mtorus-utils-parse-spec '(:type a :nother b) '(:type :name))



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


(defun mtorus-utils-keyword-type-replace-keyword-1
  (prefix keyword-type+action list-elem prop-ht)
  "DOCUMENT ME!!!"
  (cond ((keywordp list-elem)
         `',(funcall
             (mtorus-utils-symbol-conc prefix keyword-type+action)
             (mtorus-utils-keyword->symbol list-elem)
             prop-ht))
        (t list-elem)))
(defun mtorus-utils-keyword-type-replace-keyword 
  (prefix keyword-type+action list prop-ht)
  "DOCUMENT ME!!!"
  (mapcar
   #'(lambda (l-elem)
       (mtorus-utils-keyword-type-replace-keyword-1
        prefix keyword-type+action l-elem prop-ht))
   list))


(defmacro define-mtorus-keyword-action (prefix name keyword &rest specs)
  "Defines a KEYWORD and some ACTION for it using NAME and PREFIX."
  (or (boundp prefix)
      (eval `(defvar ,prefix nil
              "DOCUMENT ME!")))
  (set prefix (del-alist keyword (eval prefix)))
  (set prefix
       (append (eval prefix)
               (list (cons keyword (append `(:type ,name) specs)))))
  (let* ((type-specs
          (cdr-safe
           (assoc name (eval
                        (mtorus-utils-symbol-conc prefix 'keyword-types)))))
         (funs (mtorus-utils-parse-spec type-specs nil t)))
    (mapc
     #'(lambda (keyw-val)
         (let ((keyw (car keyw-val))
               (val (cdr keyw-val)))
           (let ((keyw-name
                  (mtorus-utils-symbol-conc
                   prefix name keyw
                   (mtorus-utils-keyword->symbol keyword))))
             (cond ((functionp val)
                    (flet ((nval (test tes2)
                             (funcall val keyword test tes2)))
                      ;;(fset keyw-name val))
                    ))
                   (t (error "Merely functions supported at the moment."))))))
         funs)
    (mapc
     #'eval
     `((defun ,(mtorus-utils-symbol-conc
                prefix 'keyword-get-type)
         (keyword)
         "Return the type of KEYWORD."
         (mtorus-keyword-get-type ',prefix keyword)))))
  `',keyword)

(defmacro define-mtorus-keyword-type (prefix name &rest specs)
  "Installs `define-<PREFIX>-<NAME>' funs by using NAME."
  (or (boundp (mtorus-utils-symbol-conc prefix 'keyword-types))
      (eval `(defvar ,(mtorus-utils-symbol-conc prefix 'keyword-types) nil
              "DOCUMENT ME!")))
  (set-alist (mtorus-utils-symbol-conc prefix 'keyword-types) name specs)
  (let ((bouncer-name (mtorus-utils-symbol-conc 'define prefix name))
        (keywtype-list-fun-name (mtorus-utils-symbol-conc prefix name 'list))
        (keywtype-pred-fun-name (mtorus-utils-symbol-conc prefix name 'p))
        (funs (mtorus-utils-parse-spec specs nil t)))
    (mapc
     #'(lambda (keyw-val)
         (let ((keyw (car keyw-val))
               (val (cdr keyw-val)))
           (let ((keyw-name
                  (mtorus-utils-symbol-conc
                   prefix name keyw)))
             (cond ((functionp val)
                    (fset keyw-name val))
                   (t (error "Merely functions supported at the moment."))))))
         funs)
    (mapc
     #'eval
     `((defmacro ,bouncer-name
         (keyword &rest specs)
         ,(format "Defines %s-KEYWORD for %s and some ACTION for it."
                  name prefix)
         (eval
          ,(list
            'backquote
            `(define-mtorus-keyword-action ,prefix
              ,name ,'(\, keyword) ,'(\,@ specs)))))
       (defun ,keywtype-list-fun-name ()
         ,(format "Returns keywords of type `%s' from `%s'." name prefix)
         (remove-if-not
          #'(lambda (keyw)
              (equal (mtorus-keyword-get-type ',prefix keyw) ',name))
          (mapcar #'car ,prefix)))
       (defun ,keywtype-pred-fun-name (keyword)
         ,(format "Returns non-nil iff KEYWORD is of keyword-type `%s-%s'." name prefix)
         (member (mtorus-utils-symbol->keyword keyword)
                 (funcall ',keywtype-list-fun-name)))
       ))
    `',bouncer-name))


(defun mtorus-keyword-get-type (prefix keyword)
  "Return the type of KEYWORD in PREFIX."
  (mtorus-utils-parse-key-cdr
   ':type
   (cdr-safe (assoc (mtorus-utils-symbol->keyword keyword)
                    (eval prefix)))))


;;; REVISE ME!
(defun mtorus-element-property-keywords ()
  ""
  (mapcar #'car
          (remove-if-not
           #'(lambda (kw-type)
               (mtorus-element-keyword-get-type (car kw-type)))
           mtorus-element)))
;;(mtorus-element-property-keywords)
;;;WTF?!





(provide 'mtorus-utils)

;;; mtorus-utils.el ends here
