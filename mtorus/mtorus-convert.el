;;; mtorus-convert.el --- type converters for mtorus
;; $Id: mtorus-convert.el,v 1.5 2004/09/25 16:24:34 hroptatyr Exp $
;; Copyright (C) 2004 by Stefan Kamphausen
;;           (C) 2004 by Sebastian Freundt
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;;         Sebastian Freundt <hroptatyr@users.berlios.de>
;; Created: 2004/09/01
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
;; This file is not loaded by default.
;; Put:
;;   (require 'mtorus-convert)
;; in your .emacs to enjoy the feature of type conversion.

;; *** ToDo:

;;; History


;;; Code:

(require 'mtorus-utils)
(require 'mtorus-type)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-convert nil
  "The type converters of elements."
  :tag "MTorus Converters"
  :prefix "mtorus-convert-"
  :group 'mtorus-type)


(defconst mtorus-convert-version "Version: 0.1 $Revision: 1.5 $"
  "Version of mtorus-convert backend.")




(define-mtorus-keyword-type mtorus-type convert
  :install
  (lambda (keyword type function)
    "Installs mtorus-type-convert-KEYWORD-TYPE."
    (let* ((fun-name
            (mtorus-utils-symbol-conc
             'mtorus-type keyword type)))
      (and (functionp function)
           (fset fun-name function))))
  :induce
  (lambda (keyword form)
    "Induces a function."
    (let* ((prop-list
            (mapcar
             #'(lambda (key)
                 (list
                  (mtorus-utils-namespace-conc 'prop key)
                  `(mtorus-element-property-get
                    ',(mtorus-utils-keyword->symbol key)
                    el-prop-ht)))
             (mtorus-element-property-list))))
      `(lambda (el-prop-ht)
         (let ,prop-list
           ,form))))
  :deduce
  (lambda (keyword form)
    "Deduces a function."
    (let* ((prop-list
            (mapcar
             #'(lambda (key)
                 (list
                  (mtorus-utils-namespace-conc 'conv key)
                  `(mtorus-type-convert-gethash
                    ',(mtorus-utils-keyword->symbol key)
                    conv-prop-ht)))
             (mtorus-type-convert-list))))
      `(lambda (conv-prop-ht)
         (let ,prop-list
           ,form))))
  :puthash 
  (lambda (keyword value conv-prop-ht)
    "Places VALUE of KEYWORD in CONV-PROP-HT."
    (puthash keyword value conv-prop-ht))
  :gethash 
  (lambda (keyword conv-prop-ht &optional default)
    "Returns value of KEYWORD in CONV-PROP-HT."
    (gethash keyword conv-prop-ht default))
  )





;; now the actual keywords

(define-mtorus-type-convert :buffer)
(define-mtorus-type-convert :buffer-name)
(define-mtorus-type-convert :buffer-file-name)
(define-mtorus-type-convert :buffer-point)
(define-mtorus-type-convert :element-name)

 



(defvar mtorus-converts nil
  "List of available converters.
This is for internal purposes only.
Do not fiddle with it.")

(defvar mtorus-convert-obarray nil
  "Obarray holding registered type converters.")




(defmacro define-mtorus-convert (name &rest properties)
  ""
  (add-to-list 'mtorus-converts name)
  (let ((conv-name (mtorus-utils-symbol-conc 'mtorus-type-convert name)))
    (mapc
     #'eval
     `((defvar ,conv-name nil
         ,(format "MTorus type."))
       (setq ,conv-name ',properties))))
  (let* ((conv-from-fun-name
          (mtorus-utils-symbol-conc
           'mtorus-type-convert-from name))
         (conv-to-fun-name
          (mtorus-utils-symbol-conc
           'mtorus-type-convert-to name))
         (conv-from-fun
          `(lambda (el-prop-ht)
             (let ((conv-prop-ht (make-hash-table :test 'equal)))
               (mapc
                #'(lambda (conv-prop)
                    (let* ((keyw (car conv-prop))
                           (cfun (cdr conv-prop))
                           (conv-val
                            (ignore-errors
                              (funcall (mtorus-type-convert-induce
                                        keyw cfun)
                                       el-prop-ht))))
                      (mtorus-type-convert-puthash
                       keyw conv-val conv-prop-ht)))
                ',(mtorus-utils-parse-spec
                   properties
                   (mtorus-type-convert-list)))
               conv-prop-ht)))
         (conv-to-fun
          `(lambda (conv-prop-ht el-prop-ht)
             (let ((new-prop-ht (copy-hash-table el-prop-ht)))
               (mapc
                #'(lambda (conv-prop)
                    (let* ((keyw (car conv-prop))
                           (cfun (cdr conv-prop))
                           (prop-val
                            (ignore-errors
                              (funcall (mtorus-type-convert-deduce
                                        keyw cfun)
                                       conv-prop-ht))))
                      (unless (eq prop-val 'n/a)
                        (mtorus-element-property-put
                         keyw new-prop-ht prop-val))))
                ',(mtorus-utils-parse-spec
                   properties
                   (mtorus-element-property-list) nil ''n/a))
               new-prop-ht))))
    (fset conv-from-fun-name conv-from-fun)
    (fset conv-to-fun-name conv-to-fun))
  `',(mtorus-utils-symbol-conc 'mtorus-type-convert name))
(defalias 'mtorus-define-type-convert 'define-mtorus-convert)
(defalias 'mtorus-type-convert-define 'define-mtorus-convert)





;;;
;;; Type Conversion
;;;
;; type conversion allows to convert an element of type1 to an element of type2
;;
;; type conversion is induced by the keywords :convert-from and :convert-to
;; :convert-from should result in a conversion hash table to provide much
;; information about the element in question
;; :convert-to should take a conversion hash table and a element property 
;; hash table result in an element property hash table

(defun mtorus-type-convert-from (el-prop-ht)
  ""
  (let* ((type (mtorus-element-property-get 'type el-prop-ht))
         (convfun (mtorus-utils-symbol-conc
                   'mtorus-type-convert-from
                   type)))
    (and (fboundp convfun)
         (funcall convfun el-prop-ht))))

(defun mtorus-type-convert-to-1 (type conv-prop-ht el-prop-ht)
  ""
  (let ((convfun (mtorus-utils-symbol-conc
                  'mtorus-type-convert-to
                  type)))
    (or (and (fboundp convfun)
             conv-prop-ht
             (funcall convfun conv-prop-ht el-prop-ht))
        el-prop-ht)))
(defun mtorus-type-convert-to (type el-prop-ht)
  ""
  (mtorus-type-convert-to-1
   type
   (mtorus-type-convert-from el-prop-ht)
   el-prop-ht))


;;; more higher level functions
(defun mtorus-element-convert-to (type element)
  "Converts ELEMENT to TYPE."
  (mtorus-element-register
   element
   (mtorus-type-convert-to
    type (gethash mtorus-current-element
                  (eval mtorus-elements-hash-table)))))

;; (defun mtorus-convert-element (type element)
;;   "Converts ELEMENT to TYPE."
;;   (interactive)
;;   )
;; (defun mtorus-convert-current-element (type)
;;   "Converts current element to TYPE."
;;   (interactive)
;;   (mtorus-convert-element type mtorus-current-element))




;;;
;;; Predefined converters
;;;

  ;;; actually this will walk to mtorus.el some day
  ;;; because it isnt really backend

(defun mtorus-convert-initialize ()
  "Initialization of predefined mtorus types."
  (interactive)

  (define-mtorus-convert ring
    :element-name prop::name
    :type 'ring
    :value conv::buffer-name
    )
  (define-mtorus-convert buffer
    :buffer prop::value
    :buffer-name (or (buffer-name prop::value)
                     (mtorus-utils-plist-get
                      :buffer-name prop::resurrection-data))
    :buffer-file-name (cond ((not (string= (buffer-file-name prop::value) " *temp*"))
                             (buffer-file-name prop::value))
                          (t (mtorus-utils-plist-get
                              :buffer-file-name prop::resurrection-data)))
    :buffer-point (or (with-current-buffer prop::value
                        (point))
                      (mtorus-utils-plist-get
                       :buffer-point prop::resurrection-data))
    :element-name (or (buffer-name prop::value)
                      (mtorus-utils-plist-get
                       :buffer-name prop::resurrection-data))
    :type 'buffer
    :value conv::buffer
    )
  (define-mtorus-convert marker
    :buffer (marker-buffer prop::value)
    :buffer-name (buffer-name (marker-buffer prop::value))
    :buffer-file-name (buffer-file-name (marker-buffer prop::value))
    :buffer-point (marker-position prop::value)
    :element-name (buffer-name (marker-buffer prop::value))
    :type 'marker
    :value (set-marker (make-marker) conv::buffer-point conv::buffer)
    )
  (define-mtorus-convert file
    ;;:buffer (marker-buffer prop::value)
    :buffer-name prop::name
    :buffer-file-name prop::value
    ;;:buffer-point (marker-position prop::value)
    :element-name prop::name
    :type 'file
    :value conv::buffer-file-name
    :name conv::buffer-name
    )
  )

(mtorus-convert-initialize)


;; (defun mtorus-convert-uninitialize ()
;;   "Uninitializes mtorus converters"
;;   (interactive)
;;   (mapc 'undefine-mtorus-type mtorus-types))



(run-hooks 'mtorus-convert-after-load-hook)


(provide 'mtorus-convert)

;;; mtorus-convert.el ends here
