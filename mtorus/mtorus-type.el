;;; mtorus-type.el --- types of the mtorus
;; $Id: mtorus-type.el,v 1.13 2004/09/25 16:24:34 hroptatyr Exp $
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
;; - type initialization shouldn't be predefined with topology neighborhood
;;   registration
;;   instead there should be user customizable auto-attach settings,
;;   what you see in `mtorus-type-initialize' is just a demonstration

;;; History


;;; Code:

(require 'mtorus-utils)
(require 'mtorus-topology) ;; just for the moment (see initialize code)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-type nil
  "The types of elements."
  :tag "MTorus Types"
  :prefix "mtorus-type-"
  :group 'mtorus)


(defconst mtorus-type-version "Version: 0.1 $Revision: 1.13 $"
  "Version of mtorus-type backend.")



(define-mtorus-keyword-type mtorus-type property
  :get 
  (lambda (keyword el-property-ht &optional default)
    (gethash keyword el-property-ht default))

  :put
  (lambda (keyword el-property-ht value)
    (puthash keyword value el-property-ht)))


(define-mtorus-keyword-type mtorus-type method
  :install
  (lambda (keyword type function)
    "Installs mtorus-type-TYPE-KEYWORD."
    (let ((fun-name
           (mtorus-utils-symbol-conc
            'mtorus-type type keyword)))
      (fset fun-name function)))
  :install-invoke
  (lambda (keyword)
    "Installs mtorus-type-invoke-KEYWORD
and mtorus-element-KEYWORD."
    (let ((kw-symb (mtorus-utils-keyword->symbol keyword)))
      (let ((fun-name
             (mtorus-utils-symbol-conc
              'mtorus-type-invoke
              kw-symb))
            (elem-fun-name
             (mtorus-utils-symbol-conc
              'mtorus-element
              kw-symb))
            (func
             `(lambda (type element)
                ,(format "Invokes mtorus-type-TYPE-%s" kw-symb)
                (let ((fun (mtorus-utils-symbol-conc
                            'mtorus-type type ',kw-symb)))
                  (and (fboundp fun)
                       (symbol-function fun)
                       (funcall fun element)))))
            (elem-func
             `(lambda (element)
                ,(format (concat "Runs mtorus-type-invoke-%s "
                                 "under the correct type of ELEMENT.")
                         kw-symb)
                (let ((fun (mtorus-utils-symbol-conc
                            'mtorus-type-invoke
                            ',kw-symb)))
                  (funcall
                   fun
                   (mtorus-element-get-property 'type element)
                   element)))))
        (fset fun-name (eval func))
        (fset elem-fun-name (eval elem-func)))))
  :install-fallback
  (lambda (keyword type)
    "Installs a fallback mtorus-type-TYPE-KEYWORD."
    (let ((fun-name
           (mtorus-utils-symbol-conc
            'mtorus-type type keyword))
          (func
           (lambda (element)
             "This is a fallback only."
             nil)))
      (fset fun-name func)))

  :fallback
  (lambda (element)
    "This is a fallback only."
    nil))


(define-mtorus-keyword-type mtorus-type hook
  :short-name
  (lambda (keyword)
    (mtorus-utils-symbol-conc
     (mtorus-utils-keyword->symbol keyword)
     'funs))

  :name
  (lambda (keyword type)
    (mtorus-utils-symbol-conc
     'mtorus-type
     (mtorus-utils-keyword->symbol type)
     (mtorus-type-hook-short-name keyword)))

  :install
  (lambda (keyword type)
    (let ((hook-vname (mtorus-type-hook-name keyword type))
          (hook-rname (mtorus-utils-symbol-conc
                       'mtorus-type-run
                       (mtorus-type-hook-short-name keyword)))
          (hook-run-fun (mtorus-type-hook-run-fun keyword)))
      (mapc
       #'eval
       `((defvar ,hook-vname nil
           ,(format "Functions called ... DOCUMENT ME ... %s" keyword))))
      (set hook-vname nil)
      (fset hook-rname hook-run-fun)))

  :add-fun
  (lambda (keyword type function)
    (let ((hook-vname (mtorus-type-hook-name keyword type)))
      (add-hook hook-vname function)))

  :rem-fun
  (lambda (keyword type function)
    (let ((hook-vname (mtorus-type-hook-name keyword type)))
      (remove-hook hook-vname function)))

  :run-fun
  (lambda (keyword)
    `(lambda (type &optional element)
       ,(format "Runs `mtorus-type-TYPE-%s-funs"
                (mtorus-utils-keyword->symbol keyword))
       (let ((hook-vname (mtorus-type-hook-name ',keyword type)))
         (run-hook-with-args hook-vname element)))))



;; now the actual keywords

(define-mtorus-type-property :attachable-to
  :if-omitted (error "what"))

(define-mtorus-type-method :predicate
  :alias p)
(define-mtorus-type-method :alive-p)
(define-mtorus-type-method :inherit-value
  :if-omitted mtorus-type-method-fallback)
(define-mtorus-type-method :inherit-selection)
(define-mtorus-type-method :inherit-resurrection-data)
(define-mtorus-type-method :resurrect)

(define-mtorus-type-hook :pre-creation)
(define-mtorus-type-hook :post-creation)
(define-mtorus-type-hook :pre-deletion)
(define-mtorus-type-hook :post-deletion)
(define-mtorus-type-hook :pre-selection)
(define-mtorus-type-hook :post-selection)

    ;; hooks run when navigating around
(define-mtorus-type-hook :pre-choose)
(define-mtorus-type-hook :post-choose)
(define-mtorus-type-hook :pre-unchoose)
(define-mtorus-type-hook :post-unchoose)



;; Some of these are essential for MTorus and listed here:
;; 
;; - post-creation
;; function(s) to be called after some element of the
;; specified type has been created
;; 
;; - pre-deletion
;; function(s) to be called just before some element of the
;; specified type is about to be deleted
;; 
;; - post-deletion
;; function(s) to be called after some element of the
;; specified type has been deleted
;; 
;; - pre-selection
;; function(s) to be called just before some element of this type
;; is selected.
;; 
;; - post-selection
;; function(s) to be called after some element of this type
;; has been selected.
;; 
;; - pre-deselection
;; function(s) to be called just before some element of this type
;; is deselected (i.e. another element is selected).
;; This hook is actually almost the same as pre-selection but
;; it is called with the `old' element in contrast
;; 
;; - post-deselection
;; function(s) to be called after some element of this type
;; has been deselected (i.e. another element is selected).
;; This hook is actually almost the same as post-selection but
;; it is called with the `old' element in contrast
 



(defvar mtorus-types nil
  "List of available types.
This is for internal purposes only.
Do not fiddle with it.")

(defvar mtorus-type-obarray nil
  "Obarray holding registered types.")






(defmacro define-mtorus-type (name &rest properties)
  "Define an element type for mtorus-torii.
NAME is the name of the type and
PROPERTIES is a list of property names as keywords that describe
the type in detail.

Valid keywords are taken from the mtorus-type hook definitions.
For each of those keywords listed there this macro provides both a
function definition and a variable symbol that hold values given by
the according values in PROPERTIES.

For your convenience you can add or remove keywords later.
See mtorus-alter-type."
  (add-to-list 'mtorus-types name)
  (let ((type-name (mtorus-utils-symbol-conc 'mtorus-type name)))
    (mapc #'(lambda (keyword)
              (mtorus-type-hook-install keyword name))
          (mtorus-type-hook-list))
    (mapc
     #'eval
     `((defvar ,type-name nil
         ,(format "MTorus type."))
       (setq ,type-name ',properties))))
  (eval `(mtorus-alter-type ,name ,@properties))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))
(defalias 'mtorus-define-type 'define-mtorus-type)
(defalias 'mtorus-type-define 'define-mtorus-type)

(defun mtorus-update-type (type keyword value)
  "Updates TYPE."
  (let ((keyword (mtorus-utils-symbol->keyword keyword))
        (keysymb (mtorus-utils-keyword->symbol keyword)))
    (cond ((mtorus-type-hook-p keyword)
           (and value (mtorus-type-hook-add-fun keyword type value)))
          ;;((mtorus-type-convert-p keyword)
          ;;(and value
          ;;(mtorus-type-convert-install keysymb type value)))
          ((mtorus-type-method-p keyword)
           (let* ((expanded-type-method-name
                   (mtorus-utils-symbol-conc 'mtorus-type type keysymb))
                  (fun (cond (value)
                             (t (mtorus-utils-parse-key-cdr
                                 :if-omitted
                                 (cdr (assoc keyword mtorus-type))))))
                  (alias (mtorus-utils-parse-key-cdr
                          :alias
                          (cdr (assoc keyword mtorus-type)))))
             (if alias
                 (mtorus-type-method-install alias type fun)
               (mtorus-type-method-install keysymb type fun))
             (mtorus-type-method-install-invoke keysymb)))
          )))

(defmacro mtorus-alter-type (name &rest properties)
  "Updates an element type."
  (if (member name mtorus-types)
      (progn
        (mapc #'(lambda (keyw+fun)
                  (mtorus-update-type name (car keyw+fun) (cdr keyw+fun)))
              (mtorus-utils-parse-spec
               properties
               (mtorus-type-hook-list)))
        (mapc #'(lambda (method)
                  (mtorus-update-type name (car method) (cdr method)))
              (mtorus-utils-parse-spec
               properties
               (mtorus-type-method-list)))
;;         (mapc #'(lambda (convert)
;;                   (mtorus-update-type name (car convert) (cdr convert)))
;;               (mtorus-utils-parse-spec
;;                properties
;;                (mtorus-type-convert-list)))
        )
    (define-mtorus-type name properties))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))

(defun undefine-mtorus-type (name)
  "Undefine an element type or some of the element type handler functions.
NAME is the name of the type."
  (setq mtorus-types
        (remove name mtorus-types))
  (mapc #'(lambda (keyw)
            (let ((hname (mtorus-type-hook-name keyw name)))
              (eval
               `(makunbound ',hname))))
        (mtorus-type-hook-list))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))
(defalias 'mtorus-undefine-type 'undefine-mtorus-type)

(defmacro mtorus-unalter-type (name &rest properties)
  "Unregisters type handler functions of an element type."
  (when (member name mtorus-types)
    (mapc #'(lambda (keyw+fun)
              (let* ((keyw (car keyw+fun))
                     (fun (cdr keyw+fun)))
                (mtorus-type-hook-rem-fun keyw fun)))
          ;;; just parse for hook properties
          (mtorus-utils-parse-spec
           properties
           (mtorus-type-hook-list))))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))






;;;
;;; Type Filter
;;;
;; type filters limit lists of mtorus elements according to given types
;; they are special forms of the more general mtorus filters (which have yet to be implemented!!!)
;; This is just a quick hack, the more general forms of filters is still to be implemented

(defun mtorus-type-filter (type element &rest elements)
  "Runs a filter on ELEMENT (and possibly ELEMENTS).
Returns just the elements of type TYPE."
  (remove-if-not
   #'(lambda (elem)
       (mtorus-type-predicate type elem))
   (let ((elements (cond ((and (symbolp element)
                               (listp elements))
                          (cons element elements))
                         ((listp element)
                          element)
                         (t nil))))
     elements)))



;;; some UI functions

(defun mtorus-type-obarray (&optional type-filter)
  "Makes an obarray from `mtorus-types'.
Optional TYPE-FILTER limits this set to only certain types."
  (let ((types (or mtorus-types
                   (progn
                     (mtorus-type-initialize)
                     mtorus-types)))
        (filt (mtorus-element-type-filter->fun-preds
               type-filter))
        (type-obarray (vector)))
    (mapc #'(lambda (type)
              (setq type-obarray
                    (vconcat type-obarray (vector type))))
           types)
    type-obarray))




;;;
;;; Predefined types
;;;

  ;;; actually this will walk to mtorus.el some day
  ;;; because it isnt really backend
  ;;; at the moment we use bad auto-attachment code here :(

(defun mtorus-type-initialize ()
  "Initialization of predefined mtorus types."
  (interactive)

  (defvar mtorus-current-ring nil
    "Holds current element of type ring for internal use.")
  (defvar mtorus-current-element nil
    "Holds current element for internal use.")

  ;; classic mtorus-1.6 ring simulation
  (define-mtorus-type
    ring
    :predicate
    (lambda (element)
      "Determines if ELEMENT is a valid ring."
      (or (eq (mtorus-element-get-property 'type element) 'ring)
          (cond ((sequencep element)
                 (every (lambda (sub) (mtorus-element-p sub)) element))
                (t nil))))

    :inherit-selection
    (lambda (element)
      (setq mtorus-current-ring element)
      (mtorus-child-element) ;;; would cause infinite loops
      )

    :alive-p
    (lambda (element)
      t)

    :post-creation
    (lambda (element)
      (mtorus-fake-attach-ring-to-rings element)
      (mtorus-element-set-current element)
      (setq mtorus-current-ring element))

    ;;:post-selection
    )


  ;; more generic ring simulation
  (define-mtorus-type
    subring
    :predicate
    (lambda (element)
      "Determines if ELEMENT is a valid ring."
      (or (eq (mtorus-element-get-type element) 'ring)
          (eq (mtorus-element-get-type element) 'subring)))

    :inherit-selection
    (lambda (element)
      (setq mtorus-current-ring element)
      (mtorus-child-element) ;;; would cause infinite loops
      )

    :alive-p
    (lambda (element)
      t)

    :post-creation
    (lambda (element)
      (mtorus-fake-attach-element-to-children-of-element
       element (cond ((or (mtorus-type-subring-p mtorus-current-element)
                          (mtorus-type-ring-p mtorus-current-element))
                      mtorus-current-element)
                     (t (mtorus-fake-attach-get-current-ring-interactive)))
       #'(lambda (elements)
           (append (mtorus-type-filter 'ring elements)
                   (mtorus-type-filter 'subring elements))))
      (mtorus-element-set-current element)
      (setq mtorus-current-ring element))

    ;;:post-selection
    )



  ;; unknown type in mtorus-1.6
  (define-mtorus-type
    buffer
    :predicate
    (lambda (element)
      "Determines if ELEMENT is a valid buffer."
      (bufferp (mtorus-element-get-property 'value element)))

    :inherit-value
    (lambda (element)
      (current-buffer))

    :inherit-selection
    (lambda (element)
      (switch-to-buffer (mtorus-element-get-property 'value element)))

    :inherit-resurrection-data
    (lambda (element)
      (let ((val (mtorus-element-get-property 'value element)))
        `(:buffer-name ,(buffer-name val)
          :buffer-file-name ,(buffer-file-name val))))
    :resurrect
    (lambda (element)
      "Resurrects ELEMENT."
      (let ((res-data
             (mtorus-element-get-property 'resurrection-data element)))
        (and
         res-data
         (let ((buffer-name
                (mtorus-utils-plist-get res-data ':buffer-name))
               (buffer-filename  ;; buffer-file-name is already bound
                (mtorus-utils-plist-get res-data ':buffer-file-name)))
           (mtorus-element-put-property
            'value element
            (cond ((and buffer-name
                        (get-buffer buffer-name)))
                  ((and buffer-filename
                        (file-readable-p buffer-filename))
                   (find-file buffer-filename))))))))

    :alive-p
    (lambda (element)
      (buffer-live-p (mtorus-element-get-property 'value element)))
    
    :post-creation
    (lambda (element)
      (mtorus-fake-attach-element-to-children-of-ring
       element (mtorus-fake-attach-get-current-ring-interactive)
       #'(lambda (elements)
           (append (mtorus-type-filter 'buffer elements)
                   (mtorus-type-filter 'marker elements))))
      (mtorus-element-set-current element))

    :pre-selection
    (lambda (element)
      (unless (mtorus-type-buffer-alive-p element)
        ;;(mtorus-element-set-current (mtorus-determine-parent-element element))
        ;;(mtorus-element-detach element)
        ))
    )


  ;; classic mtorus-1.6 marker simulation
  (define-mtorus-type
    marker
    :predicate
    (lambda (element)
      "Determines if ELEMENT is a valid marker."
      (markerp (mtorus-element-get-property 'value element)))

    :inherit-value
    (lambda (element)
      (set-marker (make-marker) (point)))

    :inherit-selection
    (lambda (element)
      (let ((buf (marker-buffer
                  (mtorus-element-get-property 'value element))))
        (with-current-buffer buf
          (goto-char (marker-position
                      (mtorus-element-get-property 'value element))))
        (switch-to-buffer buf)))

    :alive-p
    (lambda (element)
      (buffer-live-p
       (marker-buffer (mtorus-element-get-property 'value element))))

    :inherit-resurrection-data
    (lambda (element)
      (let* ((val (mtorus-element-get-property 'value element))
             (buf (marker-buffer val)))
        `(:buffer-name ,(buffer-name buf)
          :buffer-file-name ,(buffer-file-name buf)
          :buffer-point ,(marker-position val))))
    :resurrect
    (lambda (element)
      "Resurrects ELEMENT."
      (let ((res-data
             (mtorus-element-get-property 'resurrection-data element)))
        (and
         res-data
         (let ((buffer-name
                (mtorus-utils-plist-get res-data ':buffer-name))
               (buffer-filename ;; buffer-file-name is already bound
                (mtorus-utils-plist-get res-data ':buffer-file-name))
               (buffer-point
                (mtorus-utils-plist-get res-data ':buffer-point)))
           (mtorus-element-put-property
            'value element
            (cond ((and buffer-name
                        (get-buffer buffer-name))
                   (set-marker (make-marker)
                               buffer-point
                               buffer-name))
                  ((and buffer-filename
                        (file-readable-p buffer-filename))
                   (set-marker (make-marker)
                               buffer-point
                               (find-file buffer-filename)))))))))

    :post-creation
    (lambda (element)
      (mtorus-fake-attach-element-to-children-of-element
       element (cond ((or (mtorus-type-buffer-p mtorus-current-element)
                          (mtorus-type-ring-p mtorus-current-element))
                      mtorus-current-element)
                     (t (mtorus-fake-attach-get-current-ring-interactive)))
       #'(lambda (elements)
           (append (mtorus-type-filter 'buffer elements)
                   (mtorus-type-filter 'marker elements))))
      (mtorus-element-set-current element))

    :pre-selection
    (lambda (element)
      (unless (mtorus-type-marker-alive-p element)
        ;;(mtorus-element-set-current (mtorus-determine-parent-element element))
        ;;(mtorus-element-detach element)
        ))
    )

  (define-mtorus-type
    file
    :predicate
    (lambda (element)
      "Determines if ELEMENT is a valid file."
      (let ((file (mtorus-element-get-value element)))
        (or (tramp-find-foreign-file-name-handler file)
            (file-readable-p file))))

    :inherit-value
    (lambda (element)
      (read-file-name "File Name: "))

    :inherit-selection
    (lambda (element)
      (find-file (mtorus-element-get-value element)))

    :alive-p
    (lambda (element)
      (let ((file (mtorus-element-get-value element)))
        (or (tramp-find-foreign-file-name-handler file)
            (file-readable-p file))))

    :post-creation
    (lambda (element)
      (mtorus-fake-attach-element-to-children-of-ring
       element (mtorus-fake-attach-get-current-ring-interactive)
       #'(lambda (elements)
           (append (mtorus-type-filter 'buffer elements)
                   (mtorus-type-filter 'marker elements))))
      (mtorus-element-set-current element))

    :pre-selection
    (lambda (element)
      (unless (mtorus-type-file-alive-p element)
        )))


  ;; furthermore there should be some user customization here,
  ;; im talking about mtorus-type-auto-register-at-topology-p or something
  ;; see ToDo
  )

(mtorus-type-initialize)


(defun mtorus-type-uninitialize ()
  "Uninitializes mtorus types"
  (interactive)
  (mapc 'undefine-mtorus-type mtorus-types))




(run-hooks 'mtorus-type-after-load-hook)


(provide 'mtorus-type)

;;; mtorus-type.el ends here
