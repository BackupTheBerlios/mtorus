;;; mtorus-element.el --- elements of the mtorus
;; $Id: mtorus-element.el,v 1.16 2004/09/25 16:24:34 hroptatyr Exp $
;; Copyright (C) 2004 by Stefan Kamphausen
;;           (C) 2004 by Sebastian Freundt
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;;         Sebastian Freundt <hroptatyr@users.berlios.de>
;; Created: 2004/06/27
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
;; This is the second approach to a new abstract mtorus backend.
;; Regard the mtorus.el as frontend that uses the facilities of this file.
;;
;; Though we do not plan to establish more than one frontend this is seemingly
;; the best solution in order to provide comfortable development and support.
;;
;; For the moment see 
;;   http://www.math.tu-berlin.de/~freundt/MtorusPage.html
;; for some more verbose comments.
;; 
;; The first new thing you will notice is that in former times an mtorus consisted
;; of two categories of basic types: Namely rings and some other category usually
;; seen as the elements of the rings (buffers/markers).
;; 
;; However we got rid of this and implemented a more abstract view of this.
;; Currently there are elements only plus one specially treated symbol acting as
;; container: the mtorus-universe (available through the variable `mtorus-elements').
;; 
;; Actually this container is an element itself (namely one of type `ring'), but for
;; bootstrapping issues it is existent before any other element exists.
;; It is added to itself once it comes to initialization.
;;
;; The reason for this new second approach is that I realized during implementation
;; of the new abstract torus handling that adding rings to the torus and adding 
;; elements to a ring is basically the same.

;; *** ToDo:
;; - <add some> ;)


;;; History


;;; Code:

(require 'mtorus-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-element nil
  "The elements of a torus."
  :tag "MTorus Elements"
  :prefix "mtorus-element-"
  :group 'mtorus)


(defconst mtorus-element-version "Version: 0.3 $Revision: 1.16 $"
  "Version of mtorus-element backend.")


;; elements
(defvar mtorus-elements-hash-table 'mtorus-elements
  "Holds the symbol where to find the current elements hash-table.")
(defvar mtorus-elements (make-hash-table :test 'equal)
  "Stores elements ever created to reference them later.
At the moment it's uncertain how the hash-table-entries look like.
At least for every bound element that is in use there's a hash-key.")
;; I finally decided to organize elements in hash-tables because
;; - they are fast
;; - they have opaque support for basic operations 
;;   (such as adding, altering and removing of keys)
;; - they are easy to dump
;; - they approximate the set property of the mtorus-elements best
;;   (the arrangement order of the elements within the container is not
;;    not needed, further lists have longer access times for elements 
;;    beyond the cdddddddd...ddddddddr of the list)
;; 
;; Though hash-tables themselves provide various facilties to not put
;; or get accidentally elements with the same key, the keys put into
;; this hash-table are (per default) 8 digits hexadecimal random number
;; cookies.
;; 
;; At the moment it's uncertain how the hash-table-entries look like.
;; At least for every bound element that is in use there's a hash-key.



;;; stuff holding the current state of the torus
;; at least the current element is always available through this
;; variable.
;; We'll have to research some more on how to get the current 
;; element of an element, that is suppose a `ring' is the current
;; element and thus having further elements as contents
(defvar mtorus-current-element nil
  "Holds the cookie of the current element.")
(defvar mtorus-current-state nil
  "Holds the complete current state of the mtorus.")



;;;
;;; hooks
;;;
(defcustom mtorus-element-pre-creation-hook nil
  "Hook run before a new element is about to be created.
Note: This hook is run with given `element-specs' as argument."
  :group 'mtorus-element)
(defcustom mtorus-element-post-creation-hook nil
  "Hook run after a new element has been created."
  :group 'mtorus-element)
(defcustom mtorus-element-pre-deletion-hook nil
  "Hook run before an element is about to be deleted."
  :group 'mtorus-element)
(defcustom mtorus-element-post-deletion-hook nil
  "Hook run after an element has been deleted."
  :group 'mtorus-element)
(defcustom mtorus-element-pre-selection-hook nil
  "Hook run before an element is about to be selected."
  :group 'mtorus-element)
(defcustom mtorus-element-post-selection-hook nil
  "Hook run after an element has been selected."
  :group 'mtorus-element)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now the real Code ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;




;;;
;;; Handlers for creation (note these are not interactive, use wrappers)
;;;
(defcustom mtorus-element-generate-cookie-function
  'mtorus-element-generate-cookie
  "Function to be called when requesting element cookies.
This should be a function returning a cookie symbol which
in some way is (more or less) unique."
  :group 'mtorus-element
  :type 'function)

(defun mtorus-element-generate-cookie (&optional type)
  "Returns an element cookie.
If optional argument TYPE is provided the type of the
element is printed within the resulting cookie.

Otherwise and generally a cookie symbol is of the form
  mtorus-element-xxxxxxxx
is returned.
There xxxxxxxx is an 8-digit hexadecimal random number
and element is replaced by `type'.

Currently there is nothing performed to examine if a cookie
has already been given out thus you will have to check yourself
to avoid duplicate cookies.

See also: `mtorus-element-generate-cookie-function'"
  (mtorus-utils-symbol-conc  'mtorus (or type 'element)
                             (format "%.4x%.4x"
                                     (mod (random) 65536)
                                     (mod (random) 65536))))
;;(mtorus-element-generate-cookie 'ring)




;;; predefined keyword types

(define-mtorus-keyword-type mtorus-element property
  :get 
  (lambda (keyword el-property-ht &optional default)
    (gethash keyword el-property-ht default))

  :put
  (lambda (keyword el-property-ht value)
    (puthash keyword value el-property-ht)))

(define-mtorus-keyword-type mtorus-element method
    :install
  (lambda (keyword function)
    "Installs mtorus-element-KEYWORD."
    (let ((fun-name
           (mtorus-utils-symbol-conc
            'mtorus-element keyword)))
      (fset fun-name function))))
(define-mtorus-keyword-type mtorus-element hook)


(define-mtorus-element-property :type
  :if-omitted (error "Omitted mandatory keyword :type"))
(define-mtorus-element-property :symbol
  :if-omitted (funcall mtorus-element-generate-cookie-function :type))
(define-mtorus-element-property :name
  :if-omitted (format "MTorus %s: %s" :type :symbol))
(define-mtorus-element-property :value)
(define-mtorus-element-property :resurrection-data)
(define-mtorus-element-property :ctime
  :if-omitted (current-time))
(define-mtorus-element-property :atime
  :if-omitted (current-time))
(define-mtorus-element-property :deletable
  :if-omitted t)
;; (define-mtorus-element-property :order
;;   :if-omitted mtorus-order-by-age)

(define-mtorus-element-method :predicate
  :alias p)
(define-mtorus-element-method :valid-p)






;;;
;;; Predicate and property funs for elements
;;;

   ;;; until it's abstractly done by a define-element-method 
(mtorus-element-method-install
 'p
 (lambda (element)
   "Checks whether ELEMENT is an mtorus element."
   (gethash element (eval mtorus-elements-hash-table))))
(mtorus-element-method-install
 'valid-p
 (lambda (element)
   "Checks whether ELEMENT is a registered mtorus element."
   (gethash element (eval mtorus-elements-hash-table))))


(defun mtorus-element-get-property (property element &optional default)
  "Checks whether ELEMENT is an mtorus element and
returns PROPERTY."
  (and (mtorus-element-p element)
       (mtorus-element-property-get
        (mtorus-utils-keyword->symbol property)
        (gethash element (eval mtorus-elements-hash-table))
        default)))
(defun mtorus-element-put-property (property element value)
  "Checks whether ELEMENT is an mtorus element and
returns PROPERTY."
  (and (mtorus-element-p element)
       (mtorus-element-property-put
        (mtorus-utils-keyword->symbol property)
        (gethash element (eval mtorus-elements-hash-table))
        value)))
;;(mtorus-element-get-property :value 'mtorus-universe)

(defmacro mtorus-element-get-property-bouncer (&rest keywords)
  "Installs some useful mtorus-element-get-* funs
according to KEYWORDS supplied."
  (mapc #'(lambda (prop)
            (mapc
             #'eval
              `((defun ,(mtorus-utils-symbol-conc
                         'mtorus-element-get
                         (mtorus-utils-keyword->symbol prop))
                  (element &optional default)
                  ,(format "Checks whether ELEMENT is an mtorus element and
returns the %s property." prop)
                  (mtorus-element-get-property ',prop element default))
                (defun ,(mtorus-utils-symbol-conc
                         'mtorus-element-put
                         (mtorus-utils-keyword->symbol prop))
                  (element value)
                  ,(format "Checks whether ELEMENT is an mtorus element and
puts the %s property to VALUE." prop)
                  (mtorus-element-put-property ',prop element value)))))
        (mtorus-element-property-keywords))
  t)

(mtorus-element-get-property-bouncer)










(defun make-mtorus-element (&rest element-specs)
  "Creates and returns an mtorus-element property hash table.

ELEMENT-SPECS is a set of :keyword value pairs.

The mandatory keywords are:

  :type -- type of the element, this must be a valid 
    mtorus-type specifier
    see `define-mtorus-type'

Optional keywords are:

  :symbol -- symbol of the element
    If omitted `mtorus-element-generate-cookie-function'
    will be called to retrieve a symbol name.

  :name -- a human readable string name for the element
    Defaults to some descriptive composition of other
    keywords if omitted.

  :value -- the value of the symbol
    Defaults to `nil' if omitted."

  ;; look how an el-prop-ht looks like
  (let ((el-prop-ht (make-hash-table :test 'equal)))
    (mapc
     #'(lambda (keyword+specs)
         (let* ((keyword (car keyword+specs))
                (type (mtorus-keyword-get-type 'mtorus-element keyword))
                (kw-spec (cdr keyword+specs))
                (value
                 (or
                  (mtorus-utils-parse-key-cdr keyword element-specs)
                  (let ((if-om-fun
                         (mtorus-utils-parse-key-cdr :if-omitted kw-spec)))
                    (cond ((listp if-om-fun)
                           (eval
                            (mtorus-utils-keyword-type-replace-keyword
                             'mtorus-element
                             'property-get
                             if-om-fun el-prop-ht)))
                          (t if-om-fun))))))
           (mtorus-element-property-put
            (mtorus-utils-keyword->symbol keyword)
            el-prop-ht value)))
     mtorus-element)
    el-prop-ht))
;;(make-mtorus-element :type 'ring)


(defun define-mtorus-element (element-property-hashtable)
  "Creates and returns an mtorus-element and registers it.
ELEMENT-PROPERTY-HASHTABLE is a property hash-table returned by
make-mtorus-element.

Created elements are stored in `mtorus-elements' for reference."
  
  (run-hook-with-args 'mtorus-element-pre-creation-hook
                      element-property-hashtable)

  (let* ((el-prop-ht element-property-hashtable)
         (type (mtorus-element-property-get 'type el-prop-ht))
         (symbol (mtorus-element-property-get 'symbol el-prop-ht)))
    (mtorus-type-run-pre-creation-funs type symbol)

    ;; registration to not lose the element 
    (mtorus-element-register symbol el-prop-ht)
 
    (mtorus-type-run-post-creation-funs type symbol)
    (run-hook-with-args 'mtorus-element-post-creation-hook symbol)

    symbol))
(defalias 'mtorus-define-element 'define-mtorus-element)
(defalias 'mtorus-element-define 'define-mtorus-element)
(defalias 'mtorus-element-create 'define-mtorus-element)




;;;
;;; Element handlers
;;;

(defun mtorus-element-register (element element-properties)
  "Registers ELEMENT at the elements container
identified by `mtorus-elements-hash-table'."
  (let ((type (mtorus-element-property-get 'type
               element-properties 'mtorus-element)))
    (and type
         (puthash element element-properties
                  (eval mtorus-elements-hash-table)))))
(defun mtorus-element-unregister (element &optional force)
  "Unregisters ELEMENT completely.
ELEMENT is detached from any relation in any known topology."
  (when (or (and (mtorus-element-valid-p element)
                 (mtorus-element-detach element))
            force)
    (remhash element (eval mtorus-elements-hash-table))))

(defun mtorus-element-delete (element &optional force)
  "Deletes ELEMENT.
This is _hard deletion_, element gets unregistered at
`mtorus-elements-hash-table' and unbound.
If you wish to just unregister the element, see
`mtorus-element-unregister'.

Attention, deletion or unregistration of elements does
not (yet?) update rings that posess this element."
  (run-hook-with-args 'mtorus-element-pre-deletion-hook element)
  (when (or (and (mtorus-element-p element)
                 (mtorus-element-get-property 'deletable element))
            force)
    (mtorus-element-unregister element force)
    (run-hook-with-args 'mtorus-element-post-deletion-hook element)
    element))

(defun mtorus-element-detach (element)
  "Detaches ELEMENT completely from any relation."
  (and (mtorus-element-p element)
       (let ((elems (mtorus-topology-find 'standard element)))
         (mapc #'(lambda (rel)
                   (let ((el1 (car rel))
                         (el2 (cadr rel))
                         (rel (caddr rel)))
                     (mtorus-topology-undefine-relation 'standard rel el1 el2)))
               elems)))
  element)

(defun mtorus-element-detach-relations (element1 element2)
  "Detaches ELEMENT1 from any relation it has with ELEMENT2."
  (run-hook-with-args 'mtorus-element-pre-detachment-hook element1 element2)
  (and (mtorus-element-p element1)
       (mtorus-element-p element2)
       (let ((el1rels (remove-if-not
                       #'(lambda (rel)
                           (eq (car rel) element2))
                       (mtorus-topology-find 'standard element1)))
             (el2rels (remove-if-not
                       #'(lambda (rel)
                           (eq (car rel) element1))
                       (mtorus-topology-find 'standard element2))))
         (mapc #'(lambda (rel)
                   (mtorus-topology-undefine-relation 'standard (cdr rel) element1 element2))
               el1rels)
         (mapc #'(lambda (rel)
                   (mtorus-topology-undefine-relation 'standard (cdr rel) element1 element2))
               el2rels))
       (run-hook-with-args 'mtorus-element-post-detachment-hook element1 element2))
  element1)

;;; REVISE ME!!!! relation is not commutative
(defun mtorus-element-detach-relation (element1 element2 relation)
  "Detaches ELEMENT1 from RELATION it has with ELEMENT2."
  (run-hook-with-args 'mtorus-element-pre-detachment-hook element1 element2)
  (and (mtorus-element-p element1)
       (mtorus-element-p element2)
       (mtorus-topology-undefine-relation 'standard relation element1 element2)
       (mtorus-topology-undefine-relation 'standard relation element2 element1)
       (run-hook-with-args 'mtorus-element-post-detachment-hook element1 element2))
  element1)






;;; cleaning functions


;; ;;; this is butt ugly :(
;; (defun mtorus-element-detach-invalid ()
;;   ""
;;   (interactive)
;;   (let (els)
;;     (maphash #'(lambda (key val)
;;                  (and (not (mtorus-element-p key))
;;                       (add-to-list 'els key)))
;;              (eval mtorus-elements-hash-table))
;;     (mapcar #'(lambda (neighborhood)
;;                 (maphash #'(lambda (el1 ht)
;;                              (cond ((not (mtorus-element-p el1))
;;                                     (add-to-list 'els el1))
;;                                    (t
;;                                     (maphash #'(lambda (el2 rel)
;;                                                  (cond ((not (mtorus-element-p el2))
;;                                                         (add-to-list 'els el2))))
;;                                              ht))))
;;                          (eval
;;                           (mtorus-utils-symbol-conc
;;                            'mtorus-topology-standard neighborhood))))
;;             (mtorus-topology-neighborhoods 'standard))
;;     els))



 
(defun mtorus-element-set-current (element)
  "Sets ELEMENT as current element.
move me to mtorus-element.el?"
  (setq mtorus-current-element
        (or (and (mtorus-element-p element)
                 element)
            mtorus-current-element)))

(defun mtorus-element-select (element)
  "Selects ELEMENT.
This runs some hooks at the moment."
  (run-hook-with-args 'mtorus-element-pre-selection-hook element)
  (mtorus-type-run-pre-selection-funs (mtorus-element-get-property 'type element) element)
  (and (mtorus-element-alive-p element)
       (mtorus-element-inherit-selection element)
       (mtorus-element-put-atime element (current-time)))
  (mtorus-type-run-post-selection-funs (mtorus-element-get-property 'type element) element)
  (run-hook-with-args 'mtorus-element-post-selection-hook element)
  element)



;;; some UI functions

(defun mtorus-elements ()
  "Returns a list of element symbols."
  (let (elems)
    (maphash
     #'(lambda (key val)
         (setq elems
               (cons key elems)))
     (eval mtorus-elements-hash-table))
    elems))
;; (mtorus-elements)

(defun mtorus-element-type-filter->fun-preds-1 (type-filter)
  "Makes a predicate function."
  (cond ((or (eq type-filter 'all)
             (null type-filter))
         (lambda (elem elem-specs)
           t))
        ((symbolp type-filter)
         `(lambda (elem elem-specs)
            (equal (mtorus-element-property-get 'type elem-specs)
                   ',type-filter)))
        ((functionp type-filter)
         type-filter)
        ((eq (car type-filter) 'lambda)
         type-filter)
        (t nil)))
(defun mtorus-element-type-filter->fun-preds (type-filter)
  "Makes a predicate function list."
  (cond ((or (symbolp type-filter)
             (functionp type-filter)
             (eq (car type-filter) 'lambda))
         (list
          (mtorus-element-type-filter->fun-preds-1
           type-filter)))
        (t (mapcar #'mtorus-element-type-filter->fun-preds-1
                   type-filter))))
;;(mtorus-element-type-filter->fun-preds '(ring buffer))

(defun mtorus-element-obarray (&optional type-filter)
  "Makes an obarray from `mtorus-elements'.
Optional TYPE-FILTER limits this set to only certain types."
  (let ((filt (mtorus-element-type-filter->fun-preds
               type-filter))
        (element-obarray (vector)))
    (maphash
     #'(lambda (elem elem-specs)
         (and (some
               #'(lambda (pred-fun)
                   (funcall pred-fun elem elem-specs))
               filt)
              (setq element-obarray
                    (vconcat element-obarray (vector elem)))))
     (eval mtorus-elements-hash-table))
    element-obarray))
;;(mtorus-element-obarray '(marker buffer))
;;(mtorus-element-obarray #'(lambda (element type) (unless (eq element 'mtorus-universe) t)))

(defun mtorus-element-obarray-names (obarr &rest format)
  "Makes an obarray from OBARRAY returning the names of the elements.
Optional FORMAT determines the result."
  (mapcar
   #'(lambda (element)
       (let ((formspec (eval `(format ,@format))))
         (cons formspec element)))
   obarr))

(defun mtorus-element-obarray+names (&optional type-filter)
  "Makes an obarray from `mtorus-elements' returning the names of the elements.
Optional TYPE-FILTER limits this set to only certain types."
  (let ((eobarr (mtorus-element-obarray type-filter)))
    (mtorus-element-obarray-names
     eobarr "%s"
     '(mtorus-element-get-property 'name element))))
;;(mtorus-element-obarray+names 'all)
;;(mtorus-element-obarray+names #'(lambda (element type) (unless (eq element 'mtorus-universe) t)))


(defun mtorus-fake-attach-get-current-ring-interactive ()
  "Like mtorus-fake-attach-get-current-ring but asks
if a current ring cannot be determined."
  (or (mtorus-fake-attach-get-current-ring)
      (let ((table (mtorus-element-obarray+names
                    #'(lambda (element elem-spec)
                        (and (mtorus-type-ring-p element)
                             (unless (eq element 'mtorus-universe)
                               t))))))
        (setq mtorus-current-ring
              (cdr (assoc (completing-read
                           "Attach to ring: " table nil t)
                          table))))))
;;(mtorus-fake-attach-get-current-ring-interactive)




;;; This is the initialization code for bootstrapping the mtorus-universe element
;; The mtorus-universe is an element of itself (see quine set theory)
(defun mtorus-element-initialize ()
  "Initializes mtorus-elements and bootstraps the mtorus-universe."
  (interactive)
  (and (featurep 'mtorus-type)
       (mtorus-type-initialize))
  (let ((mtorus-element-parse-unsupported-keywords t))

    ;; maybe it's better to retrieve in the following call just a
    ;; symbol that is valid in this scope only(?)
    ;; this would allow constructions like (with-mtorus-element FORMS)
    (define-mtorus-element
      (make-mtorus-element
       :type 'ring
       :symbol 'mtorus-universe
       :name "MTorus universe"
       :value (and (boundp 'mtorus-universe)) ;; (mtorus-element-get-value 'mtorus-universe))
       :description "This is the only pregenerated ring: The MTorus Universe"))))

(mtorus-element-initialize)

;;(symbol-function 'mtorus-element-get-property)


(run-hooks 'mtorus-element-after-load-hook)


(provide 'mtorus-element)

;;; mtorus-element.el ends here
