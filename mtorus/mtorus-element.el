;;; mtorus-element.el --- elements of the mtorus
;; $Id: mtorus-element.el,v 1.8 2004/08/02 22:20:56 hroptatyr Exp $
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


(defconst mtorus-element-version "Version: 0.1 $Revision: 1.8 $"
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


;; following is currently not as abstract and methodologized as mtorus-types are

(defvar mtorus-element-mandatory-keywords
  '(:type)
  "List of keywords that are mandatory when creating elements via
`mtorus-element-create'.
DO NOT CHANGE this unless you know what you're doing.")

(defvar mtorus-element-optional-keywords
  '(:symbol
    :name
    :value
    :description
    :variable-documentation)
  "List of (optional) keywords when creating elements via
`mtorus-element-create'.
Again, do not change this unless you know what you're doing.

If you want to parse for your own keywords use
`mtorus-element-additional-keywords' instead.")

(defcustom mtorus-element-additional-keywords nil
  "List of additional keywords when creating elements via
`mtorus-element-create'.

Use this to parse for your own keywords."
  :group 'mtorus-element
  :type '(repeat (symbol :value ":keyword" :tag "Keyword")))

(defcustom mtorus-element-parse-unsupported-keywords nil
  "Whether to parse keywords (and copy them to object-plist) even
if they are not listed in one of
`mtorus-element-mandatory-keywords', `mtorus-element-optional-keywords',
`mtorus-element-additional-keywords'."
  :group 'mtorus-element
  :type 'boolean)




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

;;; REVISE ME!!!!!!!!!!
(defcustom mtorus-element-hooks-alist
  '((read-from-element . read-from-element-funs)
    (save-to-element . save-to-element-funs)
    (alter-element . alter-element-funs))
  "Alist of hook specifiers and corresponding hooks to be
added in the form `mtorus-element-<NAME>-<HOOKNAME>'.

This allows per-element specific functions.

Entries look like
  \(specifier . value\)

If `value' is a list ...

Some of the predefined specs:

- read-from-element

- save-to-element

- alter-element 
"
  :group 'mtorus-element)


;; next abstraction step would be to see elements just as another mtorus-type ;)




;;;
;;; Predicate and property funs for elements
;;;

(defun mtorus-element-p (element)
  "Checks whether ELEMENT is an mtorus element."
  (get element 'mtorus-element-p))

(defun mtorus-element-valid-p (element)
  "Checks whether ELEMENT is a registered mtorus element."
  (gethash element (eval mtorus-elements-hash-table)))

(defun mtorus-element-get-property (element property &optional default)
  "Checks whether ELEMENT is an mtorus element and
returns PROPERTY."
  (and (mtorus-element-p element)
       (or (get element property)
           default)))

(defmacro mtorus-element-get-property-bouncer (&rest keywords)
  "Installs some useful mtorus-element-get-* funs
according to KEYWORDS supplied."
  (mapc #'(lambda (prop)
            (eval
              `(defun
               ,(intern (format "mtorus-element-get-%s" prop))
               (element &optional default)
               ,(format "Checks whether ELEMENT is an mtorus element and
returns the %s property." prop)
               (mtorus-element-get-property element ',prop default))))
    keywords)
  t)

(mtorus-element-get-property-bouncer type name symbol)




;;;
;;; Handlers for creation (note these are not interactive, use wrappers)
;;;
(defcustom mtorus-element-generate-cookie-function 'mtorus-element-generate-cookie
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
  (intern (format "mtorus-%s-%.8x" (or type 'element) (random))))
;;(mtorus-element-generate-cookie 'ring)

(defun define-mtorus-element (&rest element-specs)
  "Creates and returns an mtorus-element.
Use ELEMENT-SPECS to determine the properties

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
    Defaults to `nil' if omitted.

Any further keywords to be parsed for should be added
to `mtorus-element-additional-keywords'.

Created elements are stored in `mtorus-elements' for reference."
  
  (run-hook-with-args 'mtorus-element-pre-creation-hook element-specs)

  (let* ((keylist (append mtorus-element-mandatory-keywords
                          mtorus-element-optional-keywords
                          mtorus-element-additional-keywords))
         (e-spec (mtorus-utils-parse-spec element-specs keylist
                                          mtorus-element-parse-unsupported-keywords))

         ;;; abstract this
         (e-type (or (cdr (assoc 'type e-spec))
                     (error "Omitted mandatory keyword :type")))
         (e-symbol (or (cdr (assoc 'symbol e-spec))
                       (funcall mtorus-element-generate-cookie-function e-type)))
         (e-name (or (cdr (assoc 'name e-spec))
                     (format "MTorus %s: %s" e-type e-symbol)))
         (e-value (cdr (assoc 'value e-spec))))

    (mtorus-type-run-pre-creation-funs e-type e-symbol)
    (set e-symbol e-value)

    ;;; this should move externally ... registration is not part of creation
    ;;(mtorus-element-register e-symbol)
 
    ;; const specs (not to be influenced by the user
    (mapc #'(lambda (propval)
              (let* ((prop (car propval))
                     (propval* (assoc prop e-spec)))
                (unless propval*
                  (add-to-list 'e-spec propval))))
          `((default-value . nil)
            (mtorus-element-p . t)
            (mtorus-element-ctime . ,(current-time))
            (mtorus-element-deletable-p . t)
            (mtorus-element-hidden-p . nil)))
    ;; set r-symbols object-plist
    (mapc #'(lambda (propval)
              (let ((prop (car propval))
                    (val (cdr propval)))
                (put e-symbol prop val)))
          e-spec)

    (mtorus-type-run-post-creation-funs e-type e-symbol)

    (run-hook-with-args 'mtorus-element-post-creation-hook e-symbol)

    e-symbol))
(defalias 'mtorus-define-element 'define-mtorus-element)
(defalias 'mtorus-element-define 'define-mtorus-element)
(defalias 'mtorus-element-create 'define-mtorus-element)


;;;
;;; Element handlers
;;;

(defun mtorus-element-register (element)
  "Registers ELEMENT at the elements container
identified by `mtorus-elements-hash-table'."
  (let ((type (mtorus-element-get-type element 'mtorus-element)))
    (and type
         (puthash element type (eval mtorus-elements-hash-table)))))
(defun mtorus-element-unregister (element)
  "Unregisters ELEMENT completely."
  (and (mtorus-element-valid-p element)
       (mtorus-element-detach element)
       (remhash element (eval mtorus-elements-hash-table))))

(defun mtorus-element-delete (element)
  "Deletes ELEMENT.
This is _hard deletion_, element gets unregistered at
`mtorus-elements-hash-table' and unbound.
If you wish to just unregister the element, see
`mtorus-element-unregister'.

Attention, deletion or unregistration of elements does
not (yet?) update rings that posess this element."
  (run-hook-with-args 'mtorus-element-pre-deletion-hook element)
  (and (mtorus-element-p element)
       (mtorus-element-get-property element 'mtorus-element-deletable-p)
       (progn (mtorus-element-unregister element)
              (makunbound element)))
  (run-hook-with-args 'mtorus-element-post-deletion-hook element)
  element)

(defun mtorus-element-detach (element)
  "Detaches ELEMENT completely from any relation."
  (and (mtorus-element-p element)
       (let ((elems (mtorus-topology-find 'standard element)))
         (mapc #'(lambda (rel)
                   (mtorus-topology-undefine-relation 'standard (cdr rel) element (car rel)))
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
       (mtorus-topology-undefine-relation 'standard relation element1 element2)
       (run-hook-with-args 'mtorus-element-post-detachment-hook element1 element2))
  element1)



 
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
  (mtorus-type-run-pre-selection-funs (mtorus-element-get-type element) element)
  (mtorus-type-inherit-selection (mtorus-element-get-type element) element)
  (mtorus-type-run-pre-selection-funs (mtorus-element-get-type element) element)
  (run-hook-with-args 'mtorus-element-post-selection-hook element)
  element)



;;; some UI functions

(defun mtorus-element-type-filter->fun-preds-1 (type-filter)
  "Makes a predicate function."
  (cond ((or (eq type-filter 'all)
             (null type-filter))
         (lambda (element type)
           t))
        ((symbolp type-filter)
         `(lambda (element type)
            (eq type ',type-filter)))
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
    (maphash #'(lambda (key val)
                 (and (some #'(lambda (pred-fun)
                                (funcall pred-fun key val))
                            filt)
                      (setq element-obarray
                            (vconcat element-obarray (vector key)))))
           mtorus-elements)
    element-obarray))
;;(mtorus-element-obarray '(marker buffer))

(defun mtorus-element-obarray-names (obarray &rest format)
  "Makes an obarray from OBARRAY returning the names of the elements.
Optional FORMAT determines the result."
  (mapcar #'(lambda (element)
              (let ((formspec (eval `(format ,@format))))
                (cons formspec element)))
          obarray))

(defun mtorus-element-obarray+names (&optional type-filter)
  "Makes an obarray from `mtorus-elements' returning the names of the elements.
Optional TYPE-FILTER limits this set to only certain types."
  (let ((eobarr (mtorus-element-obarray type-filter)))
    (mtorus-element-obarray-names eobarr "%s (%s)" '(mtorus-element-get-name element) 'element)))
;;(mtorus-element-obarray+names 'all)
;;(mtorus-element-obarray+names #'(lambda (element type) (unless (eq element 'mtorus-universe) t)))



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
    (mtorus-element-create :type 'ring
                           :symbol 'mtorus-universe
                           :name "MTorus universe"
                           :value (and (boundp 'mtorus-universe) mtorus-universe)
                           ;;:order 'mtorus-order-by-name
                           :description "This is the only pregenerated ring: The MTorus Universe"
                           :variable-documentation "This is the MTorus Universe.\nDON'T FIDDLE WITH THIS.")
    (mtorus-element-register 'mtorus-universe)))
;;(mtorus-type-ring-p 'mtorus-universe)

(mtorus-element-initialize)


(provide 'mtorus-element)

;;; mtorus-element.el ends here
