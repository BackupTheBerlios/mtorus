;;; mtorus-element.el --- elements of the mtorus
;; $Id: mtorus-element.el,v 1.2 2004/07/01 23:57:27 hroptatyr Exp $
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


;;; History


;;; Code:

(require 'cl-extra) 
;;; sorry about that, but i use (every ...) and (some ...) constructions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-element nil
  "The elements of a torus."
  :tag "MTorus"
  :prefix "mtorus-element-"
  :group 'mtorus)
(defgroup mtorus-type nil
  "The types of elements."
  :tag "MTorus"
  :prefix "mtorus-type-"
  :group 'mtorus)


(defconst mtorus-element-version "Version: 0.1 $Revision: 1.2 $"
  "Version of mtorus-element backend.")
(defconst mtorus-type-version "Version: 0.1 $Revision: 1.2 $"
  "Version of mtorus-element backend.")

(defvar mtorus-elements-hash-table 'mtorus-elements
  "Holds the symbol where to find the current elements hash-table.")
(defvar mtorus-elements (make-hash-table :test 'equal)
  "Stores elements ever created to reference them later.
At the moment it's uncertain how the hash-table-entries look like.
At least for every bound element that is in use there's a hash-key.")
;; I finally decided for hash-tables because
;; 1. they are fast
;; 2. they have opaque support for basic operations 
;;    (such as adding, altering and removing of keys)
;; 3. they are easy to dump
;; 4. they approximate the set property of the mtorus-elements best
;;    (the arrangement order of the elements within the container is not
;;     not needed, further lists have longer access times for elements 
;;     beyond the cdddddddd...ddddddddr of the list)
;;
;; Though hash-tables themselves provide various facilties to not put
;; or get accidentally elements with the same key, the keys put into
;; this hash-table are (per default) 8 digits hexadecimal random number
;; cookies.



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
    :value)
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
;; currently there are not many of them but this will change ;)



;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now the real Code ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary stuff
;;;

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






;;;
;;; Types of ring elements
;;;
(defcustom mtorus-type-hooks-alist
  '((predicate . p)
    (pre-addition . pre-addition-funs)
    (post-addition . post-addition-funs)
    (pre-deletion . pre-deletion-funs)
    (post-deletion . post-deletion-funs)
    (pre-selection . pre-selection-funs)
    (post-selection . post-seletion-funs)
    (pre-deselection . pre-deselection-funs)
    (post-deselection . post-deselection-funs))
  "Alist of hook specifiers and corresponding hooks to be
added in the form `mtorus-type-<NAME>-<HOOKNAME>'.

Entries look like
  \(hook-specifier . hook-name\)

Some of these are essential for MTorus and listed here:

- predicate
predicate function(s) to validate elements.
Any of these function(s) should return `non-nil' iff element
is of the specified type

- pre-addition
function(s) to be called just before some element of the
specified type is added

- post-addition
function(s) to be called after some element of the
specified type has been added

- pre-selection
function(s) to be called just before some element of this type
is selected.

- post-selection
function(s) to be called after some element of this type
has been selected.

- pre-deselection
function(s) to be called just before some element of this type
is deselected (i.e. another element is selected).
This hook is actually almost the same as pre-selection but
it is called with the `old' element in contrast

- post-deselection
function(s) to be called after some element of this type
has been deselected (i.e. another element is selected).
This hook is actually almost the same as post-selection but
it is called with the `old' element in contrast


You will realize that exactly the actions in between the pre- and post-
hooks are the hook actions run by mtorus-element elements."
  :group 'mtorus-type)


(defvar mtorus-types nil
  "List of available types.
This is for internal purposes only.
Do not fiddle with it.")


(defmacro define-mtorus-element-type (name &rest properties)
  "Define an element type for mtorus-torii.
NAME is the name of the type and
PROPERTIES is a list of property names as keywords that describe
the type in detail.

Valid keywords are taken from the `mtorus-type-hooks-alist'
For each of those keywords listed there this macro provides both a
function definition and a variable symbol that hold values given by
the according values in PROPERTIES.

For your convenience you can add or remove keywords later.
See `mtorus-alter-element-type'."
  (add-to-list 'mtorus-types name)
  (mapc #'(lambda (handler)
            (let* ((hname (car handler))
                   (hhook (cdr handler))
                   (expanded-type-name
                    (mtorus-utils-symbol-conc 'mtorus-type name hhook))
                   (expanded-type-fun-name
                    (mtorus-utils-symbol-conc expanded-type-name 'function)))
              (eval
               `(defvar ,expanded-type-name nil
                 ,(format "Functions called ... %s" name)))
              (eval
               `(defun ,expanded-type-fun-name (element)
                 ,(format "Calls functions listed in the hook %s.
Each function is called with argument ELEMENT."
                          expanded-type-name)
                 (run-hook-with-args ',expanded-type-name element)))))
        mtorus-type-hooks-alist)
  (eval `(mtorus-alter-element-type ,name ,@properties))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))
(defalias 'mtorus-define-element-type 'define-mtorus-element-type)

(defmacro mtorus-alter-element-type (name &rest properties)
  "Updates an element type."
  (if (member name mtorus-types)
      (mapc #'(lambda (handler)
                (let* ((hname (car handler))
                       (hhook (cdr (assoc hname mtorus-type-hooks-alist)))
                       (expanded-type-name
                        (mtorus-utils-symbol-conc 'mtorus-type name hhook))
                       (fun (cdr handler)))
                  (add-hook expanded-type-name fun)))
            (mtorus-utils-parse-spec properties (mapcar 'car mtorus-type-hooks-alist)))
    (mtorus-define-element-type name properties))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))

(defmacro undefine-mtorus-element-type (name &rest properties)
  "Undefine an element type or some of the element type handler functions.
NAME is the name of the type and
PROPERTIES is a list of property names as keywords that describe
the type in detail.

Valid keywords are taken from the `mtorus-type-hooks-alist'
For each of those keywords listed there this macro provides both a
function definition and a variable symbol that hold values given by
the according values in PROPERTIES."
  (setq mtorus-types
        (remove name 'mtorus-types))
  (mapc #'(lambda (handler)
            (let* ((hname (car handler))
                   (hhook (cdr handler))
                   (expanded-type-name
                    (mtorus-utils-symbol-conc 'mtorus-type name hhook))
                   (expanded-type-fun-name
                    (mtorus-utils-symbol-conc expanded-type-name 'function)))
              (eval
               `(makunbound ,expanded-type-name))
              (eval
               `(fmakunbound ,expanded-type-fun-name))))
        mtorus-type-hooks-alist)
  `',(mtorus-utils-symbol-conc 'mtorus-type name))
(defalias 'mtorus-undefine-element-type 'undefine-mtorus-element-type)

(defmacro mtorus-unalter-element-type (name &rest properties)
  "Unregisters type handler functions of an element type."
  (when (member name mtorus-types)
    (mapc #'(lambda (handler)
              (let* ((hname (car handler))
                     (hhook (cdr (assoc hname mtorus-type-hooks-alist)))
                     (expanded-type-name
                      (mtorus-utils-symbol-conc 'mtorus-type name hhook))
                     (fun (cdr handler)))
                (remove-hook expanded-type-name fun)))
          (mtorus-utils-parse-spec properties (mapcar 'car mtorus-type-hooks-alist))))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))




;;;
;;; Predefined types
;;;

(defun mtorus-type-initialize ()
  "Initialization of predefined mtorus types."
  (interactive)
  (mtorus-define-element-type
   ring :predicate
   (lambda (element)
     (or (eq (mtorus-element-get-type element) 'ring)
         (cond ((sequencep element)
                (every (lambda (sub) (mtorus-element-p sub)) element))
               (t nil)))))
  (mtorus-define-element-type buffer :predicate bufferp)
  (mtorus-define-element-type marker :predicate markerp))

;;(mtorus-type-buffer-p 'mtorus-universe)
;;(mtorus-type-ring-p 'mtorus-universe)





;;;;
;;;; Elements
;;;;
;;;;

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
       (get element property default)))

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

(defun mtorus-element-create (&rest element-spec)
  "Creates and returns an mtorus-element.
Use ELEMENT-SPEC to determine the properties

ELEMENT-SPEC is a set of :keyword value pairs.

The mandatory keywords are:

  :type -- type of the element, this must be a valid 
    mtorus-element-type specifier
    see `define-mtorus-element-type'

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
  (let* ((keylist (append mtorus-element-mandatory-keywords
                          mtorus-element-optional-keywords
                          mtorus-element-additional-keywords))
         (e-spec (mtorus-element-parse-spec element-spec keylist
                                            mtorus-element-parse-unsupported-keywords))

         ;;; abstract this
         (e-type (or (cdr (assoc 'type e-spec))
                     (error "Omitted mandatory keyword :type")))
         (e-symbol (or (cdr (assoc 'symbol e-spec))
                       (funcall mtorus-element-generate-cookie-function e-type)))
         (e-name (or (cdr (assoc 'name e-spec))
                     (format "MTorus %s: %s" e-type e-symbol)))
         (e-value (cdr (assoc 'value e-spec))))
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
    e-symbol))



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
       (remhash element (eval mtorus-elements-hash-table))))

(defun mtorus-element-delete (element)
  "Deletes ELEMENT.
This is _hard deletion_, element gets unregistered at
`mtorus-elements-hash-table' and unbound.
If you wish to just unregister the element, see
`mtorus-element-unregister'.

Attention, deletion or unregistration of elements does
not (yet?) update rings that posess this element."
  (and (mtorus-element-p element)
       (mtorus-element-get-property element 'mtorus-element-deletable-p)
       (progn (mtorus-element-unregister element)
              (makunbound element))))

 
;; (defun mtorus-ring-flush-rings (&rest ignore)
;;   "Deletes all rings from `mtorus-rings'."
;;   (mapc #'mtorus-ring-delete-ring mtorus-rings))



;;; This is the initialization code for bootstrapping the mtorus-universe element
;; The mtorus-universe is an element of itself (see quine set theory)
(defun mtorus-element-init ()
  "Initializes mtorus-elements and bootstraps the mtorus-universe."
  (mtorus-type-initialize)
  (let ((mtorus-element-parse-unsupported-keywords t))

    ;; maybe it's better to retrieve in the following call just a
    ;; symbol that is valid in this scope only(?)
    ;; this would allow constructions like (with-mtorus-element FORMS)
    (mtorus-element-create :type 'ring
                           :symbol 'mtorus-universe
                           :name "MTorus universe"
                           :value (and (boundp 'mtorus-universe) mtorus-universe)
                           :description "This is the only pregenerated ring: The MTorus Universe"
                           :variable-documentation "This is the MTorus Universe.\nDON'T FIDDLE WITH THIS.")
    (mtorus-element-register 'mtorus-universe)))
;;(mtorus-type-ring-p 'mtorus-universe)

;;; ToDo:
;; element creation should (just like type creation) initialize some function handlers
;; - jump-next/previous handler
;; - select/unselect-element handler
;; (basically just the stuff we've added to the types definition)




(provide 'mtorus-element)

;;; mtorus-element.el ends here
