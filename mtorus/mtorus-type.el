;;; mtorus-type.el --- types of the mtorus
;; $Id: mtorus-type.el,v 1.6 2004/08/05 19:49:03 hroptatyr Exp $
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


(defconst mtorus-type-version "Version: 0.1 $Revision: 1.6 $"
  "Version of mtorus-type backend.")

(defcustom mtorus-type-properties-alist
  '((attachable-to . attachable-to))
  ""
  :group 'mtorus-type)


(defcustom mtorus-type-methods-alist
  '((predicate . p)
    (alive-p . alive-p)
    (inherit-value . value)
    (inherit-selection . select))
  "Alist of method specifiers and corresponding functions to be
added in the form `mtorus-type-<NAME>-<METHODNAME>'.

Entries look like
  \(method-specifier . method-name\)

Some of these are essential for MTorus and listed here:

- pre-creation
function(s) to be called just before some element of the
specified type is about to be created

- inherit-selection
function(s) to be passed to element creation.
This is useful if you want to set selection functions not
per element but per type.

Methods can be defined as functions whose return values are
essential.

Note that methods just operate on a single element.
If you intend to work on lists of elements use actions instead.
\(yet have to be implemented\)"
  :group 'mtorus-type)



(defcustom mtorus-type-hooks-alist
  '((pre-creation . pre-creation-funs)
    (post-creation . post-creation-funs)
    (pre-deletion . pre-deletion-funs)
    (post-deletion . post-deletion-funs)
    (pre-selection . pre-selection-funs)
    (post-selection . post-selection-funs)

    ;; hooks run when navigating around
    (pre-choose . pre-choose-funs)
    (post-choose . post-choose-funs)
    (pre-unchoose . pre-unchoose-funs)
    (post-unchoose . post-unchoose-funs))
  "Alist of hook specifiers and corresponding hooks to be
added in the form `mtorus-type-<NAME>-<HOOKNAME>'.

Entries look like
  \(hook-specifier . hook-name\)

Some of these are essential for MTorus and listed here:

- post-creation
function(s) to be called after some element of the
specified type has been created

- pre-deletion
function(s) to be called just before some element of the
specified type is about to be deleted

- post-deletion
function(s) to be called after some element of the
specified type has been deleted

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

(defvar mtorus-type-obarray nil
  "Obarray holding registered types.")


(defmacro define-mtorus-type (name &rest properties)
  "Define an element type for mtorus-torii.
NAME is the name of the type and
PROPERTIES is a list of property names as keywords that describe
the type in detail.

Valid keywords are taken from the `mtorus-type-hooks-alist'
For each of those keywords listed there this macro provides both a
function definition and a variable symbol that hold values given by
the according values in PROPERTIES.

For your convenience you can add or remove keywords later.
See `mtorus-alter-type'."
  (add-to-list 'mtorus-types name)
  (let ((type-name (mtorus-utils-symbol-conc 'mtorus-type name)))
    (mapc #'(lambda (handler)
              (let* ((hname (car handler))
                     (hhook (cdr handler))
                     (expanded-type-name
                      (mtorus-utils-symbol-conc 'mtorus-type name hhook)))
                (eval
                 `(defvar ,expanded-type-name nil
                   ,(format "Functions called ... DOCUMENT ME ... %s" name)))
                (set expanded-type-name nil)))
          mtorus-type-hooks-alist)
    (mapc
     #'eval
     `((defvar ,type-name nil
         ,(format "MTorus type."))
       (setq ,type-name ',properties))))
  (eval `(mtorus-alter-type ,name ,@properties))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))
(defalias 'mtorus-define-type 'define-mtorus-type)
(defalias 'mtorus-type-define 'define-mtorus-type)

(defmacro mtorus-alter-type (name &rest properties)
  "Updates an element type."
  (if (member name mtorus-types)
      (progn
        (mapc #'(lambda (handler)
                  (let* ((hname (car handler))
                         (hhook (cdr (assoc hname mtorus-type-hooks-alist)))
                         (expanded-type-name
                          (mtorus-utils-symbol-conc 'mtorus-type name hhook))
                         (fun (cdr handler)))
                    (and fun (add-hook expanded-type-name fun))))
              (mtorus-utils-parse-spec properties (mapcar #'car mtorus-type-hooks-alist)))
        (mapc #'(lambda (method)
                  (let* ((mname (car method))
                         (mfunn (cdr (assoc mname mtorus-type-methods-alist)))
                         (expanded-type-method-name
                          (mtorus-utils-symbol-conc 'mtorus-type name mfunn))
                         (fun (cdr method)))
                    (eval
                     `(defun ,expanded-type-method-name (&optional element &rest args)
                       ,(format "Function called to determine a(n) %s for an element." mname)
                       ,(and fun
                             `(funcall ,fun element))))))
              (mtorus-utils-parse-spec properties (mapcar #'car mtorus-type-methods-alist))))
    (define-mtorus-type name properties))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))

(defun undefine-mtorus-type (name)
  "Undefine an element type or some of the element type handler functions.
NAME is the name of the type."
  (setq mtorus-types
        (remove name mtorus-types))
  (mapc #'(lambda (handler)
            (let* ((hname (car handler))
                   (hhook (cdr handler))
                   (expanded-type-name
                    (mtorus-utils-symbol-conc 'mtorus-type name hhook))
                   (expanded-type-fun-name
                    (mtorus-utils-symbol-conc expanded-type-name 'function)))
              (eval
               `(makunbound ',expanded-type-name))
              (eval
               `(fmakunbound ',expanded-type-fun-name))))
        mtorus-type-hooks-alist)
  `',(mtorus-utils-symbol-conc 'mtorus-type name))
(defalias 'mtorus-undefine-type 'undefine-mtorus-type)

(defmacro mtorus-unalter-type (name &rest properties)
  "Unregisters type handler functions of an element type."
  (when (member name mtorus-types)
    (mapc #'(lambda (handler)
              (let* ((hname (car handler))
                     (hhook (cdr (assoc hname mtorus-type-hooks-alist)))
                     (expanded-type-name
                      (mtorus-utils-symbol-conc 'mtorus-type name hhook))
                     (fun (cdr handler)))
                (remove-hook expanded-type-name fun)))
          (mtorus-utils-parse-spec properties
                                   (mapcar 'car mtorus-type-hooks-alist))))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))


;;; REVISE ME!!!
;; here we need something that allows other `backend modules' to
;; add something like those spec pairs in e.g. mtorus-type-methods-alist
;; and install them by those bouncer functions (just like initialization does)
;; A revise for mtorus-alter-type is needed then!
;; We must not mapc on the methods/hooks-alist but on the actual spec keywords
;; that occur.

;;(mtorus-utils-add-spec



(defmacro mtorus-type-run-methods-bouncer ()
  "Installs some useful mtorus-type-METHOD funs."
  (mapc #'(lambda (method)
            (let ((mname (car method))
                  (mfunn (cdr method)))
              (eval
               `(defun ,(mtorus-utils-symbol-conc 'mtorus-type mname)
                 (type &optional element)
                 ,(format "Runs mtorus-type-TYPE-%s" mfunn)
                 (let ((fun (intern (format "mtorus-type-%s-%s"
                                            type ',mfunn))))
                   (funcall fun element))))))
        mtorus-type-methods-alist)
  t)
(defmacro mtorus-type-run-hooks-bouncer ()
  "Installs some useful mtorus-type-run-HOOKS funs."
  (mapc #'(lambda (hook)
            (eval
             `(defun ,(mtorus-utils-symbol-conc 'mtorus-type-run hook)
               (type &optional element)
               ,(format "Runs mtorus-type-TYPE-%s-function" hook)
               (let ((fun (intern (format "mtorus-type-%s-%s"
                                          type ',hook))))
                 (run-hook-with-args fun element)))))
        (mapcar 'cdr mtorus-type-hooks-alist))
  t)

(mtorus-type-run-methods-bouncer)
(mtorus-type-run-hooks-bouncer)


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
      (or (eq (mtorus-element-get-type element) 'ring)
          (cond ((sequencep element)
                 (every (lambda (sub) (mtorus-element-p sub)) element))
                (t nil))))

    :inherit-selection
    (lambda (element)
      (setq mtorus-current-ring element)
      (mtorus-child-element))

    :alive-p
    (lambda (element)
      t)

    :post-creation mtorus-attach-element-to-universe
    ;;:post-selection
    )

  (add-hook 'mtorus-type-ring-post-creation-funs
            #'(lambda (element)
                (mtorus-fake-attach-ring-to-rings element)
                (mtorus-element-set-current element)
                (setq mtorus-current-ring element)))



  ;; unknown type in mtorus-1.6
  (define-mtorus-type
    buffer
    :predicate
    (lambda (element)
      (bufferp (eval element)))

    :inherit-value
    (lambda (element)
      (current-buffer))

    :inherit-selection
    (lambda (element)
      (switch-to-buffer (eval element)))

    :alive-p
    (lambda (element)
      (buffer-live-p (eval element)))
    
    :post-creation mtorus-attach-element-to-current-ring

    :pre-selection
    (lambda (element)
      (unless (mtorus-type-buffer-alive-p element)
        ;;(mtorus-element-set-current (mtorus-determine-parent-element element))
        ;;(mtorus-element-detach element)
        )))

  (add-hook 'mtorus-type-buffer-post-creation-funs
            #'(lambda (element)
                (mtorus-fake-attach-element-to-children-of-ring
                 element (mtorus-fake-attach-get-current-ring)
                 #'(lambda (elements)
                     (append (mtorus-type-filter 'buffer elements)
                             (mtorus-type-filter 'marker elements))))
                (mtorus-element-set-current element)))



  ;; classic mtorus-1.6 marker simulation
  (define-mtorus-type
    marker
    :predicate
    (lambda (element)
      (markerp (eval element)))

    :inherit-value
    (lambda (element)
      (set-marker (make-marker) (point)))

    :inherit-selection
    (lambda (element)
      (let ((buf (marker-buffer (eval element))))
        (with-current-buffer buf
          (goto-char (marker-position (eval element))))
        (switch-to-buffer buf)))

    :alive-p
    (lambda (element)
      (buffer-live-p (marker-buffer (eval element))))

    :post-creation mtorus-attach-element-to-current-ring

    :pre-selection
    (lambda (element)
      (unless (mtorus-type-marker-alive-p element)
        ;;(mtorus-element-set-current (mtorus-determine-parent-element element))
        ;;(mtorus-element-detach element)
        )))

  (add-hook 'mtorus-type-marker-post-creation-funs
            #'(lambda (element)
                (mtorus-fake-attach-element-to-children-of-element
                 element (cond ((or (mtorus-type-buffer-p mtorus-current-element)
                                    (mtorus-type-ring-p mtorus-current-element))
                                mtorus-current-element)
                               (t (mtorus-fake-attach-get-current-ring)))
                 #'(lambda (elements)
                     (append (mtorus-type-filter 'buffer elements)
                             (mtorus-type-filter 'marker elements))))
                (mtorus-element-set-current element)))

  ;; furthermore there should be some usre customization here,
  ;; im talking about mtorus-type-auto-register-at-topology-p or something
  ;; see ToDo
  )

(mtorus-type-initialize)


(defun mtorus-type-uninitialize ()
  "Uninitializes mtorus types"
  (interactive)
  (mapc 'undefine-mtorus-type mtorus-types))

;; I'm currently thinking of a more generic way to define types
;; type definition should be in general merely memorizing the
;; type properties
;; `applications' or code that need some property of a type can
;; ask the type's variable value then
;;
;; this would provide a nice interface to future code
;;
;; Example definition of a type then (for example for attachment settings)
;; (define-mtorus-type ring
;;   :allow-topology standard  ;; this allows the ring type to be part of the topology standard
;;   :allow-topology mtorus16  ;; this allows the ring type to be part of the topology mtorus16
;;   :allow-relation (standard parents buffer) ;; ring can be the parent of a buffer
;;   :allow-relation (standard parents marker) ;; ring can be the parent of a marker
;;   :allow-relation (standard parents ring)   ;; ring can be the parent of a ring
;;   :allow-relation (standard siblings ring)   ;; ring can be the parent of a ring
;;   ...
;;   )


(run-hooks 'mtorus-type-after-load-hook)


(provide 'mtorus-type)

;;; mtorus-type.el ends here
