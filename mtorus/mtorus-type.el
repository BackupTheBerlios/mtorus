;;; mtorus-type.el --- types of the mtorus
;; $Id: mtorus-type.el,v 1.3 2004/08/01 14:09:38 hroptatyr Exp $
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


(defconst mtorus-type-version "Version: 0.1 $Revision: 1.3 $"
  "Version of mtorus-type backend.")


(defcustom mtorus-type-methods-alist
  '((predicate . p)
    (inherit-value . inherit-value)
    (inherit-selection . inherit-selection))
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
(yet have to be implemented)"
  :group 'mtorus-type)



(defcustom mtorus-type-hooks-alist
  '((pre-creation . pre-creation-funs)
    (post-creation . post-creation-funs)
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


(defmacro mtorus-type-run-methods-bouncer ()
  "Installs some useful mtorus-element-run-HOOKS funs."
  (mapc #'(lambda (method)
            (let ((mname (car method))
                  (mfunn (cdr method)))
            (eval
              `(defun
               ,(intern (format "mtorus-type-%s" mname))
               (type &optional element)
               ,(format "Runs mtorus-type-TYPE-%s" mfunn)
               (let ((fun (intern (format "mtorus-type-%s-%s"
                                          type ',mfunn))))
                 (funcall fun element))))))
    mtorus-type-methods-alist)
  t)
(defmacro mtorus-type-run-hooks-bouncer ()
  "Installs some useful mtorus-element-run-HOOKS funs."
  (mapc #'(lambda (hook)
            (eval
              `(defun
               ,(intern (format "mtorus-type-run-%s" hook))
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



;;;
;;; code for attaching
;;; REVISE ME!!!

(defmacro mtorus-topology-standard-define-element-attach (attach-to)
  "Defines `mtorus-attach-element-to-ATTACH-TO'."
  (let* ((attach-to
          (intern
           (replace-regexp-in-string
            "^mtorus-\\(.+\\)$" "\\1" (format "%s" attach-to))))
         (m+attach-to
          (mtorus-utils-symbol-conc 'mtorus attach-to))
         (fun-name
          (mtorus-utils-symbol-conc 'mtorus-attach-element-to attach-to)))
    (when (mtorus-topology-p 'standard)
      (mapc
       #'eval
       `((defun ,fun-name (element)
           ,(format "Attaches ELEMENT to %s" attach-to)
           ;;(add-to-list ,m+attach-to element)
           (mtorus-topology-standard-define-children (or ,m+attach-to
                                                         ',m+attach-to)
                                                     element)
           (mtorus-topology-standard-define-parents element
                                                    (or ,m+attach-to
                                                        ',m+attach-to))))))
    `',fun-name))

(mtorus-topology-standard-define-element-attach mtorus-universe)
(mtorus-topology-standard-define-element-attach mtorus-current-ring)


(defun mtorus-fake-attach-element-to-current (current element &optional type-filter)
  "Attaches ELEMENT to anything in current using the 'siblings relation."
  (when (mtorus-topology-p 'standard)
    (let ((siblings
           (mtorus-topology-standard-children
            (car (mtorus-topology-standard-parents current)))))
    (mapc #'(lambda (sibling)
              (mtorus-topology-standard-define-siblings sibling element))
          (if (functionp type-filter)
              (funcall type-filter siblings)
            siblings))
    (mtorus-topology-standard-define-siblings element element))))

;;(symbol-function 'mtorus-type-buffer-p)




;;;
;;; Predefined types
;;;

  ;;; actually this will walk to mtorus.el some day
  ;;; because it isnt really backend

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

    :post-creation mtorus-attach-element-to-universe
    ;;:post-selection
    )

  (add-hook 'mtorus-type-ring-post-creation-funs
            #'(lambda (element)
                (mtorus-fake-attach-element-to-current
                 mtorus-current-ring element
                 #'(lambda (elements)
                     (mtorus-type-filter 'ring elements)))
                (setq mtorus-current-element element)
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
    
    :post-creation mtorus-attach-element-to-current-ring)

  (add-hook 'mtorus-type-buffer-post-creation-funs
            #'(lambda (element)
                (mtorus-fake-attach-element-to-current
                 mtorus-current-element element
                 #'(lambda (elements)
                     (append (mtorus-type-filter 'buffer elements)
                             (mtorus-type-filter 'marker elements))))
                (setq mtorus-current-element element)))



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

    :post-creation mtorus-attach-element-to-current-ring)

  (add-hook 'mtorus-type-marker-post-creation-funs
            #'(lambda (element)
                (mtorus-fake-attach-element-to-current
                 mtorus-current-element element
                 #'(lambda (elements)
                     (append (mtorus-type-filter 'buffer elements)
                             (mtorus-type-filter 'marker elements))))
                (setq mtorus-current-element element)))

  ;; furthermore there should be some usre customization here,
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
