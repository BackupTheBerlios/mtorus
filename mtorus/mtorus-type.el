;;; mtorus-type.el --- types of the mtorus
;; $Id: mtorus-type.el,v 1.1 2004/07/28 01:44:24 hroptatyr Exp $
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

(require 'mtorus-utils)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-type nil
  "The types of elements."
  :tag "MTorus Types"
  :prefix "mtorus-type-"
  :group 'mtorus)


(defconst mtorus-type-version "Version: 0.1 $Revision: 1.1 $"
  "Version of mtorus-type backend.")



(defcustom mtorus-type-hooks-alist
  '((predicate . p)
    (pre-creation . pre-creation-funs)
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

- predicate
predicate function(s) to validate elements.
Any of these function(s) should return `non-nil' iff element
is of the specified type

- pre-creation
function(s) to be called just before some element of the
specified type is about to be created

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
                    (mtorus-utils-symbol-conc 'mtorus-type name hhook))
                   (expanded-type-fun-name
                    (mtorus-utils-symbol-conc expanded-type-name 'function)))
              (eval
               `(defvar ,expanded-type-name nil
                 ,(format "Functions called ... %s" name)))))
        mtorus-type-hooks-alist)
  (eval `(mtorus-alter-type ,name ,@properties))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))
(defalias 'mtorus-define-type 'define-mtorus-type)
(defalias 'mtorus-type-define 'define-mtorus-type)

(defmacro mtorus-alter-type (name &rest properties)
  "Updates an element type."
  (if (member name mtorus-types)
      (mapc #'(lambda (handler)
                (let* ((hname (car handler))
                       (hhook (cdr (assoc hname mtorus-type-hooks-alist)))
                       (expanded-type-name
                        (mtorus-utils-symbol-conc 'mtorus-type name hhook))
                       (fun (cdr handler)))
                  (and fun (add-hook expanded-type-name fun))))
            (mtorus-utils-parse-spec properties (mapcar 'car mtorus-type-hooks-alist)))
    (define-mtorus-type name properties))
  `',(mtorus-utils-symbol-conc 'mtorus-type name))

(defmacro undefine-mtorus-type (name &rest properties)
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

(mtorus-type-run-hooks-bouncer)



;;;
;;; Predefined types
;;;

  ;;; actually this will walk to mtorus.el some day
  ;;; because it isnt really backend
(defun mtorus-type-initialize ()
  "Initialization of predefined mtorus types."
  (interactive)

  ;; classic mtorus-1.6 ring simulation
  (define-mtorus-type
    ring
    :predicate
    (lambda (element)
      (or (eq (mtorus-element-get-type element) 'ring)
          (cond ((sequencep element)
                 (every (lambda (sub) (mtorus-element-p sub)) element))
                (t nil))))
    :post-creation
    (lambda (element)
      ;; first of all we set the `current ring'
      (setq mtorus-current-ring element)

      ;; here should follow code to register the element with respect
      ;; to the currently defined topology to the universe
      ;;(mtorus-topology- (eval mtorus-default-topology))
      ;;; this isnt the way to do it!
      ;;; rather add a bouncer mtorus-topology-insinuate-with-type or so
      )
    :post-selection
    (lambda (element)
      (setq mtorus-current-ring element)))
  
  ;; unknown type in mtorus-1.6
  (define-mtorus-type
    buffer
    :predicate bufferp
    :post-creation
    (lambda (element)
      ;; this is old style binding the element to a father
      (add-to-list mtorus-current-ring element)

      ;; here should follow code to register the element with respect
      ;; to the currently defined topology to the `current ring'
      ;;(mtorus-topology- (eval mtorus-default-topology))
      ;;; this isnt the way to do it!
      ;;; rather add a bouncer mtorus-topology-insinuate-with-type or so
      ))
  
  ;; classic mtorus-1.6 marker simulation
  (define-mtorus-type
    marker
    :predicate markerp
    :post-creation
    (lambda (element)
      ;; this is old style binding the element to a father
      (add-to-list mtorus-current-ring element)

      ;; here should follow code to register the element with respect
      ;; to the currently defined topology to the `current ring'
      ;;(mtorus-topology- (eval mtorus-default-topology))
      ;;; this isnt the way to do it!
      ;;; rather add a bouncer mtorus-topology-insinuate-with-type or so
      )))

(mtorus-type-initialize)




(provide 'mtorus-type)

;;; mtorus-type.el ends here
