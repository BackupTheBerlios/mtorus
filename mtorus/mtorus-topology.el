;;; mtorus-topology.el --- topologies of the mtorus
;; $Id: mtorus-topology.el,v 1.1 2004/07/28 01:44:24 hroptatyr Exp $
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
(defgroup mtorus-topology nil
  "The topology of a torus."
  :tag "MTorus Topologies"
  :prefix "mtorus-topology-"
  :group 'mtorus)


(defconst mtorus-topology-version "Version: 0.1 $Revision: 1.1 $"
  "Version of mtorus-topology backend.")


;; We use a rather straightforward network/graph structure to induce a graph
;; (and thus a topology) on the set of elements:
;; 
;;           aunt - father - uncle
;;               \    |    /
;;    sister - current element - brother
;;               /         \
;;             daughter    son

(defvar mtorus-topologies nil
  "List of available topologies.
This is for internal purposes only.
Do not fiddle with it.")

(defcustom mtorus-current-topology nil
  "Current topology to use when navigating through the mtorus universe."
  :group 'mtorus-topology)

;; (defcustom mtorus-topology-alist
;;   '((neighborhood)
;;     (neighborhood-selectors))
;;   "Alist of function specifiers and corresponding funs used
;; for determining topology issues in the `mtorus-universe'.
;; 
;; These functions are really generic and neither per-type nor per-element!
;; 
;; If you want to hook into one of the latter cases use
;; `mtorus-type-hooks-alist' and `mtorus-element-hooks-alist' respectively.
;; 
;; Entries look like
;;   \(fun-specifier\)
;; 
;; Any of these is essential for MTorus and listed here:
;; 
;; - neighborhood
;; Function which takes an mtorus-element as argument and returns an alist
;; of \(neighborhood-keyword . neighbor\) pairs.
;; 
;; - neighborhood-selectors
;; Functions to be called to select an element of the neighborhood specified
;; by some neighborhood-keyword. This is an alist actually of the form
;;   \(nh-keyword . nh-keyword-selector-fun\)
;; 
;; A convenient way of defining topologies can be seen in the following sketch
;;  
;;            aunt - father - uncle
;;                \    |    /
;;     sister - current element - brother
;;                /         \
;;              daughter    son
;; 
;; In the mtorus standard topology the environment of `current element'
;; would be:
;; '\(\(:siblings sister brother\)
;;   \(:parents aunt father uncle\)
;;   \(:children daughter son\)\)
;; "
;;   :group 'mtorus-topology)

(defmacro define-mtorus-topology (name &rest properties)
  "Define an element topology for mtorus-torii.
NAME is the name of the topology and
PROPERTIES is a list of property names as keywords that describe
the topology in detail.

Valid keywords are taken from the `mtorus-topology-alist'
For each of those keywords listed there this macro provides both a
function definition and a variable symbol that hold values given by
the according values in PROPERTIES.

A topology for mtorus is a function which takes an mtorus-element as
argument and returns a `neighborhood', i.e. an alist of \(neighborhood-keyword "
  (add-to-list 'mtorus-topologies name)
  (let* ((topology-name
          (mtorus-utils-symbol-conc 'mtorus-topology name))
         (topology-neighborhoods-name
          (mtorus-utils-symbol-conc topology-name 'neighborhoods))
         (topology-neighborhoods
          (cdr-safe (mtorus-utils-parse-key :neighborhoods properties))))
    (eval
     `(defvar ,topology-name (make-hash-table :test 'equal)
       ,(format "MTorus topology.")))
    (eval
     `(defvar ,topology-neighborhoods-name topology-neighborhoods
       ,(format "MTorus topology.\nValue indicates registered neighborhoods")))
    (eval
     `(defmacro
       ,(mtorus-utils-symbol-conc topology-name 'add-neighborhood)
       (name predicate)
       ,(format "Define and add the neighborhood function NAME to %s"
                topology-name)
       (let ((neighborhood-name
              (mtorus-utils-symbol-conc ',topology-name name)))
         (eval
          ,(list
            'backquote
            `(defun ,'(\, neighborhood-name) (element)
              ,(format "MTorus neighborhood in %s"
                       topology-name)
              (let ((neighbors (gethash element ,topology-name)))
                (if neighbors
                    (eval ,'(\, predicate))
                  element)))))
         `',neighborhood-name)))
    `',(mtorus-utils-symbol-conc topology-name)))
(defalias 'mtorus-define-topology 'define-mtorus-topology)
(defalias 'mtorus-topology-define 'define-mtorus-topology)



  ;;; actually this will walk to mtorus.el some day
  ;;; because it isnt really backend
(defun mtorus-topology-initialize ()
  "Installs a topology on the current `mtorus-universe'."
  (interactive)
  
  ;; a trivial topology ... there are NO elements in the neighborhood
  (define-mtorus-topology
    trivial)

  ;; the standard topology
  (setq mtorus-current-topology
        (define-mtorus-topology
          standard
          :neighborhoods (siblings parents children)))

  (mapc #'(lambda (neighborhood)
            (eval
             `(mtorus-topology-standard-add-neighborhood
               ,neighborhood
               (let (,neighborhood)
                 (maphash #'(lambda (neighbor relation)
                              (and (eq relation ',neighborhood)
                                   (add-to-list ',neighborhood neighbor)))
                          neighbors)
                 ,neighborhood))))
        '(siblings parents children)))

(mtorus-topology-initialize)


(defcustom mtorus-default-topology 'mtorus-topology-standard
  "Topology inherited to all newly created elements."
  :group 'mtorus-element)



;;;;
;;;; Orders (part of topology)
;;;;
;;;;

(defvar mtorus-orders nil
  "List of available orders.
This is for internal purposes only.
Do not fiddle with it.")

(defmacro define-mtorus-order (name &rest properties)
  "Define an element order for mtorus-torii.
NAME is the name of the order and
PROPERTIES is a list of property names as keywords that describe
the order in detail.

defines a fun which takes a neighborhood and returns an ordered neighborhood."
  (add-to-list 'mtorus-orders name)
  (let* ((pred (cdr-safe (mtorus-utils-parse-key :predicate properties)))
         (ofun (or (cdr-safe (mtorus-utils-parse-key :order-fun properties))
                   'stable-sort))
         (order-fun-name
          (mtorus-utils-symbol-conc 'mtorus-order name))
         (order-pred-name
          (mtorus-utils-symbol-conc order-fun-name 'predicate)))
    (fset order-pred-name pred)
    (eval
     `(defun ,order-fun-name (neighborhood)
       ,(format "Determines the ordered set NEIGHBORHOOD using the order %s."
                name)
       (funcall ,ofun neighborhood #',order-pred-name)))
    `',order-fun-name))

;; predefined orders
(defun mtorus-order-initialize ()
  "Installs some predefined orders on the current `mtorus-universe'."
  (interactive)

  (define-mtorus-order
    by-name
    :predicate
    (lambda (el1 el2)
      (string< (format "%s" el1) (format "%s" el2)))
    :order-fun
    'stable-sort)

  (define-mtorus-order
    by-age
    :predicate
    (lambda (el1 el2)
      (time-less-p (get el1 'mtorus-element-ctime (current-time))
                   (get el2 'mtorus-element-ctime (current-time))))
    :order-fun
    'stable-sort))

(mtorus-order-initialize)

(defcustom mtorus-default-order 'mtorus-order-by-age
  "Order inherited to all newly created elements."
  :group 'mtorus-element)



(provide 'mtorus-topology)

;;; mtorus-topology.el ends here
