;;; mtorus-topology.el --- topologies of the mtorus
;; $Id: mtorus-topology.el,v 1.12 2004/09/04 02:37:32 hroptatyr Exp $
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
;; - undefinition of relation must be done more cleanly


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


(defconst mtorus-topology-version "Version: 0.1 $Revision: 1.12 $"
  "Version of mtorus-topology backend.")


;; We use a rather straightforward network/graph structure to induce a graph
;; (and thus a topology) on the set of elements:
;; 
;;           aunt - father - uncle
;;               \    |    /
;;    sister - current element - brother
;;               /    |    \
;;          niece - child - nephew

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
    (mapc
     #'eval
     `((defvar ,topology-name (make-hash-table :test 'equal)
         ,(format "MTorus topology."))
       (defvar ,topology-neighborhoods-name topology-neighborhoods
         ,(format "MTorus topology.\nValue indicates registered neighborhoods"))


       ;;; find functions
       ;;; find functions
     ;; this is the find function `mtorus-topology-<TOPO>-find'
       (defun ,(mtorus-utils-symbol-conc topology-name 'find)
         (element)
         ,(format "Finds all occurences of ELEMENT (along with their neighborhood type")
         (let ((neighborhoods (mtorus-topology-neighborhoods ',name))
               (relations))
           (mapc #'(lambda (neighborhood)
                     (maphash
                      #'(lambda (el1 ht)
                          (maphash
                           #'(lambda (el2 rel)
                               (and (or (equal el1 element)
                                        (equal el2 element))
                                    (add-to-list 'relations
                                                 (list el1 el2 rel))))
                           ht))
                      (eval
                       (mtorus-utils-symbol-conc
                        'mtorus-topology ',name neighborhood))))
                 neighborhoods)
           relations))
       
       ;; this is the find function `mtorus-topology-<TOPO>-find-relation'
       (defun ,(mtorus-utils-symbol-conc topology-name 'find-relation)
         (element relation)
         ,(format "Finds all occurences of ELEMENT along with relation RELATION.")
         (let ((neighborhoods (mtorus-topology-neighborhoods ',name))
               (relations))
           (mapc #'(lambda (neighborhood)
                     (maphash
                      #'(lambda (el1 ht)
                          (maphash
                           #'(lambda (el2 rel)
                               (and (or (equal el1 element)
                                        (equal el2 element))
                                    (equal rel relation)
                                    (add-to-list 'relations
                                                 (list el1 el2 rel))))
                           ht))
                      (eval
                       (mtorus-utils-symbol-conc
                        'mtorus-topology ',name relation))))
                 neighborhoods)
           relations))
     
       ;; an obarray function
       (defun ,(mtorus-utils-symbol-conc topology-name 'neighborhood 'obarray)
         (&optional filter)
         ,(format "Makes an obarray from `%s'.\nOptional FILTER limits this set to only certain neighborhoods."
                  (mtorus-utils-symbol-conc topology-name 'neighborhoods))
         (let ((neighborhood-obarray (vector)))
           (mapc #'(lambda (nhood)
                     (setq neighborhood-obarray
                           (vconcat neighborhood-obarray (vector nhood))))
                 ,(mtorus-utils-symbol-conc topology-name 'neighborhoods))
           neighborhood-obarray))

       ;; the neighborhood predicate
       (defun ,(mtorus-utils-symbol-conc topology-name 'neighborhood 'p)
         (neighborhood)
         ,(format "Checks if NEIGHBORHOOD is a valid neighbordhood in %s"
                  topology-name)
         (member neighborhood
                 (mtorus-topology-neighborhoods ',name)))


       ;; the neighborhood name function
       (defun ,(mtorus-utils-symbol-conc topology-name 'neighborhood 'name)
         (neighborhood)
         ,(format "Converts given NEIGHBORHOOD as %s-NEIGHBORHOOD to 'NEIGHBORHOOD"
                  topology-name)
         (intern
          (mtorus-utils-replace-regexp-in-string
           ,(format "%s-" name)
           ""
           (format "%s" (mtorus-utils-replace-regexp-in-string
                         "mtorus-topology-"
                         ""
                         (format "%s" neighborhood))))))


       ;; the mtorus-topology-<TOPO>-neighborhood function
       (defun ,(mtorus-utils-symbol-conc topology-name 'neighborhood)
         (neighborhood element)
         ,(format "Return neighbors of element with respect to NEIGHBORHOOD
in %s" topology-name)
         (let ((nh (,(mtorus-utils-symbol-conc topology-name 'neighborhood-name)
                    neighborhood)))
           (when (,(mtorus-utils-symbol-conc topology-name 'neighborhood 'p)
                  nh)
             (funcall
              (mtorus-utils-symbol-conc ',topology-name nh)
              element))))

       ;;; the more general macro: define-mtorus-topology-neighborhood
             ;;; <insert me here> 


       ;;; this is the define-mtorus-topology-<TOPO>-neighborhood macro
       (defmacro
         ,(mtorus-utils-symbol-conc 'define topology-name 'neighborhood)
         (name &rest properties)
         ,(format "Define and add the neighborhood function NAME to %s"
                  topology-name)
         (let ((neighborhood-name
                (mtorus-utils-symbol-conc ',topology-name name))
               (def-nh-relation-name
                 (mtorus-utils-symbol-conc ',topology-name 'define name))
               (def-nh-relation-family-name
                 (mtorus-utils-symbol-conc ',topology-name 'define name 'family))
               (undef-nh-relation-name
                (mtorus-utils-symbol-conc ',topology-name 'undefine name))
               (undef-nh-all-relation-name
                (mtorus-utils-symbol-conc ',topology-name 'undefine 'all name))
               (undirected-relation-p
                (or (mtorus-utils-parse-key-cdr ':undirected properties)
                    (not (mtorus-utils-parse-key-cdr ':directed properties t)))))
           (mapc
            #'eval
            ,(list
              'backquote
              `((defvar ,'(\, neighborhood-name) (make-hash-table :test 'equal)
                  ,(format "MTorus neighborhood."))

                ;;; more general definition function of relations
                ;; e.g. mtorus-topology-standard-define-relation
                (defun ,(mtorus-utils-symbol-conc topology-name 'define-relation)
                  (neighborhood element1 element2)
                  ,(format "Defines NEIGHBORHOOD relation in %s between ELEMENT1 and ELEMENT2."
                           topology-name)
                  ;;(when (,(mtorus-utils-symbol-conc topology-name 'neighborhood-p) neighborhood)
                  (funcall (mtorus-utils-symbol-conc ',topology-name 'define neighborhood)
                           element1 element2))

                ;;; more general undefinition function of relations
                ;; e.g. mtorus-topology-standard-undefine-relation
                (defun ,(mtorus-utils-symbol-conc topology-name 'undefine-relation)
                  (neighborhood element1 element2)
                  ,(format "Defines NEIGHBORHOOD relation in %s between ELEMENT1 and ELEMENT2."
                           topology-name)
                  ;;(when (,(mtorus-utils-symbol-conc topology-name 'neighborhood-p) neighborhood)
                  (funcall (mtorus-utils-symbol-conc ',topology-name 'undefine neighborhood)
                           element1 element2))


                ;;; e.g. mtorus-topology-standard-define-siblings
                (defun ,'(\, def-nh-relation-name) (element1 element2)
                  ,(list
                    '\,
                    `(format
                      "Defines a %s relation (%s) between ELEMENT1 and ELEMENT2 in %s"
                      name
                      (if undirected-relation-p
                          "undirected"
                        "directed")
                      ',topology-name))
                  (and (mtorus-element-p element1)
                       (mtorus-element-p element2)
                       (let ((el1-hashtable (or (gethash element1 ,'(\, neighborhood-name))
                                                (puthash element1
                                                         (make-hash-table :test 'equal)
                                                         ,'(\, neighborhood-name))))
                             (el2-hashtable (and
                                             ,'(\, undirected-relation-p)
                                             (or (gethash element2
                                                          ,'(\, neighborhood-name))
                                                 (puthash element2
                                                          (make-hash-table :test 'equal)
                                                          ,'(\, neighborhood-name))))))
                         (and ,'(\, undirected-relation-p)
                              (puthash element1 ','(\, name) el2-hashtable))
                         (puthash element2 ','(\, name) el1-hashtable))))

                ;; eg mtorus-topology-standard-define-siblings-family
                (defun ,'(\, def-nh-relation-family-name) (element1 element2 &optional relation)
                  ,(list
                    '\,
                    `(format
                      "Defines a %s relation \(%s\) between ELEMENT1 and all neighbors of ELEMENT2 in %s.
Optional argument RELATION defines how to determine the neighbors of ELEMENT2
\(by default with the '%s relation\)."
                      name
                      (if undirected-relation-p
                          "undirected"
                        "directed")
                      ',topology-name
                      name))
                  (and (mtorus-element-p element1)
                       (mtorus-element-p element2)
                       (let* ((family-rel (or relation
                                              (mtorus-topology-neighborhood-name
                                               ,'(\, neighborhood-name))))
                              (el1-hashtable (or (gethash element1 ,'(\, neighborhood-name))
                                                 (puthash element1
                                                          (make-hash-table :test 'equal)
                                                          ,'(\, neighborhood-name))))
                              (el2-neighbors (mtorus-topology-neighborhood
                                              ',name
                                              family-rel
                                              element2)))
                         (mapc #'(lambda (elem)
                                   (,'(\, def-nh-relation-name) element1 elem))
                               el2-neighbors))))


                ;;; e.g. mtorus-topology-standard-undefine-siblings
                (defun ,'(\, undef-nh-relation-name) (element1 element2)
                  ,(list
                    '\,
                    `(format
                      "Deletes a %s relation (%s) between ELEMENT1 and ELEMENT2 in %s"
                      name 
                      (if undirected-relation-p
                          "undirected"
                        "directed")
                      ',topology-name))
                  (and (mtorus-element-p element1)
                       (mtorus-element-p element2)
                       (let ((neighbors (or (gethash element1
                                                     ,'(\, neighborhood-name))
                                            (puthash element1
                                                     (make-hash-table :test 'equal)
                                                     ,'(\, neighborhood-name))))
                             (neighbors-rev (and
                                             ,'(\, undirected-relation-p)
                                             (or (gethash element2
                                                          ,'(\, neighborhood-name))
                                                 (puthash element2
                                                          (make-hash-table :test 'equal)
                                                          ,'(\, neighborhood-name))))))
                         (and ,'(\, undirected-relation-p)
                              (remhash element1 neighbors-rev))
                         (remhash element2 neighbors))))

                ;; e.g. mtorus-topology-standard-undefine-all-siblings
                (defun ,'(\, undef-nh-all-relation-name) (element)
                  ,(list
                    '\,
                    `(format
                      "Deletes all %s relations (%s) of ELEMENT in %s"
                      name 
                      (if undirected-relation-p
                          "undirected"
                        "directed")
                      ',topology-name))
                  (and (mtorus-element-p element)
                       (let ((neighbors (mtorus-topology-neighborhood
                                         ',name ','(\, name) element)))
                         (mapc #'(lambda (elem)
                                   (mtorus-topology-undefine-relation
                                    ',name
                                    ','(\, name)
                                    element elem))
                               (remove element neighbors)))))


                ;;; e.g. mtorus-topology-standard-siblings
                (defun ,'(\, neighborhood-name) (element)
                  ,(format "MTorus neighborhood in %s"
                           topology-name)
                  (let ((neighbors (gethash element ,'(\, neighborhood-name))))
                    (and neighbors
                         ,'(\,
                            (cdr-safe
                             (mtorus-utils-parse-key ':filter properties)))))))))
           `',neighborhood-name))
       (and (featurep 'xemacs)
            (defalias ',(mtorus-utils-symbol-conc topology-name 'define 'neighborhood)
              ',(mtorus-utils-symbol-conc 'define topology-name 'neighborhood)))))
    `',topology-name))
(defalias 'mtorus-define-topology 'define-mtorus-topology)
(defalias 'mtorus-topology-define 'define-mtorus-topology)
  ;;; TODO: add some check if the elements passed to the funs are really registered



;;; some auxiliary funs

(defun mtorus-topology-name (topology)
  "Converts given TOPOLOGY as mtorus-topology-TOPOLOGY to 'TOPOLOGY"
  (intern
   (mtorus-utils-replace-regexp-in-string
    "mtorus-topology-"
    ""
    (format "%s" topology))))

(defun mtorus-topology-p (topology)
  "Checks if TOPOLOGY is a valid mtorus-topology."
  (when (member (mtorus-topology-name topology)
                mtorus-topologies)
    t))

;; (defun mtorus-topology-neighborhood-name (topology neighborhood)
;;   "Converts given NEIGHBORHOOD as mtorus-topology-TOPOLOGY-NEIGHBORHOOD to 'NEIGHBORHOOD"
;;   (intern
;;    (mtorus-utils-replace-regexp-in-string
;;     "mtorus-topology-"
;;     ""
;;     (format "%s" topology))))


(defun mtorus-topology-neighborhoods (topology)
  "Return all neighborhoods currently registered with TOPOLOGY."
  (eval (mtorus-utils-symbol-conc 'mtorus-topology
                                  (mtorus-topology-name topology)
                                  'neighborhoods)))

(defun mtorus-topology-neighborhood-obarray (topology &optional filter)
  "Makes an obarray from `mtorus-topology-<TOPOLOGY>-neighborhoods'.
Optional FILTER limits this set to only certain neighborhoods."
  (when (mtorus-topology-p topology)
    (funcall
     (mtorus-utils-symbol-conc 'mtorus-topology (mtorus-topology-name topology)
                               'neighborhood-obarray)
     filter)))


(defun mtorus-topology-neighborhood (topology neighborhood element)
  "Return ELEMENT's NEIGHBORHOOD in TOPOLOGY."
  (let ((topo (mtorus-topology-name topology)))
    (when (mtorus-topology-p topo)
      (funcall
       (mtorus-utils-symbol-conc 'mtorus-topology topo 'neighborhood)
       neighborhood element))))


(defun mtorus-topology-find (topology element)
  "Return all occurences of ELEMENT in TOPOLOGY
\(along with their neighborhood type\)."
  (when (mtorus-topology-p topology)
    (funcall
     (mtorus-utils-symbol-conc
      'mtorus-topology (mtorus-topology-name topology) 'find)
     element)))

(defun mtorus-topology-find-relation (topology element relation)
  "Return all occurences of ELEMENT in TOPOLOGY with
neighborhood type RELATION."
  (when (mtorus-topology-p topology)
    (funcall
     (mtorus-utils-symbol-conc
      'mtorus-topology (mtorus-topology-name topology) 'find)
     element)))


(defun mtorus-topology-define-relation (topology neighborhood element1 element2)
  "Defines NEIGHBORHOOD relation in TOPOLOGY between ELEMENT1 and ELEMENT2."
  (when (mtorus-topology-p topology)
    (funcall
     (mtorus-utils-symbol-conc
      'mtorus-topology (mtorus-topology-name topology) 'define-relation)
     neighborhood element1 element2)))

(defun mtorus-topology-undefine-relation (topology neighborhood element1 element2)
  "Defines NEIGHBORHOOD relation in TOPOLOGY between ELEMENT1 and ELEMENT2."
  (when (mtorus-topology-p topology)
    (funcall
     (mtorus-utils-symbol-conc
      'mtorus-topology (mtorus-topology-name topology) 'undefine-relation)
     neighborhood element1 element2)))








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
             `(define-mtorus-topology-standard-neighborhood
               ,neighborhood
               :undirected t
               :filter
               (let (,neighborhood)
                 (maphash #'(lambda (neighbor relation)
                              (and (eq relation ',neighborhood)
                                   (add-to-list ',neighborhood neighbor)))
                          neighbors)
                 ,neighborhood))))
        '(siblings))
  (mapc #'(lambda (neighborhood)
            (eval
             `(define-mtorus-topology-standard-neighborhood
               ,neighborhood
               :undirected nil
               :filter
               (let (,neighborhood)
                 (maphash #'(lambda (neighbor relation)
                              (and (eq relation ',neighborhood)
                                   (add-to-list ',neighborhood neighbor)))
                          neighbors)
                 ,neighborhood))))
        '(parents children))

;;   ;; the 1.6 compatibility topology
;;   (define-mtorus-topology
;;     mtorus16
;;     :neighborhoods (parents children rings elements))
  )

(mtorus-topology-initialize)


(defcustom mtorus-default-topology 'mtorus-topology-standard
  "*Topology inherited to all newly created elements."
  :group 'mtorus-element)






;;;
;;; code for attaching
;;; REVISE ME!!!

(defmacro mtorus-topology-standard-define-element-attach (attach)
  "Defines `mtorus-attach-element-to-ATTACH' and
`mtorus-detach-element-from-ATTACH'."
  (let* ((attach
          (intern
           (mtorus-utils-replace-regexp-in-string
            "^mtorus-\\(.+\\)$" "\\1" (format "%s" attach))))
         (m+attach
          (mtorus-utils-symbol-conc 'mtorus attach))
         (attach-fun-name
          (mtorus-utils-symbol-conc 'mtorus-attach-element-to attach))
         (detach-fun-name
          (mtorus-utils-symbol-conc 'mtorus-detach-element-from attach)))
    (when (mtorus-topology-p 'standard)
      (mapc
       #'eval
       `((defun ,attach-fun-name (element)
           ,(format "Attaches ELEMENT to %s" attach)
           (mtorus-topology-standard-define-children (or ,m+attach
                                                         ',m+attach)
                                                     element)
           (mtorus-topology-standard-define-parents element
                                                    (or ,m+attach
                                                        ',m+attach)))
         (defun ,detach-fun-name (element)
           ,(format "Detaches ELEMENT from %s" attach)
           (mtorus-topology-standard-undefine-children (or ,m+attach
                                                           ',m+attach)
                                                       element)
           (mtorus-topology-standard-undefine-parents element
                                                      (or ,m+attach
                                                          ',m+attach))))))
    `',attach-fun-name))

(mtorus-topology-standard-define-element-attach mtorus-universe)
(mtorus-topology-standard-define-element-attach mtorus-current-ring)




(defun mtorus-fake-attach-element-to-current (current element &optional type-filter)
  "Attaches ELEMENT to anything in CURRENT using the 'siblings relation."
  (when (mtorus-topology-p 'standard)
    (mtorus-topology-standard-define-siblings-family element current)
    (mtorus-topology-standard-define-siblings element element)))


;;
;; this is ugly hard coding of stuff :(

(defun mtorus-fake-attach-ring-to-rings (ring &optional type-filter)
  "Attaches RING to all other rings.
Basically this is bad code and should be generalized somehow."
  (and (mtorus-type-ring-p ring)
       (progn
         (and (not (eq ring 'mtorus-universe))
              (mtorus-topology-standard-define-siblings-family ring 'mtorus-universe 'children))
         (mtorus-topology-standard-define-siblings ring ring)
         (and (not (eq ring 'mtorus-universe))
              (mtorus-topology-standard-define-children 'mtorus-universe ring))
         (mtorus-topology-standard-define-parents ring 'mtorus-universe))))

(defun mtorus-fake-attach-element-to-children-of-element (element overelement &optional type-filter)
  "Attaches ELEMENT to all other elements (children) of OVERELEMENT.
Basically this is bad code and should be generalized somehow."
  (and (not (eq element 'mtorus-universe))
       (mtorus-topology-standard-define-children overelement element))
  (mtorus-topology-standard-define-parents element overelement)
  (mtorus-topology-standard-define-siblings-family element overelement 'children)
  (mtorus-topology-standard-define-siblings element element))

(defun mtorus-fake-attach-element-to-children-of-ring (element ring &optional type-filter)
  "Attaches ELEMENT to all other elements (children) of RING.
Basically this is bad code and should be generalized somehow."
  (and (mtorus-type-ring-p ring)
       (not (eq ring 'mtorus-universe))
       (mtorus-fake-attach-element-to-children-of-element element ring type-filter)))



(defun mtorus-fake-attach-get-current-ring-1 (element)
  "Tries to find the current ring."
  (cond ((eq element 'mtorus-universe)
         nil)
        ((mtorus-type-ring-p element)
         element)
        (t (mtorus-fake-attach-get-current-ring-1
            (car (mtorus-topology-standard-parents element))))))
(defun mtorus-fake-attach-get-current-ring ()
  "Tries to find the current ring."
  (mtorus-fake-attach-get-current-ring-1 mtorus-current-element))


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
      (string< (mtorus-element-get-name el1)
               (mtorus-element-get-name el2)))
    :order-fun
    'stable-sort)

  (define-mtorus-order
    by-age
    :predicate
    (lambda (el1 el2)
      (mtorus-utils-time-less-p
       (mtorus-element-get-ctime el1 (current-time))
       (mtorus-element-get-ctime el2 (current-time))))
    :order-fun
    'stable-sort)

  (define-mtorus-order
    by-atime
    :predicate
    (lambda (el1 el2)
      (mtorus-utils-time-less-p
       (mtorus-element-get-atime el2 (current-time))
       (mtorus-element-get-atime el1 (current-time))))
    :order-fun
    'stable-sort))

(mtorus-order-initialize)

(defcustom mtorus-default-order 'mtorus-order-by-age
  "*Order inherited to all newly created elements."
  :group 'mtorus-element)


(run-hooks 'mtorus-topology-after-load-hook)


(provide 'mtorus-topology)

;;; mtorus-topology.el ends here
