;;; mtorus-rings.el --- ring functions
;; $Id: mtorus-rings.el,v 1.4 2004/05/22 22:51:40 hroptatyr Exp $
;; Copyright (C) 2003 by Stefan Kamphausen
;;           (C) 2004 by Sebastian Freundt
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;;         Sebastian Freundt <freundt@hlidskjalf.de>
;; Created: 2004/04/03
;; Keywords: bookmarks, navigation, tools, extensions, user

;; This file is not part of XEmacs.

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
;; Any type of functionality concerning rings (creation, modification, ...)
;; is here.

;;; History


;;; Code:

(require 'cl-extra) ;;; sorry about that, but i use (every ...) and (some ...) constructions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable User Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-ring nil
  "The rings of the torus."
  :tag "MTorus"
  :prefix "mtorus-ring-"
  :group 'mtorus)

;; once again the ring-specs
;;  \(ring-symbol :name ring-name \[:description ring-descr]
;;               :parent rings-parent-torus))
;; 
;; The keywords in detail:
;; :name should be a string name of this ring
;; :description should be a more descriptive string
;; :parent is a parent of this ring
;;   if this keyword is omitted `mtorus-universe' will be used
;;   (note: `mtorus-universe' is always a valid parent).
;; 
;; However there are several hidden specs:
;; :current-pos is the element 

(defvar mtorus-rings nil
  "Stores symbols of rings ever created to reference them later.
Actually this is 'mtorus-universe.")


;;;
;;; Auxiliary stuff
;;;
(defvar mtorus-ring-spec-keywords
  '((cons 'r-symbol (unless (keywordp (car spec)) (car spec)))
    :name
    :description
    :parent)
  "Keywords to parse in a ring-spec.")

(defvar mtorus-ring-element-types
  '((buffer :validity-function-list (buffer-live-p))
    (marker)
    (mtorus-ring-ring))
  "Types to parse in a ring-spec.
This is an alist of the form
\(type type-specs)
type-specs are :keyword value pairs.

Supported keywords are:
:validity-function-list function list to be applied on
  the contents of an element")

(defvar mtorus-ring-element-types-spec-keywords
  '(:validity-function-list)
  "Keywords when looking at the type of an element of a ring.
This is a list of keywords.")

(defvar mtorus-ring-element-spec-keywords
  '(:type
    :contents
    :validity-function-list)
  "Keywords when looking at the elements of a ring.
This is a list of :keywords.

Some keywords are predefined (and thus are essential for mtorus):
:type  type of an element (see `mtorus-ring-element-types')
:contents  contents of an element
:validity-check  functions to be run on the value of :contents
  to see if a certain element is still valid.")

;; hooks


;;; now the real Code

(defun mtorus-ring-parse-key (keyword spec)
  "Parses SPEC for keyword KEY and returns a cons cell of key and value."
  (let ((keypos (position keyword spec)))
    (cons (intern (substring (format "%s" keyword) 1))
          (and keypos
               (nth (1+ keypos) spec)))))
(defun mtorus-ring-parse-spec (spec &optional spec-keywords)
  "Parses SPEC and returns a list '((key . value) ...).
Traditionally "
  (let ((spec-keywords (or spec-keywords mtorus-ring-spec-keywords)))
    (mapcar (lambda (key)
              (cond ((listp key)
                     (eval key))
                    (t (mtorus-ring-parse-key key spec))))
            spec-keywords)))
;; (mtorus-ring-parse-spec '(:name "bla" :description "more bla" :parent "even more bla"))
;; (mtorus-ring-parse-spec '(:type 'buffer :contents (current-buffer)) mtorus-ring-element-spec-keywords)


(defun mtorus-ring-property (ring property)
  "Gets PROPERTY from object-plist of RING"
  (get ring (intern (format "mtorus-%s" property))))
(defun mtorus-ring-ring-p (ring)
  "Tests if RING is an mtorus-ring in `mtorus-rings'."
  (and (member ring mtorus-rings)
       (mtorus-ring-property ring 'ring-p)))



;;;
;;; Handlers for creation (note these are not interactive, use wrappers)
;;;
(defun mtorus-ring-create-ring (&rest ring-spec)
  "Create a ring by RING-SPEC.
Returns the ring-symbol.

Actually a new variable is set with RING-SPEC plus some other stuff
in the object-plist of the ring-symbol.

Created rings are stored in `mtorus-rings'."
  (let* ((r-spec (mtorus-ring-parse-spec ring-spec))
         (r-symbol (or (cdr (assoc 'r-symbol r-spec))
                       (intern (format "mtorus-ring-%.8x" (random)))))
         (r-doc (cdr (assoc 'description r-spec))))
    (set r-symbol nil)
    (add-to-list 'mtorus-rings r-symbol)

    ;; const specs (not to be influenced by the user
    (add-to-list 'r-spec '(default-value . nil))
    (and r-doc
         (add-to-list 'r-spec '(variable-documentation . r-doc)))
    (add-to-list 'r-spec '(mtorus-ring-p . t))
    (add-to-list 'r-spec '(mtorus-ring-type . 'proper))
    (add-to-list 'r-spec '(mtorus-ring-ctime . (current-time)))
    (add-to-list 'r-spec '(mtorus-ring-deletable-p . t))
    ;; set r-symbols object-plist
    (mapc (lambda (propval)
            (let ((prop (car propval))
                  (val (cdr propval)))
              (put r-symbol prop val)))
          r-spec)
    r-symbol))
;;;possible calls:
;;(mtorus-ring-create-ring 'test-ring :name "Test Ring" :description "Just to test some" :parent 'mtorus-universe)
;;(mtorus-ring-create-ring :name "Test Ring" :description "Just to test some" :parent 'mtorus-universe)
;;(mtorus-ring-create-ring)


(defun mtorus-ring-delete-ring (ring)
  "Deletes the ring RING.
"
  (and (mtorus-ring-ring-p ring)
       (mtorus-ring-property ring 'ring-deletable-p)
       (progn (setq mtorus-rings
                    (remove ring mtorus-rings))
              (makunbound ring)))
  ring)
;;;possible calls:
;;(mtorus-ring-delete-ring 'test-ring)


(defun mtorus-ring-rename-ring (&rest ring-spec)
  "Rename ring from RING-SPEC to new value of :name and/or :description."
  (let* ((r-spec (mtorus-ring-parse-spec ring-spec))
         (r-symbol (cdr (assoc 'r-symbol r-spec)))
         (r-name (cdr (assoc 'name r-spec)))
         (r-doc (cdr (assoc 'description r-spec))))

    ;; const specs (not to be influenced by the user
    (and r-symbol
         (mtorus-ring-ring-p r-symbol)
         (progn
           (and r-doc
                (progn
                  (put r-symbol 'description r-doc)
                  (put r-symbol 'variable-documentation r-doc)))
           (and r-name
                (put r-symbol 'name r-name))
           r-symbol))))
;;; possible calls:
;;(mtorus-ring-rename-ring 'test-ring :name "TTTTESSSST RING" :description "just changed")




;;;finding rings

(defun mtorus-ring-ring-name (ring)
  "Returns the name of RING."
  (get ring 'name))
(defun mtorus-ring-ring+name (ring)
  "Returns the cons \(RING . name)."
  (cons ring (mtorus-ring-ring-name ring)))
(defun mtorus-ring-ring-by-name (name)
  "Finds the ring symbol in `mtorus-ring' of the ring
with the name NAME."
  (car-safe
   (rassoc name
           (mapcar 'mtorus-ring-ring+name
                   mtorus-ring))))





;;;elements of rings

(defun mtorus-ring-elements (ring)
  "Returns elements of RING."
  ring)

(defun mtorus-ring-add-element (ring &rest element-spec)
  "Adds an element (described by ELEMENT-SPEC) to RING."
  (and (boundp ring)
       (add-to-list ring element-spec)))
;;(mtorus-ring-add-element 'test-ring :type 'buffer :contents (current-buffer))


(defun mtorus-ring-element-valid-p (element-spec)
  "Returns non-nil if :contents in ELEMENT-SPEC are a valid content
for the given :type.

The atomic ring elements should be specified by a list of :keyword value pairs
\(see `mtorus-ring-element-spec-keywords' for keywords).

Contents are checked whether the function `:type'p or `:type'-p
returns t on the :contents.
Furthermore the value of :validity-function-list (which is not more
than a local, element-specific hook then) is called with the value
of :content as argument.

For type-specific validity-check-functions see `mtorus-ring-element-types'."
  (let* ((element-list (mtorus-ring-parse-spec element-spec mtorus-ring-element-spec-keywords))
         (element-type (cdr (assoc 'type element-list)))
         (element-content (cdr (assoc 'contents element-list)))
         (validity-fun-list
          (cdr (assoc 'validity-function-list element-list)))
         (type-validity-fun-list
          (cdr (assoc 'validity-function-list
                      (mtorus-ring-parse-spec
                       (cdr (assoc element-type mtorus-ring-element-types))
                       mtorus-ring-element-types-spec-keywords))))
         (check-fun (list (or (let ((type-predicate-fun (intern (format "%sp" element-type))))
                                (and (fboundp type-predicate-fun)
                                     type-predicate-fun))
                              (let ((type-predicate-fun (intern (format "%s-p" element-type))))
                                (and (fboundp type-predicate-fun)
                                     type-predicate-fun)))
                          element-content)))
    (and (car check-fun)
         (or (not validity-fun-list)
             (every (lambda (fun)
                      (funcall fun element-content))
                    validity-fun-list))
         (or (not type-validity-fun-list)
             (every (lambda (fun)
                      (funcall fun element-content))
                    type-validity-fun-list))
         (eval check-fun))))
;; (mtorus-ring-element-valid-p (cadr test-ring))














;;; more code from ring.el where we will certainly steal from:

;; ;;;###autoload
;; (defun mtorus-ring-ring-p (x) 
;;   "Returns t if X is a ring; nil otherwise."
;;   (and (consp x) (integerp (car x))
;;        (consp (cdr x)) (integerp (car (cdr x)))
;;        (vectorp (cdr (cdr x)))))
;; 
;; ;;;###autoload
;; (defun mtorus-ring-make-ring (size)
;;   "Make a ring that can contain SIZE elements."
;;   (cons 0 (cons 0 (make-vector size nil))))
;; ;;; (setq test-ring (make-ring 14))
;; ;;; (mtorus-ring-insert-at-beginning test-ring '15)
;; 
;; (defun mtorus-ring-insert-at-beginning (ring item)
;;   "Add to RING the item ITEM.  Add it at the front (the early end)."
;;   (let* ((vec (cdr (cdr ring))) 
;;   (veclen (length vec))
;;   (hd (car ring))
;;   (ln (car (cdr ring))))
;;     (setq ln (min veclen (1+ ln))
;;    hd (ring-minus1 hd veclen))
;;     (aset vec hd item)
;;     (setcar ring hd)
;;     (setcar (cdr ring) ln)))
;; 
;; (defun mtorus-ring-plus1 (index veclen)
;;   "INDEX+1, with wraparound"
;;   (let ((new-index (+ index 1)))
;;     (if (= new-index veclen) 0 new-index)))
;; 
;; (defun mtorus-ring-minus1 (index veclen)
;;   "INDEX-1, with wraparound"
;;   (- (if (= 0 index) veclen index) 1))
;; 
;; (defun mtorus-ring-length (ring)
;;   "Number of elements in the ring."
;;   (car (cdr ring)))
;; 
;; (defun mtorus-ring-empty-p (ring)
;;   (= 0 (car (cdr ring))))
;; 
;; (defun mtorus-ring-index (index head ringlen veclen)
;;   (setq index (mod index ringlen))
;;   (mod (1- (+ head (- ringlen index))) veclen))
;; 
;; (defun mtorus-ring-insert (ring item)
;;   "Insert onto ring RING the item ITEM, as the newest (last) item.
;; If the ring is full, dump the oldest item to make room."       
;;   (let* ((vec (cdr (cdr ring))) 
;;   (veclen (length vec))
;;   (hd (car ring))
;;   (ln (car (cdr ring))))
;;     (prog1
;;  (aset vec (mod (+ hd ln) veclen) item)
;;       (if (= ln veclen)
;;    (setcar ring (ring-plus1 hd veclen))
;;  (setcar (cdr ring) (1+ ln))))))
;; 
;; (defun mtorus-ring-remove (ring &optional index)
;;   "Remove an item from the RING.  Return the removed item.
;; If optional INDEX is nil, remove the oldest item.  If it's
;; numeric, remove the element indexed."
;;   (if (ring-empty-p ring)
;;       (error "Ring empty")
;;     (let* ((hd (car ring))
;;    (ln (car (cdr ring)))
;;    (vec (cdr (cdr ring)))
;;    (veclen (length vec))
;;    (tl (mod (1- (+ hd ln)) veclen))
;;    oldelt)
;;       (if (null index)
;;    (setq index (1- ln)))
;;       (setq index (ring-index index hd ln veclen))
;;       (setq oldelt (aref vec index))
;;       (while (/= index tl)
;;  (let ((next-index (ring-plus1 index veclen)))
;;    (aset vec index (aref vec next-index))
;;    (setq index next-index)))
;;       (aset vec tl nil)
;;       (setcar (cdr ring) (1- ln))
;;       oldelt)))
;; 
;; (defun mtorus-ring-ref (ring index)
;;   "Returns RING's INDEX element.
;; INDEX need not be <= the ring length, the appropriate modulo operation
;; will be performed.  Element 0 is the most recently inserted; higher indices
;; correspond to older elements until they wrap."
;;   (if (ring-empty-p ring)
;;       (error "indexed empty ring")
;;     (let* ((hd (car ring))  (ln (car (cdr ring)))  (vec (cdr (cdr ring))))
;;       (aref vec (ring-index index hd ln (length vec))))))



(provide 'mtorus-rings)

;;; mtorus-rings.el ends here
