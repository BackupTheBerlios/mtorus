;;; mtorus-auto-rings.el --- automatically created rings
;; $Id: mtorus-auto-rings.el,v 1.2 2004/08/09 01:11:44 hroptatyr Exp $
;; Copyright (C) 2004 by Sebastian Freundt
;; Author: Sebastian Freundt <freundt@math.tu-berlin.de>
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
;; This stuff is the migration of the former special rings to its own module
;; Use 
;;   (require 'mtorus-auto-rings)
;; to initialize the auto-rings from here
;;
;; Auto rings are rings automatically created and updated but they keep in the
;; mtorus-universe until you delete them explicitly
;; 
;; You will find predefined auto-rings when you
;;
;;     M-x mtorus-auto-rings-initialize RET


;;; History


;;; Code:

(require 'mtorus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable User Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-auto-ring nil
  "Externalization of the former special rings.

Auto rings are rings automatically created and updated but they keep in the
mtorus-universe until you delete them explicitly."
  :tag "MTorus Auto Rings"
  :prefix "mtorus-auto-ring-"
  :group 'mtorus)


;; 
;; (defcustom mtorus-buffer-list-name "*buffer-list*"
;;   "The name of the ring that always contains all open buffers.
;; Cycling within this ring is different from cycling the others since it
;; always uses the real buffer list.  It skips all buffers that
;; `mtorus-buffer-skip-p' returns t for and is not editable."
;;   :type 'string
;;   :group 'mtorus)
;; 


(defvar mtorus-auto-rings nil
  "Alist containing the automatically created rings (formerly known
as special rings).

This alist should be of the form:
 \(\(ring-symbol :name ring-name \[:creator ring-creation-function]
               :parent rings-parent-torus))

The keywords in detail:
:name should be a string name of this ring
:creator should be a function that creates this ring
  if this keyword is omitted mtorus-ar-create-`ring-symbol' is attempted
  to be used
:parent is a parent of this ring
  if this keyword is omitted `mtorus-universe' will be used
  (note: `mtorus-universe' is always a valid parent).

With every auto-ring the following hooks are created:
`ring-symbol'-create-pre-hook -- hook run before creating this ring.
`ring-symbol'-create-post-hook -- hook run after creation of this ring
`ring-symbol'-delete-pre-hook -- hook run before deleting this ring
`ring-symbol'-delete-post-hook -- hook run after deletion of this ring

Each of the auto-rings will have a property '(auto-ring t) in its
object-plist.")

;; just a bunch of them -- propose more or contribute (:
;; (setq mtorus-auto-rings
;;   '((mtorus-ar-buffer-list :name "*buffer-list*"
;;                            :creator mtorus-ar-create-buffer-list
;;                            :parent mtorus-universe)
;;     ))
 

;; (defmacro define-mtorus-auto-ring (name &rest properties)
;;   "Defines an mtorus-auto-ring."
;;   (let ((curels (let (els)
;;                   (maphash
;;                    #'(lambda (key val)
;;                        (and (equal val 'auto-ring)
;;                             (add-to-list 'els key)))
;;                    (eval mtorus-elements-hash-table))
;;                   els)))
;;     (mapc #'mtorus-delete-element curels))
;;   (and (not (boundp mtorus-auto-ring))
;;        (setq mtorus-auto-ring
;;              (mtorus-element-create
;;               :type 'auto-ring
;;               :name name
;;               :value "auto ring buffer"
;;               :description "This is an MTorus Auto Ring Buffer."
;;               :variable-documentation "This is an MTorus Auto Ring Buffer."))
;;        (mtorus-element-register mtorus-auto-ring))
;;   (let ((blist (mapcar #'buffer-name (buffer-list))))
;;     (mapc #'(lambda (elem)
;;               (mtorus-topology-standard-define-children elem mtorus-auto-ring))
;;           blist))
;;   `',name)
;; 
;; 
;; 
;; 
;; 
;; ;;; some pre-defined auto-rings
;; 
;; (define-mtorus-type
;;   auto-ring-buffer
;;   :predicate
;;   (lambda (element)
;;     (bufferp (eval element)))
;; 
;;   :inherit-selection
;;   (lambda (element)
;;     (switch-to-buffer (eval element)))
;; 
;;   :alive-p
;;   (lambda (element)
;;     (buffer-live-p (eval element))))
;; 
;; (define-mtorus-type
;;   auto-ring
;;   :predicate
;;   (lambda (element)
;;     (bufferp (eval element)))
;; 
;;   :inherit-selection
;;     (lambda (element)
;;       (setq mtorus-current-ring element)
;;       (mtorus-child-element))
;; 
;;   :alive-p
;;   (lambda (element)
;;     (buffer-live-p (eval element)))
;; 
;;   :post-creation
;;   (lambda (element)
;;     (mtorus-fake-attach-element-to-children-of-element element 'mtorus-universe)
;;     (mtorus-element-set-current element)
;;     (setq mtorus-current-ring element)
;;     ;;(mtorus-auto-ring-create-buffer-list)
;;     )
;; 
;;   :pre-selection
;;   (lambda (element)
;;     (mtorus-auto-ring-create-buffer-list)))
;; 
;; 
;; (defun mtorus-auto-ring-create-buffer-list ()
;;   (let ((blist (buffer-list))
;;         (curels (let (els)
;;                   (maphash
;;                    #'(lambda (key val)
;;                        (and (equal val 'auto-ring-buffer)
;;                             (add-to-list 'els key)))
;;                    (eval mtorus-elements-hash-table))
;;                   els)))
;;     (mapc #'mtorus-delete-element curels)
;;     (mapc #'(lambda (elem)
;;               (let ((elsymb (mtorus-element-create
;;                              :type 'auto-ring-buffer
;;                              :name (buffer-name elem)
;;                              :value elem)))
;;                 (mtorus-element-register elsymb)
;;                 (mtorus-fake-attach-element-to-children-of-element elsymb mtorus-auto-ring)))
;;           blist)))
;; 
;; ;; buffer-list (formerly known as special ring "*buffer-list*"
;; (define-mtorus-auto-ring
;;   "*buffer-list*")


(run-hooks 'mtorus-auto-rings-after-load-hook)

(provide 'mtorus-auto-rings)

;;; mtorus-auto-rings.el ends here
