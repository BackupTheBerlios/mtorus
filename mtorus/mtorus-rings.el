;;; mtorus-rings.el --- ring functions
;; $Id: mtorus-rings.el,v 1.1 2004/04/04 23:07:36 hroptatyr Exp $
;; Copyright (C) 2003 by Stefan Kamphausen
;; Author: Stefan Kamphausen <mail@skamphausen.de>
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable User Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-rings nil
  ""
  :tag "MTorus"
  :prefix "mtorus-ring"
  :group 'mtorus)

;; once again the ring-specs
;;  \(\(ring-symbol :name ring-name \[:description ring-descr]
;;                :parent rings-parent-torus))
;; 
;; The keywords in detail:
;; :name should be a string name of this ring
;; :description should be a more descriptive string
;; :parent is a parent of this ring
;;   if this keyword is omitted `mtorus-universe' will be used
;;   (note: `mtorus-universe' is always a valid parent).
;; 


;;; Handlers for creation

(defun mtorus-ring-create (ring-spec)
  "Create a ring with name RING-NAME (asked from user).
If `mtorus-init-rings-emtpy' is non nil a marker at the current point
is created and pushed on the list, otherwise the ring stays empty for
the moment.  Makes the new ring the current ring.

It won't create a ring with a name that already exists."
  (interactive "sRing name: ")
  (if (mtorus-ringp ring-name)
      (mtorus-message
       (format "A ring with name \"%s\" already exists."
               ring-name))
    (let* ((content (mtorus-initial-ring-contents ring-name))
          (ring (cons ring-name (list content))))
      (setq mtorus-torus
            (append mtorus-torus
                    (list ring))))
    (mtorus-switch-to-ring ring-name t)))

(defun mtorus-ring-delete (&optional ring-name)
  "Delete the ring with name RING-NAME.
If none is given it is asked from the user."
  (interactive)
  (let ((rname (or ring-name (mtorus-ask-ring))))
    (if (not (mtorus-special-ringp rname))
        (if (y-or-n-p (format "delete ring \"%s\"? " rname))
            (setq mtorus-torus
                  (delete* rname mtorus-torus :key 'car
                           :test 'equal)))
      (mtorus-message "can't delete special rings"))))

(defun mtorus-ring-rename (&optional ring-name new-name)
  "Rename RING-NAME to NEW-NAME asking if omitted."
  (interactive)
  (let* ((rname (or ring-name (mtorus-ask-ring)))
        (nname (or new-name (read-string
                             (format "rename \"%s\" to: " rname)))))
    (if (not (mtorus-special-ringp rname))
        (setcar (assoc rname mtorus-torus) nname)
      (mtorus-message "can't rename special rings"))))

(provide 'mtorus-rings)

;;; mtorus-rings.el ends here
