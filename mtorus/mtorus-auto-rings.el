;;; mtorus-auto-rings.el --- automatically created rings
;; $Id: mtorus-auto-rings.el,v 1.1 2004/04/04 23:07:36 hroptatyr Exp $
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


;;; History


;;; Code:

(require 'mtorus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable User Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-auto-rings nil
  ""
  :tag "MTorus"
  :prefix "mtorus-ar"
  :group 'mtorus)


(defcustom mtorus-init-hook nil
  "Hook run after the torus is initialized."
  :type 'hook
  :group 'mtorus)

(defcustom mtorus-init-rings-emtpy nil
  "*Whether to create a new ring with a marker at point."
  :type 'boolean
  :group 'mtorus)

(defcustom mtorus-notify-method 't
  "*Controls how the status is displayed to the user.
If set to 't' mtorus uses a popup window and the echo erea.

If set to 'popup' only the popup window will be used

If set to 'echo'  only the echo area will be used.

Set this to 'nil' to avoid notifying at all."
  :type '(choice (const t) (const nil) (const popup) (const echo))
  :group 'mtorus)

(defcustom mtorus-buffer-list-name "*buffer-list*"
  "The name of the ring that always contains all open buffers.
Cycling within this ring is different from cycling the others since it
always uses the real buffer list.  It skips all buffers that
`mtorus-buffer-skip-p' returns t for and is not editable."
  :type 'string
  :group 'mtorus)



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
(setq mtorus-auto-rings
  '((mtorus-ar-buffer-list :name "*buffer-list*"
                           :creator mtorus-ar-create-buffer-list
                           :parent mtorus-universe)
    ))



;;; general init functions
(defun mtorus-ar-init-auto-rings ()
  "Creates the bunch of auto-rings defined by `mtorus-auto-rings'."
  (mapc 'mtorus-ar-init-auto-ring mtorus-auto-rings))

(defun mtorus-ar-init-auto-ring (ring-spec)
  "Creates an auto-ring described by RING-SPEC.
See `mtorus-auto-rings' documentation for a description of RING-SPEC."
  (let ((rname ))
    )
  (message "%s created" (car ring-spec)))


(defun mtorus-ar-uninit-auto-rings ()
  "Removes and unloads all auto-rings from `mtorus-universe'."
  )
(defun mtorus-ar-uninit-auto-ring (ring)
  "Removes and unloads auto-ring given by RING."
  )

;; (add-hook 'mtorus-init-hook 'mtorus-ar-init-auto-rings) ;; recommended



;;; some pre-defined auto-rings

;; buffer-list (formerly known as special ring "*buffer-list*"
(defun mtorus-ar-create-buffer-list ()
  ""
  )


(provide 'mtorus-auto-rings)

;;; mtorus-auto-rings.el ends here
