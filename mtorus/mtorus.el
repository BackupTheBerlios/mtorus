;;; mtorus.el --- navigation with marks on a ring of rings (torus)
;; $Id: mtorus.el,v 1.15 2004/08/02 22:20:56 hroptatyr Exp $
;; Copyright (C) 2003 by Stefan Kamphausen
;;           (C) 2004 by Sebastian Freundt
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;;         Sebastian Freundt <hroptatyr@users.berlios.de>
;; Created: Winter 2002
;; Keywords: bookmarks, navigation, tools, extensions, user

;; This file is not part of XEmacs.

(defconst mtorus-version "2.0 $Revision: 1.15 $"
  "Version number of MTorus.")

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
;; This file contains both the old mtorus code from revision 1.6
;; and the new (more abstract) mtorus frontend code from the current
;; development
;; If I (hroptatyr) ever get that synch done from my personal wiki page
;; to the berlios project page you can follow these new changes yourselves.
;; Finally I'd like to add that I kept all the ;;; Usage and ;;; History
;; lines until the MTorus 2.0 is stable at last.
;; For information on how to fiddle with the new frontend/backend stuff
;; you can visit the EmacsWiki page listed below
;;
;; FIXMEs
;;  - generic interface for special lists, because currently the
;;    (only) special list (buffer list) behaviour is hard coded in a
;;    way that's probably not sensible for lists like recent-files or
;;    the like
;;  - handling of invalid markers
;;    -> (don't display, quietly discard?)
;;    -> the correct while-loop which deletes invalid in
;;    mtorus-jump-current-marker or mtorus-current-buffer
;;    -> what happens when a buffer is reverted (or killed and
;;    reopened)?
;; MTorus on the Web:
;; Main page:
;; http://mtorus.berlios.de
;; Stefan's pages:
;; http://www.skamphausen.de/software/skamacs/mtorus.html
;; German intro:
;; http://www.skamphausen.de/xemacs/lisp/mtorus.html
;; English intro:
;; http://www.emacswiki.org/cgi-bin/wiki/MTorus
;; Project page:
;; http://developer.berlios.de/projects/mtorus
;; EmacsWiki page:
;; http://www.emacswiki.org/wiki.pl?MTorus
;;

;;; Getting Started
;;  ===============
;; - Place the MTorus directory somewhere in your 'load-path and put
;; 
;;     (require 'mtorus)
;; 
;; in your .emacs
;; Now you have a useless element on your torus called mtorus-universe.
;; 
;; * Try to find some buffers that are worth to form a ring and do
;; 
;;     M-x mtorus-create-ring RET
;; 
;; Enter some appropriate name for that ring.
;; Go to a buffer which should be part of the ring created and enter
;; 
;;     M-x mtorus-create-element RET
;; 
;; When asked for a type just enter buffer or marker.
;; These are the only types predefined by default.
;; 
;; - Repeat this element creation step until you have enough buffers on your ring.
;; 
;; - Now you are able to navigate through your torus with the commands
;; 
;;     mtorus-next-element
;;     mtorus-prev-element
;;     mtorus-parent-element
;;     mtorus-child-element
;; 
;; To treat your command history with care, bind these commands to some keys.
;; 
;; Suggested Keybindings:
;; - I have H-kp-6, H-kp-4, H-kp-8 and H-kp-2 keys bound to
;;   mtorus-next-element, mtorus-prev-element, mtorus-parent-element and
;;   mtorus-child-element respectively -- [[hroptatyr]]
;;   For selection of an element I user H-kp-5
;; - I prefer shift -left/right/up/down -- StefanKamphausen


;;; Usage:
;;  ======
;; MTorus lets you work with several groups of buffers, each group
;; being a separate ring.  This is all for easier navigation through
;; buffers.  I've been using some buffer cycling functions on (shift
;; left and shift right) for quite a long time now and I find myself
;; cycling through larger and larger lists every day.  Starting one
;; instance of (X)Emacs for each editing context isn't the way and I
;; found no way of doing what I want with frames.  An `editing context'
;; means a logical grouping of buffers.  This could be a group for
;; quick edit of the emacs configuration files while you're actually
;; working on some (Ruby/Perl/whatever-) program or it could be all
;; the headers of your C project while all the .c-files make up
;; another group.  Whatever you can think of.  You could even make
;; different parts of your buffers (point positions) show up in
;; different groups like when one set of functions spread throughout
;; one (or more) files is responsible for one specific task or like
;; working on a chapter in a LaTeX-document while defining some macros
;; at the top of the file.  There is always a default group which
;; contains all the open buffers and can not be altered.
;;
;; Like so:
;; +- Ring: C-code -------------------------------------------------+
;; |                                                                |
;; | +- marker ---+  +- marker ---+  +- marker ---+                 |
;; | | main.cc/224|  | main.cc/567|  | main.hh/312|                 |
;; | +------------+  +------------+  +------------+                 |
;; |                                                                |
;; | +- marker ---+  +- marker ----+                                |
;; | | *Occur*/84 |  | README/1388 |                                |
;; | +------------+  +-------------+                                |
;; |                                                                |
;; +----------------------------------------------------------------+
;;
;; +- Ring: quick --------------------------------------------------+
;; |                                                                |
;; |                                                                |
;; | +- marker ---+  +- marker -----+                               |
;; | | .emacs/224 |  | *scratch*/33 |                               |
;; | +------------+  +--------------+                               |
;; |                                                                |
;; +----------------------------------------------------------------+
;;
;; +- Ring: doc ----------------------------------------------------+
;; |                                                                |
;; |                                                                |
;; | +- marker ---+  +- marker ---+                                 |
;; | | *info*/999 |  | Man: ls/1  |                                 |
;; | +------------+  +------------+                                 |
;; |                                                                |
;; +----------------------------------------------------------------+
;;
;; Choosing another entry in such a group is done by actually cycling
;; through the group (like with e.g. the kill-ring) and since the
;; entries are made of a buffer together with a position (and that is a
;; marker) each of the groups is comparable to the mark-ring.  And
;; since we are cycling through a collection of those rings we are
;; using a Torus here.  Hence the name: "mtorus": mark-torus.
;; (A "group" will usually be referred to as a "ring" from now.)

;;; History
;; Just as many other people I have once (co-)written a buffer cycling
;; mechanism, that was later expanded to using a skipping
;; predicate.  At some point it wasn't enough anymore and I still had
;; the question nagging: what could I possible do with Shift Up and
;; Down? Then I discovered that I often used several buffers in one
;; context (and they were not neighbors on the buffer-list).  This made
;; me think of groups of buffers and led to a first implementation
;; under the name "session-stack.el".  Then it occurred to me that I
;; wanted to navigate different positions in the same buffer with it
;; and expanded the code.  Sometimes I wondered that those hotspots
;; seemed to move around until I read a chapter about markers in the
;; elisp info file and suddenly I understood that the points I stored
;; were not moving together with the text.  During that rewrite I
;; renamed the whole thing to mtorus reflecting the topology of the
;; main data structure.  All this happened during autumn and winter of
;; 2002.  In early 2003 I came across swbuff.el which had a very nice
;; popup window feature that I definetely admired and wanted to
;; have.  It became clear that a full rewrite had to be done once again
;; because the code (which was then just released to the public one
;; week ago) had become quite a mess.  The time of this writing is
;; right during that rewrite.  Why I tell this all here? Hm, probably
;; mainly for myself to remember when I'm grey and old (and still be
;; using XEmacs ;-)

;;; Code:
(eval-when-compile
  (require 'cl)
  (require 'timer))

;; aux stuff
(require 'mtorus-utils)

;; backend
(require 'mtorus-topology)
(require 'mtorus-type)
(require 'mtorus-element)

;; GUI functions
(require 'mtorus-display)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable User Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus nil
  "An interface to navigating groups of buffers.
This code should work fast so intuition is a very important
matter.  Some of the customizable variables are switches to tune the
behavior of the functions to your habits.  Hopefully it is possible to
find good settings for many people."
  :tag "MTorus"
  :link '(url-link :tag "Home Page"
          "http://mtorus.berlios.de")
  :link '(emacs-commentary-link
          :tag "Commentary in mtorus.el" "mtorus.el")
  :prefix "mtorus-"
  :group 'environment
  :group 'extensions
  :group 'convenience)


(defcustom mtorus-init-hook nil
  "Hook run after the torus is initialized."
  :type 'hook
  :group 'mtorus)

(defcustom mtorus-after-load-hook nil
  "Hook run after mtorus is loaded."
  :type 'hook
  :group 'mtorus)

;;; REVISE ME!!
(defcustom mtorus-init-ring-emtpy nil
  "*Whether to create a new ring with a marker at point."
  :type 'boolean
  :group 'mtorus)



;;; this is what i'd call mtorus-display stuff
(defcustom mtorus-notify-method 't
  "*Controls how the status is displayed to the user.
If set to 't' mtorus uses a popup window and the echo erea.

If set to 'popup' only the popup window will be used

If set to 'echo'  only the echo area will be used.

Set this to 'nil' to avoid notifying at all."
  :type '(choice (const t) (const nil) (const popup) (const echo))
  :group 'mtorus)

;; REVISE ME!
(defcustom mtorus-buffer-skip-p
  'mtorus-default-buffer-skip-p
  "Predicate to use to skip buffers when cycling the real buffer list.
This has nothing to do with the cycling inside a normal ring.
A good example would be to use the result of
  (string-match \"^[ \\*]+\" (buffer-name buffer))
which skips the buffers with a star or a space at the beginning of
their buffer names.
The default predicate `mtorus-default-buffer-skip-p'  skips
buffers whose names begin with a space."
  :type 'function
  :group 'mtorus)

;; REVISE ME!
;;; this would go to mtorus-state.el
(defcustom mtorus-save-on-exit nil
  "*Whether to save the current torus to the current dir on exit.
This is an ALPHA feature."
  :type 'boolean
  :group 'mtorus)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to display information
;; REVISE ME!
(defgroup mtorus-notify nil
  "Controls the display of information in mtorus."
  :tag "MTorus Notify"
  :prefix "mtorus-notify"
  :group 'mtorus)

(defcustom mtorus-notify-popup-clear-timeout 4
  "*Time in seconds before the pop up window is removed."
  :type 'number
  :group 'mtorus-pop-up)

(defcustom mtorus-notify-popup-separator " - "
  "String appearing between two entries in pop up window."
  :type 'string
  :group 'mtorus-pop-up)


(defvar mtorus-notify-popup-buffer-name " *mtorus*"
  "Name of the temporary buffer to display the torus.")

(defvar mtorus-notify-popup-timer nil
  "The timer used to remove the popup window.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AutoAttach elements per-type to the topology network 
(defcustom mtorus-auto-attach-p t
  "Whether to use auto attaching.
We will define later what this actually means."
  :group 'mtorus)




(defun mtorus-install-suggested-bindings ()
  "This sets the key-bindings that I consider useful.
The bindings don't not fulfill the requirements of good key-defining
but I like it and I provide it only as a convenience function.

Special care for CUA users is taken."
  (interactive)
  (message "installed")

  ;;; old 1.6
  (cond
   ((featurep 'cua) ;; FIXME how to detect?
    (global-set-key '[(f10)] 'mtorus-next-marker)
    (global-set-key '[(f9)]  'mtorus-prev-marker)
    (global-set-key '[(shift f10)]    'mtorus-next-ring)
    (global-set-key '[(shift f9)]  'mtorus-prev-ring))
   (t
    (global-set-key '[(shift right)] 'mtorus-next-marker)
    (global-set-key '[(shift left)]  'mtorus-prev-marker)
    (global-set-key '[(shift up)]    'mtorus-next-ring)
    (global-set-key '[(shift down)]  'mtorus-prev-ring)))
   
  ;; ring handling: f11
  (global-set-key '[(f11)]
    'mtorus-new-ring)
  (global-set-key '[(shift f11)]
    'mtorus-delete-ring)
  (global-set-key '[(control f11)]
    'mtorus-notify)
  ;; marker handling: f12
  (global-set-key '[(f12)]
    'mtorus-new-marker)
  (global-set-key '[(shift f12)]
    'mtorus-delete-current-marker)
  (global-set-key '[(control f12)]
    'mtorus-update-current-marker)

   ;;; new keybindings
  (global-set-key '[(hyper kp-6)] 'mtorus-next-element)
  (global-set-key '[(hyper kp-4)] 'mtorus-prev-element)
  (global-set-key '[(hyper kp-8)] 'mtorus-parent-element)
  (global-set-key '[(hyper kp-2)] 'mtorus-child-element)
  (global-set-key '[(hyper kp-0)] 'mtorus-create-element)
  (global-set-key '[(hyper kp-5)] 'mtorus-select-current-element)
  (global-set-key '[(hyper kp-decimal)] 'mtorus-delete-current-element)

  )


;;;;;;;;;;;;;;;;;;;
;; MTorus internals

;; Default Settings
(defun mtorus-default-buffer-skip-p (buffer)
  "The default predicate used for `mtorus-buffer-skip-p'
is to skip only the special buffers whose name begins with a space."
  (string-match "[ ]+" (buffer-name buffer)))

(defcustom mtorus-buffer-list-name "*buffer-list*"
  "The name of the ring that always contains all open buffers.
Cycling within this ring is different from cycling the others since it
always uses the real buffer list.  It skips all buffers that
`mtorus-buffer-skip-p' returns t for and is not editable."
  :type 'string
  :group 'mtorus)

;; Variables
(defvar mtorus-torus nil ;; obsolete?
  "Alist containing the rings of markers.
The main data structure of MTorus:
 \(\(\"ring1\" \(\marker1
            marker2
            marker3
            marker4))
 \(\"ring2\" \(\marker5
            marker6
            marker7)))

Positions are stored as markers so that they keep in place when
altering the contents of the buffer.")


;;; thus here we go:
(defvar mtorus-universe nil
  "This is the universe, i.e. a torus which cannot have siblings.

For convenience:
- some rings form a torus (the elements of the torus are called rings)
- some tori form an overtorus (the overtorus elements are tori then)
- we don't distinguish between torus and overtorus, as it does not
  matter whether the rings of the torus are tori or elementary rings
- a ring shall be a list of either rings or markers/buffers
- a sibling of a ring is another element of the torus of the ring
- a sibling of a torus is a sibling of the torus seen as ring
- the parent of a ring is the torus having this ring as element
- the parent of a torus is the parent of this torus seen as ring
- there is exactly one ring that has no parents nor siblings:
  the universe.

The universe will dynamically extend when you try to create siblings
for it (as we cannot represent a real universe).")


;;; some hooks
(defcustom mtorus-clear-universe-hook nil
  "Hook run when the universe is cleared."
  :group 'mtorus)

(defcustom mtorus-new-ring-hook nil
  "Hook run after a new ring has been successfully
added to the universe."
  :group 'mtorus)


(defcustom mtorus-create-element-pre-hook nil
  "Hook run before an element is about to be created interactively.
Note there's an equivalent hook `mtorus-element-pre-creation-hook'
which is always run."
  :group 'mtorus)
(defcustom mtorus-create-element-post-hook nil
  "Hook run after an element has been created interactively.
Note there's an equivalent hook `mtorus-element-post-creation-hook'
which is always run."
  :group 'mtorus)
(defcustom mtorus-delete-element-pre-hook nil
  "Hook run before an element is about to be deleted interactively.
Note there's an equivalent hook `mtorus-element-pre-deletion-hook'
which is always run."
  :group 'mtorus)
(defcustom mtorus-delete-element-post-hook nil
  "Hook run after an element has been deleted interactively.
Note there's an equivalent hook `mtorus-element-post-deletion-hook'
which is always run."
  :group 'mtorus)
(defcustom mtorus-select-element-pre-hook nil
  "Hook run before an element is about to be selected interactively.
Note there's an equivalent hook `mtorus-element-pre-selection-hook'
which is always run."
  :group 'mtorus)
(defcustom mtorus-select-element-post-hook nil
  "Hook run after an element has been selected interactively.
Note there's an equivalent hook `mtorus-element-post-selection-hook'
which is always run."
  :group 'mtorus)






;; Testing function
;(defun tt ()
;  ""
;  (interactive)
;  (setq debug-on-error t)
;  (mtorus-init)
;  (mtorus-new-ring "ring1")
;  (forward-line)
;  (mtorus-new-marker)
;  (describe-variable 'mtorus-torus)
;  )


;; Commands

;; ;; all other functions depend on this being run first, so not further
;; ;; autoloads
;; ;;###autoload
;; (defun mtorus-init ()
;;   "This inits the torus in `mtorus-torus' and creates the special ring
;;   `mtorus-buffer-list-name' (see there)."
;;   (interactive)
;;   (setq mtorus-torus nil)
;;   (mtorus-new-ring mtorus-buffer-list-name)
;;   (mtorus-maybe-install-kill-hook)
;;   (run-hooks 'mtorus-init-hook))

;; REVISE ME!
(defun mtorus-universe-init ()
  "This inits the universal ring `mtorus-universe'."
  (interactive)
  ;;(mtorus-clear-universe)
  ;;(mtorus-maybe-install-kill-hook)
  (run-hooks 'mtorus-init-hook))
;;(defalias 'mtorus-init 'mtorus-universe-init)

(defun mtorus-clear-universe (&optional universe)
  "This creates a new clean universe.

The auto-rings will be set up by running 
`mtorus-auto-ring-setup-hook'"
  (interactive)
  (mtorus-clear-topologies)
  (mtorus-clear-elements)
  (mtorus-element-initialize)
  (run-hooks 'mtorus-clear-universe-hook 'mtorus-auto-ring-setup-hook))

;; (defun mtorus-cleanse-universe (&optional universe)
;;   "Finds and deletes rings from the ring-stack `mtorus-rings'
;; that are not in the universe."
;;   (mapc (lambda (ring)
;;           (and (not (mtorus-ring-in-universe-p ring))
;;                (mtorus-ring-delete-ring ring)))
;;         mtorus-rings))


(defun mtorus-clear-elements ()
  "Clears (and unregisters) all elements."
  (interactive)
  (maphash #'(lambda (key val)
               (mtorus-element-delete key))
           mtorus-elements))

   ;;; probably be moved to mtorus-topology.el
(defun mtorus-clear-topologies ()
  "Clears all topology arrangements of current torus."
  (interactive)
  (mapc #'(lambda (topology)
            (mapcar #'(lambda (neighborhood)
                        (set (mtorus-utils-symbol-conc
                              'mtorus-topology topology neighborhood)
                             (make-hash-table :test 'equal)))
                    (mtorus-topology-neighborhoods topology)))
        mtorus-topologies)
  (setq mtorus-current-element 'mtorus-universe
        mtorus-current-ring 'mtorus-universe))



;; these are frontend/user functions for element creation

(defun mtorus-create-element (type name)
  "Create an element of type TYPE and with name NAME (asked from user).
Unlike mtorus-1.6 elements duplicate names are allowed."
  (interactive
   (let* ((type
           (intern
            (completing-read
             "Type: "
             (mtorus-type-obarray) nil t)))
          (name
           ;;; REVISE ME!
           (read-string "Name: " (cond ((eq type 'buffer)
                                        (buffer-name))
                                       ((eq type 'marker)
                                        (format "%s#%s"
                                                (buffer-name)
                                                (point)))))))
     (list type name)))
  (run-hook-with-args 'mtorus-create-element-pre-hook type name)
  (let ((element
         (mtorus-element-create
          :type type
          :name name
          :value (mtorus-type-inherit-value type name)
          :description "User defined mtorus-element"
          :variable-documentation
          (format "Manually generated mtorus element of type %s." type))))
    (mtorus-element-register element)
    (run-hook-with-args 'mtorus-create-element-post-hook element)
    (mtorus-display-message
     message
     :element element
     :message '("new element: %s" element))
    element))

;; this is just a wrapper function to mtorus-create-element
(defun mtorus-create-ring (name)
  "Create an mtorus element of type `ring' with name NAME (asked from user).
If `mtorus-init-ring-emtpy' is nil a marker at the current point
is created and pushed on the list, otherwise the ring stays empty for
the moment. Makes the new ring the current ring.

Unlike mtorus-1.6 rings duplicate names are allowed."
  (interactive "sRing name: ")
  (mtorus-create-element 'ring name))
(defalias 'mtorus-new-ring-3 'mtorus-create-ring)



(defun mtorus-delete-current-element ()
  "Delete the current mtorus element.

This will use `mtorus-element-delete' to get rid of the element.
If you intend to detach an element from a ring for example use
`mtorus-detach-element' instead.

Unlike detaching deletion will entirely make the current element
unknown to mtorus."
  (interactive)
  (mtorus-delete-element mtorus-current-element))
(defun mtorus-delete-element (element &optional type-filter)
  "Delete an mtorus element.
ELEMENT can be the element name or an element itself.

This will use `mtorus-element-delete' to get rid of the element.
If you intend to detach an element from a ring for example use
`mtorus-detach-element' instead.

Unlike detaching deletion will entirely make ELEMENT unknown to mtorus.

Optional arg TYPE-FILTER specifies a filter to limit the set of all
elements to only those of a certain type."
  (interactive
   (list (let* ((table (mtorus-element-obarray+names 'all))
                (curel (car (rassoc mtorus-current-element table))))
           (cdr
            (assoc (completing-read
                    "Element: "
                    table nil t curel)
                   table)))))
  (run-hook-with-args 'mtorus-delete-element-pre-hook element)
  (mtorus-element-delete element)
  (run-hook-with-args 'mtorus-delete-element-post-hook element)
  (mtorus-display-message
   message
   :element element
   :message '("deleted: %s" element))
  element)

(defun mtorus-detach-element (element1 element2 &optional type-filter)
  "Detach an mtorus element ELEMENT1 from ELEMENT2."
  (interactive
   (let* ((el1 (let* ((table (mtorus-element-obarray+names 'all))
                      (curel (car (rassoc mtorus-current-element table))))
                 (cdr
                  (assoc (completing-read
                          "Detach element: "
                          table nil t curel)
                         table))))
          (rels (mapcar #'car (mtorus-topology-find 'standard el1)))
          (el2 (let* ((eobarr (mtorus-element-obarray
                               #'(lambda (element type)
                                   (when
                                       (member element
                                               rels)
                                     t))))
                      (table 
                       (mtorus-element-obarray-names
                        eobarr
                        "%s (%s)"
                        '(mtorus-element-get-name element) 'element))
                      (curel (car (rassoc mtorus-current-element table))))
                 (cdr
                  (assoc (completing-read
                          "From element: "
                          table nil t curel)
                         table)))))
     (list el1 el2)))
  (run-hook-with-args 'mtorus-detach-element-pre-hook element1 element2)
  (mtorus-element-detach-relations element1 element2)
  (run-hook-with-args 'mtorus-detach-element-post-hook element1 element2)
  (mtorus-display-message
   message
   :element element1
   :otherelement element2
   :message '("%s detached from %s" element otherelement))
  element1)

(defun mtorus-attach-element-relation (element1 element2 relation &optional type-filter)
  "Attach an mtorus element ELEMENT1 to ELEMENT2 with RELATION."
  (interactive
   (let* ((table (mtorus-element-obarray+names 'all))
          (curel (car (rassoc mtorus-current-element table)))
          (el1 (cdr
                (assoc (completing-read
                        "Attach element: "
                        table nil t curel)
                       table)))
          (el2 (cdr
                (assoc (completing-read
                        "To element: "
                        table nil t)
                       table)))
          (rels (mtorus-topology-neighborhood-obarray 'standard))
          (rel (completing-read
                "Relation: "
                rels nil t)))
     (list el1 el2 rel)))
  (run-hook-with-args 'mtorus-detach-element-pre-hook element1 element2)
  (mtorus-element-detach-relation element1 element2 relation)
  (run-hook-with-args 'mtorus-detach-element-post-hook element1 element2)
  (mtorus-display-message
   message
   :element element1
   :otherelement element2
   :message '("%s attached to %s" element otherelement))
  element1)


(defun mtorus-detach-element-relation (element1 element2 relation &optional type-filter)
  "Detach an mtorus element ELEMENT1 from ELEMENT2 with RELATION."
  (interactive
   (let* ((el1 (let* ((table (mtorus-element-obarray+names 'all))
                      (curel (car (rassoc mtorus-current-element table))))
                 (cdr
                  (assoc (completing-read
                          "Detach element: "
                          table nil t curel)
                         table))))
          (rels (mapcar #'car (mtorus-topology-find 'standard el1)))
          (el2 (let* ((eobarr (mtorus-element-obarray
                               #'(lambda (element type)
                                   (when
                                       (member element
                                               rels)
                                     t))))
                      (table 
                       (mtorus-element-obarray-names
                        eobarr
                        "%s (%s)"
                        '(mtorus-element-get-name element) 'element))
                      (curel (car (rassoc mtorus-current-element table))))
                 (cdr
                  (assoc (completing-read
                          "From element: "
                          table nil t curel)
                         table))))
          (rel (let* ((rels (mapcar #'(lambda (rel)
                                        (cons (format "%s" (cdr rel)) (cdr rel)))
                                    (remove-if-not
                                     #'(lambda (rel)
                                         (eq (car rel) el2))
                                     (mtorus-topology-find 'standard el1)))))
                 (cdr
                  (assoc (completing-read
                          "Relation: "
                          rels nil t)
                         rels)))))
     (list el1 el2 rel)))
  (run-hook-with-args 'mtorus-detach-element-pre-hook element1 element2)
  (mtorus-element-detach-relation element1 element2 relation)
  (run-hook-with-args 'mtorus-detach-element-post-hook element1 element2)
  (mtorus-display-message
   message
   :element element1
   :otherelement element2
   :message '("%s detached from %s" element otherelement))
  element1)







;; old 1.6
;; (defun mtorus-new-ring (ring-name)
;;   "Create a ring with name RING-NAME (asked from user).
;; If `mtorus-init-ring-emtpy' is nil a marker at the current point
;; is created and pushed on the list, otherwise the ring stays empty for
;; the moment.  Makes the new ring the current ring.
;; 
;; It won't create a ring with a name that already exists."
;;   (interactive "sRing name: ")
;;   (if (mtorus-ringp ring-name)
;;       (mtorus-message
;;        (format "A ring with name \"%s\" already exists."
;;                ring-name))
;;     (let* ((content (mtorus-initial-ring-contents ring-name))
;;           (ring (cons ring-name (list content))))
;;       (setq mtorus-torus
;;             (append mtorus-torus
;;                     (list ring))))
;;     (mtorus-switch-to-ring ring-name t)))


;; old 1.6
;; (defun mtorus-delete-ring (&optional ring-name)
;;   "Delete the ring with name RING-NAME.
;; If none is given it is asked from the user."
;;   (interactive)
;;   (let ((rname (or ring-name (mtorus-ask-ring))))
;;     (if (not (mtorus-special-ringp rname))
;;         (if (y-or-n-p (format "delete ring \"%s\"? " rname))
;;             (setq mtorus-torus
;;                   (delete* rname mtorus-torus :key 'car
;;                            :test 'equal)))
;;       (mtorus-message "can't delete special rings"))))


;; old 1.6
;; (defun mtorus-rename-ring (&optional ring-name new-name)
;;   "Rename RING-NAME to NEW-NAME asking if omitted."
;;   (interactive)
;;   (let* ((rname (or ring-name (mtorus-ask-ring)))
;;         (nname (or new-name (read-string
;;                              (format "rename \"%s\" to: " rname)))))
;;     (if (not (mtorus-special-ringp rname))
;;         (setcar (assoc rname mtorus-torus) nname)
;;       (mtorus-message "can't rename special rings"))))

;; old 1.6
;; (defun mtorus-switch-to-ring (ring-name &optional quiet)
;;   "Make RING-NAME the current ring."
;;   (while (not (mtorus-current-ringp ring-name))
;;     (mtorus-rotate-rings 1))
;;   (unless quiet (mtorus-notify)))

;; old 1.6
;; (defun mtorus-next-ring ()
;;   "Make the next ring on the torus the current ring."
;;   (interactive)
;;   (mtorus-switch-to-ring (mtorus-nth-ring-name 1)))

;; old 1.6
;; (defun mtorus-prev-ring ()
;;   "Make the next ring on the torus the current ring."
;;   (interactive)
;;   (mtorus-switch-to-ring
;;    (mtorus-nth-ring-name (1- (length mtorus-torus)))))



;; ;; Marker
;; (defun mtorus-modify-torus (ring-name func)
;;   (if (not (mtorus-special-ringp ring-name))
;;       (let ((new-ring
;;              (funcall func (copy-list
;;                             (mtorus-ring-by-name ring-name)))))
;;         (setq mtorus-torus
;;               (mapcar #'(lambda (item)
;;                           (if (string= (first item)
;;                                        ring-name)
;;                               new-ring
;;                             item))
;;                       mtorus-torus))
;;         )
;;     (mtorus-message "can't edit special lists")))
;; 
;; 
;; (defun mtorus-rename-current-ring (&optional ring-name new-name)
;;   "Rename RING-NAME to NEW-NAME asking if omitted."
;;   (interactive)
;;   (let* ((rname (or ring-name (mtorus-ask-ring)))
;;         (nname
;;          (or new-name
;;              (read-string (format "rename \"%s\" to: " rname)))))
;;     (mtorus-modify-torus
;;      ring-name #'(lambda (ring)
;;                    (list nname (second ring))))))
;; 
;; (defun mtorus-new-marker ()
;;   "Create a new marker at the current position.
;; It is added to the current ring and made the current entry in that
;; ring."
;;   (interactive)
;;   (mtorus-modify-torus
;;    (mtorus-current-ring-name)
;;     #'(lambda (ring)
;;          (list (first ring)
;;                (cons (point-marker)
;;                      (second ring))))))
;; 
;; 
;; 
;; (defun mtorus-update-current-marker ()
;;   "Make the current marker point to the current position."
;;   (interactive)
;;   (mtorus-modify-torus
;;    (mtorus-current-ring-name)
;;     #'(lambda (ring)
;;         (set-marker (first (second ring))
;;                     (point))
;;         ring)))
;; 
;; (defun mtorus-next-marker ()
;;   "Switch to the next marker in the current ring.
;; Handles special rings different like cycling the buffer list when the
;; current's ring name is equal to `mtorus-buffer-list-name'."
;;   (interactive)
;;   (if (mtorus-special-ringp (mtorus-current-ring-name))
;;       (mtorus-blist-next)
;;     (mtorus-rotate-entries 0 1))
;;   (mtorus-jump-current-marker))
;; (defalias 'mtorus-next-entry 'mtorus-next-marker)
;; 
;; (defun mtorus-prev-marker ()
;;   (interactive)
;;   (if (mtorus-special-ringp (mtorus-current-ring-name))
;;       (mtorus-blist-prev)
;;     (mtorus-rotate-entries 0 -1))
;;   (mtorus-jump-current-marker))
;; (defalias 'mtorus-prev-entry 'mtorus-prev-marker)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new generation navigation

(defcustom mtorus-next-element-function #'1+
  "Fun used to determine the successor of an element.
Note this is invariant under the current topology."
  :group 'mtorus
  :type 'function-name)
(defcustom mtorus-prev-element-function #'1-
  "Fun used to determine the predecessor of an element.
Note this is invariant under the current topology."
  :group 'mtorus
  :type 'function-name)
(defcustom mtorus-parent-element-function #'car
  "Fun used to determine the parent of a torus element."
  :group 'mtorus
  :type 'function-name)
(defcustom mtorus-child-element-function #'car
  "Fun used to determine the child a torus element."
  :group 'mtorus
  :type 'function-name)



(defun mtorus-determine-neighbor-elements (elements element &optional neighbor-function)
  "Determines elements in the neighborhood of ELEMENT on the current torus.
Optional NEIGHBOR-FUNCTION measures distances between the neighbor element.

The element symbol is returned."
  ;; REVISE ME!!!
  ;; at the moment this fun does neither
  ;; respect current-topology
  ;; nor orders
  (let* ((nhlen (length elements))
         (othelt
          (and (< 0 nhlen)
               (let* ((curpos (position element elements))
                      (othpos (funcall neighbor-function curpos)))
                 (nth (mod othpos nhlen) elements)))))
    othelt))

(defun mtorus-determine-next-element (element)
  "Determines the next element on the current ring.
See `mtorus-next-element-function' on how to determine this."
  (let* ((nextelt (mtorus-determine-neighbor-elements
                   (mtorus-topology-standard-siblings element)
                   element
                   mtorus-next-element-function)))
    nextelt))
(defun mtorus-determine-prev-element (element)
  "Determines the previous element on the current torus.
See `mtorus-prev-element-function' on how to determine this."
  (let* ((prevelt (mtorus-determine-neighbor-elements
                   (mtorus-topology-standard-siblings element)
                   element
                   mtorus-prev-element-function)))
    prevelt))
(defun mtorus-determine-parent-element (element)
  "Determines the next element on the current ring.
See `mtorus-next-element-function' on how to determine this."
  (let* ((parent (funcall mtorus-parent-element-function
                          (mtorus-topology-standard-parents element))))
    parent))
(defun mtorus-determine-child-element (element)
  "Determines the previous element on the current torus.
See `mtorus-prev-element-function' on how to determine this."
  (let* ((child (funcall mtorus-child-element-function
                         (mtorus-topology-standard-children element))))
    child))


;;; these are user frontend funs :) ... finally

(defun mtorus-next-element ()
  "Make the next element the current element.
This is done with respect to the current topology."
  (interactive)
  (let* ((curelt mtorus-current-element)
         (newelt (mtorus-determine-next-element curelt)))
    (mtorus-element-set-current newelt)
    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))
(defun mtorus-prev-element ()
  "Make the previous entry on the ring the current element."
  (interactive)
  (let* ((curelt mtorus-current-element)
         (newelt (mtorus-determine-prev-element curelt)))
    (mtorus-element-set-current newelt)
    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))
(defun mtorus-parent-element ()
  "Make the next ring on the torus the current ring.
This is done with respect to the current topology."
  (interactive)
  (let* ((curelt mtorus-current-element)
         (newelt (mtorus-determine-parent-element curelt)))
    (mtorus-element-set-current newelt)
    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))
(defun mtorus-child-element ()
  "Make the next ring on the torus the current ring.
This is done with respect to the current topology."
  (interactive)
  (let* ((curelt mtorus-current-element)
         (newelt (mtorus-determine-child-element curelt)))
    (mtorus-element-set-current newelt)
    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))


(defun mtorus-select-current-element ()
  "Select the current element (whatever this means).

Selection can be done by:
- Defining :inherit-selection in the type definition
- ... <add more>"
  (interactive)
  (mtorus-select-element mtorus-current-element))
(defun mtorus-select-element (element &optional type-filter)
  "Select an element (whatever this means).

Selection can be done by:
- Defining :inherit-selection in the type definition
- ... <add more>"
  (interactive
   (list (let* ((table (mtorus-element-obarray+names 'all))
                (curel (car (rassoc mtorus-current-element table))))
           (cdr
            (assoc (completing-read
                    "Element: "
                    table nil t curel)
                   table)))))
  (run-hook-with-args 'mtorus-select-element-pre-hook element)
  (mtorus-element-select element)
  (run-hook-with-args 'mtorus-select-element-post-hook element)
  (mtorus-display-message
   message
   :element element
   :message '("selected: %s" element))
  element)







;; ;; old 1.6
;; (defun mtorus-jump-current-marker ()
;;   "Move point to the point and buffer defined by current marker."
;;   (interactive)
;;   (let ((buf (mtorus-current-buffer))
;;         (pos (mtorus-current-pos)))
;;     (if (buffer-live-p buf)
;;         (progn
;;           (switch-to-buffer buf)
;;           (goto-char pos)
;;           (mtorus-notify))
;;       (mtorus-message (format "no such buffer %s"
;;                               (buffer-name buf)))
;;       (mtorus-delete-current-marker))))
;; ;(defun mtorus-delete-marker ()
;; ;  "Delete the current marker from the current ring."
;; ;  (interactive)
;; ;  (if (not (mtorus-special-ringp
;; ;            (mtorus-current-ring-name)))
;; ;      (let ((ring (mtorus-current-ring)))
;; ;        (set-marker (mtorus-current-marker) nil)
;; ;        (setf (second ring)
;; ;              (cdr (second (mtorus-current-ring)))))
;; ;    (mtorus-message "can't delete from special rings")))
;; 
;; ;(defun mtorus-update-current-marker ()
;; ;  "Make the current marker point to the current position.
;; ;Actually the current marker is deleted and a new one is created at the
;; ;current position."
;; ;  (interactive)
;; ;  (and (mtorus-delete-marker)
;; ;       (mtorus-new-marker)))
;; 
;; ;; FIXME: can the actual rotation be done by one backend function for
;; ;; this and for mtorus-rotate-rings?
;; (defun mtorus-rotate-entries (nth-ring amount)
;;   "Rotate the NTH-RING AMOUNT times."
;;   (let* ((ring (mtorus-ring-by-name
;;                 (mtorus-nth-ring-name nth-ring)))
;;          (rlist (second ring)))
;;     (if (> amount 0)
;;         (while (> amount 0)
;;           (setq rlist
;;                 (append (cdr rlist)
;;                         (list (car rlist))))
;;           (setq amount (1- amount)))
;;       (while (< amount 0)
;;         (setq rlist
;;               (append (last rlist)
;;                       (butlast rlist)))
;;         (setq amount (1+ amount))))
;;     (setf (second ring) rlist)))
;;     
;; ;; Special Ring: The Buffer List
;; (defun mtorus-blist-next ()
;;   "Cycle the real buffer list.
;; This can be used separately and is always used when navigating through
;; the default ring.  It skips buffers for that
;; `mtorus-buffer-skip-p' returns t."
;;   (interactive)
;;   (let ((blist (mtorus-buffer-list)))
;;     (when (> (length blist) 1)
;;       (bury-buffer)
;;       (while (funcall mtorus-buffer-skip-p (current-buffer))
;;         (bury-buffer)))))
;; 
;; (defun mtorus-blist-prev ()
;;   "Cycle the real buffer list.
;; This can be used separately and is always used when navigating through
;; the default ring.  It skips buffers for that
;; `mtorus-buffer-skip-p' returns t."
;;   (interactive)
;;   (let ((blist (mtorus-buffer-list)))
;;     (if blist (switch-to-buffer (car (reverse blist))))))
;; 
;; (defun mtorus-buffer-list ()
;;   "Return a filtered buffer list according to `mtorus-buffer-skip-p'."
;;   (mtorus-grep (buffer-list)
;;                (lambda (buf)
;;                  (not (funcall mtorus-buffer-skip-p buf)))))
;; 
;; (defun mtorus-grep (l predicate)
;;   "Helper function for a grep over a list.
;; Apply predicate PREDICATE to all elements of list L and return a list
;; consisting of all elements PREDICATE returned t for."
;;   (defun helper (ret-list rest)
;;     (if (null rest)
;;         (reverse ret-list)
;;       (progn
;;         (if (funcall predicate (car rest))
;;             (setq ret-list (cons (car rest) ret-list)))
;;         (helper ret-list (cdr rest)))))
;;   (helper '() l))


;; Backend Level Functions
;; (defun mtorus-initial-ring-contents (ring-name)
;;   "Return a list to initialize RING-NAME with.
;; Takes care of special ring names"
;;   (list 
;;    (cond
;;     ((mtorus-special-ringp ring-name)
;;      (concat "special: "
;;              ring-name))
;;     ((not mtorus-init-ring-emtpy)
;;      (point-marker))
;;     (t
;;      ()))))
;; 
;; (defun mtorus-ask-ring ()
;;   "Ask the user to choose a ring (with completion)."
;;   (let ((default (mtorus-current-ring-name)))
;;     (completing-read
;;      (format "choose a ring (%s): " default)
;;      mtorus-torus
;;      nil t nil nil
;;      default)))
;; 
;; (defun mtorus-rotate-rings (amount)
;;   "Rotate the rings on the torus AMOUNT times."
;;   (if (> amount 0)
;;       (while (> amount 0)
;;         (setq mtorus-torus
;;               (append (cdr mtorus-torus)
;;                       (list (car mtorus-torus))))
;;         (setq amount (1- amount)))
;;     (while (< amount 0)
;;       (setq mtorus-torus
;;             (append (last mtorus-torus)
;;                     (butlast (car mtorus-torus))))
;;       (setq amount (1+ amount)))))
;; 
;; 
;; ;; Ring Accessors
;; (defun mtorus-current-ring-name ()
;;   "Return a string containing the name of the current ring."
;;   (mtorus-nth-ring-name 0))
;; 
;; (defun mtorus-nth-ring-name (nth)
;;   "Return a string containing the name of the NTH ring on the torus.
;; For the current entry NTH is 0 and for the last NTH is length -1."
;;   (car (elt mtorus-torus (% nth (length mtorus-torus)))))
;; 
;; (defun mtorus-ring-names ()
;;   "Return a list of all available ring names as strings."
;;   (mapcar 'car mtorus-torus))
;; 
;; (defun mtorus-current-ring ()
;;   "Return the current ring as a list.
;; The current ring is always the CDR of the 0th in the list."
;;   (first mtorus-torus))
;; 
;; (defun mtorus-ring-by-name (ring-name)
;;   "Return the ASSOC of NAME in `mtorus-torus'."
;;   (assoc ring-name mtorus-torus))
;; 
;; (defun mtorus-add-to-ring (ring-name marker)
;;   "Put MARKER on the ring named RING-NAME."
;;   (let ((ring (mtorus-ring-by-name ring-name)))
;;     (setf (second ring)
;;           (cons marker
;;                 (second ring)))))
;; 
;; ;; Entry Accessors
;; (defun mtorus-current-entry-string ()
;;   "Return a string containing a description of the current entry."
;;   (mtorus-entry-to-string (mtorus-current-marker)))
;; 
;; (defun mtorus-entry-to-string (marker)
;;   "Return a stringified description of MARKER."
;;   (if (buffer-live-p (marker-buffer marker))
;;     (format "%s@%d"
;;             (buffer-name (marker-buffer marker))
;;             (marker-position marker))
;;     "*del*"))
;; 
;; (defun mtorus-current-marker ()
;;   "Return the current marker in the current ring."
;;   (if (mtorus-special-ringp (mtorus-current-ring-name))
;;     ;; FIXME: good enough for GC?
;;       (point-marker)
;;     (first (second (first mtorus-torus)))))
;; 
;; (defun mtorus-current-pos ()
;;   "Return the current entry's position in the buffer."
;;   (marker-position (mtorus-current-marker)))
;; 
;; (defun mtorus-current-buffer ()
;;   "Return the current entry's buffer."
;;   (marker-buffer (mtorus-current-marker)))
;; 
;; ;; Predicates
;; (defun mtorus-current-ringp (ring-name)
;;   "Return non-nil if RING-NAME is the current ring.
;; That is if it is first on the torus."
;;  (string-equal (mtorus-current-ring-name) ring-name))
;; 
;; (defun mtorus-special-ringp (ring-name)
;;   "Return non-nil if the ring-name looks like a special ring.
;; By convention special ring names begin with a '*'."
;;   (char-equal ?* (elt ring-name 0)))
;; 
;; (defun mtorus-ringp (ring-name)
;;   "Return non-nil if ring-name is found on `mtorus-torus'."
;;   (and (stringp ring-name)
;;        (assoc ring-name mtorus-torus)))
;; 
;; (defun mtorus-maybe-install-kill-hook ()
;;   "Install some functions on the `kill-emacs-hook' according to custom
;;   settings and assuring no duplicates."
;;   (when (and mtorus-save-on-exit
;;              (not (memq 'mtorus-quit kill-emacs-hook)))
;;     (add-hook 'kill-emacs-hook
;;               'mtorus-quit)))




(run-hooks 'mtorus-after-load-hook)


(provide 'mtorus)

;;; mtorus.el ends here
