;;; mtorus.el --- navigation with marks on a ring of rings (torus)
;; $Id: mtorus.el,v 1.31 2004/11/24 16:32:55 hroptatyr Exp $
;; Copyright (C) 2003 by Stefan Kamphausen
;;           (C) 2004 by Sebastian Freundt
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;;         Sebastian Freundt <hroptatyr@users.berlios.de>
;; Created: Winter 2002
;; Keywords: bookmarks, navigation, tools, extensions, user

;; This file is not part of XEmacs.

(defconst mtorus-version "2.2 $Revision: 1.31 $"
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

;;; MTorus on the Web:
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

;;; ToDo: (in planner syntax)
;; #A1  _ manage currying for keyword definition (2004.09.14)
;; #A2  _ write docs (2004.09.14)
;; #A3  _ bring back mtorus-1.6 special lists (2004.09.14)
;; #A4  _ make current-element/ring frame/window-local (2004.09.14)
;; #A5  _ mtorus-state doesnt restore topology correctly (2004.09.14)

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
;; - If you rather want to simulate mtorus-1.6 navigation behavior try
;;   navigating with the commands
;;
;;     mtorus-uncle-element
;;     mtorus-aunt-element
;;     mtorus-nephew-element
;;     mtorus-niece-element
;;
;; 
;; To treat your command history with care, bind these commands to some keys.
;; 
;; Suggested Keybindings:
;; - I have H-kp-6, H-kp-4, H-kp-8 and H-kp-2 keys bound to
;;   mtorus-next-element, mtorus-prev-element, mtorus-parent-element and
;;   mtorus-child-element respectively -- [[hroptatyr]]
;;   For selection of an element I user H-kp-5
;; - I prefer shift -left/right/up/down -- StefanKamphausen
;;
;;
;; To see some of the modularized external stuff in action put
;;
;;     (require 'mtorus-auto-rings)
;;
;; in your .emacs. :D


;;; Usage: (still recent, since everything tries to emulate mtorus-1.6)
;;  ===================================================================
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

;;; History:
;;  ========
;; - Waaaay back in time:
;;   Just as many other people I (Stefan Kamphausen) have once (co-)written a
;;   buffer cycling mechanism, that was later expanded to using a skipping
;;   predicate.
;;   At some point it wasn't enough anymore and I still had
;;   the question nagging: what could I possible do with Shift Up and Down?
;;   Then I discovered that I often used several buffers in one context
;;   (and they were not neighbors on the buffer-list). This made me think
;;   of groups of buffers and led to a first implementation under the name
;;   "session-stack.el".
;;
;; - Winter 2002:
;;   Then it occurred to me that I wanted to navigate different positions
;;   in the same buffer with it and expanded the code. Sometimes I wondered
;;   that those hotspots seemed to move around until I read a chapter about
;;   markers in the elisp info file and suddenly I understood that the points
;;   I stored were not moving together with the text.
;;   During that rewrite I renamed the whole thing to mtorus reflecting the
;;   topology of the main data structure.
;;
;; - Early 2003:
;;   I came across swbuff.el which had a very nice popup window feature that
;;   I definetely admired and wanted to have. It became clear that a full
;;   rewrite had to be done once again because the code (which was then just
;;   released to the public one week ago) had become quite a mess.
;;
;; - November 2003: 
;;   The MTorus project turned once more to public interest. This time I
;;   (Sebastian Freundt) wanted a more generic implementation and some kind of
;;   API because I wanted to implement something like a `auto-mtorus' which
;;   grows automagically on some defined ruleset of mine.
;;
;; - Early 2004:
;;   Asking Stefan if he's still actively contributing to the project he nayed.
;;   When I told him about my intentions new enthusiasm has been grown.
;;   We opened a project at berlios.de and started to revise the code.
;;   This is where we are currently in the time line.
;;   The project is still open for other contributors and developers.
;;
;; Why I tell this all here? Hm, probably mainly for myself to remember when
;; I'm grey and old -- and still be using XEmacs ;-) -- Stefan


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


(defvar mtorus-read-string-history nil
  "The history to use when asking the user.")


;;;;;;;;;;;;;
;;; Behaviour
;;;;;;;;;;;;;

;;; Static Behaviour
;; I'd call this static because you usually dont want to change the settings 
;; during an mtorus session


;; AutoAttach elements per-type to the topology network 
(defcustom mtorus-auto-attach-p t
  "*Whether to use auto attaching.
We will define later what this actually means."
  :group 'mtorus)

;; AutoWatch elements if you're on a buffer which is in the torus
(defcustom mtorus-auto-watch-p nil
  "*Whether to track current element according to the current (point)."
  :type 'boolean
  :group 'mtorus)

;;; variable wrappers for dynamic induce behaviour
(defcustom mtorus-ring-induces-buffer nil
  "*Whether to create a new ring with a buffer in it."
  :type 'boolean
  :group 'mtorus)
(defcustom mtorus-ring-induces-marker nil
  "*Whether to create a new ring with a marker at point."
  :type 'boolean
  :group 'mtorus)
(defcustom mtorus-buffer-induces-marker nil
  "*Whether to create a new buffer with a marker at point."
  :type 'boolean
  :group 'mtorus)

;;; REVISE ME!
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


   ;;; this is a general default behaviour installer
(defun mtorus-install-default-behaviour ()
  "Installs the default behaviour of how things in mtorus work."
  (interactive)
  (if mtorus-ring-induces-buffer
      (mtorus-enable-ring-induces-buffer)
    (mtorus-disable-ring-induces-buffer))
  (if mtorus-ring-induces-marker
      (mtorus-enable-ring-induces-marker)
    (mtorus-disable-ring-induces-marker))
  (if mtorus-buffer-induces-marker
      (mtorus-enable-buffer-induces-marker)
    (mtorus-disable-buffer-induces-marker))
  )

;; additional behaviour installers
(defun mtorus-install-suggested-bindings-ska ()
  "This sets the key-bindings that I (Stefan Kamphausen) consider useful.
The bindings don't not fulfill the requirements of good key-defining
but I like it and I provide it only as a convenience function.
Special care for CUA users is taken."
  (interactive)

  ;;; old 1.6, but compatible
  (cond
   ((featurep 'cua) ;; FIXME how to detect?
    (global-set-key '[(f10)]         'mtorus-nephew-element)
    (global-set-key '[(f9)]          'mtorus-niece-element)
    (global-set-key '[(shift f10)]   'mtorus-uncle-element)
    (global-set-key '[(shift f9)]    'mtorus-aunt-element))
   (t
    (global-set-key '[(shift right)] 'mtorus-nephew-element)
    (global-set-key '[(shift left)]  'mtorus-niece-element)
    (global-set-key '[(shift up)]    'mtorus-uncle-element)
    (global-set-key '[(shift down)]  'mtorus-aunt-element)))
   
  ;; ring handling: f11
  (global-set-key '[(f11)] 'mtorus-create-ring)
;;   (global-set-key '[(shift f11)]
;;     'mtorus-delete-ring)
  ;; does exist now, but doesnt really make sense, no?
  ;; M-x mtorus-delete-ring RET would ask for a ring interactively

;;   (global-set-key '[(control f11)]
;;     'mtorus-notify)
  ;; doesnt exist atm

  ;; marker handling: f12
  (global-set-key '[(f12)]
    'mtorus-create-element)
;;   (global-set-key '[(shift f12)]
;;     'mtorus-delete-current-marker)
  ;; doesnt exist atm

;;   (global-set-key '[(control f12)]
;;     'mtorus-update-current-marker)
;;   (global-set-key '[(control f12)]
;;     'mtorus-update-current-element)
  ;; doesnt exist atm
  ;; maybe it's what mtorus-update-current-element does

  ;; auto selection
  (mtorus-enable-select-follows-choose)

  (message "ska bindings installed"))
(defalias 'mtorus-install-suggested-bindings
  'mtorus-install-suggested-bindings-ska)

(defun mtorus-install-suggested-bindings-hroptatyr ()
  "This sets the key-bindings that I (Sebastian Freundt) consider useful."
  (interactive)

  (global-set-key '[(control kp-6)] 'mtorus-next-element)
  (global-set-key '[(control kp-4)] 'mtorus-prev-element)
  (global-set-key '[(control kp-7)] 'mtorus-aunt-element)
  (global-set-key '[(control kp-8)] 'mtorus-parent-element)
  (global-set-key '[(control kp-9)] 'mtorus-uncle-element)
  (global-set-key '[(control kp-1)] 'mtorus-niece-element)
  (global-set-key '[(control kp-2)] 'mtorus-child-element)
  (global-set-key '[(control kp-3)] 'mtorus-nephew-element)
  (global-set-key '[(control kp-5)] 'mtorus-select-current-element)
  (global-set-key '[(control kp-0)] 'mtorus-create-element)
  (global-set-key '[(control kp-decimal)] 'mtorus-delete-element)

  ;; auto selection
  (mtorus-disable-select-follows-choose)

  ;; resurrection behaviour
  (mtorus-enable-select-resurrects-dead)

  (message "hroptatyr bindings and behaviours installed"))



;;; Dynamic Behaviour
;; I'd call this dynamic because you may have the urge to switch it on or off
;; during the mtorus session

;; navigating selects the element
(defvar mtorus-behaviour-select-follows-choose nil
  "Indicates whether `mtorus-behaviour-select-follows-choose' is enabled.")
(defun mtorus-enable-select-follows-choose ()
  "Use this to make mtorus automatically display a selected item.
Use `mtorus-disable-select-follows-choose' to turn it off."
  (interactive)
  (add-hook 'mtorus-type-buffer-post-choose-funs 'mtorus-select-element)
  (add-hook 'mtorus-type-marker-post-choose-funs 'mtorus-select-element)
  (message "select follows choose")
  (setq mtorus-behaviour-select-follows-choose t))
(defun mtorus-disable-select-follows-choose ()
  "See `mtorus-enable-select-follows-choose'."
  (interactive)
  (remove-hook 'mtorus-type-buffer-post-choose-funs 'mtorus-select-element)
  (remove-hook 'mtorus-type-marker-post-choose-funs 'mtorus-select-element)
  (message "select follows choose ... off")
  (setq mtorus-behaviour-select-follows-choose nil))


;; dead elements are resurrected
(defvar mtorus-behaviour-select-resurrects-dead nil
  "Indicates whether `mtorus-behaviour-select-resurrects-dead' is enabled.")
(defun mtorus-behaviour-select-resurrects-dead (element)
  "MTorus automatically opens files for selected items.
Whenever an item is selected and the according file for it is not
currently available it will be reopened.  This feature can be used in
conjunction with the saving of the mtorus to get an ever growing map
of all your files and positions.
Use `mtorus-enable-select-resurrects-dead' to turn it on.
Use `mtorus-disable-select-resurrects-dead' to turn it off."
  (unless (mtorus-element-alive-p element)
    (mtorus-element-resurrect element)))
(defun mtorus-enable-select-resurrects-dead ()
  "Enables `mtorus-behaviour-select-resurrects-dead'.
Use `mtorus-disable-select-resurrects-dead' to turn it off."
  (interactive)
  (add-hook 'mtorus-select-element-pre-hook 'mtorus-behaviour-select-resurrects-dead)
  (message "select resurrects dead")
  (setq mtorus-behaviour-select-resurrects-dead t))
(defun mtorus-disable-select-resurrects-dead ()
  "Disables `mtorus-behaviour-select-resurrects-dead'.
Use `mtorus-enable-select-resurrects-dead' to turn it on."
  (interactive)
  (remove-hook 'mtorus-select-element-pre-hook 'mtorus-behaviour-select-resurrects-dead)
  (message "select resurrects dead ... off")
  (setq mtorus-behaviour-select-resurrects-dead nil))


;; creating a ring, also creates a buffer
(defvar mtorus-behaviour-ring-induces-buffer nil
  "Indicates whether `mtorus-behaviour-ring-induces-buffer' is enabled.")
(defun mtorus-behaviour-ring-induces-buffer (element)
  "Automatically create a buffer-element when creating a new ring."
  (and (mtorus-type-ring-p element)
       (mtorus-create-element 'buffer (mtorus-default-name 'buffer))))
(defun mtorus-enable-ring-induces-buffer ()
  "Enables `mtorus-behaviour-ring-induces-buffer'."
  (interactive)
  (add-hook 'mtorus-create-element-post-hook 'mtorus-behaviour-ring-induces-buffer)
  (message "ring induces buffer")
  (setq mtorus-behaviour-ring-induces-buffer t))
(defun mtorus-disable-ring-induces-buffer ()
  "Disables `mtorus-behaviour-ring-induces-buffer'."
  (interactive)
  (remove-hook 'mtorus-create-element-post-hook 'mtorus-behaviour-ring-induces-buffer)
  (message "ring induces buffer ... off")
  (setq mtorus-behaviour-ring-induces-buffer nil))


;; creating a ring, also creates a marker
(defvar mtorus-behaviour-ring-induces-marker nil
  "Indicates whether `mtorus-behaviour-ring-induces-marker' is enabled.")
(defun mtorus-behaviour-ring-induces-marker (element)
  "Automatically create a marker-element when creating a new ring."
  (and (mtorus-type-ring-p element)
       (mtorus-create-element 'marker (mtorus-default-name 'marker))))
(defun mtorus-enable-ring-induces-marker ()
  "Enables `mtorus-behaviour-ring-induces-marker'.
Use `mtorus-disable-ring-induces-marker' to turn off."
  (interactive)
  (add-hook 'mtorus-create-element-post-hook 'mtorus-behaviour-ring-induces-marker)
  (message "ring induces marker")
  (setq mtorus-behaviour-ring-induces-marker t))
(defun mtorus-disable-ring-induces-marker ()
  "Disables `mtorus-behaviour-ring-induces-marker'.
Use `mtorus-enable-ring-induces-marker' to turn on."
  (interactive)
  (remove-hook 'mtorus-create-element-post-hook 'mtorus-behaviour-ring-induces-marker)
  (message "ring induces marker ... off")
  (setq mtorus-behaviour-ring-induces-marker nil))


;; creating a buffer, also creates a marker
(defvar mtorus-behaviour-buffer-induces-marker nil
  "Indicates whether `mtorus-behaviour-buffer-induces-marker' is enabled.")
(defun mtorus-behaviour-buffer-induces-marker (element)
  "Automatically create a marker-element when creating a new buffer.."
  (and (mtorus-type-buffer-p element)
       (mtorus-create-element 'marker (mtorus-default-name 'marker))))
(defun mtorus-enable-buffer-induces-marker ()
  "Enables `mtorus-behaviour-buffer-induces-marker'.
Use `mtorus-disable-buffer-induces-marker' to turn off."
  (interactive)
  (add-hook 'mtorus-create-element-post-hook 'mtorus-behaviour-buffer-induces-marker)
  (message "buffer induces marker")
  (setq mtorus-behaviour-buffer-induces-marker t))
(defun mtorus-disable-buffer-induces-marker ()
  "Disables `mtorus-behaviour-buffer-induces-marker'.
Use `mtorus-enable-buffer-induces-marker' to turn on."
  (interactive)
  (remove-hook 'mtorus-create-element-post-hook 'mtorus-behaviour-buffer-induces-marker)
  (message "buffer induces marker ... off")
  (setq mtorus-behaviour-buffer-induces-marker nil))












;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;
;;; MTorus internals
;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

;; Default Settings
(defun mtorus-default-buffer-skip-p (buffer)
  "The default predicate used for `mtorus-buffer-skip-p'
is to skip only the special buffers whose name begins with a space."
  (string-match "[ ]+" (buffer-name buffer)))

;;; REVISE ME!
;; this is code you will find in two minutes in the auto-rings.el
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
(defvar mtorus-universe nil ;; obsolete??
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
  \(Actually the parents and siblings of the universe in the sense
  of the 'standard topology is the universe itself\)

The universe will dynamically extend when you try to create siblings
for it (as we cannot represent a real universe).")


;;; some hooks
(defcustom mtorus-clear-universe-hook nil
  "Hook run when the universe is cleared."
  :type 'hook
  :group 'mtorus)


;;; interactive hooks
(defcustom mtorus-create-element-pre-hook nil
  "Hook run before an element is about to be created interactively.
Note there's an equivalent hook `mtorus-element-pre-creation-hook'
which is always run."
  :type 'hook
  :group 'mtorus)
(defcustom mtorus-create-element-post-hook nil
  "Hook run after an element has been created interactively.
Note there's an equivalent hook `mtorus-element-post-creation-hook'
which is always run."
  :type 'hook
  :group 'mtorus)
(defcustom mtorus-delete-element-pre-hook nil
  "Hook run before an element is about to be deleted interactively.
Note there's an equivalent hook `mtorus-element-pre-deletion-hook'
which is always run."
  :type 'hook
  :group 'mtorus)
(defcustom mtorus-delete-element-post-hook nil
  "Hook run after an element has been deleted interactively.
Note there's an equivalent hook `mtorus-element-post-deletion-hook'
which is always run."
  :type 'hook
  :group 'mtorus)
(defcustom mtorus-select-element-pre-hook nil
  "Hook run before an element is about to be selected interactively.
Note there's an equivalent hook `mtorus-element-pre-selection-hook'
which is always run."
  :type 'hook
  :group 'mtorus)
(defcustom mtorus-select-element-post-hook nil
  "Hook run after an element has been selected interactively.
Note there's an equivalent hook `mtorus-element-post-selection-hook'
which is always run."
  :type 'hook
  :group 'mtorus)

;; navigation hooks
(defcustom mtorus-navigate-pre-hook nil
  "Hook run before navigation takes place.
This hook is run with the old current element."
  :type 'hook
  :group 'mtorus)
(defcustom mtorus-navigate-post-hook nil
  "Hook run before navigation takes place.
This hook is run with the new current element."
  :type 'hook
  :group 'mtorus)





;; Commands


;;; REVISE ME!
;;###autoload
(defun mtorus-universe-init ()
  "This inits the universal ring `mtorus-universe'."
  (interactive)
  ;;(mtorus-clear-universe)
  ;;(mtorus-maybe-install-kill-hook)
  (mtorus-install-per-type-hooks)
  (mtorus-install-default-behaviour)
  (run-hooks 'mtorus-init-hook))
(defalias 'mtorus-init 'mtorus-universe-init)


(defun mtorus-clear-universe (&optional universe)
  "This creates a new clean universe.

The auto-rings will be set up by running 
`mtorus-auto-ring-setup-hook'"
  (interactive)
  (mtorus-clear-topologies)
  (mtorus-clear-elements)
  (mtorus-element-initialize)
  (run-hooks 'mtorus-clear-universe-hook 'mtorus-auto-ring-setup-hook))


(defun mtorus-run-per-type-pre-hooks (element)
  "Runs hooks in `mtorus-type-run-pre-choose-funs' with ELEMENT."
  (let ((type (mtorus-element-get-property 'type element)))
    (and type
         (mtorus-type-run-pre-choose-funs type element))))
(defun mtorus-run-per-type-post-hooks (element)
  "Runs hooks in `mtorus-type-run-post-choose-funs' with ELEMENT."
  (let ((type (mtorus-element-get-property 'type element)))
    (and type
         (mtorus-type-run-post-choose-funs type element))))
(defun mtorus-install-per-type-hooks ()
  "Adds `mtorus-run-per-type-pre/post-hooks' to
`mtorus-navigate-pre/post-hook'."
  (interactive)
  (add-hook 'mtorus-navigate-pre-hook 'mtorus-run-per-type-pre-hooks)
  (add-hook 'mtorus-navigate-post-hook 'mtorus-run-per-type-post-hooks))
(defun mtorus-uninstall-per-type-hooks ()
  "Removes `mtorus-run-per-type-pre/post-hooks' from
`mtorus-navigate-pre/post-hook'."
  (interactive)
  (remove-hook 'mtorus-navigate-pre-hook 'mtorus-run-per-type-pre-hooks)
  (remove-hook 'mtorus-navigate-post-hook 'mtorus-run-per-type-post-hooks))


(defun mtorus-clear-elements ()
  "Clears (and unregisters) all elements."
  (interactive)
  (maphash
   #'(lambda (elem elem-specs)
       (mtorus-element-delete elem t))
   (eval mtorus-elements-hash-table)))

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



(defcustom mtorus-creation-default-names
  '((ring . "")
    (buffer . #'buffer-name)
    (marker . (format "%s#%s" (buffer-name) (point)))
    (file . (let ((bfn (buffer-file-name)))
              (and bfn
                   (file-name-nondirectory bfn)))))
  "*Names used as defaults when creating elements.
This is an alist of \(type . default-name-function-or-string\)."
  :group 'mtorus)


(defun mtorus-default-name (type)
  "Computes a default element name if element were of TYPE."
  (let* ((str-or-fun
          (cdr-safe
           (assoc type
                  mtorus-creation-default-names)))
         (result
          (cond ((functionp str-or-fun)
                 (funcall str-or-fun))
                ((functionp (eval str-or-fun))
                 (funcall (eval str-or-fun)))
                ((stringp (eval str-or-fun))
                 (eval str-or-fun))
                ((stringp str-or-fun)
                 str-or-fun))))
    (format "%s" (or result ""))))



;; these are frontend/user functions for element creation

(defun mtorus-create-element (type name)
  "Create an element of type TYPE and with name NAME (asked from user).
Unlike mtorus-1.6 elements duplicate names are allowed."
  (interactive
   ;; REVISE ME!
   (let* ((type
           (intern
            (completing-read
             "Type: "
             (mtorus-type-obarray) nil t)))
          (default-name
            (mtorus-default-name type))
          (name
           (read-string
            (format "Name (%s): " default-name)
            nil 'mtorus-read-string-history
            default-name)))
     (list type name)))
  (run-hook-with-args 'mtorus-create-element-pre-hook type name)
  (let ((element
         (define-mtorus-element
           (make-mtorus-element
            :type type
            :name name
            :value (mtorus-type-invoke-inherit-value type name)
            :resurrection-data
            (mtorus-type-invoke-inherit-resurrection-data type name)
            :description "User defined mtorus-element"))))
    (run-hook-with-args 'mtorus-create-element-post-hook element)
    (mtorus-display-message
     message
     :element element
     :message '("new element: %s" element))
    element))

;; these are just wrapper functions to mtorus-create-element
(defun mtorus-create-bouncer (types)
  "Creates `mtorus-create-<TYPE>' functions."
  (interactive)
  (mapc
   #'(lambda (type)
       (mapc
        #'eval
        `((defun ,(mtorus-utils-symbol-conc 'mtorus-create type)
            (name)
            ,(format
              "Create an mtorus element of type `%s' with NAME (asked from user)."
              type)
            (interactive
             (list (read-string
                    ,(format "%s name: "
                             (capitalize (format "%s" type)))
                    (mtorus-default-name ',type))))
            (mtorus-create-element ',type name))
          (defalias ',(mtorus-utils-symbol-conc 'mtorus-new type)
            ',(mtorus-utils-symbol-conc 'mtorus-create type)))))
   types))
(mtorus-create-bouncer mtorus-types)




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
                    (format "Element (%s): " curel)
                    table nil t nil 'mtorus-read-string-history curel)
                   table)))))
  (run-hook-with-args 'mtorus-delete-element-pre-hook element)
  (mtorus-element-delete element)
  (run-hook-with-args 'mtorus-delete-element-post-hook element)
  (mtorus-display-message
   message
   :element element
   :message '("deleted: %s" element))
  element)


;; these are just wrapper functions to mtorus-delete-element
(defun mtorus-delete-bouncer (types)
  "Creates `mtorus-delete-<TYPE>' functions."
  (interactive)
  (mapc
   #'(lambda (type)
       (mapc
        #'eval
        `((defun ,(mtorus-utils-symbol-conc 'mtorus-delete type)
            (name)
            ,(format
              "Delete an mtorus element of type `%s' with NAME (asked from user)."
              type)
            (interactive
             (list (let* ((table (mtorus-element-obarray+names ',type))
                          (curel (car (rassoc mtorus-current-element table))))
                     (cdr
                      (assoc (completing-read
                              (format "Element (%s): " curel)
                              table nil t nil 'mtorus-read-string-history curel)
                             table)))))
            (mtorus-delete-element name)))))
   types))
(mtorus-delete-bouncer mtorus-types)





(defun mtorus-rename-element (element newname &optional type-filter)
  "Rename an mtorus element."
  (interactive
   (let* ((table (mtorus-element-obarray+names 'all))
          (curel (car (rassoc mtorus-current-element table)))
          (element
           (cdr
            (assoc (completing-read
                    (format "Element (%s): " curel)
                    table nil t nil 'mtorus-read-string-history curel)
                   table)))
          (newname
           (read-string
            "New Name: "
            (mtorus-element-get-property 'name element)
            'mtorus-read-string-history)))
     (list element newname)))

  ;;; hooks??
  (mtorus-element-put-property 'name element newname))

(defun mtorus-rename-current-element (newname &optional type-filter)
  "Rename the current mtorus element."
  (interactive
   (list (read-string
          "New Name: "
          (mtorus-element-get-property 'name mtorus-current-element)
          'mtorus-read-string-history)))

  ;;; hooks??
  (mtorus-rename-element mtorus-current-element newname))





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
                        '(mtorus-element-get-property 'name element) 'element))
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
          (rel (intern
                (completing-read
                 "Relation: "
                 rels nil t))))
     (list el1 el2 rel)))
  (run-hook-with-args 'mtorus-detach-element-pre-hook element1 element2)
  (mtorus-topology-define-relation 'standard relation element1 element2)
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
                        '(mtorus-element-get-property 'name element) 'element))
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



(defun mtorus-update-element (element)
  "Update ELEMENT with value from mtorus-element-inherit-value."
  (interactive
   (let* ((table (mtorus-element-obarray+names 'all))
          (curel (car (rassoc mtorus-current-element table)))
          (element
           (cdr
            (assoc (completing-read
                    (format "Element (%s): " curel)
                    table nil t nil 'mtorus-read-string-history curel)
                   table))))
     (list element)))
  (let ((newval (mtorus-element-inherit-value element))
        (newres (mtorus-element-inherit-resurrection-data element)))
    (mtorus-element-put-resurrection-data element newres)))

(defun mtorus-update-current-element ()
  "Update the current element with value from mtorus-element-inherit-value."
  (interactive)
  (mtorus-update-element mtorus-current-element))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new generation navigation

(defcustom mtorus-next-element-function #'1+
  "*Fun used to determine the successor of an element.
Note this is invariant under the current topology."
  :group 'mtorus
  :type 'function-name)
(defcustom mtorus-prev-element-function #'1-
  "*Fun used to determine the predecessor of an element.
Note this is invariant under the current topology."
  :group 'mtorus
  :type 'function-name)
(defcustom mtorus-parent-element-function #'car
  "*Fun used to determine the parent of a torus element."
  :group 'mtorus
  :type 'function-name)
(defcustom mtorus-child-element-function
  #'(lambda (elements)
      (car-safe (remove 'mtorus-universe elements)))
  "*Fun used to determine the child a torus element."
  :group 'mtorus
  :type 'function-name)



(defun mtorus-determine-neighbor-elements (elements element &optional neighbor-function)
  "Determines elements in the neighborhood of ELEMENT on the current torus.
Optional NEIGHBOR-FUNCTION measures distances between the neighbor element.

The element symbol is returned."
  ;; REVISE ME!!!
  ;; this function is blind for anything else than siblings
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
  (let* ((orderfun mtorus-default-order)
         (nextelt (mtorus-determine-neighbor-elements
                   (funcall orderfun
                            (mtorus-topology-neighborhood
                             mtorus-current-topology
                             'siblings
                             element))
                   element
                   mtorus-next-element-function)))
    (or nextelt element)))
(defun mtorus-determine-prev-element ( element)
  "Determines the previous element on the current torus.
See `mtorus-prev-element-function' on how to determine this."
  (let* ((orderfun mtorus-default-order)
         (prevelt (mtorus-determine-neighbor-elements
                   (funcall orderfun
                            (mtorus-topology-neighborhood
                             mtorus-current-topology
                             'siblings
                             element))
                   element
                   mtorus-prev-element-function)))
    (or prevelt element)))
(defun mtorus-determine-parent-element (element)
  "Determines the next element on the current ring.
See `mtorus-next-element-function' on how to determine this."
  (let* ((orderfun mtorus-default-order)
         (parent
          (funcall mtorus-parent-element-function
                   (funcall orderfun
                            (mtorus-topology-neighborhood
                             mtorus-current-topology
                             'parents
                             element)))))
    (or parent 'mtorus-universe)))
(defun mtorus-determine-child-element (element)
  "Determines the previous element on the current torus.
See `mtorus-prev-element-function' on how to determine this."
  (let* ((orderfun mtorus-default-order)
         (child
          (funcall mtorus-child-element-function
                   (funcall orderfun
                            (mtorus-topology-neighborhood
                             mtorus-current-topology
                             'children
                             element)))))
    (or child element)))


;;; these are user frontend funs :) ... finally

(defun mtorus-next-element ()
  "Make the current element's next sibling the current element.
This is done with respect to the current topology."
  (interactive)
  (run-hook-with-args 'mtorus-navigate-pre-hook mtorus-current-element)
  (let* ((curelt mtorus-current-element)
         (newelt (mtorus-determine-next-element curelt)))
    (mtorus-element-set-current newelt)
    (run-hook-with-args 'mtorus-navigate-post-hook mtorus-current-element)

    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))
(defun mtorus-prev-element ()
  "Make the current element's previous sibling the current element.
This is done with respect to the current topology."
  (interactive)
  (run-hook-with-args 'mtorus-navigate-pre-hook mtorus-current-element)
  (let* ((curelt mtorus-current-element)
         (newelt (mtorus-determine-prev-element curelt)))
    (mtorus-element-set-current newelt)
    (run-hook-with-args 'mtorus-navigate-post-hook mtorus-current-element)

    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))
(defun mtorus-parent-element ()
  "Make the first parent of the current element's parents the current element.
This is done with respect to the current topology."
  (interactive)
  (run-hook-with-args 'mtorus-navigate-pre-hook mtorus-current-element)
  (let* ((curelt mtorus-current-element)
         (newelt (mtorus-determine-parent-element curelt)))
    (mtorus-element-set-current newelt)
    (run-hook-with-args 'mtorus-navigate-post-hook mtorus-current-element)

    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))
(defun mtorus-child-element ()
  "Make the first child of the current element's children the current element.
This is done with respect to the current topology."
  (interactive)
  (run-hook-with-args 'mtorus-navigate-pre-hook mtorus-current-element)
  (let* ((curelt mtorus-current-element)
         (newelt (mtorus-determine-child-element curelt)))
    (mtorus-element-set-current newelt)
    (run-hook-with-args 'mtorus-navigate-post-hook mtorus-current-element)

    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))

;;; mtorus 1.6 compatibility navigation
(defun mtorus-aunt-element ()
  "Make the prev sibling of the current element's parents the current element.
This is done with respect to the current topology."
  (interactive)
  (run-hook-with-args 'mtorus-navigate-pre-hook mtorus-current-element)
  (let* ((curelt (cond ((mtorus-type-ring-p mtorus-current-element)
                        mtorus-current-element)
                       (t (mtorus-determine-parent-element
                           mtorus-current-element))))
         (newelt (mtorus-determine-child-element
                  (mtorus-determine-prev-element
                   curelt))))
    (mtorus-element-set-current newelt)
    (run-hook-with-args 'mtorus-navigate-post-hook mtorus-current-element)

    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))
(defun mtorus-uncle-element ()
  "Make the next sibling of the current element's parents the current element.
This is done with respect to the current topology."
  (interactive)
  (run-hook-with-args 'mtorus-navigate-pre-hook mtorus-current-element)
  (let* ((curelt (cond ((mtorus-type-ring-p mtorus-current-element)
                        mtorus-current-element)
                       (t (mtorus-determine-parent-element
                           mtorus-current-element))))
         (newelt (mtorus-determine-child-element
                  (mtorus-determine-next-element
                   curelt))))
    (mtorus-element-set-current newelt)
    (run-hook-with-args 'mtorus-navigate-post-hook mtorus-current-element)

    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))
(defun mtorus-niece-element ()
  "Make the prev sibling of the current element's child the current element.
This is done with respect to the current topology."
  (interactive)
  (run-hook-with-args 'mtorus-navigate-pre-hook mtorus-current-element)
  (let* ((curelt (cond ((mtorus-type-ring-p mtorus-current-element)
                        (mtorus-determine-child-element mtorus-current-element))
                       (t mtorus-current-element)))
         (newelt (mtorus-determine-prev-element
                  curelt)))
    (mtorus-element-set-current newelt)
    (run-hook-with-args 'mtorus-navigate-post-hook mtorus-current-element)

    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))
(defun mtorus-nephew-element ()
  "Make the next sibling of the current element's child the current element.
This is done with respect to the current topology."
  (interactive)
  (run-hook-with-args 'mtorus-navigate-pre-hook mtorus-current-element)
  (let* ((curelt (cond ((mtorus-type-ring-p mtorus-current-element)
                        (mtorus-determine-child-element
                         mtorus-current-element))
                       (t mtorus-current-element)))
         (newelt (mtorus-determine-next-element
                  curelt)))
    (mtorus-element-set-current newelt)
    (run-hook-with-args 'mtorus-navigate-post-hook mtorus-current-element)

    (mtorus-display-message
     message
     :element mtorus-current-element
     :otherelement curelt
     :message '("new current: %s (was: %s)" element otherelement))
    newelt))


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

(defun mtorus-select-current-element ()
  "Select the current element (whatever this means).

Selection can be done by:
- Defining :inherit-selection in the type definition
- ... <add more>"
  (interactive)
  (mtorus-select-element mtorus-current-element))


(defun mtorus-change-order (order)
  "Globally change the order of mtorus elements when displaying."
  (interactive
   (let* ((table (mapcar
                  #'(lambda (order)
                      (cons (format "mtorus-order-%s" order)
                            (mtorus-utils-symbol-conc
                             'mtorus-order order)))
                  mtorus-orders))
          (curorder mtorus-default-order)
          (order (or
                  (cdr
                   (assoc
                    (completing-read
                     (format "Order (%s): "
                             curorder)
                     table nil t)
                    table))
                  curorder)))
     (list order)))
  (setq mtorus-default-order order)
  (mtorus-display-message
   message
   :element mtorus-current-element
   :message '("still current: %s" element)))




(defun mtorus-install-mtorus16-names ()
  "Installs some defaliases due to mtorus-1.6 compatibility."
  (interactive)
  (defalias 'mtorus-next-ring 'mtorus-uncle-element)
  (defalias 'mtorus-prev-ring 'mtorus-aunt-element)
  (defalias 'mtorus-next-marker 'mtorus-nephew-element)
  (defalias 'mtorus-prev-marker 'mtorus-niece-element))






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

;; Local variables:
;; adaptive-fill-mode: t
;; adaptive-fill-regexp: "[     ]*\\([#;>*]+ +\\)?\\(?:#?[ABCD0-9] +[_X] +?\\)?"
;; End:
