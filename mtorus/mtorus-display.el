;;; mtorus-display.el --- display functions of the mtorus
;; $Id: mtorus-display.el,v 1.9 2004/09/04 02:37:32 hroptatyr Exp $
;; Copyright (C) 2004 by Stefan Kamphausen
;;           (C) 2004 by Sebastian Freundt
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;;         Sebastian Freundt <hroptatyr@users.berlios.de>
;; Created: 2004/08/01
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
;; This file holds useful code to
;; - display events/output from the mtorus

;; *** ToDo:


;;; History


;;; Code:

(require 'mtorus-utils)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-display nil
  "The display functions of MTorus."
  :tag "MTorus Display"
  :prefix "mtorus-display-"
  :group 'mtorus)


(defconst mtorus-display-version "Version: 0.1 $Revision: 1.9 $"
  "Version of mtorus-display backend.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to display information
;; REVISE ME!
(defcustom mtorus-notify-method 't
  "*Controls how the status is displayed to the user.
If set to 't' mtorus uses a popup window and the echo erea.

If set to 'popup' only the popup window will be used

If set to 'echo'  only the echo area will be used.

Set this to 'nil' to avoid notifying at all."
  :type '(choice (const t) (const nil) (const popup) (const echo))
  :group 'mtorus)

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



;;; REVISE ME!!
(defmacro define-mtorus-display (name &rest display-spec)
  "yet to be documented ..."
  ;; yet to be implemented
  t)
(defalias 'mtorus-define-display 'define-mtorus-display)
(defalias 'mtorus-display-define 'define-mtorus-display)



;;; Predefined displays

(defun mtorus-display-initialize ()
  ""
  (define-mtorus-display
    element-by-name))

(mtorus-display-initialize)



(defcustom mtorus-display-variable-transformation-map
  '((element . #'mtorus-element-get-name)
    (otherelement . #'mtorus-element-get-name)
    (message . #'format))
  "Alist of functions (in the cdr) to be applied to the variable (in the car).
The variable's value is replaced by the function result then."
  :group 'mtorus-display)

(defcustom mtorus-displays
  `((message . (lambda (message)
                 (mtorus-display-siblings-2)
                 ;;(mtorus-notify-popup)
                 (cond ((featurep 'xemacs)
                        ;;(display-message 'no-log message)
                        )
                       (t
                        ;;(message message)
                        ))))
    (notify . ()))
  "Alist of display instances."
  :group 'mtorus-display)


(defmacro mtorus-display-message (display &rest display-specs)
  "Displays a message on some action."
  (let* ((speclist (mtorus-utils-parse-spec display-specs nil t))
         (tempbind '(tempbind-dummy)))
    (mapc
     #'(lambda (var)
         (let* ((varname (car var))
                (varfun
                 (cdr-safe
                  (assoc varname mtorus-display-variable-transformation-map)))
                (varval (eval (cdr var)))
                (varval
                 (cond (varfun
                        (if (listp varval)
                            (eval `(let ,tempbind
                                    (,(eval varfun) ,@varval)))
                          (eval `(let ,tempbind
                                  (,(eval varfun) ',varval)))))
                       (t varval))))
           (setq tempbind
                 (append tempbind
                         (list (list varname varval))))))
     speclist)
    (let ((dispfun (cdr-safe (assoc display mtorus-displays))))
      (and dispfun
           (eval
            `(let ,tempbind
              `(,dispfun ,(eval display))))))))








;; GUI
;; ;; Code and idea highly inspired by swbuff.el by D. Ponce. Thanks!
;; (defvar mtorus-notify-status-freeze nil
;;   "A temporary freezed description of the current torus.
;; Avoids rearrangement of the popup window every time the main data
;; structure `mtorus-torus' is changed.  It's structure is similar to
;; mtorus but allows for better access during the creation of the notify
;; window.")
;; 


;; (defun mtorus-freeze-torus ()
;;   "Create a freezed status snapshot of `mtorus-torus'.
;; This changes the torus by creating the current entry strings for all
;; entries to avoid repeated calls."
;;   (mapcar
;;    #'(lambda (ring)
;;        (let ((cringn (car ring)))
;;          (cons
;;           cringn
;;           (list
;;            (if (mtorus-special-ringp cringn)
;;                (cond
;;                 ((string-equal cringn mtorus-buffer-list-name)
;;                  (mapcar #'(lambda (buf)
;;                              (with-current-buffer buf
;;                                (mtorus-entry-to-string
;;                                 ;; FIXME: good enough for GC?
;;                                 (point-marker))))
;;                          (mtorus-buffer-list))))
;;              (mapcar #'(lambda (marker)
;;                          (mtorus-entry-to-string marker))
;;                      (second ring)))))))
;;    mtorus-torus))
;; 
;; 

(defface mtorus-highlight-face
  '((((class color) (background light))
     (:background "khaki"))
    (((class color) (background dark))
     (:background "sea green"))
    (((class grayscale monochrome)
      (background light))
     (:background "black"))
    (((class grayscale monochrome)
      (background dark))
     (:background "white")))
  "Face for the highlighting of the line jumped to."
  :group 'mtorus-display)
(setq frame-background-mode 'light)

(defface mtorus-notify-highlight-face
  '((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "green"))
    (((class grayscale monochrome)
      (background light))
     (:foreground "black"))
    (((class grayscale monochrome)
      (background dark))
     (:background "white")))
  "Face for the highlighting the current entry in the notify window."
  :group 'mtorus-display)



;;; Various display functions
  
(defun mtorus-notify-popup ()
  "Open a navigation window at the bottom of the current window.
The upper line shows all rings and the lower line all the current
entries in that ring.  The window will vanish on the next action taken
or if `mtorus-notify-popup-clear-timeout' seconds go by."
  (let ((currentry-s mtorus-current-element)
        (window-min-height 4))
    (with-current-buffer
        (get-buffer-create mtorus-notify-popup-buffer-name)
      (let ((w (or (get-buffer-window mtorus-notify-popup-buffer-name)
                   (split-window-vertically -4))))
        (set-window-buffer w (current-buffer))
        ;; insert the torus description
        (erase-buffer)
        (mtorus-insert-status currentry-s)
        (add-hook 'pre-command-hook
                  'mtorus-notify-maybe-cleanup)
        (if (timerp mtorus-notify-popup-timer)
            (cancel-timer mtorus-notify-popup-timer))
        (setq mtorus-notify-popup-timer
              (run-with-timer
               mtorus-notify-popup-clear-timeout nil
               #'mtorus-notify-maybe-cleanup))))))
(defalias 'mtorus-notify 'mtorus-notify-popup)


(defun mtorus-insert-status (curelt)
  "Insert a description of the torus in the current buffer.
This is used inside the notify popup window and displays all ring
names plus the contents of the current ring."
  (let* ((varfun (eval
                  (cdr-safe (assoc 'element mtorus-display-variable-transformation-map))))
         (rings (remove-if-not 'mtorus-type-ring-p (mtorus-elements)))
         (rings-sort (mtorus-order-by-age rings))
         (cringn (or (and (mtorus-type-ring-p curelt)
                          curelt)
                     mtorus-current-ring))
         entry start)
    (while rings-sort
      (setq entry (car rings-sort)
            rings-sort (cdr rings-sort)
            start (point))
      (insert (funcall varfun entry))
      (when (equal entry cringn)
        (set-text-properties
         start (point) '(face mtorus-notify-highlight-face))
        (save-excursion
          (insert "\n")
          (mtorus-insert-ring-description curelt
                                          (mtorus-topology-standard-children cringn))))
      (insert " ")
      )))
;;(mtorus-insert-status mtorus-current-element)

(defun mtorus-insert-ring-description (curr entries)
  "Insert descriptions of ENTRIES in the current buffer.
This is used inside mtorus-insert-status to insert the description of
all the current entries."
  ;; (message (format "%s" entries))
  (let* ((varfun (eval
                  (cdr-safe (assoc 'element mtorus-display-variable-transformation-map))))
         (entries-sort (mtorus-order-by-age entries))
         entry start)
    (while entries-sort
      (setq entry (car entries-sort)
            entries-sort (cdr entries-sort)
            start (point))
      (insert (funcall varfun entry))
      (when (equal entry curr)
        (set-text-properties
         start (point) '(face mtorus-notify-highlight-face)))
      (insert "  "))))


;; Used to prevent discarding the notify window on some mouse event.
(defalias 'mtorus-ignore 'ignore)

(defun mtorus-notify-maybe-cleanup ()
  "Function to sit on `pre-command-hook' and track successive calls
to the cycling commands."
  (if (memq this-command '(mtorus-next-element
                           mtorus-prev-element
                           mtorus-parent-element
                           mtorus-child-element
                           mtorus-aunt-element
                           mtorus-uncle-element
                           mtorus-niece-element
                           mtorus-nephew-element
                           mtorus-next-ring
                           mtorus-prev-ring
                           mtorus-next-marker
                           mtorus-prev-marker
                           mtorus-ignore))
      nil
    (mtorus-notify-cleanup)
    (if (timerp mtorus-notify-popup-timer)
        (cancel-timer mtorus-notify-popup-timer))
    (setq mtorus-notify-status-freeze nil)
    (setq mtorus-notify-popup-timer nil)
    (remove-hook 'pre-command-hook
                 'mtorus-notify-maybe-cleanup)))

(defun mtorus-notify-cleanup ()
  "Discard the notify window."
  (let ((w (get-buffer-window mtorus-notify-popup-buffer-name))
        (b (get-buffer mtorus-notify-popup-buffer-name)))
    (and w (delete-window w))
    (and b (kill-buffer b))))


;; FIXME: cleanly merge this with the notify code?
;; (defun mtorus-highlight-line ()
;;   "Show the line you jumped to by highlighting it."
;;   (setq mtorus-highlight-extent
;;         (mtorus-make-extent (mtorus-point-at-bol)
;;                             (mtorus-point-at-eol)))
;;   (mtorus-set-extent-face mtorus-highlight-extent
;;                           'mtorus-highlight-face)
;;   (add-hook 'pre-command-hook
;;             'mtorus-unhighlight-line))
;; 
;; (defun mtorus-unhighlight-line ()
;;   "Remove highlighting of the current line if any."
;;   (if mtorus-highlight-extent
;;       (progn
;;         (mtorus-delete-extent mtorus-highlight-extent)
;;         (setq mtorus-highlight-extent nil)
;;         (remove-hook 'pre-command-hook
;;                      'mtorus-unhighlight-current-line))))


(defun mtorus-display-mtorus ()
  (interactive)
  (require 'tree-widget)
  (with-current-buffer (get-buffer-create "*MTorus*")
    (erase-buffer)
    (eval 
     `(widget-create
       'tree-widget
       :open t
       :format "%v"
       :tag "MTorus Universe"
       ,@(mapcar #'(lambda (wid) `',wid) (mtorus-tree-widget-structure 'mtorus-universe))))
    (use-local-map widget-keymap)
    (display-buffer (current-buffer))))

(defun mtorus-tree-widget-structure (elem &optional neighborhood)
  (let* ((neighborhood (or neighborhood
                           'children))
         (topo (mtorus-topology-neighborhood 'standard neighborhood elem))
         (graph
          (mapcar
           #'(lambda (elem)
               `(tree-widget
                 :open t
                 ,@(and (eq elem mtorus-current-element) '(:format "%[%v%]"))
                 ,@(and (eq elem mtorus-current-element) '(:button-face mtorus-highlight-face))
                 :tag ,(format "%s" (mtorus-element-get-name elem))

                 ,@(let ((more (mtorus-topology-standard-children elem)))
                     (and more
                          (not (eq elem 'mtorus-universe))
                          (mtorus-tree-widget-structure elem)))))
           topo)))
    graph))

(defun mtorus-display-siblings ()
  "Displays current element's siblings in the modeline."
  (interactive)
  (let* ((varfun (eval
                  (cdr-safe (assoc 'element mtorus-display-variable-transformation-map))))
         (curelt mtorus-current-element)
         (siblings (mtorus-topology-standard-siblings mtorus-current-element))
         (siblsort (mtorus-order-by-age siblings))
         (msgstr (with-temp-buffer
                   (mapc #'(lambda (elem)
                             (let ((felem (funcall varfun elem))
                                   (pbeg (point)))
                               (insert (format "%s" felem))
                               (cond ((equal elem curelt)
                                      (save-excursion
                                        (set-text-properties
                                         pbeg
                                         (point)
                                         '(face mtorus-highlight-face)))))
                               (insert "  ")))
                         siblsort)
                     (buffer-string))))
    (cond ((fboundp 'display-message)
           (display-message 'no-log msgstr))
          (t (let (message-log-max)
               (message msgstr))))))



(defcustom mtorus-display-siblings-separator ","
  "*Separator between elements."
  :group 'mtorus-display
  :type 'string)

(defcustom mtorus-display-siblings-max-neighbourhood 4
  "*Defines how many elements left and right of the current one are
displayed when using mtorus-display-siblings-2 display function."
  :group 'mtorus-display
  :type 'integer)

(defcustom mtorus-display-siblings-hide-too-distant nil
  "*Determines what happens with siblings exceeding the
`mtorus-display-siblings-max-neighborhood'.

Possible values are:
nil -- don't use hiding at all
hide -- simply drop too distant siblings
shorten -- shorten the displayed element string"
  :group 'mtorus-display)


(defun mtorus-display-siblings-2 ()
  "Displays current element's siblings in the modeline."
  (interactive)
  (let* ((varfun (eval
                  (cdr-safe (assoc 'element mtorus-display-variable-transformation-map))))
         (curelt mtorus-current-element)
         (parents  (mtorus-topology-standard-parents  mtorus-current-element))
         (siblings (mtorus-topology-standard-siblings mtorus-current-element))
         ;;(children (mtorus-topology-standard-children mtorus-current-element))
         (order-fun mtorus-default-order)
         (siblsort (funcall order-fun siblings))
         ;;(chilsort (mtorus-order-by-age children))

         ;;; REVISE ME! ... this is hardcoded thus bad :)
         (curelt-in-siblsort-pos (position curelt siblsort))
         (too-dist-fun
          (cond ((eq mtorus-display-siblings-hide-too-distant 'shorten)
                 (lambda (elem)
                   (substring (format "%s" elem) 0 3)))
                ((eq mtorus-display-siblings-hide-too-distant 'hide)
                 (lambda (elem)
                   ""))
                (t #'identity)))

         (msgstr (with-temp-buffer
                   (insert (format "%s: " (funcall varfun (car parents))))
                   (mapc #'(lambda (elem)
                             (let* ((elem-too-dist-p
                                     (< mtorus-display-siblings-max-neighbourhood
                                        (abs (- (position elem siblsort)
                                                curelt-in-siblsort-pos))))
                                    (felem (funcall varfun elem))
                                    (felem (or (and elem-too-dist-p
                                                    (funcall too-dist-fun felem))
                                               felem))
                                    (pbeg (point)))
                               (insert (format "%s" felem))
                               (cond ((equal elem curelt)
                                      (save-excursion
                                        (set-text-properties
                                         pbeg
                                         (point)
                                         '(face mtorus-highlight-face)))))
                               (insert mtorus-display-siblings-separator)))
                         siblsort)
                     (buffer-string))))
    (cond ((fboundp 'display-message)
           (display-message 'no-log msgstr))
          (t (let (message-log-max)
               (message msgstr))))))



(run-hooks 'mtorus-display-after-load-hook)


(provide 'mtorus-display)

;;; mtorus-display.el ends here
