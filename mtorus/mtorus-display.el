;;; mtorus-display.el --- display functions of the mtorus
;; $Id: mtorus-display.el,v 1.2 2004/08/02 22:20:56 hroptatyr Exp $
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


(defconst mtorus-display-version "Version: 0.1 $Revision: 1.2 $"
  "Version of mtorus-display backend.")


;;; REVISE ME!!
(defmacro define-mtorus-display (name &rest display-spec)
  "yet to be documented ..."
  )
(defalias 'mtorus-define-display 'define-mtorus-display)
(defalias 'mtorus-display-define 'define-mtorus-display)



;;; Predefined displays

(defun mtorus-display-initialize ()
  ""
  (define-mtorus-display
    element-by-name
    )
  )

(mtorus-display-initialize)

(defcustom mtorus-display-variable-transformation-map
  '((element . #'mtorus-element-get-name)
    (otherelement . #'mtorus-element-get-name)
    (message . #'format))
  ""
  :group 'mtorus-display)

(defcustom mtorus-displays
  `((message . ,(cond ((featurep 'xemacs)
                       #'(lambda (message)
                           (display-message 'no-log message)))
                      (t
                       #'message)))
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
;; (defun mtorus-notify ()
;;   "Notify the user of the current status of the torus.
;; This might just use the echo area or popup it's own window according
;; to the settings of `mtorus-notify-method'."
;;   (interactive)
;;   (when (not mtorus-notify-status-freeze)
;;     (setq mtorus-notify-status-freeze
;;           (mtorus-freeze-torus)))
;;   (cond ;; FIXME: other methods
;;    ((eq mtorus-notify-method t)
;;     (mtorus-notify-popup))
;;    ((eq mtorus-notify-method 'popup)
;;     (mtorus-notify-popup))
;;    ((eq mtorus-notify-method 'echo))
;;    (t))
;;   (mtorus-highlight-line))
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
;; (defun mtorus-notify-popup ()
;;   "Open a navigation window at the bottom of the current window.
;; The upper line shows all rings and the lower line all the current
;; entries in that ring.  The window will vanish on the next action taken
;; or if `mtorus-notify-popup-clear-timeout' seconds go by."
;;   (let ((currentry-s (mtorus-current-entry-string))
;;         (window-min-height 4))
;;     (with-current-buffer
;;         (get-buffer-create mtorus-notify-popup-buffer-name)
;;       (let ((w (or (get-buffer-window mtorus-notify-popup-buffer-name)
;;                    (split-window-vertically -4))))
;;         (set-window-buffer w (current-buffer))
;;         ;; insert the torus description
;;         (erase-buffer)
;;         (mtorus-insert-status currentry-s)
;;         (add-hook 'pre-command-hook
;;                   'mtorus-notify-maybe-cleanup)
;;         (if (timerp mtorus-notify-popup-timer)
;;             (cancel-timer mtorus-notify-popup-timer))
;;         (setq mtorus-notify-popup-timer
;;               (run-with-timer
;;                mtorus-notify-popup-clear-timeout nil
;;                #'mtorus-notify-maybe-cleanup))))))
;; 
;; (defface mtorus-highlight-face
;;   '((((class color) (background light))
;;      (:background "khaki"))
;;     (((class color) (background dark))
;;      (:background "sea green"))
;;     (((class grayscale monochrome)
;;       (background light))
;;      (:background "black"))
;;     (((class grayscale monochrome)
;;       (background dark))
;;      (:background "white")))
;;   "Face for the highlighting of the line jumped to."
;;   :group 'mtorus)
;; (setq frame-background-mode 'light)
;; 
;; (defface mtorus-notify-highlight-face
;;   '((((class color) (background light))
;;      (:foreground "red"))
;;     (((class color) (background dark))
;;      (:foreground "green"))
;;     (((class grayscale monochrome)
;;       (background light))
;;      (:foreground "black"))
;;     (((class grayscale monochrome)
;;       (background dark))
;;      (:background "white")))
;;   "Face for the highlighting the current entry in the notify window."
;;   :group 'mtorus)
;;   
;; 
;; (defun mtorus-insert-status (currentry-s)
;;   "Insert a description of the torus in the current buffer.
;; This is used inside the notify popup window and displays all ring
;; names plus the contents of the current ring."
;;   (let ((rings (mapcar 'car mtorus-notify-status-freeze))
;;         (cringn (mtorus-current-ring-name))
;;         entry start)
;;     (while rings
;;       (setq entry (car rings)
;;             rings (cdr rings)
;;             start (point))
;;       (insert entry)
;;       (when (string-equal entry cringn)
;;         (set-text-properties
;;          start (point) '(face mtorus-notify-highlight-face))
;;         (save-excursion
;;           (insert "\n")
;;           (mtorus-insert-ring-description currentry-s
;;                                           (cadr (assoc entry mtorus-notify-status-freeze)))))
;;       (insert " ")
;;       )))
;; 
;; (defun mtorus-insert-ring-description (curr entries)
;;   "Insert descriptions of ENTRIES in the current buffer.
;; This is used inside mtorus-insert-status to insert the description of
;; all the current entries."
;;   ;; (message (format "%s" entries))
;;   (let* (entry start)
;;     (while entries
;;       (setq entry (car entries)
;;             entries (cdr entries)
;;             start (point))
;;       (insert entry)
;;       (when (string-equal entry curr)
;;         (set-text-properties
;;          start (point) '(face mtorus-notify-highlight-face)))
;;       (insert "  "))))
;;    
;;    
;; ;; Used to prevent discarding the notify window on some mouse event.
;; (defalias 'mtorus-ignore 'ignore)
;; 
;; (defun mtorus-notify-maybe-cleanup ()
;;   "Function to sit on `pre-command-hook' and track successive calls
;; to the cycling commands."
;;   (if (memq this-command '(mtorus-next-ring
;;                            mtorus-prev-ring
;;                            mtorus-next-marker
;;                            mtorus-prev-marker
;;                            mtorus-ignore))
;;       nil
;;     (mtorus-notify-cleanup)
;;     (if (timerp mtorus-notify-popup-timer)
;;         (cancel-timer mtorus-notify-popup-timer))
;;     (setq mtorus-notify-status-freeze nil)
;;     (setq mtorus-notify-popup-timer nil)
;;     (remove-hook 'pre-command-hook
;;                  'mtorus-notify-maybe-cleanup)))
;; 
;; (defun mtorus-notify-cleanup ()
;;   "Discard the notify window."
;;   (let ((w (get-buffer-window mtorus-notify-popup-buffer-name))
;;         (b (get-buffer mtorus-notify-popup-buffer-name)))
;;     (and w (delete-window w))
;;     (and b (kill-buffer b))))
;; 
;; ;; FIXME: cleanly merge this with the notify code?
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





(run-hooks 'mtorus-display-after-load-hook)


(provide 'mtorus-display)

;;; mtorus-element.el ends here
