;;; mtorus-state.el --- state functions of the mtorus
;; $Id: mtorus-state.el,v 1.8 2004/11/24 16:32:55 hroptatyr Exp $
;; Copyright (C) 2004 by Stefan Kamphausen
;;           (C) 2004 by Sebastian Freundt
;; Author: Stefan Kamphausen <mail@skamphausen.de>
;;         Sebastian Freundt <hroptatyr@users.berlios.de>
;; Created: 2004/07/31
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
;; - dump the complete torus to a file
;; - read a complete torus from a file

;; *** ToDo:
;; - Topology is not always restored correctly

;;; History


;;; Code:

(require 'mtorus-utils)
(require 'mtorus-convert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup mtorus-state nil
  "The state of a torus."
  :tag "MTorus State"
  :prefix "mtorus-state-"
  :group 'mtorus)


(defconst mtorus-state-version "Version: 0.1 $Revision: 1.8 $"
  "Version of mtorus-state backend.
CONSIDER THIS ALPHA AT THE MOMENT!")


(defcustom mtorus-state-save-on-exit nil
  "*Whether to save the current torus to the current dir on exit.
This is an ALPHA feature."
  :type 'boolean
  :group 'mtorus-state)
(defcustom mtorus-save-on-exit nil
  "*Whether to save the current torus to the current dir on exit.
This variable is obsolete. Use `mtorus-state-save-on-exit'."
  :type 'boolean
  :group 'mtorus-state)

(defcustom mtorus-state-save-deads t
  "*Whether to save elements that actually would need to be resurrected
in order to have correct dump data."
  :type 'boolean
  :group 'mtorus-state)

(defcustom mtorus-state-ask-for-state-file nil
  "*Whether to always ask to specify a state file."
  :type 'boolean
  :group 'mtorus-state)

(defcustom mtorus-state-file "~/.mtorus.dump"
  "*The filename where the state of the mtorus-universe will be dumped.
This can also be a form which is evaluated and should result to a
file name string."
  :type 'file
  :group 'mtorus-state)


;;; now real code

(define-mtorus-type dump
  :predicate
  (lambda (element)
    (eq (mtorus-element-get-property 'type element) 'dump))
  :inherit-selection
  (lambda (element)
    (error "MTorus type dump is not selectable"))
  :alive-p
  (lambda (element)
    t)
  )
  

(eval
 `(define-mtorus-convert dump
   ,@(let (spec)
       (mapc #'(lambda (keyw)
                 (setq spec
                       (append
                        spec
                        (list keyw
                              `(mtorus-utils-plist-get
                                prop::value ',keyw)))))
             (mtorus-type-convert-list))
       spec)
   :type 'dump
   :value
   ,(cons 'list
          (let (spec)
            (mapc #'(lambda (keyw)
                      (setq spec
                            (append
                             spec
                             `(',keyw
                               ,(mtorus-utils-namespace-conc 'conv keyw)))))
                  (mtorus-type-convert-list))
            spec))))


;;(mtorus-type-convert-to 'dump (gethash mtorus-current-element mtorus-elements))
;;(mtorus-type-convert-from (gethash mtorus-current-element mtorus-elements))



(defun mtorus-state-object-dumpable-p (object)
  "Returns OBJECT if it is dumpable, nil otherwise."
  (cond ((stringp object))
        ((string= (format "%s" object)
                  (format "%S" object)))))
(defun mtorus-state-keyvalpair-dumpable-p (keyw object)
  "Returns the pair if OBJECT is dumpable, nil otherwise."
  (and (mtorus-state-object-dumpable-p object)
       (list keyw object)))
(defun mtorus-state-filter-dumpable (&rest spec)
  ""
  (let (result)
    (loop for (key val) on spec by #'cddr
      do
      (and (mtorus-state-object-dumpable-p val)
           (setq result
                 (append result (list key val)))))
    result))

(defun mtorus-state-save (&optional state-file)
  "Saves current mtorus to STATE-FILE.
If omitted the value of `mtorus-state-file' is used."
  (interactive
   (and (or mtorus-state-ask-for-state-file
            current-prefix-arg)
        (list
         (setq mtorus-state-file
               (read-file-name
                "MTorus state file: " (eval mtorus-state-file))))))

  ;; first we dump all elements
  (let (;;(tempbuf (get-buffer-create "*MTorus Dump*"))
        (state-file (or state-file
                        (eval mtorus-state-file))))
    ;;(erase-buffer tempbuf)
    (with-temp-buffer

      ;; the elements themselves
      (maphash
      #'(lambda (elem el-prop-ht)
           (and (mtorus-element-alive-p elem)
                (insert
                 (format
                  "(%s)\n"
                  (mapconcat
                   #'(lambda (obj)
                       (format "%S" obj))
                   (eval 
                    `(mtorus-state-filter-dumpable
                      :type ',(mtorus-element-get-type elem)
                      :symbol ',elem
                      ,@(mtorus-element-property-get
                         'value
                         (mtorus-type-convert-to 'dump el-prop-ht))))
                   " ")))))
       (eval mtorus-elements-hash-table))

      ;; now the topology
      (mapc #'(lambda (nh)
                (maphash #'(lambda (key val)
                             (maphash
                              #'(lambda (el rel)
                                  (and (mtorus-element-alive-p el)
                                       (mtorus-element-alive-p key)
                                       (insert (format "[%s %s %s]\n" rel key el))))
                              val))
                         (eval (mtorus-utils-symbol-conc
                                'mtorus-topology-standard nh))))
            mtorus-topology-standard-neighborhoods)
      (write-region (point-min) (point-max) state-file))
    (message "MTorus state dumped to %s" state-file)
    state-file))


(defun mtorus-state-load (&optional state-file)
  "Restores a dumped mtorus from STATE-FILE.
If omitted the value of `mtorus-state-file' is used."
  (interactive
   (and (or mtorus-state-ask-for-state-file
            current-prefix-arg)
        (list
         (setq mtorus-state-file
               (read-file-name
                "MTorus state file: " (eval mtorus-state-file) nil t)))))

  (let ((state-file (or state-file
                        (eval mtorus-state-file)))
        records)
    (with-temp-buffer 
      (erase-buffer)
      (insert-file-contents state-file)
      (goto-char (point-min)) (insert "(\n")
      (goto-char (point-max)) (insert "\n)")
      (goto-char (point-min))
      (setq records (read (current-buffer))))
    (mapc
     #'(lambda (state-vec)
         (cond
          ((listp state-vec)
           (let ((symbol
                  (mtorus-utils-plist-get state-vec ':symbol))
                 (type
                  (mtorus-utils-plist-get state-vec ':type)))
             (cond
              ((not (eq symbol 'mtorus-universe))
               (mtorus-element-register
                symbol 
                (mtorus-type-convert-to
                 type
                 (make-mtorus-element
                  :type 'dump
                  :symbol symbol
                  :name (mtorus-utils-plist-get state-vec ':element-name)
                  :value (cddddr state-vec)
                  :resurrection-data (cddddr state-vec)
                  :description "Restored from dump.")))))))
          ((vectorp state-vec)
           (mtorus-topology-standard-define-relation
            (aref state-vec 0) (aref state-vec 1) (aref state-vec 2)))))
     records))
  (message "Dumped torus state loaded.")
  state-file)


(run-hooks 'mtorus-state-after-load-hook)

(provide 'mtorus-state)

;;; mtorus-state.el ends here
