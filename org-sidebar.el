;;; org-sidebar.el --- Helpful sidebar for Org buffers  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-sidebar
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.1") (s "1.10.0") (dash "2.13") (org "9.0") (org-ql) (org-agenda-ng))
;; Keywords: hypermedia, outlines, Org, agenda

;;; Commentary:

;; This package presents a helpful sidebar view for Org buffers.  At the top is a chronological list
;; of scheduled and deadlined tasks in the current buffer (similar to the Org agenda ,but without
;; all its features), and below that is a list of all other non-done to-do items.  If the buffer is
;; narrowed, the sidebar only shows items in the narrowed portion; this allows seeing an overview of
;; tasks in a subtree.

;; NOTE: This package is in an early stage of development.

;;;; Installation

;; Install the required packages (see the "Package-Requires" line in the headers at the top).
;; org-ql and org-agenda-ng may be found at <http://github.com/alphapapa/org-agenda-ng>.  Then put
;; this file in your `load-path'.

;;;; Usage

;; Eval (require 'org-sidebar).  Then, in an Org buffer, run the command `org-sidebar'.

;; Customization options are in the `org-sidebar' group.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'org)

(require 'dash)
(require 'dash-functional)
(require 's)

(require 'org-ql)
(require 'org-agenda-ng)

;;;; Variables

(defvar org-sidebar-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "RET" org-sidebar--jump
                    "<mouse-1>" org-sidebar--jump
                    "g" org-sidebar--update
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map)
  "Keymap for `org-sidebar' buffers.")

(defvar-local org-sidebar-source-buffer nil
  "The source Org buffer for entries in this sidebar buffer.")

;;;; Customization

(defgroup org-sidebar nil
  "Options for `org-sidebar'."
  :group 'org)

(defcustom org-sidebar-date-format "%e %B %Y"
  "Format string for date headers.
See `format-time-string'."
  :type 'string)

(defcustom org-sidebar-format-fn (lambda (element)
                                   (->> element
                                        org-agenda-ng--add-markers
                                        org-agenda-ng--format-element))
  "Function used to format elements.
Takes a single argument: the Org element being formatted.  This
function should return a string which has the text property
`org-marker' set to a marker at the entry in the source Org
buffer."
  :type 'function)

(defcustom org-sidebar-side 'right
  "Which side to show the sidebar on."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))

;;;; Commands

;;;###autoload
(cl-defun org-sidebar (&key (fns '(org-sidebar--agenda-items org-sidebar--to-do-items))
                            (group t))
  "This package presents a helpful sidebar view for Org buffers.
At the top is a chronological list of scheduled and deadlined
tasks in the current buffer, and below that is a list of all
other non-done to-do items.  If the buffer is narrowed, the
sidebar only shows items in the narrowed portion; this allows
seeing an overview of tasks in a subtree.

FNS is a list of functions that return Org headline elements (as
returned by `org-element-headline-parser').  Such functions
should take a keyword argument `group' which causes them to
return elements grouped with `-group-by' (or they may omit
grouping, in which case the GROUP argument to this function
must not be used).

GROUP specifies to call each function in FNS with its group
keyword argument non-nil."
  (interactive)
  (let ((source-buffer (current-buffer))
        (slot 0)
        (inhibit-read-only t))
    (--each fns
      (when-let ((items (with-current-buffer source-buffer
                          (if group
                              (funcall it :group t)
                            (funcall it)))))
        (with-current-buffer (get-buffer-create (format " *org-sidebar: %s*" slot))
          (org-sidebar--prepare-buffer source-buffer (buffer-name source-buffer))
          (--> items
               (org-sidebar--format-grouped-items it)
               (insert it))
          (goto-char (point-min))
          (display-buffer-in-side-window (current-buffer) (a-list 'side org-sidebar-side
                                                                  'slot slot))
          (cl-incf slot))))))

;;;; Functions

(defun org-sidebar--format-grouped-items (groups)
  "Return items in GROUPS formatted as a string.
GROUPS should be grouped like with `-group-by'."
  (with-temp-buffer
    (--each groups
      (-let (((header . items) it))
        (insert "\n" header "\n\n")
        (--each items
          (insert it "\n"))))
    (buffer-string)))

(cl-defun org-sidebar--agenda-items (&key group)
  "Return list of agenda items for current buffer.
When GROUP is non-nil, group items by date.  Items are formatted
with `org-sidebar-format-fn'."
  (cl-flet ((date-header (item)
                         (propertize (org-timestamp-format (or (get-text-property 0 'scheduled item)
                                                               (get-text-property 0 'deadline item))
                                                           org-sidebar-date-format)
                                     'face '(:inherit variable-pitch :weight bold))))
    (--> (org-ql (current-buffer)
           (and (or (scheduled)
                    (deadline))
                (not (done)))
           :sort (date priority todo)
           :narrow t)
         (-map org-sidebar-format-fn it)
         (if group
             (-group-by #'date-header it)
           it))))

(cl-defun org-sidebar--to-do-items (&key group)
  "Return list of to-do items for current buffer.
When GROUP is non-nil, group items by to-do keyword. Items are
formatted with `org-sidebar-format-fn'."
  (--> (org-ql (current-buffer)
         (and (todo)
              (not (or (done)
                       (scheduled)
                       (deadline))))
         :sort (todo priority)
         :narrow t)
       (-map org-sidebar-format-fn it)
       (if group
           (--group-by (get-text-property 0 'todo-state it) it)
         it)))

(defun org-sidebar--prepare-buffer (source-buffer name)
  "Prepare current buffer as a sidebar buffer.
Header line is set to NAME string, and
`org-sidebar-source-buffer' is set to SOURCE-BUFFER."
  (let ((inhibit-read-only t))
    (setq org-sidebar-source-buffer source-buffer)
    (read-only-mode 1)
    (setq header-line-format (propertize name 'face '(:inherit org-agenda-date-today))
          mode-line-format nil)
    (use-local-map org-sidebar-map)
    (erase-buffer)
    (goto-char (point-min))
    (toggle-truncate-lines 1)))

(defun org-sidebar--jump ()
  "Jump to entry at sidebar buffer's point in source buffer."
  (interactive)
  (if-let* ((marker (get-text-property (point) 'org-marker))
            (buffer (marker-buffer marker)))
      (progn (--if-let (get-buffer-window buffer)
                 (select-window it)
               (pop-to-buffer buffer))
             (goto-char marker)
             (org-reveal))
    (user-error "Item's buffer no longer exists")))

(defun org-sidebar--update ()
  "Update `org-sidebar' buffer."
  (interactive)
  (unless (buffer-live-p org-sidebar-source-buffer)
    (user-error "Sidebar's source buffer no longer exists"))
  (with-current-buffer org-sidebar-source-buffer
    (org-sidebar)))

;;;; Footer

(provide 'org-sidebar)

;;; org-sidebar.el ends here
