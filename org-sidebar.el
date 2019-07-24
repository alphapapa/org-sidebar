;;; org-sidebar.el --- Helpful sidebar for Org buffers  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-sidebar
;; Version: 0.2-pre
;; Package-Requires: ((emacs "25.1") (s "1.10.0") (dash "2.13") (dash-functional "1.2.0") (org "9.0") (org-ql) (org-super-agenda "1.0"))
;; Keywords: hypermedia, outlines, Org, agenda

;;; Commentary:

;; This package presents a helpful sidebar view for Org buffers.  At the top is a chronological list
;; of scheduled and deadlined tasks in the current buffer (similar to the Org agenda ,but without
;; all its features), and below that is a list of all other non-done to-do items.  If the buffer is
;; narrowed, the sidebar only shows items in the narrowed portion; this allows seeing an overview of
;; tasks in a subtree.

;; NOTE: Please note: this package is in an early stage of development, so incompatible changes may
;; be made in the future.  However, it's stable and usable now.  Feedback is appreciated.

;;;; Installation

;; Install the required packages (see the "Package-Requires" line in the headers at the top).
;; org-ql and org-ql-agenda may be found at <http://github.com/alphapapa/org-ql>.  Then put
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
(require 'org-element)

(require 'dash)
(require 'dash-functional)
(require 's)

(require 'org-super-agenda)

(require 'org-ql)
(require 'org-ql-agenda)

;;;; Variables

(defvar org-sidebar-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "RET" org-sidebar-jump
                    "<mouse-1>" org-sidebar-jump
                    "g" org-sidebar-update
                    "q" bury-buffer
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map)
  "Keymap for `org-sidebar' buffers.")

(defvar org-sidebar-updating nil
  "Is non-nil when updating a sidebar.")

;; MAYBE: Use a list of variables, stored as an alist in a single variable.
(defvar-local org-sidebar-source-buffer nil
  "The source Org buffer for entries in this sidebar buffer.")
(defvar-local org-sidebar-group nil
  "The group setting for entries in this sidebar buffer.")
(defvar-local org-sidebar-super-groups nil
  "The super groups for entries in this sidebar buffer.")
(defvar-local org-sidebar-fns nil
  "The functions for entries in this sidebar buffer.")
(defvar-local org-sidebar-header nil
  "The header in this sidebar buffer.")

;;;; Customization

(defgroup org-sidebar nil
  "Options for `org-sidebar'."
  :group 'org)

(defcustom org-sidebar-group-items t
  "Group items by default."
  :type 'boolean)

(defcustom org-sidebar-jump-indirect t
  "Show items with `org-tree-to-indirect-buffer'."
  :type 'boolean)

(defcustom org-sidebar-date-format "%e %B %Y"
  "Format string for date headers.
See `format-time-string'."
  :type 'string)

(defcustom org-sidebar-format-fn #'org-ql-agenda--format-element
  "Function used to format elements.
Takes a single argument: the Org element being formatted.
Generally, `org-ql-agenda--format-element' should be used; if
not, the function used should set appropriate text properties,
imitating the Org Agenda, for commands and features which use the
text properties to act on items."
  :type 'function)

(defcustom org-sidebar-side 'right
  "Which side to show the sidebar on."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))

(defcustom org-sidebar-default-fns '(org-sidebar--upcoming-items org-sidebar--todo-items)
  "FIXME: docstring"
  :type '(choice (const :tag "Upcoming items" org-sidebar--upcoming-items)
                 (const :tag "To-do items" org-sidebar--todo-items)
                 (function :tag "Other function")))

;;;; Structs

(cl-defstruct org-sidebar
  ;; Naming things is hard.  Maybe this will help organize things.

  ;; This struct is intended to be made at runtime.  The ITEMS slot should be
  ;; the result of an `org-ql' query with its MARKERS argument non-nil.  See
  ;; default item functions.  The struct is passed by function `org-sidebar'
  ;; to function `org-sidebar--items-buffer', which returns a buffer
  ;; displaying the struct's items.
  name description items group-fn super-groups)

;;;; Commands

;;;###autoload
(cl-defun org-sidebar (&key buffers fns sidebars group super-groups)
  "Display the Org Sidebar.

Interactively, display the sidebars configured in
`org-sidebar-default-fns'.

BUFFERS may be one or a list of buffers to display in the
sidebar.

FNS may be one or a list of functions, each of which may return a
buffer or a `org-sidebar' struct.

SIDEBARS may be one or a list of `org-sidebar' structs.

When GROUP is non-nil (interactively, with one universal prefix
argument), and when SUPER-GROUPS is nil, call each function with
the `group' keyword argument non-nil.

SUPER-GROUPS may be a list of groups according to
`org-super-agenda-groups', in which case the items in the buffers
will be grouped accordingly (where applicable).  Interactively,
with two universal prefix arguments, the global value of
`org-super-agenda-groups' is used."
  (interactive (list :fns org-sidebar-default-fns
                     :group (equal current-prefix-arg '(4))
                     :super-groups (when (equal current-prefix-arg '(16))
                                     org-super-agenda-groups)))
  (let* ((buffers (cl-etypecase buffers
                    (list buffers)
                    (buffer (list buffers))))
         (fns (cl-etypecase fns
                (list fns)
                (function (list fns))))
         (sidebars (cl-etypecase sidebars
                     (list sidebars)
                     (org-sidebar (list sidebars))))
         (fns-buffers (cl-loop for fn in fns
                               for result = (funcall fn :group group :super-groups super-groups)
                               when result
                               collect (cl-etypecase result
                                         (buffer result)
                                         (org-sidebar (org-sidebar--items-buffer result)))))
         (sidebars-buffers (-map #'org-sidebar--items-buffer sidebars))
         (buffers (append buffers fns-buffers sidebars-buffers)))
    (org-sidebar--display-buffers buffers)))

;;;###autoload
(cl-defun org-sidebar-ql (&key query buffers-files narrow group-property sort)
  "Display a sidebar for `org-ql' QUERY.
Interactively, with prefix, prompt for these variables:

BUFFERS-FILES: A list of buffers and/or files to search.

NARROW: When non-nil, don't widen buffers before searching.

GROUP-PROPERTY: One of the following symbols: `category',
`parent', `priority', `todo'.

SORT: One or a list of `org-ql' sorting functions, like `date' or
`priority'."
  (interactive (progn
                 (unless (or (equal current-prefix-arg '(4))
                             (derived-mode-p 'org-mode))
                   (user-error "Not an Org buffer: %s" (buffer-name)))
                 (list :query (read-minibuffer "Query: ")
                       :buffers-files (if (equal current-prefix-arg '(4))
                                          (--if-let (read-from-minibuffer "Buffers/Files (blank for current buffer): ")
                                              (pcase it
                                                ("" (list (current-buffer)))
                                                ((rx bos "(") (-flatten (eval (read it))))
                                                (_ (s-split (rx (1+ space)) it)))
                                            (list (current-buffer)))
                                        (list (current-buffer)))
                       :narrow (not (eq current-prefix-arg '(4)))
                       :group-property (when (equal current-prefix-arg '(4))
                                         (pcase (completing-read "Group by: "
                                                                 (list "Don't group"
                                                                       "category"
                                                                       "parent"
                                                                       "priority"
                                                                       "todo-keyword"))
                                           ("Don't group" nil)
                                           (property (intern property))))
                       :sort (when (equal current-prefix-arg '(4))
                               (pcase (completing-read "Sort by: "
                                                       (list "Don't sort"
                                                             "date"
                                                             "deadline"
                                                             "priority"
                                                             "scheduled"
                                                             "todo"))
                                 ("Don't sort" nil)
                                 (sort (intern sort)))))))
  (org-sidebar
   :sidebars (make-org-sidebar
              :items (org-ql-query buffers-files
                       query
                       :narrow narrow
                       :sort sort
                       ;; MAYBE: In `org-ql-query' make default action, or support :markers arg.
                       :action (lambda ()
                                 (org-ql--add-markers
                                  (org-element-headline-parser (line-end-position)))))
              :name (prin1-to-string query)
              :super-groups (list (pcase group-property
                                    ('nil nil)
                                    ('category '(:auto-category))
                                    ('parent '(:auto-parent))
                                    ('priority '(:auto-priority))
                                    ('todo-keyword '(:auto-todo)))))
   :group group-property))

(defun org-sidebar-update ()
  "Update current sidebar buffer."
  ;; FIXME: Test and fix, might not work anymore.
  (interactive)
  (let ((org-sidebar-updating t))
    (org-sidebar)))

(defun org-sidebar-jump ()
  "Jump to entry at sidebar buffer's point in source buffer."
  (interactive)
  (if-let* ((marker (get-text-property (point) 'org-marker))
            (buffer (marker-buffer marker)))
      (progn (--if-let (get-buffer-window buffer)
                 (select-window it)
               (pop-to-buffer buffer))
             (goto-char marker)
             (org-reveal)
             (when org-sidebar-jump-indirect
               (org-tree-to-indirect-buffer)))
    (user-error "Item's buffer no longer exists")))

;;;; Functions

(cl-defun org-sidebar--display-buffers (buffers)
  "Display BUFFERS in the sidebar."
  (let ((slot 0))
    (--each buffers
      (display-buffer-in-side-window
       it
       (list (cons 'side org-sidebar-side)
             (cons 'slot slot)
             (cons 'window-parameters (list (cons 'no-delete-other-windows t)))))
      (cl-incf slot))))

(cl-defun org-sidebar--items-buffer (items)
  "Return a buffer containing ITEMS, ready to be displayed.
ITEMS should be an `org-sidebar' struct.
FIXME: Note that group-fn and super-groups can't both be set.  Or figure out a smart way to handle it."
  (pcase-let* (((cl-struct org-sidebar name description items group-fn super-groups) items)
               (buffer-name (propertize name 'help-echo description))
               (string (cond (group-fn (->> items
                                            (-group-by group-fn)
                                            (org-sidebar--format-grouped-items)))
                             (super-groups (let ((org-super-agenda-groups super-groups))
                                             (->> items
                                                  (mapcar org-sidebar-format-fn)
                                                  (org-super-agenda--group-items)
                                                  (s-join "\n"))))
                             (t (->> items
                                     (mapcar org-sidebar-format-fn)
                                     (s-join "\n")))))
               (inhibit-read-only t))
    (with-current-buffer (get-buffer-create buffer-name)
      (org-sidebar--prepare-buffer buffer-name)
      (insert string)
      (goto-char (point-min))
      (current-buffer))))

(defun org-sidebar--format-grouped-items (groups)
  "Return items in GROUPS formatted as a string.
GROUPS should be grouped like with `-group-by'."
  (with-temp-buffer
    (--each groups
      (-let (((header . items) it))
        (insert "\n" (or header "None") "\n\n")
        (--each items
          (insert (funcall org-sidebar-format-fn it) "\n"))))
    (buffer-string)))

(defun org-sidebar--prepare-buffer (name)
  "Prepare current buffer as a sidebar buffer.
Header line is set to NAME string, and
`org-sidebar-source-buffer' is set to SOURCE-BUFFER."
  (let ((inhibit-read-only t))
    (setq header-line-format (propertize name 'face '(:inherit org-agenda-date-today))
          mode-line-format nil)
    (read-only-mode 1)
    (use-local-map org-sidebar-map)
    (erase-buffer)
    (goto-char (point-min))
    (toggle-truncate-lines 1)))

;;;;; Default item functions

(cl-defun org-sidebar--upcoming-items (&rest _ignore)
  "Return upcoming items in current buffer.
The `org-ql' query is:

    (and (or (scheduled)
             (deadline))
         (not (done)))"
  (make-org-sidebar
   :name (concat "Upcoming items in " (buffer-name))
   :description "Not-done items with a deadline or scheduled date in the current buffer"
   ;; NOTE: This commented code produces date headers that are more visually pleasing
   ;; to me, but since moving the functionality into `org-super-agenda', it probably
   ;; doesn't make sense to do this here.  But I'm going to keep the code here for now.
   ;; :group-fn (lambda (item)
   ;;             ;; Return a date header string for grouping.
   ;;             (propertize (org-timestamp-format (or (org-element-property :scheduled item)
   ;;                                                   (org-element-property :deadline item))
   ;;                                               org-sidebar-date-format)
   ;;                         'face '(:inherit variable-pitch :weight bold)))
   :super-groups '((:auto-date))
   :items (org-ql (current-buffer)
            (and (or (scheduled)
                     (deadline))
                 (not (done)))
            :sort date
            :narrow t
            :markers t)))

(defun org-sidebar--todo-items (&rest _ignore)
  "Return incomplete to-do items without scheduled dates or deadlines in current buffer."
  (make-org-sidebar
   :name (concat "To-do items in " (buffer-name))
   :description "Unscheduled to-do keyword items without a deadline in the current buffer"
   :items (org-ql (current-buffer)
            (and (todo)
                 (not (or (done)
                          (scheduled)
                          (deadline))))
            :sort (todo priority)
            :narrow t
            :markers t)
   :super-groups '((:auto-todo))))

;;;; Footer

(provide 'org-sidebar)

;;; org-sidebar.el ends here
