;;; org-sidebar.el --- Helpful sidebar for Org buffers  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-sidebar
;; Version: 0.2-pre
;; Package-Requires: ((emacs "25.1") (s "1.10.0") (dash "2.13") (org "9.0") (org-ql) (org-ql-agenda) (org-super-agenda "1.0"))
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
                    "RET" org-sidebar--jump
                    "<mouse-1>" org-sidebar--jump
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

;;;; Commands

;;;###autoload
(cl-defun org-sidebar (&key (fns '(org-sidebar--agenda-items org-sidebar--to-do-items))
                            (group nil group-passed)
                            super-groups
                            header)
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
grouping, in which case the GROUP argument to this function must
not be used).  Elements returned by each function are formatted
with `org-sidebar-format-fn'.

GROUP specifies to call each function in FNS with its group
keyword argument non-nil.  SUPER-GROUPS may be set instead, which
specifies groups to be passed to `org-super-agenda'.

HEADER specifies a string to use as the header line.  If not
specified, it will be set automatically."
  (interactive)
  (let ((source-buffer (if org-sidebar-updating
                           org-sidebar-source-buffer
                         (current-buffer)))
        (slot 0)
        (fns (or org-sidebar-fns
                 fns))
        (group (cond (group-passed group)
                     (org-sidebar-updating org-sidebar-group)
                     (t org-sidebar-group-items)))
        (super-groups (or org-sidebar-super-groups
                          super-groups))
        (header (or org-sidebar-header
                    header))
        (inhibit-read-only t))
    (--each fns
      (when-let ((items (with-current-buffer source-buffer
                          (if group
                              (funcall it :group group)
                            (funcall it)))))
        (with-current-buffer (get-buffer-create (format " *org-sidebar: %s*" slot))
          (setq org-sidebar-source-buffer source-buffer
                org-sidebar-group group
                org-sidebar-super-groups super-groups
                org-sidebar-fns fns
                org-sidebar-header header)
          (setq-local org-sidebar-format-fn org-sidebar-format-fn)
          (org-sidebar--prepare-buffer (or header (buffer-name source-buffer)))
          (insert (cond (group (org-sidebar--format-grouped-items items))
                        ;; FIXME: Document super-groups in readme
                        (super-groups (let ((org-super-agenda-groups super-groups))
                                        (s-join "\n" (org-super-agenda--group-items
                                                      (mapcar org-sidebar-format-fn items)))))
                        (t (s-join "\n" (mapcar org-sidebar-format-fn items)))))
          (goto-char (point-min))
          (display-buffer-in-side-window (current-buffer)
                                         (list (cons 'side org-sidebar-side)
                                               (cons 'slot slot)
                                               (cons 'window-parameters (list (cons 'no-delete-other-windows t)))))
          (cl-incf slot))))))

;;;###autoload
(defun org-sidebar-ql (query &optional buffers-files narrow group sort)
  "Display a sidebar for `org-ql' QUERY.
Interactively, with prefix, prompt for these variables:

BUFFERS-FILES: A list of buffers and/or files to search.

GROUP: A text-property symbol present in each item (e.g. when
items are formatted by `org-ql-agenda--format-element', it might
be `priority' or `todo-state').

NARROW: Don't widen buffers before searching.

SORT: One or a list of `org-ql' sorting functions, like `date' or `priority'."
  (interactive (progn
                 (unless (or (equal current-prefix-arg '(4))
                             (derived-mode-p 'org-mode))
                   (user-error "Not an Org buffer: %s" (buffer-name)))
                 (list (read-minibuffer "Query: ")
                       (if (equal current-prefix-arg '(4))
                           (--if-let (read-from-minibuffer "Buffers/Files (blank for current buffer): ")
                               (pcase it
                                 ("" (current-buffer))
                                 ((rx bos "(") (-flatten (eval (read it))))
                                 (_ (s-split (rx (1+ space)) it)))
                             (current-buffer))
                         (current-buffer))
                       (not (eq current-prefix-arg '(4)))
                       (when (equal current-prefix-arg '(4))
                         (pcase (completing-read "Group by: "
                                                 (list "Don't group"
                                                       "priority"
                                                       "todo-state"))
                           ("Don't group" nil)
                           (property (intern (concat ":" property)))))
                       (when (equal current-prefix-arg '(4))
                         (pcase (completing-read "Sort by: "
                                                 (list "Don't sort"
                                                       "date"
                                                       "deadline"
                                                       "priority"
                                                       "scheduled"
                                                       "todo"))
                           ("Don't sort" nil)
                           (sort (intern sort)))))))
  (org-sidebar :header (prin1-to-string query)
               :fns (list (cl-function
                           (lambda (&key group)
                             (--> (eval `(org-ql ',buffers-files
                                           ,query
                                           :narrow ,narrow
                                           :markers t
                                           :sort ,sort))
                                  (if group
                                      (--> (--group-by (org-element-property group it) it)
                                           (-sort (-on #'string<
                                                       (lambda (item)
                                                         (--> (car item)
                                                              (pcase group
                                                                (:priority (char-to-string it))
                                                                ((pred numberp) (number-to-string it))
                                                                ((pred null) "None")
                                                                ((pred stringp) it)))))
                                                  it))
                                    ;; Not grouping
                                    it)))))
               :group group))

(defun org-sidebar-update ()
  "Update current sidebar buffer."
  (interactive)
  (let ((org-sidebar-updating t))
    (org-sidebar)))

;;;; Functions

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

(cl-defun org-sidebar--agenda-items (&key group)
  "Return list of agenda items for current buffer.
When GROUP is non-nil, group items by date.  Items are formatted
with `org-sidebar-format-fn'."
  (cl-flet ((date-header (item)
                         (propertize (org-timestamp-format (or (org-element-property :scheduled item)
                                                               (org-element-property :deadline item))
                                                           org-sidebar-date-format)
                                     'face '(:inherit variable-pitch :weight bold))))
    (--> (org-ql (current-buffer)
           (and (or (scheduled)
                    (deadline))
                (not (done)))
           :sort (date priority todo)
           :narrow t
           :markers t)
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
         :narrow t
         :markers t)
       (if group
           (--group-by (org-element-property :todo-state it) it)
         it)))

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

(defun org-sidebar--jump ()
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

;;;; Macros

(cl-defmacro org-sidebar-defsidebar (name &key sidebars super-groups header group
                                          (files '(org-agenda-files))
                                          (sort '(date todo priority)))
  "Define a function, NAME, that calls `org-sidebar' with the given arguments.

SIDEBARS is a list of `org-ql' query forms.  Each one may be
followed by the same keyword arguments this macro accepts (except
`:sidebars'), which override the arguments given to this macro.
For example:

  (org-sidebar-defsidebar my-sidebar
    :header \"My Sidebar\"
    :sidebars ((and (not (done))
                    (or (deadline <=)
                        (date = today)))
               :files \"file.org\"))

FILES is an expression which should evaluate to one or a list of
files or buffers.  It defaults to the `org-agenda-files'
function.

SORT is passed to `org-ql', which see.

SUPER-GROUPS is optionally used as the value of
`org-super-agenda-groups', which see."
  (declare (indent defun))
  `(defun ,name ()
     ,(format "org-sidebar command defined with `org-sidebar-defsidebar'.")
     (org-sidebar :header ,header
                  ;; TODO: Improve handling of SIDEBARS arg to reduce nesting level.  See proposed
                  ;; plist pcase matcher.
                  :fns ',(--map (-let* (((query &keys :files this-files :sort this-sort) it)
                                        (files (or this-files files))
                                        (sort (or this-sort sort)))
                                  `(lambda (&rest _args)
                                     (org-ql ,files
                                       ,query
                                       :sort ,sort
                                       :markers t)))
                                sidebars)
                  :super-groups ,super-groups
                  :group ,group)))

;;;; Footer

(provide 'org-sidebar)

;;; org-sidebar.el ends here
