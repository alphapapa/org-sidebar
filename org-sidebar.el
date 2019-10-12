;;; org-sidebar.el --- Helpful sidebar for Org buffers  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/org-sidebar
;; Version: 0.3-pre
;; Package-Requires: ((emacs "26.1") (s "1.10.0") (dash "2.13") (dash-functional "1.2.0") (org "9.0") (org-ql "0.2") (org-super-agenda "1.0"))
;; Keywords: hypermedia, outlines, Org, agenda

;;; Commentary:

;; This package presents a helpful sidebar view for Org buffers.
;; Sidebars are customizable using `org-ql' queries and
;; `org-super-agenda' grouping.

;; The default view includes a chronological list of scheduled and
;; deadlined tasks in the current buffer (similar to the Org agenda
;; ,but without all its features) at the top, and a list of all other
;; non-done to-do items below.  If the buffer is narrowed, the sidebar
;; only shows items in the narrowed portion; this allows seeing an
;; overview of tasks in a subtree.

;;;; Usage

;; Call these commands to display sidebars:

;; - `org-sidebar:' Display the default item sidebars for the current
;;                  Org buffer.
;; - `org-sidebar-tree:' Display tree-view sidebar for current Org
;;                       buffer.

;; Toggling versions of those commands are also available:

;; - `org-sidebar-toggle'
;; - `org-sidebar-tree-toggle'

;; Customization options are in the `org-sidebar' group.

;; The functions `org-sidebar-tree-view-buffer' and
;; `org-sidebar--subtree-buffer' return buffers.

;; To display custom-defined sidebars, call the function `org-sidebar'
;; with the arguments described in its docstring.  See examples in
;; documentation, as well as the definitions of functions
;; `org-sidebar--todo-items' and `org-sidebar--upcoming-items'.

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
(require 'subr-x)

(require 'dash)
(require 'dash-functional)
(require 's)

(require 'org-ql)
(require 'org-ql-view)
(require 'org-super-agenda)

;;;; Variables

(defvar org-sidebar-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "RET" org-sidebar-jump
                    "<mouse-1>" org-sidebar-jump
                    "g" org-sidebar-refresh
                    "q" bury-buffer
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map)
  "Keymap for `org-sidebar' buffers.")

;; Buffer-local vars for refreshing sidebars.
(defvar-local org-sidebar-source-buffer nil
  "The source Org buffer for entries in this sidebar buffer.")
(defvar-local org-sidebar-buffers nil
  "The `buffers' argument to `org-sidebar'.")
(defvar-local org-sidebar-sidebars nil
  "The `sidebars' argument to `org-sidebar'.")
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
  :group 'org
  :link '(url-link "https://github.com/alphapapa/org-sidebar"))

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

(defcustom org-sidebar-format-fn #'org-ql-view--format-element
  "Function used to format elements.
Takes a single argument: the Org element being formatted.
Generally, `org-ql-view--format-element' should be used; if
not, the function used should set appropriate text properties,
imitating the Org Agenda, for commands and features which use the
text properties to act on items."
  :type 'function)

(defcustom org-sidebar-side 'right
  "Which side to show the sidebar on."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))

(defcustom org-sidebar-default-fns '(org-sidebar--upcoming-items org-sidebar--todo-items)
  "Default sidebar functions."
  :type '(repeat (choice (const :tag "Upcoming items" org-sidebar--upcoming-items)
			 (const :tag "To-do items" org-sidebar--todo-items)
			 (const :tag "Tree-view" org-sidebar-tree-view-buffer)
			 (function :tag "Other function"))))

;;;; Structs

(cl-defstruct org-sidebar
  ;; Naming things is hard.  Maybe this will help organize things.

  ;; This struct is intended to be made at runtime.  The ITEMS slot should
  ;; be the result of an `org-ql' query with ACTION `element-with-markers'.
  ;; See default item functions.  The struct is passed by function
  ;; `org-sidebar' to function `org-sidebar--items-buffer', which returns a
  ;; buffer displaying the struct's items.
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
  (let* ((source-buffer (current-buffer))
         (buffers (cl-etypecase buffers
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
         (display-buffers (append buffers fns-buffers sidebars-buffers)))
    (--each display-buffers
      ;; Save settings in buffers for refreshing.
      (with-current-buffer it
        (setf org-sidebar-source-buffer source-buffer
              org-sidebar-buffers buffers
              org-sidebar-fns fns
              org-sidebar-sidebars sidebars
              org-sidebar-group group
              org-sidebar-super-groups super-groups)))
    (org-sidebar--display-buffers display-buffers
                                  :window-parameters (list (cons 'org-sidebar-window t)
                                                           (cons 'org-sidebar-source-buffer-point-min (point-min))))))

;;;###autoload
(defun org-sidebar-toggle ()
  "Toggle default sidebar window.
If it is open and shows the view for the current buffer, delete
it.  Otherwise, show it for current buffer."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (point-min (point-min))
         (sidebar-window (--first (window-parameter it 'org-sidebar-window)
                                  (window-at-side-list nil org-sidebar-side))))
    ;; We only compare the first sidebar window, but that should be good enough.
    (if (and sidebar-window
             (with-current-buffer (window-buffer sidebar-window)
               (and (eq org-sidebar-source-buffer current-buffer)
                    ;; Compare point-min to detect narrowed buffers.
                    (eq (window-parameter sidebar-window 'org-sidebar-source-buffer-point-min)
                        point-min))))
        ;; Sidebar is for current buffer: delete sidebar windows.
        (mapc #'delete-window (--select (window-parameter it 'org-sidebar-window)
                                        (window-at-side-list nil org-sidebar-side)))
      ;; Sidebar is for a different buffer: show sidebar for current buffer.
      (call-interactively #'org-sidebar))))

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
              :items (org-ql-query
                       :select 'element-with-markers
                       :from buffers-files
                       :where query
                       :narrow narrow
                       :order-by sort)
              :name (prin1-to-string query)
              :super-groups (list (pcase group-property
                                    ('nil nil)
                                    ('category '(:auto-category))
                                    ('parent '(:auto-parent))
                                    ('priority '(:auto-priority))
                                    ('todo-keyword '(:auto-todo)))))
   :group group-property))

(defun org-sidebar-refresh ()
  "Refresh current sidebar buffers."
  (interactive)
  ;; `org-sidebar' needs to be called in the original source buffer, but we
  ;; need to get the saved arguments that are stored in the sidebar buffer.
  (let ((buffers org-sidebar-buffers)
        (fns org-sidebar-fns)
        (group org-sidebar-group)
        (super-groups org-sidebar-super-groups))
    (save-excursion
      (when org-sidebar-source-buffer
        (switch-to-buffer org-sidebar-source-buffer))
      (org-sidebar :buffers buffers
                   :fns fns
                   :group group
                   :super-groups super-groups))))

(defun org-sidebar-jump ()
  "Jump to entry at sidebar buffer's point in source buffer."
  (interactive)
  (if-let* ((marker (get-text-property (point) 'org-marker))
            (buffer (marker-buffer marker)))
      (progn
        (--if-let (get-buffer-window buffer)
            (select-window it)
          (pop-to-buffer buffer))
        (goto-char marker)
        (org-reveal)
        (org-show-entry)
        (when org-sidebar-jump-indirect
          (org-tree-to-indirect-buffer)))
    (user-error "Item's buffer no longer exists")))

;;;; Functions

(cl-defun org-sidebar--display-buffers (buffers &key window-parameters)
  "Display BUFFERS in the sidebar.
WINDOW-PARAMETERS are applied to each window that is displayed."
  (--each (window-at-side-list nil org-sidebar-side)
    ;; Delete existing org-sidebar windows on our side.
    (when (buffer-local-value 'org-sidebar-source-buffer (window-buffer it))
      (delete-window it)))
  (let ((slot 0)
        (window-parameters (append (list (cons 'no-delete-other-windows t))
                                   window-parameters)))
    (--each buffers
      (display-buffer-in-side-window
       it
       (list (cons 'side org-sidebar-side)
             (cons 'slot slot)
             (cons 'window-parameters window-parameters)))
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
    (setq header-line-format (concat " " name)
          mode-line-format nil)
    (read-only-mode 1)
    (use-local-map org-sidebar-map)
    (erase-buffer)
    (goto-char (point-min))
    (toggle-truncate-lines 1)))

;;;;; Default item functions

(cl-defun org-sidebar--upcoming-items (&rest _ignore)
  "Return `org-sidebar' struct for upcoming items in current buffer.
If no items are found, return nil.
The `org-ql' query is:

    (and (or (scheduled)
             (deadline))
         (not (done)))"
  (when-let* ((items (org-ql-select (current-buffer)
                       '(and (or (scheduled)
                                 (deadline))
                             (not (done)))
                       :action 'element-with-markers
                       :narrow t :sort 'date)))
    (make-org-sidebar
     :name (concat "Upcoming items: " (buffer-name))
     :description "Non-done items with a deadline or scheduled date in the current buffer"
     ;; NOTE: This commented code produces date headers that are more visually pleasing
     ;; to me, but since moving the functionality into `org-super-agenda', it probably
     ;; doesn't make sense to do this here.  But I'm going to keep the code here for now.
     ;; :group-fn (lambda (item)
     ;;             ;; Return a date header string for grouping.
     ;;             (propertize (org-timestamp-format (or (org-element-property :scheduled item)
     ;;                                                   (org-element-property :deadline item))
     ;;                                               org-sidebar-date-format)
     ;;                         'face '(:inherit variable-pitch :weight bold)))
     :super-groups '((:auto-planning))
     :items items)))

(defun org-sidebar--todo-items (&rest _ignore)
  "Return sidebar for unscheduled, un-deadlined to-do items in current buffer.
If no items are found, return nil."
  (when-let* ((items (org-ql-select (current-buffer)
                       '(and (todo)
                             (not (or (scheduled)
                                      (deadline))))
                       :sort '(priority todo)
                       :narrow t
                       :action 'element-with-markers)))
    (make-org-sidebar
     :name (concat "To-do items: " (buffer-name))
     :description "Unscheduled, un-deadlined to-do items in current buffer."
     :items items
     :super-groups '((:auto-todo)))))

;;;; Tree-view

(defvar org-sidebar-tree-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "<return>" org-sidebar-tree-jump
                    "<mouse-1>" org-sidebar-tree-jump-mouse
                    "<double-mouse-1>" org-sidebar-tree-jump-mouse
                    "<triple-mouse-1>" org-sidebar-tree-jump-mouse
                    "<mouse-2>" org-sidebar-tree-cycle-mouse
                    "<double-mouse-2>" org-sidebar-tree-cycle-mouse
                    "<triple-mouse-2>" org-sidebar-tree-cycle-mouse
                    "<drag-mouse-1>" org-sidebar-tree-jump-branches-mouse
                    "<drag-mouse-2>" org-sidebar-tree-jump-entries-mouse
                    "<tab>" org-sidebar-tree-cycle
                    ;; I don't know if it's universally necessary to bind
                    ;; all three of these, but it seems to be on my Org.
                    "<S-tab>" org-sidebar-tree-cycle-global
                    "<S-iso-lefttab>" org-sidebar-tree-cycle-global
                    "<backtab>" org-sidebar-tree-cycle-global
                    )))
    (set-keymap-parent map org-mode-map)
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map)
  "Keymap for `org-sidebar-tree' buffers.")

(defcustom org-sidebar-tree-jump-fn #'org-sidebar-tree-jump-indirect
  "Default function used to jump to entries from tree-view buffer."
  :type '(choice (const :tag "Indirect buffer" org-sidebar-tree-jump-indirect)
                 (const :tag "Source buffer" org-sidebar-tree-jump-source)
                 (function :tag "Custom function")))

(defcustom org-sidebar-tree-side 'left
  "Which side to show the tree sidebar on."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)))

;;;###autoload
(defun org-sidebar-tree ()
  "Show tree-view sidebar."
  ;; TODO: I accidentally discovered that this almost works perfectly in Elisp
  ;; buffers because of the outline regexps.  It wouldn't take much to make it
  ;; work in a similar way, clicking on a function to edit it in a buffer.
  (interactive)
  (let ((org-sidebar-side org-sidebar-tree-side))
    (org-sidebar--display-buffers (list (org-sidebar-tree-view-buffer))
                                  :window-parameters (list (cons 'org-sidebar-tree-window t)))))

;;;###autoload
(defun org-sidebar-tree-toggle ()
  "Toggle tree-view sidebar window.
If it is open and shows the view for the current buffer, delete
it.  Otherwise, show it for current buffer."
  (interactive)
  (let* ((parent-point-min (point-min))
         (parent-buffer (or (buffer-base-buffer)
                            (current-buffer)))
         (tree-window (--first (window-parameter it 'org-sidebar-tree-window)
                               (window-at-side-list nil org-sidebar-tree-side))))
    (if (and tree-window
             (with-current-buffer (window-buffer tree-window)
               (and (eq parent-point-min (point-min))
                    (or (eq parent-buffer (buffer-base-buffer))
                        (eq parent-buffer (current-buffer))))))
        ;; Tree displays current buffer: delete tree window.
        (delete-window tree-window)
      ;; Tree displays a different buffer: show tree for current buffer.
      (org-sidebar-tree))))

;;;###autoload
(cl-defun org-sidebar-tree-view-buffer (&key (buffer (current-buffer)) &allow-other-keys)
  "Return a tree-view buffer for BUFFER."
  (-let* ((buffer-name (concat "<tree>" (buffer-name buffer)))
          ((min max) (with-current-buffer buffer
                       (list (point-min) (point-max))))
          (existing-buffer (get-buffer buffer-name))
          tree-buffer)
    (when existing-buffer
      ;; Buffer with name already exists.
      (if (buffer-base-buffer existing-buffer)
          ;; Buffer is indirect: kill it so we can remake it.
          (kill-buffer existing-buffer)
        ;; Buffer is not indirect: something is probably wrong, so warn.
        (warn "Existing tree buffer that is not indirect: %s" existing-buffer)))
    (setf tree-buffer (clone-indirect-buffer buffer-name nil 'norecord))
    (with-current-buffer tree-buffer
      (use-local-map org-sidebar-tree-map)
      (setf mode-line-format nil
            header-line-format (concat "Tree: " (buffer-name buffer)))
      (toggle-truncate-lines 1)
      (save-excursion
        (goto-char min)
        (unless (org-before-first-heading-p)
          ;; Tree view only shows one subtree: expand its branches.
          (outline-show-branches)))
      (narrow-to-region min max)
      (save-excursion
        ;; Hide visible entry bodies.
        (goto-char (point-min))
        (when (org-before-first-heading-p)
          (outline-next-visible-heading 1))
        (cl-loop do (outline-hide-body)
                 while (outline-next-visible-heading 1)))
      (unless (org-before-first-heading-p)
        (outline-back-to-heading)))
    tree-buffer))

(defun org-sidebar-tree-cycle-or-jump (&optional children)
  "Cycle visibility of current node, or jump to it in indirect buffer.
If point is on heading stars, cycle visibility, otherwise jump to
current heading in an indirect buffer.  If CHILDREN is
non-nil (interactively, with prefix), also show child headings in
the indirect buffer."
  (interactive "P")
  (unless (buffer-base-buffer)
    (error "Must be in a tree buffer"))
  (if (or (eq ?* (char-after))
          (eq ?* (char-before)))
      ;; Point is on heading stars: cycle visibility.
      (org-sidebar-tree-cycle)
    ;; Point on heading text: jump.
    (org-sidebar-tree-jump children)))

(defun org-sidebar-tree-jump (&optional children)
  "Jump to heading at point using `org-sidebar-tree-jump-fn'.
If point is before first heading, show base buffer.  Argument
CHILDREN controls how child entries are displayed:

If nil (interactively, without prefix), only show the entry's own
body text.  If `children' (with one universal prefix), also show
child headings.  If `branches' (two prefixes), show all
descendant headings.  If `entries' (three prefixes), show all
descendants and their body text."
  (interactive "p")
  (unless (buffer-base-buffer)
    (error "Must be in a tree buffer"))
  (if (org-before-first-heading-p)
      (org-sidebar-tree-jump-source)
    (funcall org-sidebar-tree-jump-fn
             :children (pcase children
                         (1 nil)
                         (4 'children)
                         (16 'branches)
                         (64 'entries)))))

(cl-defun org-sidebar-tree-jump-indirect (&key children)
  "Jump to an indirect buffer showing the heading at point.
If CHILDREN is non-nil (interactively, with prefix), also show
child headings in the indirect buffer.  Should be called from a
tree-view buffer."
  (interactive "P")
  (unless (buffer-base-buffer)
    (error "Must be in a tree buffer"))
  (let* ((new-buffer (org-sidebar--subtree-buffer children))
         (base-buffer (buffer-base-buffer))
         (indirect-buffers
          ;; Collect list of indirect buffers for the tree buffer's
          ;; base buffer, not including the tree buffer.
          (cl-loop for buffer in (buffer-list)
                   when (and (eq (buffer-base-buffer buffer) base-buffer)
                             (not (eq buffer (current-buffer))))
                   collect buffer into buffers
                   finally return (-uniq buffers)))
         (displayed-buffer (--first (get-buffer-window it) indirect-buffers))
         (window (when displayed-buffer
                   (get-buffer-window displayed-buffer))))
    (if window
        (progn
          (select-window window)
          (switch-to-buffer new-buffer))
      (pop-to-buffer new-buffer
                     (cons 'display-buffer-use-some-window
                           (list (cons 'inhibit-same-window t)))))))

(cl-defun org-sidebar-tree-jump-source (&key children)
  "Jump to the heading at point in its source buffer.
If CHILDREN is non-nil (interactively, with prefix), also expand
child entries.  Should be called from a tree-view buffer."
  (interactive "P")
  (unless (buffer-base-buffer)
    (error "Must be in a tree buffer"))
  (let* ((pos (point))
         (base-buffer (buffer-base-buffer))
         (window (get-buffer-window base-buffer)))
    (if window
        (progn
          (select-window window)
          (switch-to-buffer base-buffer))
      (pop-to-buffer base-buffer
                     (cons 'display-buffer-use-some-window
                           (list (cons 'inhibit-same-window t)))))
    (goto-char pos)
    (org-show-entry)
    (org-show-children)
    (when children
      (org-show-subtree))))

(defun org-sidebar-tree-cycle ()
  "Cycle visibility of heading at point and its descendants.
Similar to `org-cycle-internal-local', but does not expand entry
bodies.

If heading at point has invisible children, show them.  Or, if
this command is being repeated and heading at point has invisible
descendants, show them.  Otherwise, hide the subtree."
  (interactive)
  (cond ((org-sidebar--children-p 'invisible)
         ;; Has invisible children: show children.
         (outline-show-children))
        ((and (eq last-command this-command)
              (save-excursion
                (save-restriction
                  (narrow-to-region (point) (save-excursion (org-end-of-subtree)))
                  (cl-loop while (outline-next-heading)
                           thereis (outline-invisible-p)))))
         ;; This was last command and has invisible descendants: show branches.
         (outline-show-branches))
        (t ;; Nothing more to expand: hide tree.
         (outline-hide-subtree))))

(defun org-sidebar-tree-cycle-global ()
  "Cycle global visiblity.
Similar to `org-cycle-internal-global', but does not expand entry
bodies."
  (interactive)
  (let* ((highest-invisible-heading-level
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (when (org-before-first-heading-p)
                (outline-next-heading))
              (cl-loop when (outline-invisible-p)
                       return (org-current-level)
                       while (outline-next-heading)))))
         (regexp (rx-to-string `(seq bol (repeat ,(or highest-invisible-heading-level 1) "*") (1+ blank)))))
    (if highest-invisible-heading-level
        ;; Some headings are invisible: Show all headings at that level.
        (save-excursion
          (goto-char (point-min))
          (cl-loop while (re-search-forward regexp nil t)
                   do (progn
                        (org-up-heading-safe)
                        (outline-show-children)
                        (org-end-of-subtree))))
      ;; All headings visible: Hide all.
      (save-excursion
        (goto-char (point-min))
        (when (org-before-first-heading-p)
          (outline-next-heading))
        (cl-loop do (outline-hide-subtree)
                 while (re-search-forward regexp nil t))))))

(defun org-sidebar-tree-cycle-mouse (event)
  "Cycle visibility of heading at EVENT and its descendants.
Like `org-cycle-internal-local', but doesn't show entry bodies."
  (interactive "e")
  (-let* (((_type position _count) event)
          ((window _pos-or-area (_x . _y) _timestamp
                   _object text-pos . _) position))
    (with-selected-window window
      (goto-char text-pos)
      (org-sidebar-tree-cycle))))

(cl-defun org-sidebar-tree-jump-mouse (event &key children)
  "Jump to tree for EVENT.
If point is before first heading, jump to base buffer.  If
CHILDREN is non-nil, also show children."
  (interactive "e")
  (-let* (((_type position _count) event)
          ((window _pos-or-area (_x . _y) _timestamp
                   _object text-pos . _) position))
    (with-selected-window window
      (goto-char text-pos)
      (goto-char (point-at-bol))
      (if (org-before-first-heading-p)
          (org-sidebar-tree-jump-source)
        (funcall org-sidebar-tree-jump-fn :children children)))))

(defun org-sidebar-tree-jump-branches-mouse (event)
  "Jump to tree for EVENT, showing branches."
  (interactive "e")
  (org-sidebar-tree-jump-mouse event :children 'branches))

(defun org-sidebar-tree-jump-entries-mouse (event)
  "Jump to tree for EVENT, showing entries."
  (interactive "e")
  (org-sidebar-tree-jump-mouse event :children 'entries))

(defun org-sidebar--subtree-buffer (&optional children)
  "Return indirect buffer for subtree at point.
If CHILDREN is `children', also show its child headings in the
indirect buffer.  If `branches', show all descendant headings.  If
`entries', show all descendant headings and entry text."
  ;; Unfortunately, `org-tree-to-indirect-buffer' doesn't really do
  ;; what we need in a reusable way, so we have to reimplement it.
  (org-with-wide-buffer
   ;; TODO: Use `org-get-heading' after upgrading to newer Org.
   (let* ((buffer-name (concat (nth 4 (org-heading-components)) "::" (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))))
          (old-buffer (get-buffer buffer-name))
          (_killed-old-buffer-p (when old-buffer
                                  (if (buffer-base-buffer old-buffer)
                                      ;; Existing buffer is indirect: kill it.
                                      (kill-buffer old-buffer)
                                    ;; Existing buffer is not indirect: error.
                                    (error "Existing, non-indirect buffer named: %s" buffer-name))))
          (new-buffer (clone-indirect-buffer buffer-name nil t))
          (children (if children
                        children
                      ;; If `children' is nil, we set it to whether the entry has children.
                      ;; This is either an elegant hack or an ugly dual-purpose variable.
                      (org-sidebar--children-p)))
          (pos (point))
          (beg (org-entry-beginning-position))
          (end (if children
                   (save-excursion
                     (org-end-of-subtree)
                     (point))
                 (org-entry-end-position))))
     (with-current-buffer new-buffer
       (goto-char pos)
       (org-show-entry)
       (pcase-exhaustive children
         ('branches (outline-show-branches))
         ('children (org-show-children))
         ('entries (org-sidebar-show-subtree-entries))
         ('nil nil)
         (_ (org-show-children)))
       (narrow-to-region beg end)
       (current-buffer)))))

(defun org-sidebar--children-p (&optional invisible)
  "Return non-nil if entry at point has child headings.
If INVISIBLE is non-nil, return non-nil if entry has invisible
child headings.  Only children are considered, not other
descendants."
  ;; Code from `org-cycle-internal-local'.
  (save-excursion
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (org-at-heading-p t)
	   (> (funcall outline-level) level)
           (or (not invisible)
               (outline-invisible-p))))))

(defun org-sidebar-show-subtree-entries ()
  "Like `org-show-subtree', but only expands entry text.
Unlike `org-show-subtree', does not expand drawers."
  ;; TODO: Should we use `org-cycle-hide-drawers' instead?
  (save-excursion
    (cl-loop do (org-show-entry)
             while (outline-next-heading))))

;;;; Footer

(provide 'org-sidebar)

;;; org-sidebar.el ends here
