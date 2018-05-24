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

;;;; Customization

(defgroup org-sidebar nil
  "Options for `org-sidebar'."
  :group 'org)

(defcustom org-sidebar-date-format "%e %B %Y"
  "Format string for date headers.
See `format-time-string'.")

(defcustom org-sidebar-format-fn #'org-agenda-ng--format-element
  "Function used to format elements.
Takes a single argument: the Org element being formatted.")

;;;; Commands

(defun org-sidebar ()
  "This package presents a helpful sidebar view for Org buffers.
At the top is a chronological list of scheduled and deadlined
tasks in the current buffer, and below that is a list of all
other non-done to-do items.  If the buffer is narrowed, the
sidebar only shows items in the narrowed portion; this allows
seeing an overview of tasks in a subtree."
  (interactive)
  (cl-flet ((date-header (item)
                         (propertize (org-timestamp-format (or (get-text-property 0 'scheduled item)
                                                               (get-text-property 0 'deadline item))
                                                           org-sidebar-date-format)
                                     'face '(:inherit variable-pitch :weight bold))))
    (let* ((buffer (current-buffer))
           (agenda-items (--> (org-ql buffer
                                (and (or (scheduled)
                                         (deadline))
                                     (not (done)))
                                :sort (date priority todo)
                                :narrow t)
                              (-map org-sidebar-format-fn it)
                              (-group-by #'date-header it)))
           (todo-items (--> (org-ql buffer
                              (and (todo)
                                   (not (or (done)
                                            (scheduled)
                                            (deadline))))
                              :sort (todo priority)
                              :narrow t)
                            (-map org-sidebar-format-fn it)))
           (agenda-string (with-temp-buffer
                            (--each agenda-items
                              (-let (((header . items) it))
                                (insert "\n" header "\n\n")
                                (--each items
                                  (insert it "\n"))))
                            (buffer-string)))
           (todo-string (s-join "\n" todo-items))
           (frame (selected-frame))
           (buffer-name-string (concat (when (buffer-narrowed-p)
                                         "[narrowed] ")
                                       (buffer-name)))
           main-window agenda-window todo-window)
      (with-selected-frame frame
        (delete-other-windows)
        (setq main-window (selected-window))
        (setq agenda-window (split-window nil -50 'right))
        (setq todo-window (with-selected-window agenda-window
                            (split-window-vertically)))

        (org-sidebar--prepare-window agenda-window (format " %s: Agenda" buffer-name-string) agenda-string)
        (org-sidebar--prepare-window todo-window (format " %s: Other TODOs" buffer-name-string) todo-string)

        (select-window main-window)))))

;;;; Functions

(defun org-sidebar--prepare-window (window name contents)
  "Prepare WINDOW as a sidebar buffer.
Use NAME and insert CONTENTS."
  (let ((org-buffer (current-buffer))
        (org-buffer-window main-window))
    (with-selected-window window
      (switch-to-buffer (get-buffer-create (format " *%s*" name)))
      (setq header-line-format (propertize name
                                           'face '(:inherit org-agenda-date-today))
            mode-line-format nil)
      (set-window-parameter nil 'org-buffer org-buffer)
      (set-window-parameter nil 'org-buffer-window org-buffer-window)
      (use-local-map org-sidebar-map)
      (erase-buffer)
      (insert contents)
      (goto-char (point-min))
      (toggle-truncate-lines 1))))

(defun org-sidebar--jump ()
  "Jump to entry at sidebar buffer's point in source buffer."
  (interactive)
  (when-let ((begin (get-text-property (point) 'begin))
             (org-buffer (window-parameter nil 'org-buffer))
             (org-buffer-window (window-parameter nil 'org-buffer-window)))
    (select-window org-buffer-window)
    (goto-char begin)
    (org-reveal)))

(defun org-sidebar--update ()
  "Update `org-sidebar' buffer."
  (interactive)
  (when-let ((org-buffer-window (window-parameter nil 'org-buffer-window)))
    (select-window org-buffer-window)
    (org-sidebar)))

;;;; Footer

(provide 'org-sidebar)

;;; org-sidebar.el ends here
