;; NOTE: Maybe this should be called something like org-agenda-sidebar instead, or just org-sidebar.  Ahh, naming things is hard...

(require 'org)

(require 'dash)
(require 'dash-functional)

(require 'org-ql)
(require 'org-agenda-ng)

(defgroup org-sidebar nil
  "Options for `org-sidebar'.")

(defcustom org-sidebar-date-format "%e %B %Y"
  "Format string for date headers.
See `format-time-string'.")

(defcustom org-sidebar-format-fn #'org-agenda-ng--format-element
  "Function used to format elements.
Takes a single argument: the Org element being formatted.")

(defvar org-sidebar-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "RET" org-sidebar--jump
                    "<mouse-1>" org-sidebar--jump
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map)
  "Keymap for `org-sidebar' buffers.")

(defun ap/org-sidebar-format-element (element)
  "Calls `org-agenda-ng--format-element' and adds `variable-pitch' face."
  (add-face-text-property 0 (length string)
                          'variable-pitch t
                          (org-agenda-ng--format-element element)))

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

(defun org-sidebar ()
  "FIXME"
  (interactive)
  (cl-flet ((date-header (item)
                         (propertize (org-timestamp-format (or (get-text-property 0 'scheduled item)
                                                               (get-text-property 0 'deadline item))
                                                           org-sidebar-date-format)
                                     'face '(:inherit variable-pitch :weight bold))))
    (let* ((file (buffer-file-name (current-buffer)))
           (agenda-items (mapcar org-sidebar-format-fn
                                 (org-ql file
                                   (and (or (scheduled)
                                            (deadline))
                                        (not (done)))
                                   :sort (date)
                                   :narrow t)))
           (todo-items (mapcar org-sidebar-format-fn
                               (org-ql file
                                 (and (todo)
                                      (not (or (done)
                                               (scheduled)
                                               (deadline))))
                                 :sort (todo priority)
                                 :narrow t)))
           (agenda-string (with-temp-buffer
                            (--each (-group-by #'date-header agenda-items)
                              (insert "\n" (car it) "\n\n")
                              (--each (cdr it)
                                (insert it "\n")))
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

(defun org-sidebar--jump ()
  "Jump to entry at sidebar buffer's point in source buffer."
  (interactive)
  (when-let ((begin (get-text-property (point) 'begin))
             (org-buffer (window-parameter nil 'org-buffer))
             (org-buffer-window (window-parameter nil 'org-buffer-window)))
    (select-window org-buffer-window)
    (goto-char begin)
    (org-reveal)))

;;;; Footer

(provide 'org-sidebar)

;;; org-sidebar.el ends here
