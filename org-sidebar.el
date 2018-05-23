;; NOTE: Maybe this should be called something like org-agenda-sidebar instead, or just org-sidebar.  Ahh, naming things is hard...

(require 'org)

(require 'dash)
(require 'dash-functional)

(require 'org-ql)
(require 'org-agenda-ng)


(defun org-sidebar ()
  "FIXME"
  (interactive)
  (cl-flet* ((buffer-name-string ()
                                 (concat (when (buffer-narrowed-p)
                                           "[narrowed] ")
                                         (buffer-name)))
             (bold (string)
                   (propertize string 'face '(:weight bold)))
             (date-header (item)
                          (propertize (org-timestamp-format (or (get-text-property 0 'scheduled item)
                                                                (get-text-property 0 'deadline item))
                                                            ;; "%Y-%m-%d"
                                                            "%e %B %Y"
                                                            )
                                      'face '(:inherit variable-pitch :weight bold)))
             (prepare-window (window name string)
                             (let ((org-buffer (current-buffer))
                                   (org-buffer-window main-window))
                               (with-selected-window window
                                 (with-current-buffer (get-buffer-create (format " *%s*" name))
                                   (setq header-line-format (propertize name
                                                                        'face '(:inherit org-agenda-date-today))
                                         mode-line-format nil)
                                   (set-window-parameter nil 'org-buffer org-buffer)
                                   (set-window-parameter nil 'org-buffer-window org-buffer-window)
                                   (erase-buffer)
                                   (insert string)
                                   (goto-char (point-min))
                                   (local-set-key (kbd "RET") #'org-sidebar--jump)
                                   (local-set-key (kbd "<mouse-1>") #'org-sidebar--jump)
                                   (switch-to-buffer (current-buffer))
                                   (toggle-truncate-lines 1)))))
             (format-item (item)
                          (let ((string (org-agenda-ng--format-element item)))
                            (add-face-text-property 0 (length string) 'variable-pitch t string)
                            string)))
    (let* ((file (buffer-file-name (current-buffer)))
           (agenda-items (mapcar #'format-item
                                 (org-ql file
                                   (and (or (scheduled)
                                            (deadline))
                                        (not (done)))
                                   :sort (date)
                                   :narrow t)))
           (agenda-string (with-temp-buffer
                            (--each (-group-by #'date-header agenda-items)
                              (insert "\n" (car it) "\n\n")
                              (--each (cdr it)
                                (insert it "\n")))
                            (buffer-string)))
           (todo-items (mapcar #'format-item
                               (org-ql file
                                 (and (todo)
                                      (not (or (done)
                                               (scheduled)
                                               (deadline))))
                                 :sort (todo priority)
                                 :narrow t)))
           (todo-string (s-join "\n" todo-items))
           (frame (selected-frame))
           main-window agenda todo)
      (with-selected-frame frame
        (delete-other-windows)
        (setq main-window (selected-window))
        (setq agenda (split-window nil -50 'right))
        (setq todo (with-selected-window agenda
                     (split-window-vertically)))

        (prepare-window agenda (format " %s: Agenda" (buffer-name-string)) agenda-string)
        (prepare-window todo (format " %s: Other TODOs" (buffer-name-string)) todo-string)

        (select-window main-window)
        (switch-to-buffer (find-buffer-visiting file))))))

(defun org-sidebar--jump ()
  "Jump to entry at sidebar buffer's point in source buffer."
  (interactive)
  (when-let ((begin (get-text-property (point) 'begin))
             (org-buffer (window-parameter nil 'org-buffer))
             (org-buffer-window (window-parameter nil 'org-buffer-window)))
    (select-window org-buffer-window)
    (goto-char begin)
    (org-reveal)))
