;; -*- lexical-binding: t; -*-
;; Magit Dashboard
;; Author: Anthony Fairchild
;;
;; See LICENSE file for license info.
;; See README.md for project info.

(require 'cl-lib)
(require 'magit)

(defstruct magit-dashboard-line
  name
  dir
  branch
  remote/branch
  inbound-count
  outbound-count
  behind)

(defun magit-dashboard-get-number-of-outbound ()
  (+ (magit-dashboard-get-section-count "Unstaged changes:")
     (magit-dashboard-get-section-count "Untracked files:")
     (magit-dashboard-get-section-count "Staged changes:")
     (magit-dashboard-get-section-count "Unpushed commits:")
     (magit-dashboard-get-section-count "Pending commits:")))

(defun magit-dashboard-get-number-of-inbound ()
  (magit-dashboard-get-section-count "Unpulled commits:"))

(defun magit-dashboard-parse-line (name dir)
  (unless (eq major-mode 'magit-status-mode)
    (error "magit-dashboard-parse-line must be called from a magit status buffer"))
  (make-magit-dashboard-line :name name
                             :dir dir
                             :inbound-count (magit-dashboard-get-number-of-inbound)
                             :outbound-count (magit-dashboard-get-number-of-outbound)
                             :branch (magit-get-current-branch)
                             :remote/branch (magit-get-remote/branch)))

(defun magit-dashboard-get-lines ()
  (let ((lines nil))
    (magit-dashboard-map-magit-buffers
     (lambda (name dir)
       (push (magit-dashboard-parse-line name dir)
             lines)))
    (reverse lines)))

(defun magit-dashboard ()
  (interactive)
  (magit-dashboard-refresh)
  (switch-to-buffer-other-window "*magit-dashboard*"))

(defun magit-dashboard-get-section-count (section-header)
  (save-excursion
    (goto-char (point-min))
    (cond ((search-forward-regexp (format "^%s$" section-header)
                                  nil
                                  t)
           (let ((beg (point)))
             (search-forward-regexp "\n\n")
             (- (count-lines beg (point))
                2)))
          (t 0))))

(defun magit-dashboard-refresh ()
  (interactive)
  (let ((lines (magit-dashboard-get-lines)))
    (with-current-buffer (get-buffer-create "*magit-dashboard*")
      (magit-dashboard-mode)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (loop for line in lines
           do (progn (insert-button (magit-dashboard-line-name line)
                                    'action (let ((line line))
                                              (lambda (button)                                      
                                                (magit-status (magit-dashboard-line-dir line)))))
                     (insert  " "
                              (number-to-string (magit-dashboard-line-outbound-count line)) "/"
                              (number-to-string (magit-dashboard-line-inbound-count line)) " "
                              (magit-dashboard-line-branch line) " "
                              "(" (magit-dashboard-line-remote/branch line) ") "
                              (magit-dashboard-line-dir line) " "
                              "\n")))
        (align-regexp (point-min) (point-max) "\\( \\)" nil nil t)))))

(defun magit-dashboard-map-magit-buffers (fun)
  (loop for (name . dir) in (magit-list-repos magit-repo-dirs)
     do (let ((magit-buffer (get-buffer (format "*magit: %s*" name))))
          (cond (magit-buffer
                 (with-current-buffer magit-buffer
                   (funcall fun name dir)))
                (t
                 (save-window-excursion
                   (magit-status dir)
                   (funcall fun name dir)))))))


(defun magit-dashboard-fetch ()
  (interactive)
  (magit-dashboard-map-magit-buffers
   (lambda (name dir)     
     (magit-fetch-current)))
  (magit-dashboard-refresh))

(defvar magit-dashboard-mode-map (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "g") 'magit-dashboard-refresh)
                                   (define-key map (kbd "f") 'magit-dashboard-fetch)
                                   map))

(define-derived-mode magit-dashboard-mode fundamental-mode
  "Magit Dashboard"
  )


(provide 'magit-dashboard)
