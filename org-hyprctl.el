;;; org-hyprctl.el --- Manage Hyprctl workspaces from Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: unix tools
;; URL: https://github.com/akirak/org-hyprctl/

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; (add-hook 'org-ctrl-c-ctrl-c-hook #'org-hyprctl-update)

;;; Code:

(defgroup org-hyprctl nil
  ""
  :group 'org)

(defcustom org-hyprctl-executable "hyprctl"
  ""
  :type 'file)

;;;###autoload
(defun org-hyprctl-init ()
  "Insert an Org block for managing the workspaces."
  (interactive)
  (insert "#+begin_hyprctl\n"
          (org-hyprctl--serialize (org-hyprctl--query "clients"))
          "#+end_hyprctl\n"))

;;;###autoload
(defun org-hyprctl-update ()
  "Update the workspaces according to the block at point."
  (when-let (element (org-element-context))
    (when (and element
               (eq 'special-block (org-element-type element))
               (equal "hyprctl" (org-element-property :type element))
               (equal "Hyprland" (getenv "XDG_CURRENT_DESKTOP")))
      (save-excursion
        (atomic-change-group
          (if-let* ((inner-begin (org-element-property :contents-begin element))
                    (inner-end (org-element-property :contents-end element)))
              (progn
                (goto-char inner-begin)
                (org-hyprctl--apply (org-element-context)
                                    (org-hyprctl--query "clients"))
                (delete-region inner-begin inner-end))
            (goto-char (org-element-property :begin element))
            (beginning-of-line 2))
          (insert (org-hyprctl--serialize (org-hyprctl--query "clients")))
          t)))))

(defun org-hyprctl--query (command)
  (with-temp-buffer
    (unless (zerop (call-process org-hyprctl-executable nil t nil
                                 command
                                 "-j"))
      (error "hyprctl returned non-zero exit code: "
             (buffer-string)))
    (goto-char (point-min))
    (json-parse-buffer :array-type 'list :object-type 'alist)))

(defun org-hyprctl--dispatch (&rest args)
  (apply #'call-process org-hyprctl-executable nil nil nil
         "dispatch" args))

(defun org-hyprctl--apply (config clients)
  (if (and (eq 'plain-list (org-element-type config))
           (equal 'descriptive (org-element-property :type config)))
      (let ((initial-active-window (thread-last
                                     (org-hyprctl--query "activewindow")
                                     (alist-get 'address)))
            (current-wss (thread-last
                           clients
                           (mapcar (lambda (x) (alist-get 'workspace x)))
                           (seq-uniq)
                           (mapcar (lambda (x)
                                     (cons (alist-get 'id x)
                                           (alist-get 'name x))))))
            (layout (org-element-property :structure config))
            client-config)
        (while layout
          (let* ((workspace (pop layout))
                 (workspace-id (string-to-number (nth 5 workspace)))
                 (workspace-el (save-excursion
                                 (goto-char (car workspace))
                                 (org-element-item-parser
                                  nil (list workspace))))
                 (workspace-name (string-trim
                                  (buffer-substring-no-properties
                                   (org-element-property :contents-begin workspace-el)
                                   (or (caar layout)
                                       (org-element-property :contents-end workspace-el)))))
                 (current-ws-name (alist-get workspace-id current-wss)))
            (when (and (> workspace-id 0)
                       (not (equal current-ws-name workspace-name)))
              (org-hyprctl--dispatch "renameworkspace"
                                     (number-to-string workspace-id)
                                     workspace-name))
            (while (and layout
                        (= 2 (nth 1 (car layout))))
              (let* ((client (pop layout))
                     (address (nth 5 client))
                     (current-ws (seq-some `(lambda (x)
                                              (when (equal ,address (alist-get 'address x))
                                                (thread-last
                                                  x
                                                  (alist-get 'workspace)
                                                  (alist-get 'id))))
                                           clients)))
                (when (and current-ws
                           (/= workspace-id current-ws))
                  (org-hyprctl--dispatch "movetoworkspace"
                                         (format "%d,address:%s"
                                                 workspace-id
                                                 address)))))))
        (unless (equal initial-active-window
                       (thread-last
                         (org-hyprctl--query "activewindow")
                         (alist-get 'address)))
          (org-hyprctl--dispatch "focuswindow"
                                 (format "address:%s" initial-active-window))))
    (user-error "Invalid content of hyprctl block")))

(defun org-hyprctl--serialize (clients)
  (with-temp-buffer
    (pcase-dolist (`(,group . ,group-clients)
                   (thread-last
                     clients
                     (seq-group-by (lambda (x) (alist-get 'workspace x)))
                     (seq-sort-by (lambda (g) (alist-get 'id (car g)))
                                  #'<)))
      (insert (format "- %d :: %s\n"
                      (alist-get 'id group)
                      (alist-get 'name group)))
      (dolist (client group-clients)
        (insert (format "  - %s :: %s\n"
                        (alist-get 'address client)
                        (alist-get 'title client)))))
    (buffer-string)))

(provide 'org-hyprctl)
;;; org-hyprctl.el ends here