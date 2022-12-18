;;; denote-refs.el --- Show links and backlinks in Denote notes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-12-18
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: hypermedia outlines files
;; URL: https://codeberg.org/akib/emacs-denote-refs

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Denote-Refs shows the list of linked file and backlinks to current
;; file.  This list is shown just below the front matter of your note.
;; To enable do M-x denote-refs-mode.  You can also enable it in your
;; `denote-directory' with .dir-locals.el.

;;; Code:

(require 'denote)
(require 'subr-x)

(defgroup denote-refs nil
  "Show links and backlinks in Denote notes."
  :group 'denote
  :group 'hypermedia
  :group 'outlines
  :group 'files
  :link '(url-link "https://codeberg.org/akib/emacs-denote-refs")
  :prefix "denote-refs-")

(defcustom denote-refs-update-delay '(0.2 1 60)
  "Idle delay before updating reference lists.

The value is a list of form (FIRST INIT MAINTAIN).  FIRST the delay
before initializing the reference lists just after enabling the mode.
INIT the delay before initializing the reference lists for the first
time, used if the initialization was interrupted.  MAINTAIN the delay
before updating the reference lists to keep the lists to updated."
  :type '(list (number :tag "Delay after mode enabled")
               (number :tag "Delay before initialization")
               (number :tag "Delay after initialized")))

(defcustom denote-refs-sections '(links backlinks)
  "The sections to show.

Available sections are `links' and `backlinks', which shows the list
of linked file and the list of backlinks respectively."
  :type '(set (const :tag "Links" links)
              (const :tag "Backlinks" backlinks)))

(defvar denote-refs--links 'not-ready
  "Alist of linked files.

The key is the path relative to user option `denote-directory', and
the key is the absolute path.")

(defvar denote-refs--backlinks 'not-ready
  "Alist of backlinks.

The key is the path relative to user option `denote-directory', and
the key is the absolute path.")

(defvar denote-refs--idle-update-timer nil
  "Timer to update references while idle.")

(defun denote-refs--render (section)
  "Render SECTION."
  (let ((refs (pcase section
                ('links denote-refs--links)
                ('backlinks denote-refs--backlinks))))
    (pcase major-mode
      ('org-mode
       ;; Insert references count.
       (insert (if (eq refs 'not-ready)
                   (format "# ... %s\n" (if (eq section 'links)
                                            "links"
                                          "backlinks"))
                 (format "# %i %s%s\n" (length refs)
                         (if (eq section 'links)
                             "link"
                           "backlink")
                         (pcase (length refs)
                           (0 "")
                           (1 ":")
                           (_ "s:")))))
       ;; Insert reference list.
       (when (listp refs)
         (dolist (ref refs)
           (insert "#   ")
           (insert-button (car ref)
                          'help-echo (cdr ref)
                          'face 'denote-faces-link
                          'action (lambda (_)
                                    (funcall denote-link-button-action
                                             (cdr ref))))
           (insert ?\n)))))))

(defun denote-refs--goto-end-of-front-matter ()
  "Go to the end of front matter of the note."
  ;; All default front matters end with at least an empty line.  But
  ;; advanced users can change that.  So we keep this code in separate
  ;; function for them to advice.
  (goto-char (point-min))
  (search-forward "\n\n"))

(defun denote-refs--remove ()
  "Remove the references shown."
  ;; We ignore errors, because `denote-refs--goto-end-of-front-matter'
  ;; might fail.
  (ignore-errors
    (save-excursion
      (denote-refs--goto-end-of-front-matter)
      (when (get-text-property (point) 'denote-refs--sections)
        (let ((end (or (next-single-property-change
                        (point) 'denote-refs--sections)
                       (point-max))))
          (when (< end (point-max))
            (setq end (1+ end)))
          (let ((mod-flag (buffer-modified-p))
                (inhibit-read-only t))
            (delete-region (point) end)
            (restore-buffer-modified-p mod-flag)))))))

(defun denote-refs--show ()
  "Show references."
  ;; We ignore errors, because `denote-refs--goto-end-of-front-matter'
  ;; might fail.
  (ignore-errors
    (denote-refs--remove)
    (save-excursion
      (denote-refs--goto-end-of-front-matter)
      (let ((begin (point))
            (mod-flag (buffer-modified-p))
            (inhibit-read-only t))
        (dolist (section denote-refs-sections)
          (pcase-exhaustive section
            ('links
             (denote-refs--render 'links))
            ('backlinks
             (denote-refs--render 'backlinks))))
        (put-text-property begin (point) 'read-only t)
        (put-text-property begin (point) 'denote-refs--sections t)
        (insert ?\n)
        (restore-buffer-modified-p mod-flag)))))

(defun denote-refs--make-path-relative (path)
  "Return a cons of relative and absolute version of PATH.

The car is PATH relative to user option `denote-directory'."
  (cons (string-remove-prefix (denote-directory) path) path))

(defun denote-refs--fetch ()
  "Fetch reference information."
  (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (dolist (section denote-refs-sections)
      (pcase-exhaustive section
        ('links
         (setq denote-refs--links
               (mapcar #'denote-refs--make-path-relative
                       (denote-link--expand-identifiers
                        (denote--link-in-context-regexp
                         (denote-filetype-heuristics
                          (buffer-file-name)))))))
        ('backlinks
         (setq denote-refs--backlinks
               (mapcar #'denote-refs--make-path-relative
                       (delete (buffer-file-name)
                               (denote--retrieve-files-in-xrefs
                                (denote-retrieve-filename-identifier
                                 (buffer-file-name)))))))))))

(defun denote-refs-update ()
  "Update Denote references shown."
  (interactive)
  (denote-refs--fetch)
  (denote-refs--show))

(defun denote-refs--idle-update (buffer)
  "Update Denote references shown on BUFFER, but don't block."
  (with-current-buffer buffer
    (while-no-input
      (denote-refs-update))
    (denote-refs--show)
    (cancel-timer denote-refs--idle-update-timer)
    (setq denote-refs--idle-update-timer
          (run-with-idle-timer
           (if (or (eq denote-refs--links 'not-ready)
                   (eq denote-refs--backlinks 'not-ready))
               (car denote-refs-update-delay)
             (cdr denote-refs-update-delay))
           t #'denote-refs--idle-update buffer))))

(define-minor-mode denote-refs-mode
  "Toggle showing links and backlinks in Denote notes."
  :lighter " Denote-Refs"
  (let ((locals '(denote-refs--links
                  denote-refs--backlinks
                  denote-refs--idle-update-timer)))
    (if denote-refs-mode
        (progn
          (mapc #'make-local-variable locals)
          (denote-refs--show)
          (add-hook 'before-save-hook #'denote-refs--remove nil t)
          (add-hook 'after-save-hook #'denote-refs--show nil t)
          (setq denote-refs--idle-update-timer
                (run-with-idle-timer
                 (car denote-refs-update-delay) t
                 #'denote-refs--idle-update (current-buffer))))
      (cancel-timer denote-refs--idle-update-timer)
      (denote-refs--remove)
      (remove-hook 'before-save-hook #'denote-refs--remove t)
      (remove-hook 'after-save-hook #'denote-refs--show t)
      (mapc #'kill-local-variable locals))))

(provide 'denote-refs)
;;; denote-refs.el ends here
