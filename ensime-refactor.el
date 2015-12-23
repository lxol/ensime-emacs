;;; ensime-refactor.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros)
  (require 'diff-mode))

(defvar ensime-refactor-id-counter 0
  "Each refactoring is given a unique id.")

(defvar ensime-refactor-info-buffer-name "*ENSIME-Refactoring*")

(defvar ensime-refactor-info-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") (lambda()(interactive)
                               (funcall continue-refactor)
                               (ensime-popup-buffer-quit-function)
                               ))
    (define-key map (kbd "q") (lambda()(interactive)
                               (funcall cancel-refactor)
                               (ensime-popup-buffer-quit-function)
                               ))
   map)
  "Key bindings for the refactor confirmation popup.")

(defun ensime-refactor-notify-failure (result)
  (message "Refactoring failed: %s" (plist-get result :reason)))


(defun ensime-refactor-organize-imports ()
  "Do a syntactic organization of the imports in the current buffer."
  (interactive)
  (cond ((ensime-visiting-java-file-p)
	 (ensime-refactor-organize-java-imports)
	 (message "Organized."))

	(t
	 (ensime-refactor-prepare
	  'organizeImports
	  `(file ,buffer-file-name)))))

(defun ensime-refactor-organize-java-imports ()
  "Sort all import statements lexicographically."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^\\s-*package\\s-" nil t)
    (goto-char (point-at-eol))
    (let ((p (point)))

      ;; Advance past all imports
      (while (looking-at "[\n\t ]*import\\s-\\(.+\\)\n")
	(search-forward-regexp "import" nil t)
	(goto-char (point-at-eol)))
      (sort-lines nil p (point)))))


(defun ensime-refactor-rename (&optional new-name)
  "Rename a symbol, project-wide."
  (interactive)
  (let ((sym (ensime-sym-at-point)))
    (if sym
	(let* ((start (plist-get sym :start))
	       (end (plist-get sym :end))
	       (old-name (plist-get sym :name))
	       (name (or new-name
			 (read-string (format "Rename '%s' to: " old-name)))))
	  (ensime-refactor-prepare
	   'rename
	   `(file ,buffer-file-name
	     start ,(ensime-externalize-offset start)
             end ,(ensime-externalize-offset end)
             newName ,name)))
      (message "Please place cursor on a symbol."))))


(defun ensime-refactor-inline-local ()
  "Get rid of an intermediate variable."
  (interactive)
  (let ((sym (ensime-sym-at-point)))
    (if sym
	(let* ((start (plist-get sym :start))
	       (end (plist-get sym :end)))
	  (ensime-refactor-prepare
	   'inlineLocal
	   `(file ,buffer-file-name
	     start ,(ensime-externalize-offset start)
	     end ,(ensime-externalize-offset end))))
      (message "Please place cursor on a local value."))))

(defun ensime-refactor-extract-method ()
  "Extract a range of code into a method."
  (interactive)
  (let ((name (read-string "Name of method: ")))
    (destructuring-bind (start end)
        (ensime-computed-range)
      (ensime-refactor-prepare
       'extractMethod
       `(file ,buffer-file-name
         start ,start
         end ,end
         methodName ,name)))))

(defun ensime-refactor-extract-local ()
  "Extract a range of code into a val."
  (interactive)
  (let ((name (read-string "Name of local value: ")))
    (destructuring-bind (start end)
        (ensime-computed-range)
      (ensime-refactor-prepare
       'extractLocal
       `(file ,buffer-file-name
         start ,start
         end ,end
         name ,name)))))

(defun ensime-refactor-add-import (&optional qual-name)
  "Insert import statement."
  (interactive)
  (let ((qualified-name
         (or qual-name
             (read-string "Qualified name of type to import: "))))
    (let ((result (ensime-refactor-prepare
                   'addImport
                   `(file ,buffer-file-name
                     qualifiedName ,qualified-name) t t)))
      (ensime-refactor-handle-result result))))

(defun ensime-refactor-prepare (refactor-type params &optional non-interactive blocking)
  (if (buffer-modified-p) (ensime-write-buffer nil t))
  (incf ensime-refactor-id-counter)
  (if (not blocking) (message "Please wait..."))
  (ensime-rpc-refactor-prepare
   ensime-refactor-id-counter
   refactor-type
   params
   non-interactive
   (if non-interactive
       'ensime-refactor-handle-result
     'ensime-refactor-prepare-handler)
   blocking))

(defun ensime-refactor-prepare-handler (result)
  (let ((refactor-type (plist-get result :refactor-type))
	(status (plist-get result :status))
	(id (plist-get result :procedure-id))
	(changes (plist-get result :changes)))
    (if (equal status 'success)
	(let ((cont `(lambda () (ensime-rpc-refactor-exec
                                 ,id ',refactor-type
                                 'ensime-refactor-handle-result)))
              (cancel `(lambda () (ensime-rpc-refactor-cancel ,id))))

          (ensime-with-popup-buffer
           (ensime-refactor-info-buffer-name t t)
           ;; Override ensime-popup-buffer-mode's normal keymap
           ;; because of "q"
           (add-to-list
            'minor-mode-overriding-map-alist
            (cons 'ensime-popup-buffer-mode ensime-refactor-info-map))
           (set (make-local-variable 'cancel-refactor) cancel)
           (set (make-local-variable 'continue-refactor) cont)
           (ensime-refactor-populate-confirmation-buffer
            refactor-type changes)
           (goto-char (point-min)))

          (ensime-event-sig :refactor-at-confirm-buffer))

      (ensime-refactor-notify-failure result))))


(defun ensime-refactor-handle-result (result)
  (let ((touched (plist-get result :touched-files)))
    (ensime-revert-visited-files touched t)
    (ensime-event-sig :refactor-done touched)
    (kill-buffer ensime-refactor-info-buffer-name)))

(defun ensime-refactor-populate-confirmation-buffer (refactor-type changes)
  (let ((header
	 "Please review the proposed changes."))

    (ensime-insert-with-face
     (concat header " (c to confirm, q to cancel)")
     'font-lock-constant-face)
    (insert "\n\n\n")

    (if (null changes)
	(insert "Nothing to be done.")
      (ensime-insert-change-list changes))))

(defun ensime-preview-refactor-rename (&optional new-name)
  "Rename a symbol, project-wide."
  (interactive)
  (let ((sym (ensime-sym-at-point)))
    (if sym
        (let* ((start (plist-get sym :start))
               (end (plist-get sym :end))
               (old-name (plist-get sym :name))
               (name (or new-name
                         (read-string (format "Rename '%s' to: " old-name)))))
          (ensime-preview-refactor
           'rename
           `(file ,buffer-file-name
                  start ,(ensime-externalize-offset start)
                  end ,(ensime-externalize-offset end)
                  newName ,name)))
      (message "Please place cursor on a symbol."))))

(defun ensime-preview-refactor-organize-imports ()
  "Do a syntactic organization of the imports in the current buffer."
  (interactive)
  (cond ((ensime-visiting-java-file-p)
         (ensime-refactor-organize-java-imports)
         (message "Organized."))
        (t
         (ensime-preview-refactor
          'organizeImports
          `(file ,buffer-file-name)))))

(defun ensime-preview-refactor-extract-local ()
  "Extract a range of code into a val."
  (interactive)
  (let ((name (read-string "Name of local value: ")))
    (destructuring-bind (start end)
        (ensime-computed-range)
      (ensime-preview-refactor
       'extractLocal
       `(file ,buffer-file-name
              start ,start
              end ,end
              name ,name)))))

(defun ensime-preview-refactor-extract-method ()
  "Extract a range of code into a method."
  (interactive)
  (let ((name (read-string "Name of method: ")))
    (destructuring-bind (start end)
        (ensime-computed-range)
      (ensime-preview-refactor
       'extractMethod
       `(file ,buffer-file-name
              start ,start
              end ,end
              methodName ,name)))))

(defun ensime-preview-refactor-inline-local ()
  "Get rid of an intermediate variable."
  (interactive)
  (let ((sym (ensime-sym-at-point)))
    (if sym
        (let* ((start (plist-get sym :start))
               (end (plist-get sym :end)))
          (ensime-preview-refactor
           'inlineLocal
           `(file ,buffer-file-name
                  start ,(ensime-externalize-offset start)
                  end ,(ensime-externalize-offset end))))
      (message "Please place cursor on a local value."))))

(defun ensime-preview-refactor (refactor-type params &optional non-interactive blocking)
  (if (buffer-modified-p) (ensime-write-buffer nil t))
  (incf ensime-refactor-id-counter)
  (if (not blocking) (message "Please wait..."))
  (ensime-rpc-preview-refactor
   ensime-refactor-id-counter
   params
   non-interactive
   'ensime-preview-refactor-handler
   blocking))

(defun ensime-preview-refactor-handler (result)
  (let ((refactor-type (plist-get result :refactor-type))
        (id (plist-get result :procedure-id))
        (diff (plist-get result :diff)))
    (if  (ensime-preview-refactor-auto-apply-p refactor-type diff)
        (ensime-with-popup-buffer (ensime-refactor-info-buffer-name
                                   nil nil 'diff-mode)
                                  (insert-file-contents diff)
                                  (ensime-preview-refactor-apply-all-hunks)
                                  (ensime-preview-refactor-save-all-diff-source-buffers))
      (ensime-with-popup-buffer (ensime-refactor-info-buffer-name
                                 nil t 'diff-mode)
                                (insert-file-contents diff)))
    (delete-file diff)
    (ensime-event-sig :refactor-preview-done diff)))

(defun ensime-preview-refactor-auto-apply-p (refactor-type diff)
  "Check if refactor changes should be applied automatically"
  (or (memq refactor-type ensime-refactor-auto-apply-types)
      (with-temp-buffer
        (insert-file-contents diff)
        (goto-char (point-min))
        (or (re-search-forward diff-file-header-re nil t
                               ensime-refactor-auto-apply-file-limit)
            (progn (goto-char (point-min))
                   (re-search-forward diff-hunk-header-re nil t
                                      ensime-refactor-auto-apply-hunk-limit))))))

(defun ensime-preview-refactor-apply-all-hunks ()
  "Apply or undo all hunks in the *ENSIME-Refactoring* buffer."
  (interactive)
  (make-local-variable 'diff-advance-after-apply-hunk)
  (setq diff-advance-after-apply-hunk nil)
  (goto-char (point-min))
  (while (re-search-forward diff-hunk-header-re nil t)
    (diff-apply-hunk)))

(defun ensime-preview-refactor-save-all-diff-source-buffers ()
  "Save all source buffers in the *ENSIME-Refactoring* buffer.
Do not asks user about each one if
`ensime-refactor-save-with-no-questions' is t."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward diff-file-header-re nil t)
    (-when-let (src-buffer-name (buffer-name (car (diff-find-source-location))))
      (save-some-buffers
       ensime-refactor-save-with-no-questions
       (-partial (lambda (src-buffer-name)
                   (equal src-buffer-name (buffer-name)))
                 src-buffer-name)))))


(provide 'ensime-refactor)

;; Local Variables:
;; End:

