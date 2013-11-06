;;; helm-pydoc.el --- pydoc with helm interface

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-pydoc
;; Version: 0.01
;; Package-Requires: ((helm "1.0"))

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

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'helm)

(defgroup helm-pydoc nil
  "Pydoc with helm interface"
  :group 'helm)

(defvar helm-pydoc--collect-command
  (if load-file-name
      (concat (file-name-directory load-file-name) "helm-pydoc.py")
    "helm-pydoc.py"))

(defvar helm-pydoc--view-buffer "*helm python view*")

(defun helm-pydoc--collect-imported-modules ()
  (with-helm-current-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((modules nil))
        (while (re-search-forward "^\\s-*\\(?:import\\|from\\)\\s-+\\([^ \t\r\n]+\\)" nil t)
          (push (match-string 1) modules))
        (reverse modules)))))

(defun helm-pydoc--init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((cmd (format "python %s" helm-pydoc--collect-command)))
      (unless (zerop (call-process-shell-command cmd nil t))
        (error "Failed helm-pydoc--init")))))

(defun helm-pydoc--do-pydoc (module)
  (with-current-buffer (get-buffer-create helm-pydoc--view-buffer)
    (view-mode -1)
    (erase-buffer)
    (let ((cmd (concat "pydoc " module)))
      (unless (zerop (call-process-shell-command cmd nil t))
        (error (format "Failed: '%s'" cmd)))
      (goto-char (point-min))
      (view-mode +1)
      (pop-to-buffer (current-buffer)))))

(defun helm-pydoc--module-file (module)
  (with-temp-buffer
    (let* ((cmd (format "python -c 'import %s;print(%s.__file__)'"
                        module module)))
      (unless (zerop (call-process-shell-command cmd nil t))
        (error (format "Not found module '%s' source code" module)))
      (goto-char (point-min))
      (let ((modname (buffer-substring (point) (line-end-position))))
        (if (string-match "^\\(\.+\\.py\\)c$" modname)
            (match-string 1 modname)
          modname)))))

(defun helm-pydoc--view-source (candidate)
  (let* ((modfile (helm-pydoc--module-file candidate))
         (content (with-current-buffer (find-file-noselect modfile)
                    (buffer-string))))
    (with-current-buffer (get-buffer-create helm-pydoc--view-buffer)
      (view-mode -1)
      (erase-buffer)
      (insert content)
      (goto-char (point-min))
      (python-mode)
      (view-mode +1)
      (pop-to-buffer (current-buffer)))))

(defun helm-pydoc--check-imported (module)
  (save-excursion
    (let ((regexp (format "^\\s-*\\(?:from\\|import\\)\\s-+%s\\>" module)))
      (re-search-backward regexp nil t))))

(defun helm-pydoc--collect-import-modules ()
  (loop for module in (helm-marked-candidates)
        when (not (helm-pydoc--check-imported module))
        collect module into modules
        finally return (sort modules #'string<)))

(defun helm-pydoc--construct-import-statement (modules)
  (cond ((null (cdr modules))
         (format "import %s\n" (car modules)))
        (t
         (mapconcat (lambda (m) (concat "import " m)) modules "\n"))))

(defun helm-pydoc--insert-import-statement (inserted)
  (save-excursion
    (goto-char (line-end-position))
    (if (re-search-backward "^\\s-*\\(?:from\\|import\\)\\s-+" nil t)
        (forward-line 1)
      (helm-pydoc--skip-comments))
    (insert inserted)))

(defun helm-pydoc--skip-comments ()
  (goto-char (point-min))
  (loop while (string-match "^#" (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))
        do
        (forward-line 1)))

(defun helm-pydoc--import-module (candidate)
  (let* ((modules (helm-pydoc--collect-import-modules))
         (statements (helm-pydoc--construct-import-statement modules)))
    (helm-pydoc--insert-import-statement statements)))

(defun helm-pydoc--construct-from-import (module imports &optional name)
  (format "from %s import %s%s\n"
          module imports
          (if name
              (format " as name")
            "")))

(defun helm-pydoc--from-import-module (candidate)
  (let* ((imports (read-string (format "Identifiers in %s: " candidate)))
         (statement (helm-pydoc--construct-from-import candidate imports)))
    (helm-pydoc--insert-import-statement statement)))

(defun helm-pydoc--from-import-as-module (candidate)
  (let* ((imports (read-string (format "Identifiers in %s: " candidate)))
         (name (read-string (format "As name [%s]: " candidate)))
         (statement (helm-pydoc--construct-from-import
                     candidate imports name)))
    (helm-pydoc--insert-import-statement statement)))

(define-helm-type-attribute 'pydoc
  '((action . (("Pydoc Module" . helm-pydoc--do-pydoc)
               ("View Source Code" . helm-pydoc--view-source)
               ("Import Module(import module)" . helm-pydoc--import-module)
               ("Import Module(from module import identifiers)"
                . helm-pydoc--from-import-module)
               ("Import Module(from module import identifiers as name)"
                . helm-pydoc--from-import-as-module)))
    "pydoc helm attribute"))

(defvar helm-pydoc--imported-source
  '((name . "Imported Modules")
    (candidates . helm-pydoc--collect-imported-modules)
    (type . pydoc)
    (candidate-number-limit . 9999)))

(defvar helm-pydoc--installed-source
  '((name . "Installed Modules")
    (init . helm-pydoc--init)
    (candidates-in-buffer)
    (type . pydoc)
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-pydoc ()
  (interactive)
  (helm :sources '(helm-pydoc--imported-source helm-pydoc--installed-source)
        :buffer (get-buffer-create "*helm pydoc*")))

(provide 'helm-pydoc)

;;; helm-pydoc.el ends here
