;;; helm-pydoc.el --- pydoc with helm interface

;; Copyright (C) 2012 by Syohei YOSHIDA

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

(defvar helm-c-pydoc-collect-command
  (if load-file-name
      (concat (file-name-directory load-file-name) "helm-pydoc.py")
    "helm-pydoc.py"))

(defvar helm-c-pydoc-view-buffer
  (get-buffer-create "*helm python view*"))

(defun helm-c-pydoc-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (let* ((cmd (format "python %s" helm-c-pydoc-collect-command))
           (ret (call-process-shell-command cmd nil t)))
      (unless (= ret 0)
        (error "Failed helm-c-pydoc-init")))))

(defun helm-c-pydoc-do-pydoc (module)
  (with-current-buffer helm-c-pydoc-view-buffer
    (let ((ret (call-process "pydoc" nil t t module)))
      (unless (= ret 0)
        (error (format "Failed: 'pydoc %s'" module)))
      (goto-char (point-min))))
  (pop-to-buffer helm-c-pydoc-view-buffer))

(defun helm-c-pydoc-module-file (module)
  (with-temp-buffer
    (let* ((cmd (format "python -c 'import %s;print(%s.__file__)'"
                        module module))
           (ret (call-process-shell-command cmd nil t)))
      (unless (= ret 0)
        (error (format "Not found module '%s' source code" module)))
      (goto-char (point-min))
      (let ((modname (buffer-substring (point) (line-end-position))))
        (if (string-match "^\\(\.+\\.py\\)c$" modname)
            (match-string 1 modname)
          modname)))))

(defun helm-c-pydoc-view-source (candidate)
  (let ((modfile (helm-c-pydoc-module-file candidate)))
    (with-current-buffer helm-c-pydoc-view-buffer
      (insert-file modfile)
      (python-mode)))
  (pop-to-buffer helm-c-pydoc-view-buffer))

(defun helm-c-pydoc-check-imported (module)
  (save-excursion
    (let ((regexp (format "^\\s-*\\(from\\|import\\)\\s-+%s\\>" module)))
      (re-search-backward regexp nil t))))

(defun helm-c-pydoc-collect-import-modules ()
  (loop for module in (helm-marked-candidates)
        when (not (helm-c-pydoc-check-imported module))
        collect module into modules
        finally return (sort modules #'string<)))

(defun helm-c-pydoc-construct-import-statement (modules)
  (cond ((null (cdr modules))
         (format "import %s\n" (car modules)))
        (t
         (mapconcat (lambda (m) (format "import %s" m)) modules "\n"))))

(defun helm-c-pydoc-insert-import-statement (inserted)
  (save-excursion
    (goto-char (line-end-position))
    (if (re-search-backward "^\\s-*\\(from\\|import\\)\\s-+" nil t)
        (forward-line 1)
      (helm-c-pydoc-skip-comments))
    (insert inserted)))

(defun helm-c-pydoc-skip-comments ()
  (goto-char (point-min))
  (loop while (string-match "^#" (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))
        do
        (forward-line 1)))

(defun helm-c-pydoc-import-module (candidate)
  (let* ((modules (helm-c-pydoc-collect-import-modules))
         (statements (helm-c-pydoc-construct-import-statement modules)))
    (helm-c-pydoc-insert-import-statement statements)))

(defun helm-c-pydoc-construct-from-import (module imports &optional name)
  (format "from %s import %s%s\n"
          module imports
          (if name
              (format " as name")
            "")))

(defun helm-c-pydoc-from-import-module (candidate)
  (let* ((imports (read-string (format "Identifiers in %s: " candidate)))
         (statement (helm-c-pydoc-construct-from-import candidate imports)))
    (helm-c-pydoc-insert-import-statement statement)))

(defun helm-c-pydoc-from-import-as-module (candidate)
  (let* ((imports (read-string (format "Identifiers in %s: " candidate)))
         (name (read-string (format "As name [%s]: " candidate)))
         (statement (helm-c-pydoc-construct-from-import
                     candidate imports name)))
    (helm-c-pydoc-insert-import-statement statement)))

(defvar helm-c-pydoc-source
  '((name . "helm pydoc")
    (init . helm-c-pydoc-init)
    (candidates-in-buffer)
    (action . (("Pydoc Module" . helm-c-pydoc-do-pydoc)
               ("Import Module(import module)" .
                helm-c-pydoc-import-module)
               ("Import Module(from module import identifiers)"
                . helm-c-pydoc-from-import-module)
               ("Import Module(from module import identifiers as name)"
                . helm-c-pydoc-from-import-as-module)
               ("View Source Code" . helm-c-pydoc-view-source)))
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-pydoc ()
  (interactive)
  (let ((buf (get-buffer-create "*helm pydoc*")))
    (helm :sources '(helm-c-pydoc-source) :buffer buf)))

(provide 'helm-pydoc)

;;; helm-pydoc.el ends here
