;;; helm-pydoc.el --- pydoc with helm interface

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-pydoc
;; Version: 0.01

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
        (error (format "Not found %s file" module)))
      (goto-char (point-min))
      (let ((modname (buffer-substring (point) (line-end-position))))
        (if (string-match "^\\(\.+\\.py\\)c$" modname)
            (match-string 1 modname)
          modname)))))

(defun helm-c-pydoc-view-source (module)
  (let ((modfile (helm-c-pydoc-module-file module)))
    (with-current-buffer helm-c-pydoc-view-buffer
      (insert-file modfile)
      (python-mode)))
  (pop-to-buffer helm-c-pydoc-view-buffer))

(defvar helm-c-pydoc-source
  '((name . "helm pydoc")
    (init . helm-c-pydoc-init)
    (candidates-in-buffer)
    (action . (("Pydoc Module" . helm-c-pydoc-do-pydoc)
               ("View Source Code" . helm-c-pydoc-view-source)))
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-pydoc ()
  (interactive)
  (let ((buf (get-buffer-create "*helm pydoc*")))
    (helm :sources '(helm-c-pydoc-source) :buffer buf)))

(provide 'helm-pydoc)

;;; helm-pydoc.el ends here
