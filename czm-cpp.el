;;; czm-cpp.el --- cpp convenience functions, e.g., for project management  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-cpp.el
;; Package-Requires: ((emacs "29.1") (cmake-build "1.0") (s "1.13.1"))
;; Keywords: c, tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; cpp convenience functions, e.g., for project management.

;;; Code:

(require 'cmake-build)

(defcustom czm-cpp-template-directory
  (let ((dir (if load-file-name
                 (file-name-directory load-file-name)
               default-directory)))
    (expand-file-name "template" dir))
  "Directory containing the C++ project template."
  :type 'directory
  :group 'czm-cpp)

;;;###autoload
(defun czm-cpp-new-project (dir)
  "Create a new C++ project in directory DIR."
  (interactive "sNew directory name: ")
  (let* ((template-directory (file-name-as-directory czm-cpp-template-directory))
         (new-directory (file-name-as-directory dir)))
    (make-directory new-directory t)
    (dolist (file (directory-files template-directory))
      (unless (member file '("." ".."))
        (let ((source (expand-file-name file template-directory))
              (destination (expand-file-name file new-directory)))
          (if (file-directory-p source)
              (copy-directory source destination t t)
            (copy-file source destination t t)))))
    (find-file
     (expand-file-name "main.cpp"
                       (file-name-as-directory
                        (expand-file-name "src" new-directory))))
    (cmake-build-clear-cache-and-configure)
    (cmake-build-set-config 'all)))

(defcustom czm-cpp-scratch-directory
  (expand-file-name "~/scratch/cpp")
  "Directory to create new C++ projects in."
  :type 'directory
  :group 'czm-cpp)

;;;###autoload
(defun czm-cpp-new-project-scratch ()
  "Create a new C++ project in the scratch directory with a timestamped name."
  (interactive)
  (let ((directory
         (expand-file-name
          (format-time-string "%Y-%m-%d-%H%M%S")
          (file-name-as-directory czm-cpp-scratch-directory))))
    (setq directory (read-string "Directory: " directory))
    (czm-cpp-new-project directory)))

;;;###autoload
(defun czm-cpp-new-project-relative (dir)
  "Create a new C++ project in directory DIR."
  (interactive "sNew directory name (relative to current directory): ")
  (czm-cpp-new-project (expand-file-name dir)))

;;;###autoload
(defun czm-cpp-init-header ()
  "Insert a header guard at the top of the file."
  (interactive)
  (let ((guard
	        (upcase
	         (replace-regexp-in-string
	          "[.-]" "_"
	          (concat
	           (project-name (project-current))
	           "_"
	           (file-name-nondirectory (buffer-file-name))))))
	       (notice nil))
    (goto-char (point-min))
    (if notice (progn (insert notice) (newline)))
    (insert (concat "#ifndef " guard "\n" "#define " guard "\n\n"))
    (goto-char (point-max))
    (insert (concat "#endif // " guard "\n"))
    (forward-line -2)))

(require 's)
(require 'eglot)

;;;###autoload
(defun czm-cpp-make-snake-case ()
  "Convert the symbol at point to snake_case."
  (interactive)
  (let ((symb (thing-at-point 'symbol t)))
    (unless symb
      (error "No symbol at point"))
    (let* ((beg (car symb))
	          (end (cdr symb))
	          (camelCase (buffer-substring beg end))
	          (snake_case (s-snake-case camelCase)))
      (eglot-rename snake_case))))

(provide 'czm-cpp)
;;; czm-cpp.el ends here
