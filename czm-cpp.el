;;; czm-cpp.el --- cpp convenience functions, e.g., for project management  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/czm-cpp.el
;; Package-Requires: ((emacs "29.1") cmake-build)
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

;; cpp convenience, e.g., for project management.

;;; Code:

(require 'cmake-build)

(defun czm-cpp-init-header ()
  (interactive)
  (let ((guard
	 (upcase
	  (replace-regexp-in-string
	   "[.-]" "_" 
	   (concat
	    (project-name (project-current))
	    "_"
	    (file-name-nondirectory (buffer-file-name)))
	   )))
	(notice nil)
	)
    (goto-char (point-min))
    (if notice (progn (insert notice) (newline)))
    (insert (concat "#ifndef " guard "\n" "#define " guard "\n\n"))
    (goto-char (point-max))
    (insert (concat "#endif // " guard "\n"))
    (forward-line -2)))

(defun czm-cpp-make-snake-case ()
  (interactive)
  (let ((symb (thing-at-point 'symbol t)))
    (unless symb
      (error "No symbol at point"))
    (let* ((beg (car symb))
	   (end (cdr symb))
	   (camelCase (buffer-substring beg end))
	   (snake_case (s-snake-case camelCase)))
      (eglot-rename snake_case))))

(defun czm-rename-then-save-all ()
  (interactive)
  (call-interactively 'lsp-rename)
  (save-some-buffers t))

(defun czm-cpp-new-project-relative (new-directory-name)
  (interactive "sNew directory name (relative to current directory): ")
  (let* ((new-directory-absolute (expand-file-name new-directory-name))
         (template-directory-name "~/code/scratch/template/")
         (template-src-directory-name "~/code/scratch/template/src/")
         (new-src-directory-name (concat new-directory-absolute "/src/"))
         (files-to-copy '(".clang-format" "CMakeLists.txt" ".cmake-build.el" ".project" ".gitignore"))
         (src-files-to-copy '("CMakeLists.txt" "main.cpp")))
    (unless (file-exists-p new-directory-absolute)
      (make-directory new-directory-absolute t))
    (unless (file-exists-p new-src-directory-name)
      (make-directory new-src-directory-name t))
    (dolist (file files-to-copy)
      (copy-file (concat template-directory-name file) new-directory-absolute))
    (dolist (file src-files-to-copy)
      (copy-file (concat template-src-directory-name file) new-src-directory-name))
    (magit-init new-directory-absolute)
    (find-file (concat new-src-directory-name "main.cpp"))
    (cmake-build-clear-cache-and-configure)
    (cmake-build-set-config 'all)))

(defun czm-cpp-new-project (new-directory-name)
  (interactive "sNew directory name: ")
  (let
      ((template-directory-name (format-time-string "~/code/scratch/template/"))
       (template-src-directory-name (format-time-string "~/code/scratch/template/src/"))
       (new-src-directory-name (concat new-directory-name "src/"))
       (files-to-copy '(".clang-format" "CMakeLists.txt" ".cmake-build.el" ".project" ".gitignore"))
       (src-files-to-copy '("CMakeLists.txt" "main.cpp")))
    (make-directory new-directory-name)
    (make-directory new-src-directory-name)
    (dolist (file files-to-copy)
      (copy-file (concat template-directory-name file) new-directory-name))
    (dolist (file src-files-to-copy)
      (copy-file (concat template-src-directory-name file) new-src-directory-name))
    ;; TODO: replace the following with something using eglot?
    ;; (lsp-workspace-folders-add new-directory-name)
    (magit-init new-directory-name)
    (find-file (concat new-src-directory-name "main.cpp"))
    (cmake-build-clear-cache-and-configure)
    (cmake-build-set-config 'all)
    ;; (cmake-build-current)
    ))  

(defun czm-cpp-scratch ()
  (interactive)
  (let ((directory (format-time-string "~/code/scratch/%Y-%m-%d-%H%M%S")))
    (setq directory (read-string "Directory: " directory))
    (czm-cpp-new-project
     (concat directory "/")))
  )

(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))

(provide 'czm-cpp)
;;; czm-cpp.el ends here
