;;; conner.el --- define and run project specific commands  -*- lexical-binding: t -*-

;; Authors: Tomás Ralph <tomasralph2000@gmail.com>
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/tralph3/conner
;; Keywords: tools

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Conner is a Command Runner. Define your commands in a .conner file
;; and execute them in your project.
;;
;; NOTE: The source code for the loading of environment variables is
;; taken straight out of diasjorge's load-env-vars
;; https://github.com/diasjorge/emacs-load-env-vars. I didn't want to
;; include it as a dependency due to its small size, and needing some
;; modification in order to fit this package's purposes.

;;; Code:

(defgroup conner nil
  "Conner is a Command Runner for GNU Emacs."
  :link '(url-link :tag "Homepage" "https://github.com/tralph3/conner")
  :prefix "conner-")

(defcustom conner-file-name ".conner"
  "Filename where the launch commands will be defined."
  :type 'string)

(defcustom conner-env-file ".env"
  "Filename where env variables are defined."
  :type 'string)

(defcustom conner-read-env-file t
  "If non-nil, conner will look for a `conner-env-file' in the
provided root dir and load any environment variables within,
passing them to every command when called.

This will not modify `process-environment'. The changes will only
apply and be visible to conner commands."
  :type 'boolean)

(defvar conner--env-var-regexp
  (rx
   line-start
   (0+ space)
   (optional "export" (0+ space)) ;; optional export
   (group (1+ (in "_" alnum))) ;; key
   (or
    (and (0+ space) "=" (0+ space))
    (and ":" (1+ space))) ;; separator
   (or
    (and "'" (group (0+ (or "\\'" (not (any "'"))))) "'") ;; single quoted value
    (and ?\" (group (0+ (or "\\\"" (not (any "\""))))) ?\") ;; double quoted value
    (group (1+ (not (in "#" "\n" space)))) ;; unquoted value
    (0+ space)
    (optional "#" (0+ any))))
  "Regexp to match env vars in file.")

(defvar conner--commands nil
  "List of commands of the last `conner-file-name' file read.")

(defun conner--read-commands (root-dir)
  "Reads the contents of ROOT-DIR's `conner-file-name' file into
`conner--commands'."
  (let ((conner-file (file-name-concat root-dir conner-file-name)))
    (setq conner--commands
          (when (file-exists-p conner-file)
            (with-temp-buffer
              (insert-file-contents conner-file)
              (read (current-buffer)))))))

(defun conner--write-commands (root-dir)
  "Writes the contents of `conner--commands' to ROOT-DIR's
`conner-file-name' file."
  (let ((conner-file (file-name-concat root-dir conner-file-name))
        (commands conner--commands))
    (with-temp-buffer
      (insert (pp commands))
      (write-file conner-file))))

(defun conner--read-env-file (root-dir)
  "Reads the `conner-env-file' located in ROOT-DIR and returns a
list of strings formatted for use in `process-environment'."
  (setq-local conner--env-var-list nil)
  (let* ((env-file (file-name-concat root-dir conner-env-file))
         (env-vars (conner--load-env-vars env-file)))
    (dolist (element env-vars)
      (let ((key (car element))
            (value (cadr element)))
        (add-to-list 'conner--env-var-list (concat key "=" value))))
    conner--env-var-list))

(defun conner--get-env-vars-in-buffer (regexp)
  "Get a list of all REGEXP matches in a buffer."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let (matches)
        (while (re-search-forward regexp nil t)
          (push (list (match-string-no-properties 1) (or (match-string-no-properties 2) (match-string-no-properties 3) (match-string-no-properties 4))) matches))
        matches))))

(defun conner--load-env-vars (env-file-path)
  "Load environment variables found in ENV-FILE-PATH. Returns a
 list containing the results."
  (with-temp-buffer
    (when (file-exists-p env-file-path)
      (insert-file-contents env-file-path))
    (conner--get-env-vars-in-buffer conner--env-var-regexp)))

(defun conner--annotation-function (candidate)
  "Reads the associated command for CANDIDATE and formats the string
for use in the minibuffer annotations."
  (let* ((max-width (apply #'max (mapcar #'length (mapcar #'car conner--commands))))
         (indent (make-string (- max-width (length candidate)) ?\s))
         (command (cdr (assoc candidate conner--commands)))
         (tabs (make-string 6 ?\t)))
    (format "%s%s%s" indent tabs command)))

(defun conner-run-project-command (&optional project)
  "Project aware variant of `conner-run-command'. Will use
PROJECT's root dir as an argument for the corresponding function.

If no PROJECT is provided, it will use the value of
`project-current'. If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-run-command (project-root project))))

(defun conner-add-project-command (&optional project)
  "Project aware variant of `conner-add-command'. Will use
PROJECT's root dir as an argument for the corresponding function.

If no PROJECT is provided, it will use the value of
`project-current'. If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-add-command (project-root project))))

(defun conner-delete-project-command (&optional project)
  "Project aware variant of `conner-delete-command'. Will use
PROJECT's root dir as an argument for the corresponding function.

If no PROJECT is provided, it will use the value of
`project-current'. If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-delete-command (project-root project))))

(defun conner-update-project-command (&optional project)
  "Project aware variant of `conner-update-command'. Will use
PROJECT's root dir as an argument for the corresponding function.

If no PROJECT is provided, it will use the value of
`project-current'. If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-update-command (project-root project))))

(defun conner-run-command (root-dir &optional command-name)
  "Runs command COMMAND-NAME from ROOT-DIR's
`conner-file-name'. The user will be prompted for every optional
parameter not specified.

The command will be ran in ROOT-DIR.

If `conner-read-env-file' is non-nil, it will read ROOT-DIR's
`conner-env-file' before executing the command."
  (interactive "D")
  (conner--read-commands root-dir)
  (let* ((completion-extra-properties '(:annotation-function conner--annotation-function))
         (process-environment (if conner-read-env-file
                                  (append (conner--read-env-file root-dir) process-environment)
                                process-environment))
         (names (mapcar #'car conner--commands))
         (command-name (or command-name (completing-read "Select a command: " names)))
         (command (cdr (assoc command-name conner--commands)))
         (default-directory root-dir))
    (compile command)))

(defun conner-add-command (root-dir &optional command-name command)
  "Adds command COMMAND-NAME with value COMMAND to ROOT-DIR's
`conner-file-name'. The user will be prompted for every optional
parameter not specified."
  (interactive "D")
  (conner--read-commands root-dir)
  (let ((command-name (or command-name (read-string "Enter command name: ")))
        (command (or command (read-string "Enter command: "))))
    (if (assoc command-name conner--commands)
        (warn "A command with this name already exists")
      (progn (add-to-list 'conner--commands `(,command-name . ,command))
             (conner--write-commands root-dir)))))

(defun conner-delete-command (root-dir &optional command-name)
  "Deletes command COMMAND-NAME from ROOT-DIR's
`conner-file-name'. The user will be prompted for every optional
parameter not specified."
  (interactive "D")
  (conner--read-commands root-dir)
  (let* ((completion-extra-properties '(:annotation-function conner--annotation-function))
         (names (mapcar #'car conner--commands))
         (command-name (or command-name (completing-read "Delete command: " names)))
         (element (assoc command-name conner--commands)))
    (setq conner--commands (delete element conner--commands))
    (conner--write-commands root-dir)))

(defun conner-update-command (root-dir &optional command-name new-name new-command)
  "Updates command COMMAND-NAME from ROOT-DIR's `conner-file-name'
to NEW-NAME and NEW-COMMAND. The user will be prompted for every
optional parameter not specified.

If a non-existent COMMAND-NAME is provided, it will be created
instead."
  (interactive "D")
  (conner--read-commands root-dir)
  (let* ((completion-extra-properties '(:annotation-function conner--annotation-function))
         (names (mapcar #'car conner--commands))
         (command-name (or command-name (completing-read "Update command: " names)))
         (command (cdr (assoc command-name conner--commands)))
         (new-name (or new-name (read-string "Enter new name: " command-name)))
         (new-command (or new-command (read-string "Enter new command: " command))))
    (conner-delete-command root-dir command-name)
    (conner-add-command root-dir new-name new-command)))


(provide 'conner)

;;; conner.el ends here
