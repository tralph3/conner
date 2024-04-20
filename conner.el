;;; conner.el --- Define and run project specific commands  -*- lexical-binding: t -*-

;; Authors: Tomás Ralph <tomasralph2000@gmail.com>
;; Created: 2024
;; Version: 0.2
;; Package-Requires: ((emacs "28.1"))
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

;; Conner is a Command Runner for GNU Emacs.
;;
;; Conner allows you to define arbitrary commands for each of your
;; projects.  Every project could have a different way to compile it,
;; to run it, to test it, to prettify it, to watch for changes, to
;; debug it, to install it, or any other thing.  With conner, you can
;; define a command for each of these actions, or any other you want.
;;
;; Commands are defined in the conner file, by default called .conner,
;; situated at the root of your project.  Inside it, you'll find a Lisp
;; object that contains a list of command names, and the commands
;; themselves.
;;
;; Conner also provides a multitude of functions to add, delete,
;; update, and of course, run these commands from within Emacs.  It
;; integrates with project.el, so you can run these commands on
;; arbitrary folders, or have it automatically detect the current
;; project's root.
;;
;; Additionally, conner also has support for .env files.  By default,
;; conner will look in the root directory of your project for a .env
;; file and load any environment variables found within.  These
;; variables are then accessible to conner commands, and won't pollute
;; the regular Emacs session.


;;; Code:

(defgroup conner nil
  "Conner is a Command Runner for GNU Emacs."
  :link '(url-link :tag "Homepage" "https://github.com/tralph3/conner")
  :group 'development
  :prefix "conner-")

(defcustom conner-file-name ".conner"
  "Filename where the launch commands will be defined."
  :type 'string)

(defcustom conner-env-file ".env"
  "Filename where env variables are defined."
  :type 'string)

(defcustom conner-read-env-file t
  "Wether to read env files before running commands.

If non-nil, conner will look for a `conner-env-file' in the provided
root dir and load any environment variables within, passing them to
every command when called.

This will not modify `process-environment'.  The changes will only
apply and be visible to conner commands."
  :type 'boolean)

(defcustom conner-default-file-behavior 'project
  "Where should conner operate by default.

If set to 'project', conner will read, write and update commands
defined in the `conner-file-name' of the directory by
default.  You would need to pass \\[universal-argument] to these
functions to have them operate on the associated local file.

If set to 'local', the inverse is true.  It will operate on the
local file by default, and you will need to pass
\\[universal-argument] to have it operate on the project file."
  :type '(choice (const :tag "Project file" project)
                 (const :tag "Local file" local)))

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

(defvar conner--current-command nil
  "The name of the command currently being executed.
Does not get reset after the command finishes.")

(setq-local compilation-buffer-name-function #'conner--make-compilation-buffer-name)

(defun conner--make-compilation-buffer-name (_)
  "Create the buffer name based on `conner--current-command'."
  (concat "*conner-compilation-" conner--current-command "*"))

(defun conner--construct-file-path (root-dir)
  "Return the path to ROOT-DIR's `conner-file-name'."
  (file-name-concat (expand-file-name root-dir) conner-file-name))

(defun conner--construct-local-file-path (root-dir)
  "Return the path to the local conner file associated with ROOT-DIR."
  (let* ((backup-directory-alist `(,`("." . ,(file-name-concat user-emacs-directory "conner"))))
	     (name (make-backup-file-name-1 (expand-file-name root-dir))))
    (concat name ".#conner#")))

(defun conner--update-commands-from-disk (root-dir &optional read-project read-local)
  "Update `conner--commands' with values stored on disk.

If READ-PROJECT is non-nil, update with ROOT-DIR's
`conner-file-name' contents.

If READ-LOCAL is non-nil, update with ROOT-DIR's associated local
file.

If both options are nil, read both files and append project
specific commands to the local ones, making the local ones take
precedence."
  (if (or read-project read-local)
      (let ((project-contents (and read-project (conner--read-commands (conner--construct-file-path root-dir))))
            (local-contents (and read-local (conner--read-commands (conner--construct-local-file-path root-dir)))))
        (setq conner--commands (append local-contents project-contents)))
    (let ((project-contents (conner--read-commands (conner--construct-file-path root-dir)))
          (local-contents (conner--read-commands (conner--construct-local-file-path root-dir))))
      (setq conner--commands (append local-contents project-contents)))))

(defun conner--read-commands (conner-file)
  "Read the contents of CONNER-FILE."
  (when (file-exists-p conner-file)
    (with-temp-buffer
      (insert-file-contents (expand-file-name conner-file))
      (read (current-buffer)))))

(defun conner--write-commands (root-dir &optional prefix-arg)
  "Write the contents of `conner--commands' to disk.

If PREFIX-ARG is nil, write to ROOT-DIR's `conner-file-name'.

If PREFIX-ARG is non-nil, write to ROOT-DIR's associated local
file.

This logic is inversed if `conner-default-file-behavior' is set
to 'local'."
  (let ((conner-file   (if (or
                            (and current-prefix-arg (eq conner-default-file-behavior 'project))
                            (and (not current-prefix-arg) (eq conner-default-file-behavior 'local)))
                           (conner--construct-local-file-path root-dir)
                         (conner--construct-file-path root-dir))))
    (with-temp-buffer
      (insert (pp conner--commands))
      (write-file conner-file))))

(defun conner--read-env-file (root-dir)
  "Read ROOT-DIR's `conner-env-file' and return a list of strings."
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
  "Read env vars in ENV-FILE-PATH.  Return list."
  (with-temp-buffer
    (when (file-exists-p env-file-path)
      (insert-file-contents env-file-path))
    (conner--get-env-vars-in-buffer conner--env-var-regexp)))

(defun conner--annotation-function (candidate)
  "Get CANDIDATE's command and format for use in minibuffer annotation."
  (let* ((max-width (apply #'max (mapcar #'length (mapcar #'car conner--commands))))
         (indent (make-string (- max-width (length candidate)) ?\s))
         (command (cdr (assoc candidate conner--commands)))
         (tabs (make-string 6 ?\t)))
    (format "%s%s%s" indent tabs command)))

(defun conner-run-project-command (&optional project)
  "Project aware variant of `conner-run-command'.

Will use PROJECT's root dir as an argument for the corresponding
function.

If no PROJECT is provided, it will use the value of
`project-current'.  If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-run-command (project-root project))))

(defun conner-add-project-command (&optional project)
  "Project aware variant of `conner-add-command'.

Will use PROJECT's root dir as an argument for the corresponding
function.

If no PROJECT is provided, it will use the value of
`project-current'.  If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-add-command (project-root project))))

(defun conner-delete-project-command (&optional project)
  "Project aware variant of `conner-delete-command'.

Will use PROJECT's root dir as an argument for the corresponding
function.

If no PROJECT is provided, it will use the value of
`project-current'.  If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-delete-command (project-root project))))

(defun conner-update-project-command (&optional project)
  "Project aware variant of `conner-update-command'.

Will use PROJECT's root dir as an argument for the corresponding
function.

If no PROJECT is provided, it will use the value of
`project-current'.  If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-update-command (project-root project))))

(defun conner-run-command (root-dir &optional command-name)
  "Run command COMMAND-NAME.

The user will be prompted for every optional parameter not
specified.

Commands are read from both ROOT-DIR's `conner-file-name' and
ROOT-DIR's associated local file.

The command will be ran in ROOT-DIR.

If `conner-read-env-file' is non-nil, it will read ROOT-DIR's
`conner-env-file' before executing the command."
  (interactive "D")
  (conner--update-commands-from-disk root-dir)
  (let* ((completion-extra-properties '(:annotation-function conner--annotation-function))
         (process-environment (if conner-read-env-file
                                  (append (conner--read-env-file root-dir) process-environment)
                                process-environment))
         (names (mapcar #'car conner--commands))
         (command-name (or command-name (completing-read "Select a command: " names)))
         (command (cdr (assoc command-name conner--commands)))
         (default-directory root-dir))
    (setq conner--current-command command-name)
    (compile command)))

(defun conner-add-command (root-dir &optional command-name command)
  "Add command COMMAND-NAME with value COMMAND.

The user will be prompted for every optional parameter not
specified.

Write to ROOT-DIR's `conner-file-name' by default.  If invoked
with \\[universal-argument], write to a local file associated
with ROOT-DIR.

This logic is inversed if `conner-default-file-behavior' is set
to 'local'."
  (interactive "D")
  (if (or
       (and current-prefix-arg (eq conner-default-file-behavior 'project))
       (and (not current-prefix-arg) (eq conner-default-file-behavior 'local)))
      (conner--update-commands-from-disk root-dir nil t)
    (conner--update-commands-from-disk root-dir t))
  (let* ((command-name (or command-name (read-string "Enter command name: ")))
         (command (or command (read-string "Enter command: ")))
         (updated-list (conner--add-command-to-list conner--commands command-name command)))
    (setq conner--commands updated-list)
    (conner--write-commands root-dir current-prefix-arg)))

(defun conner-delete-command (root-dir &optional command-name)
  "Delete command COMMAND-NAME and write to disk.

The user will be prompted for every optional parameter not
specified.

Write to ROOT-DIR's `conner-file-name' by default.  If invoked
with \\[universal-argument], write to a local file associated
with ROOT-DIR.

This logic is inversed if `conner-default-file-behavior' is set
to 'local'."
  (interactive "D")
  (if (or
       (and current-prefix-arg (eq conner-default-file-behavior 'project))
       (and (not current-prefix-arg) (eq conner-default-file-behavior 'local)))
      (conner--update-commands-from-disk root-dir nil t)
    (conner--update-commands-from-disk root-dir t))
  (let* ((completion-extra-properties '(:annotation-function conner--annotation-function))
         (names (mapcar #'car conner--commands))
         (command-name (or command-name (completing-read "Delete command: " names)))
         (updated-list (conner--delete-command-from-list conner--commands command-name)))
    (setq conner--commands updated-list)
    (conner--write-commands root-dir current-prefix-arg)))

(defun conner-update-command (root-dir &optional command-name new-name new-command)
  "Update command COMMAND-NAME to NEW-NAME and NEW-COMMAND.

Command will be read from ROOT-DIR's `conner-file-name' by
default.  If invoked with \\[universal-argument], read from a
local file associated with ROOT-DIR.

This logic is inversed if `conner-default-file-behavior' is set
to 'local'.

The user will be prompted for every optional parameter not
specified.

If a non-existent COMMAND-NAME is provided, it will be created
instead."
  (interactive "D")
  (if (or
       (and current-prefix-arg (eq conner-default-file-behavior 'project))
       (and (not current-prefix-arg) (eq conner-default-file-behavior 'local)))
      (conner--update-commands-from-disk root-dir nil t)
    (conner--update-commands-from-disk root-dir t))
  (let* ((completion-extra-properties '(:annotation-function conner--annotation-function))
         (names (mapcar #'car conner--commands))
         (command-name (or command-name (completing-read "Update command: " names)))
         (command (cdr (assoc command-name conner--commands)))
         (new-name (or new-name (read-string "Enter new name: " command-name)))
         (new-command (or new-command (read-string "Enter new command: " command)))
         (current-prefix-arg current-prefix-arg))
    (conner-delete-command root-dir command-name)
    (conner-add-command root-dir new-name new-command)))

(defun conner--add-command-to-list (command-list command-name command)
  "Add command COMMAND-NAME with value COMMAND to COMMAND-LIST."
  (if (assoc command-name command-list)
      (error "A command with this name already exists")
    (add-to-list 'command-list `(,command-name . ,command))))

(defun conner--delete-command-from-list (command-list command-name)
  "Delete command COMMAND-NAME from COMMAND-LIST."
  (delete (assoc command-name command-list) command-list))


(provide 'conner)

;;; conner.el ends here
