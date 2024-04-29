;;; conner.el --- Define and run project specific commands  -*- lexical-binding: t -*-

;; Authors: Tomás Ralph <tomasralph2000@gmail.com>
;; Created: 2024
;; Version: 0.4
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/tralph3/conner
;; Keywords: tools

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

;; Conner allows you to define custom commands tailored to your
;; projects' needs.  Whether it's compiling, running, testing,
;; prettifying, monitoring changes, debugging, installing, or any
;; other task specific to your workflow, Conner makes it easy to
;; integrate with Emacs.
;;
;; Commands are configured in a .conner file, typically located at the
;; root of your project.  Inside this file, you'll define a Lisp object
;; containing a list of command names, their respective commands, and
;; their types.
;;
;; Integration with project.el enables seamless execution of these
;; commands within Emacs, either on arbitrary directories or
;; automatically detecting the current project's root.
;;
;; Additionally, Conner also has support for .env files.  By default,
;; Conner will look in the root directory of your project for a .env
;; file and load any environment variables found within.  These
;; variables are then accessible to Conner commands, and won't pollute
;; the regular Emacs session.
;;
;; Conner is configurable, so you can add your own command types if
;; what's available doesn't quite suit your needs.


;;; Code:

(require 'project)

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

If set to `project', conner will read, write and update commands
defined in the `conner-file-name' of the directory by
default.  You would need to pass \\[universal-argument] to these
functions to have them operate on the associated local file.

If set to `local', the inverse is true.  It will operate on the
local file by default, and you will need to pass
\\[universal-argument] to have it operate on the project file."
  :type '(choice (const :tag "Project file" project)
                 (const :tag "Local file" local)))

(defcustom conner-command-types-alist
  `(("compile" ,#'conner--run-compile-command)
    ("eat" ,#'conner--run-eat-command))
  "Alist of command types and their associated functions.

You can add your own command types here.  Each associated function
will be given two arguments.

1. PLIST, which is the plist that represents the command that has
been called. You can use `plist-get' to fetch data from it.

2. ROOT-DIR, which is the path given to `conner-run-command'."
  :type '(repeat (list
                  (string :tag "Type name")
                  (function :tag "Handler function"))))

(defcustom conner-default-command-type "compile"
  "Command type to use when not specified otherwise.

This value will be used when adding or reading commands that
don't have their commands types specified.  As such this value
must never be nil, and should always resolve to a valid command
type with an associated function in `conner-command-types-alist'."
  :type 'string)

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

(defvar conner--command-template '(:name "Command name"
                                         :command "The command to run"
                                         :type "See available types in `conner-command-types-alist'"
                                         :workdir nil
                                         :environment nil)
  "Command template that's presented to the user when adding a new command.")

(defun conner--construct-file-path (root-dir)
  "Return the path to ROOT-DIR's `conner-file-name'."
  (file-name-concat (expand-file-name root-dir) conner-file-name))

(defun conner--is-valid-command-plist (plist)
  "Return t if PLIST is a valid Conner command plist."
  (let ((command-types (mapcar #'car conner-command-types-alist)))
    (when (not (plistp plist))
      (error "Not a plist"))
    (when (not (stringp (plist-get plist :name)))
      (error "Name is not a string"))
    (when (not (stringp (plist-get plist :command)))
      (error "Command is not a string"))
    (when (not (or (stringp (plist-get plist :type))
                   (not (plist-get plist :type))))
      (error "Type is not a string"))
    (when (not (member (plist-get plist :type) command-types))
      (error "Unknown command type"))
    (when (not (or (stringp (plist-get plist :workdir))
                   (not (plist-get plist :workdir))))
      (error "Workdir is not a string"))
    (when (not (or (listp (plist-get plist :environment))
                   (not (plist-get plist :environment))))
      (error "Environment is not a list"))
    t))


(defun conner--pp-plist (plist)
  "Pretty print PLIST using line breaks after every value."
  (when (not (plistp plist))
    (error "Not a valid plist"))
  (with-temp-buffer
    (let ((inhibit-message t)
          (message-log-max nil))
      (lisp-data-mode)
      (insert (pp-to-string plist))
      (goto-char (point-min))
      (dotimes (i (proper-list-p plist))
        (when (and (cl-evenp i) (not (eq i 0)))
          (re-search-forward (pp (nth i plist)))
          (goto-char (match-beginning 0))
          (newline-and-indent)))
      (buffer-string))))

(defun conner--pp-plist-list (plist-list)
  "Pretty print PLIST-LIST using line breaks."
  (with-temp-buffer
    (let ((inhibit-message t)
          (message-log-max nil))
      (lisp-data-mode)
      (insert "(")
      (dolist (plist plist-list)
        (insert (conner--pp-plist plist)))
      (backward-delete-char-untabify 1)
      (when (length> (buffer-string) 0)
        (insert ")"))
      (indent-region (point-min) (point-max))
      (buffer-string))))

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
      (condition-case nil
          (read (current-buffer))
        (error nil)))))

(defun conner--write-commands (root-dir)
  "Write the contents of `conner--commands' to disk.

Write to ROOT-DIR's `conner-file-name' by default.

If invoked with \\[universal-argument], write to ROOT-DIR's
associated local file.

This logic is inversed if `conner-default-file-behavior' is set
to `local'."
  (let ((conner-file (if (or
                          (and current-prefix-arg (eq conner-default-file-behavior 'project))
                          (and (not current-prefix-arg) (eq conner-default-file-behavior 'local)))
                         (conner--construct-local-file-path root-dir)
                       (conner--construct-file-path root-dir))))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (insert (conner--pp-plist-list conner--commands))
        (write-file conner-file)))))

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

(defun conner--get-env-vars-in-buffer ()
  "Get a list of all REGEXP matches in a buffer."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let (matches)
        (while (re-search-forward conner--env-var-regexp nil t)
          (push (list
                 (match-string-no-properties 1)
                 (or
                  (match-string-no-properties 2)
                  (match-string-no-properties 3)
                  (match-string-no-properties 4)))
                matches))
        matches))))

(defun conner--load-env-vars (env-file-path)
  "Read env vars in ENV-FILE-PATH.  Return list."
  (with-temp-buffer
    (when (file-exists-p env-file-path)
      (insert-file-contents env-file-path))
    (conner--get-env-vars-in-buffer)))

(defun conner--find-command-with-value (key value &optional plist-list)
  "Find the plist in `conner--commands' where KEY has VALUE.

If PLIST-LIST is non-nil, search it instead."
  (cl-find-if (lambda (plist)
                (equal value (plist-get plist key)))
              (or plist-list conner--commands)))

(defun conner--get-command-names ()
  "Return a list of defined command names as strings."
  (mapcar (lambda (plist) (plist-get plist :name)) conner--commands))

(defun conner--command-annotation-function (candidate)
  "Get CANDIDATE's command and format for use in minibuffer annotation."
  (let* ((max-width (apply #'max (mapcar #'length (conner--get-command-names))))
         (indent (make-string (- max-width (length candidate)) ?\s))
         (command (car
                   (cl-remove-if #'string-blank-p
                                 (split-string
                                  (plist-get
                                   (conner--find-command-with-value :name candidate)
                                   :command) "\n"))))
         (tabs (make-string 6 ?\t)))
    (format "%s%s%s" indent tabs command)))

(defun conner--edit-command (&optional command)
  "Open a buffer for the user to edit COMMAND.

If COMMAND is not specified, a template is provided instead.

Once finished, the command is verified to be valid with
`conner--is-valid-command-plist'.  If non-nil, the command is
returned.  Otherwise, an error is raised."
  (let ((buffer (generate-new-buffer "*conner-edit-command*"))
        (keymap (make-sparse-keymap)))
    (switch-to-buffer buffer)
    (lisp-data-mode)
    (define-key keymap (kbd "C-c C-c") #'exit-recursive-edit)
    (define-key keymap (kbd "C-c C-k") (lambda ()
                                         (interactive)
                                         (kill-buffer)
                                         (abort-recursive-edit)))
    (insert (conner--pp-plist (or command conner--command-template)))
    (setq header-line-format "Edit, then exit with ‘C-c C-c’ or abort with ‘C-c C-k’")
    (use-local-map keymap)
    (recursive-edit)
    (goto-char (point-min))
    (let ((contents (read (current-buffer))))
      (kill-buffer)
      (if (conner--is-valid-command-plist contents)
          contents
        (error "Command is not a valid Conner command")))))

;;;###autoload
(defun conner-run-project-command (&optional project)
  "Project aware variant of `conner-run-command'.

Will use PROJECT's root dir as an argument for the corresponding
function.

If no PROJECT is provided, it will use the value of
`project-current'.  If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-run-command (project-root project))))

;;;###autoload
(defun conner-add-project-command (&optional project)
  "Project aware variant of `conner-add-command'.

Will use PROJECT's root dir as an argument for the corresponding
function.

If no PROJECT is provided, it will use the value of
`project-current'.  If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-add-command (project-root project))))

;;;###autoload
(defun conner-delete-project-command (&optional project)
  "Project aware variant of `conner-delete-command'.

Will use PROJECT's root dir as an argument for the corresponding
function.

If no PROJECT is provided, it will use the value of
`project-current'.  If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-delete-command (project-root project))))

;;;###autoload
(defun conner-update-project-command (&optional project)
  "Project aware variant of `conner-update-command'.

Will use PROJECT's root dir as an argument for the corresponding
function.

If no PROJECT is provided, it will use the value of
`project-current'.  If nil, it will prompt the user."
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-update-command (project-root project))))

;;;###autoload
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
  (let* ((completion-extra-properties
          '(:annotation-function conner--command-annotation-function))
         (process-environment (if conner-read-env-file
                                  (append (conner--read-env-file root-dir) process-environment)
                                process-environment))
         (command-name (or command-name (completing-read "Select a command: " (conner--get-command-names))))
         (plist (conner--find-command-with-value :name command-name))
         (command-type (plist-get plist :type))
         (command-workdir (plist-get plist :workdir))
         (command-func (cadr (assoc command-type conner-command-types-alist)))
         (default-directory (file-name-concat root-dir command-workdir)))
    (funcall command-func plist root-dir)))

;;;###autoload
(defun conner-add-command (root-dir &optional command-plist)
  "Add command COMMAND-PLIST.

If no plist is provided, a buffer will open for the user to
configure the command.

Write to ROOT-DIR's `conner-file-name' by default.  If invoked
with \\[universal-argument], write to a local file associated
with ROOT-DIR.

This logic is inversed if `conner-default-file-behavior' is set
to `local'."
  (interactive "D")
  (when (and command-plist (not (conner--is-valid-command-plist command-plist)))
    (error "Not a valid Conner command"))
  (if (or
       (and current-prefix-arg (eq conner-default-file-behavior 'project))
       (and (not current-prefix-arg) (eq conner-default-file-behavior 'local)))
      (conner--update-commands-from-disk root-dir nil t)
    (conner--update-commands-from-disk root-dir t))
  (let* ((new-command (or command-plist (conner--edit-command)))
         (updated-list
          (conner--add-command-to-list conner--commands new-command)))
    (setq conner--commands updated-list)
    (conner--write-commands root-dir)))

;;;###autoload
(defun conner-delete-command (root-dir &optional command-name)
  "Delete command COMMAND-NAME and write to disk.

The user will be prompted for every optional parameter not
specified.

Write to ROOT-DIR's `conner-file-name' by default.  If invoked
with \\[universal-argument], write to a local file associated
with ROOT-DIR.

This logic is inversed if `conner-default-file-behavior' is set
to `local'."
  (interactive "D")
  (if (or
       (and current-prefix-arg (eq conner-default-file-behavior 'project))
       (and (not current-prefix-arg) (eq conner-default-file-behavior 'local)))
      (conner--update-commands-from-disk root-dir nil t)
    (conner--update-commands-from-disk root-dir t))
  (let* ((completion-extra-properties
          '(:annotation-function conner--command-annotation-function))
         (names (conner--get-command-names))
         (command-name (or command-name (completing-read "Delete command: " names)))
         (plist (conner--find-command-with-value :name command-name))
         (updated-list (conner--delete-command-from-list conner--commands plist)))
    (setq conner--commands updated-list)
    (conner--write-commands root-dir)))

;;;###autoload
(defun conner-update-command (root-dir &optional command-name new-command-plist)
  "Update command COMMAND-NAME to NEW-COMMAND-PLIST.

Command will be read from ROOT-DIR's `conner-file-name' by
default.  If invoked with \\[universal-argument], read from a
local file associated with ROOT-DIR.

This logic is inversed if `conner-default-file-behavior' is set
to `local'.

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
  (let* ((completion-extra-properties
          '(:annotation-function conner--command-annotation-function))
         (names (conner--get-command-names))
         (command-name (or command-name (completing-read "Update command: " names)))
         (command-plist (conner--find-command-with-value :name command-name))
         (new-command (or new-command-plist (conner--edit-command (conner--find-command-with-value :name command-name))))
         (updated-list
          (conner--update-command-from-list
           conner--commands command-plist new-command)))
    (setq conner--commands updated-list)
    (conner--write-commands root-dir)))

(defun conner--add-command-to-list (command-list command-plist)
  "Add command COMMAND-PLIST to COMMAND-LIST."
  (if (and command-list
       (conner--find-command-with-value
        :name (plist-get command-plist :name) command-list))
      (error "A command with this name already exists"))
  (if (not (conner--is-valid-command-plist command-plist))
      (error "Not a valid Conner command"))
  (push command-plist command-list))

(defun conner--delete-command-from-list (command-list command-plist)
  "Delete COMMAND-PLIST from COMMAND-LIST."
  (delete command-plist command-list))

(defun conner--update-command-from-list (command-list command-plist new-command-plist)
  "Update command COMMAND-PLIST from COMMAND-LIST with NEW-COMMAND-PLIST."
  (let* ((command-deleted (conner--delete-command-from-list command-list command-plist))
         (updated-list
          (conner--add-command-to-list
           command-deleted new-command-plist)))
    updated-list))

(defun conner--run-compile-command (plist &rest _)
  "Run the command COMMAND-PLIST in an unique compilation buffer."
  (let* ((command-name (plist-get plist :name))
         (compilation-buffer-name-function
          (lambda (_) (concat "*conner-compilation-" command-name "*"))))
    (compile (plist-get plist :command))))

(defun conner--run-eat-command (plist &rest _)
  (when (not (featurep 'eat))
    (error "Eat is not installed or not loaded. Aborting"))
  (let* ((command-name (plist-get plist :name))
         (eat-buffer-name (concat "*conner-eat-" command-name "*")))
    (eat (plist-get plist :command))))

(provide 'conner)

;;; conner.el ends here
