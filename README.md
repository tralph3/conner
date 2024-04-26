[![tests](https://github.com/tralph3/conner/actions/workflows/tests.yml/badge.svg)](https://github.com/tralph3/conner/actions/workflows/tests.yml)


# Conner

Conner is a **Co**mmand Ru**nner** for GNU Emacs.

---

> [!WARNING]
> Conner is currently alpha software. Nothing is set in stone, so the
> API or the .conner file format may change. As such, until I release
> 1.0, I reserve the right to break configs.

Conner allows you to define custom commands tailored to your projects'
needs. Whether it's compiling, running, testing, prettifying,
monitoring changes, debugging, installing, or any other task specific
to your workflow, Conner makes it easy to integrate with Emacs.

Commands are configured in a `.conner` file, typically located at the
root of your project. Inside this file, you'll define a Lisp object
containing a list of command names, their respective commands, and
their types.

Integration with `project.el` enables seamless execution of these
commands within Emacs, either on arbitrary directories or
automatically detecting the current project's root.

Additionally, Conner also has support for `.env` files. By default,
Conner will look in the root directory of your project for a `.env`
file and load any environment variables found within. These variables
are then accessible to Conner commands, and won't pollute the regular
Emacs session.

Conner is configurable, so you can add your own command types if
what's available doesn't quite suit your needs.


## Usage

To add a command, simply execute `M-x conner-add-project-command` or
`M-x conner-add-command`. Once defined, run the command using `M-x
conner-run-project-command` or `M-x conner-run-command`.

By default, each command gets defined with the `compile` type. This
can be changed by modifying `conner-default-command-type`.

Compile commands run on their own compilation buffer. This design
allows concurrent execution of multiple commands, useful if you run
commands with a long run time, or commands that don't exit at all.


## Local Conner files

For those who prefer keeping their repositories free from IDE-specific
files, Conner offers support for "local" Conner files. These files are
stored in your `emacs-user-directory`.

Whenever you invoke `conner-run-command`, it will load both the
project `.conner` file, as well as the associated file for that
directory stored in your `emacs-user-directory`.

To utilize this feature, simply prefix any user-facing function with
`C-u`. Note that `conner-run-command` always reads both local and
project files.

If you prefer using local files by default, set the variable
`conner-default-file-behavior` to `'local`. This setting makes all
functions default to local files, with the prefix argument allowing
access to project files.


## Command types

While Conner ships with the `compile` command type, you can define
custom types tailored to your requirements. Adding a custom command
type is straightforward:

1. Define a function to execute the command.
2. Register the function in `conner-command-types-alist`.

Custom command functions receive the command to run, a list
representing the command configuration, and the root directory. This
directory corresponds to the directory with which `conner-run-command`
was originally invoked.

Below is an example of a custom command type for running commands in
the [eat](https://codeberg.org/akib/emacs-eat) terminal:

```emacs-lisp
(defun conner--run-eat-command (command element &rest _)
  (when (not (featurep 'eat))
    (error "Eat is not installed or not loaded. Aborting"))
  (let* ((command-name (car element))
         (eat-buffer-name (concat "*conner-eat-" command-name "*")))
    (eat command)))

(add-to-list 'conner-command-types-alist
             `("eat" ,#'conner--run-eat-command "Run command with the eat terminal."))
```


## Acknowledgments

The processing of `.env` files in Conner is based on [diasjorge's
load-env-vars](https://github.com/diasjorge/emacs-load-env-vars).
