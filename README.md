[![tests](https://github.com/tralph3/conner/actions/workflows/tests.yml/badge.svg)](https://github.com/tralph3/conner/actions/workflows/tests.yml)
[![MELPA](https://melpa.org/packages/conner-badge.svg)](https://melpa.org/#/conner)


# Conner

Conner is a **Co**mmand Ru**nner** for GNU Emacs.

---

> [!WARNING]
> Conner is currently alpha software. Nothing is set in stone, so the
> API or the .conner file format may change. As such, until I release
> 1.0, I reserve the right to break configs.

https://github.com/tralph3/conner/assets/41462117/1bef5907-db95-43a8-9061-f191e50614a6

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


## Installation

Conner can be installed from [MELPA](https://melpa.org/#/conner). If
you don't know how to use this repository go
[here](https://melpa.org/#/getting-started) for instructions.

It comes with no keybindings by default, so I recommend to add some to
make it easier to use. This is my personal config at the time of
writing:

```emacs-lisp
(use-package conner
  :ensure t
  :bind (([remap project-compile] . conner-run-project-command)))
```

The code above binds `conner-run-project-command` to the key used for
`project-compile` (`C-x p c` by default) thereby replacing the latter
by the former.

## Features

- Integrated command editor which streamlines the experience.
- Project aware thanks to `project.el` integration.
- Allows for custom command runners.
- Format specifiers let you get information at run-time.
- Loading of environment variables via `.env` files.
- Per-command environment overrides.
- Change working directory.
- Meta command type allows for complex setups.
- User-facing functions for CRUD operations on commands.


## Usage

To add a command, simply execute `M-x conner-add-project-command` or
`M-x conner-add-command`. A new buffer will open where you can edit
your command. These are the available keys:

- `:name` The name of the command. Will be used to identify the
  command. Most runners use it to name their buffers. Must be a
  string.
- `:command` The command to run. Its interpretation will vary
  depending on the runner. Most runners support format specifiers that
  are calculated at runtime. Read `conner-expand-command` for
  details. Must be defined.
- `:type` The type decides which runner to call when executing the
  command. See `conner-command-types-alist` for available types. Must
  be a string.
- `:workdir` Specifies a working directory from where to run the
  command. The directory is relative to the root directory of the
  project. Must be a string.
- `:environment` List of strings containing environment variables in
  the form of "KEY=value". These variables will be accessible to the
  command they are declared in only. Must be a list.

Note that `:name`, `:command`, and `:type` are the only keys that must
be defined. The rest are optional. If you wish to not provide a value,
you must either provide `nil` or remove them altogether. A key with an
empty value is not a valid plist.

Once defined, run the command using `M-x conner-run-project-command`
or `M-x conner-run-command`.

Command runners can also support their own keys. For instance, the
`compile` runner supports the `:comint` key, which when non-nil, runs
the compilation buffer in [comint
mode](https://www.emacswiki.org/emacs/ComintMode), allowing to pass
user input to the running process. These optional keys are documented
in the help buffer for each runner.


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

While Conner ships with a variety of command types already, you can
define custom types tailored to your requirements. Adding a custom
command type is straightforward:

1. Define a function to execute the command.
2. Register the function in `conner-command-types-alist`.

Custom command functions receive the plist of the command, which
contains all the data saved in the Conner file, and the root
directory. This directory corresponds to the directory with which
`conner-run-command` was originally invoked.

Below is an example of a command type that prints the command plist
along with the root directory.

```emacs-lisp
(defun my-conner-command-type-print (plist root-dir)
  "Print command PLIST and ROOT-DIR."
  (message "Command plist: %s\nRoot dir: %s" plist root-dir))

(add-to-list 'conner-command-types-alist
             `("print" ,#'my-conner-command-type-print))
```

You will then be able to define commands with type `print`.

Do make sure to use the result of `conner-expand-command` as the
command to run to ensure each format specifier gets expanded. If not
applicable to your runner, it can of course be omitted.

## Acknowledgments

The processing of `.env` files in Conner is based on [diasjorge's
load-env-vars](https://github.com/diasjorge/emacs-load-env-vars).
