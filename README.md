[Tests](https://github.com/tralph3/conner/actions/workflows/tests.yml/badge.svg)

# Conner

Conner is a **Co**mmand Ru**nner** for GNU Emacs.

---

Conner allows you to define arbitrary commands for each of your
projects. Every project could have a different way to compile it, to
run it, to test it, to prettify it, to watch for changes, to debug it,
to install it, or any other thing. With conner, you can define a
command for each of these actions, or any other you want.

Commands are defined in the conner file, by default called `.conner`,
situated at the root of your project. Inside it, you'll find a lisp
object that contains a list of command names, and the commands
themselves.

Conner also provides a multitude of functions to add, delete, update,
and of course, run these commands from within Emacs. It integrates
with `project.el`, so you can run these commands on arbitrary folders,
or have it automatically detect the current project's root.

Additionally, conner also has support for `.env` files. By default,
conner will look in the root directory of your project for a `.env`
file and load any environment variables found within. These variables
are then accessible to conner commands, and won't pollute the regular
Emacs session.


## Usage

The first thing you'll want to do is to add a command, simply run `M-x
conner-add-project-command` or `M-x conner-add-command` to get
started. After it has been defined, you can run `M-x
conner-run-project-command` or `M-x conner-run-command` to run it.

A compilation buffer will open and run your command. Separate buffers
are assigned to each command based on their names, so if one takes too
long, or it simply doesn't exit, you can still run other commands in
the meantime.


## Using local conner files

If you don't like to litter your repository with IDE specific files
(like the typical `.vscode` or `.idea` folders, among others), then
conner has support for "local" files. That is, files that store your
conner commands in your `emacs-user-directory`, instead of the root of
your project.

To use this feature, simply pass a prefix argument to any of the user
facing functions with `C-u`. The only function that doesn't accept
this argument is `conner-run-command`, since it will always read both
local and project files.

If you prefer to always use local files instead of project files, you
can set the variable `conner-default-file-behavior` to `'local`. Doing
so will make all of the functions default to local files, and passing
the prefix argument to them will make them read the project files.

## Acknowledgments

The source code for the processing of `.env` files was taken from
[diasjorge's load-env-vars](https://github.com/diasjorge/emacs-load-env-vars).
