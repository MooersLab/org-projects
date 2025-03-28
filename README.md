![Version](https://img.shields.io/static/v1?label=org-projects&message=0.2&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

# org-projects

## The problem addressed by this package

This Emacs package manages TODO items in project-specific log files written in org-mode.
The org-projects.el package is designed to ease daily writing project management with the [writing log template](https://github.com/MooersLab/writingLogTemplateInOrg) written in org-mode.
This writing log is the metacognition document for writing projects; we use it to keep the manuscript document free of notes.
This `org-projects.el` package eliminates the refiling of TODOs from a task.org file to a TODO list in a project-specific file.
This second step of the standard TODO note capture wastes time when you know the project to which you will assign the TODO.


## More context more the problem addressed by this package

Our project-driven work has been divided into about 1200 projects over the past three years.
Usually, ideas for TODOs emerge for the projects we are most actively engaged in.
In these situations, we already have the number and name at the front of our minds.
Before this package, when we could not remember the project name, we looked up the project name using `ls` in the terminal, in the favorites sidebar of the Mac's finder GUI, or we looked it up in a database file.
Now, we can use this package, along with the invaluable packages `orderless` and `vertico`, to find the project name by entering `C-c j t` and searching for the project name with keywords in the minibuffer.
We can use this package for project look-up and then escape the TODO note completion by entering `C-g`.

If we have a TODO note for which we cannot think of a target project for assignment, we may need to start a new project, but in the meantime, we can always use a regular `org-capture` TODO note to save it to the standard `task.org` of the Getting Things Done method for later refiling.
With this package, we can follow the principle of handling a TODO item only once, at least 90% of the time, and have to refile the remaining TODOs from the `tasks.org` file once a week.

This package is helpful if you manage many projects and assign each project to a four-digit index number 
that starts with the name of the project's folder in your home directory. 
We use this flat directory approach in our 10K project management system for lifelong project management. 

The TODO list is below the Daily Log entries in the middle of this template.
This project does not use `org-capture`, which often places TODOs at the file's bottom. 
The file's bottom can be thousands of lines away as the project matures.

## Overview

`org-projects` provides a streamlined way to add TODO items to project log files. 
It is mainly designed for projects organized with a 4-digit number prefix in the home directory (e.g., `~/1234projectName/`).

### Assumptionsz;

- The project directory starts with four digit code. 
- The writing log or project log filename is of the form `log####.org`.
- A third level headline is above your todo list. The new todo is added at the bottom of the todo subtree. The head line has the tag `:appendtodos`.
- Existing todos may or may be present.

### Key features:

- Quickly add TODOs to project-specific log files.
- Eliminates the need for >90% of refiling.
- Simple project directory selection via minibuffer completion.
- Does not rely on org-capture (avoids TODO placement issues).

## Installation

### Using straight.el

Add this to your Emacs configuration:

```elisp
;; org-projects
(use-package org-projects
    :straight (org-projects :type git 
                            :host github 
                            :repo "mooerslab/org-projects")
(require 'org-projects)
(org-projects-setup)
```

### Manual Installation

1. Clone or download this repository
2. Add the directory to your load path:
   ```elisp
   (add-to-list 'load-path "/path/to/org-projects")
   (require 'org-projects)
   (org-projects-setup)
   ```

## Usage

### Adding a TODO

1. Press `C-c j t` or run `M-x add-todo`.
2. Select a project from the completion list.
3. Enter the TODO headline.
4. Enter the TODO description.
5. The TODO will be added to the project's log file under the `:appendtodos:` tagged headline.

### Other Commands

- `C-c j l` - View a list of all projects.
- `C-c j o` - Open a project log file.

## Project Structure Requirements

This package expects:

1. Project directories in your home folder with 4-digit number prefixes (e.g., `~/1234-project-name/`)
2. Log files named `log####.org` where `####` matches the project number. See [template](https://github.com/MooersLab/writingLogTemplateInOrg/writingLogTemplateVer082.org).
3. Log files contain a level-3 headline with the `:appendtodos:` tag where TODOs should be placed

If a log file doesn't exist, it will be created with an appropriate structure when you first add a TODO to that project.


## Default File Structure

When creating a new log file, the package generates this structure:

```org
#+TITLE: Project #### Log

* Tasks

*** TODO List :appendtodos:
**** TODO first todo. Append to the bottom of list.
```

## Customization

You can customize the package by setting variables before calling `org-projects-setup`:

```elisp
;; Example customization
(setq org-projects-keymap-prefix "C-c p")  ;; Change the keybinding prefix
(org-projects-setup)
```

## Requirements

- Emacs 27.1 or higher
- org-mode 9.3 or higher

## Status
It works, but it is still alpha. Not fully tested. Not ready for MELPA

## Update history

|Version      | Changes                                                                                                                                                                         | Date                 |
|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------|:--------------------|
| Version 0.1 |   Added badges, funding, and update table.  Initial commit.                                                                                                                | 2025 March 20  |
| Version 0.2 |   Switch keybinding prefex to C-c j.                                                                                                                | 2025 March 21  |

## Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)

