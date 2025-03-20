![Version](https://img.shields.io/static/v1?label=org-projects&message=0.1&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

# org-projects

This is an Emacs package for managing TODO items in project-specific log files written in org-mode.
This package eliminates the refiling of TODOs from a task.org file to a TODO list in a project-specific file.
This second step of the standard TODO note capture wastes time when you know the project to which you will assign the TODO.

Our work is project-driven and has been divided into about 1200 projects over the past three years.
Usually, ideas for TODOs emerge for the projects that we are most actively engaged in at the moment.
In these situations, we already have the number and name at the front of our mind.
Before this package, when we could not remember the project name, we looked up the project name using `ls` in the terminal, in the favorites sidebar of the Mac's finder GUI, or we looked it up in a database file.
Now, we can use this package, along with the invaluable packages `orderless` and `vertico`, to find the project name by entering `C-c o l` and searching for the project name with keywords in the minibuffer.

If we have a TODO note for which we cannot think of a project to assign it to, we may need to start a new project, but in the meantime, we can always use a regular org-capture TODO note to save it to the standard `task.org` for later refiling.
With this package, we can follow the principle of handling a TODO item only once at least 90% of the time and have to refile the remaining TODOs from the `tasks.org` file once a week.

This package is helpful if you manage many projects and assign each project to a four-digit index number 
that starts with the name of the project's folder in your home directory. 
We use this flat directory approach in our 10K project management system for lifelong project management. 
The org-projects.el package is designed to work with the [writing log template](https://github.com/MooersLab/writingLogTemplateInOrg) written in org-mode.

The TODO list is below the Daily Log entries in the middle of this template.
This project does not use org-capture, which often places TODOs at the file's bottom. 
The file's bottom can be thousands of lines away can the project matures.

## Overview

`org-projects` provides a streamlined way to add TODO items to project log files. 
It is mainly designed for projects organized with a 4-digit number prefix in the home directory (e.g., `~/1234projectName/`).

Key features:
- Quickly add TODOs to project-specific log files.
- Eliminate the need for >90% of refiling.
- Simple project selection via minibuffer completion.
- Works with your existing directory structure.
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

1. Press `C-c o t` or run `M-x add-todo`.
2. Select a project from the completion list.
3. Enter the TODO headline.
4. Enter the TODO description.
5. The TODO will be added to the project's log file under the `:appendtodos:` tagged headline.

### Other Commands

- `C-c o l` - View a list of all projects.
- `C-c o o` - Open a project log file.

## Project Structure Requirements

This package expects:

1. Project directories in your home folder with 4-digit number prefixes (e.g., `~/1234-project-name/`)
2. Log files named `log####.org` where #### matches the project number. See for template: 
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
It works, but it is still alpha.

## Update history

|Version      | Changes                                                                                                                                                                         | Date                 |
|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------|:--------------------|
| Version 0.1 |   Added badges, funding, and update table.  Initial commit.                                                                                                                | 2025 March 20  |

## Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)

