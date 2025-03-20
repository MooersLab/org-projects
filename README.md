![Version](https://img.shields.io/static/v1?label=org-projects&message=0.1&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

# org-projects

An Emacs package for managing TODO items in project-specific log files written in org-mode.
This package eliminates the need to refile TODOs from a task.org file to a TODO list in a project-specific file.
This second step is a waste of time when you already know which project to which you will assign the TODO.
We are following the principle of handling a TODO item only once.

This package is useful if you manage many projects and assign each project to an four digit index number 
that starts the name of the project's folder in your home directory. We use this flat directory approach in our 10K project 
management system for academics. It is designed to work with the [writing log template](https://github.com/MooersLab/writingLogTemplateInOrg) written in org-mode.
The TODO list is below the Daily Log entries in the middle of this template.
This project does not use org-capture, which often places TODOs at the bottom of the file. 


## Overview

`org-projects` provides a streamlined way to add TODO items to project log files. 
It is particularly designed for projects organized with a 4-digit number prefix in the home directory (e.g., `~/1234projectName/`).

Key features:
- Quickly add TODOs to project-specific log files.
- Automatically organizes TODOs under tagged headers.
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
**** TODO first todo. Append to bottom of list.
```

## Customization

You can customize the package by setting variables before calling `org-projects-setup`:

```elisp
;; Example customization
(setq org-projects-keymap-prefix "C-c p")  ;; Change the keybinding prefix
(org-projects-setup)
```

## AI assistance
This code was made after wrestling for 12 hours with 3 chatbots: Claude 3.5 Sonnet, Copilot, and Claude 3.7 Sonnet.
The first generated complex code, the second gave results that were too simple, and the third did well.

## Requirements

- Emacs 27.1 or higher
- org-mode 9.3 or higher

## Update history

|Version      | Changes                                                                                                                                                                         | Date                 |
|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------|:--------------------|
| Version 0.1 |   Added badges, funding, and update table.  Initial commit.                                                                                                                | 2025 March 20  |

## Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)

