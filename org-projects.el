
;;; org-projects.el --- Manage project-based TODO lists in org files -*- lexical-binding: t -*-  

;; Copyright (C) 2025 Blaine Mooers  

;; Author: Blaine Mooers  
;; Keywords: org, projects, todo  
;; Version: 1.0.0  
;; Package-Requires: ((emacs "27.1") (org "9.3"))  

;;; Commentary:  

;; This package provides functionality to add TODO items to project log files  
;; where projects are identified by 4-digit numbers in the home directory.  
;; TODOs are added under a level-3 headline with the :appendtodos: tag.  
;;  
;; Usage:  
;; Include the following in the init.el file.  
;; (require 'org-projects)  
;; (org-projects-setup)  
;;  
;; Then use M-x add-todo to add new TODOs to your project files.  

;;; Code:  

(require 'org)  

(defgroup org-projects nil  
  "Customization for org-projects."  
  :group 'org  
  :prefix "org-projects-")  

;; Template-related customization options  
(defcustom org-projects-template-local-path  
  "~/6112MooersLabGitHubLabRepos/writingLogTemplateInOrg/writingLogTemplateVer082.org"  
  "Path to the local log template file."  
  :type 'string  
  :group 'org-projects)  

(defcustom org-projects-template-repo-url  
  "https://github.com/MooersLab/writingLogTemplateInOrg.git"  
  "URL of the template repository."  
  :type 'string  
  :group 'org-projects)  

(defcustom org-projects-template-repo-local-path  
  "~/6112MooersLabGitHubLabRepos/writingLogTemplateInOrg"  
  "Local path where the template repository should be cloned."  
  :type 'string  
  :group 'org-projects)  

;; We'll use a simple prefix key that's less likely to conflict  
(defcustom org-projects-prefix-key "C-c j"  
  "Prefix key for org-projects commands."  
  :type 'string  
  :group 'org-projects)  

;; Template management functions  
(defun org-projects-ensure-template ()  
  "Ensure the log template is available, downloading if necessary."  
  (unless (file-exists-p org-projects-template-local-path)  
    (if (yes-or-no-p "Template not found. Clone from GitHub? ")  
        (progn  
          (message "Cloning template repository...")  
          (unless (file-exists-p org-projects-template-repo-local-path)  
            (make-directory org-projects-template-repo-local-path t))  
          (let ((default-directory org-projects-template-repo-local-path))  
            (shell-command (format "git clone %s ." org-projects-template-repo-url)))  
          (message "Template repository cloned successfully"))  
      (error "Template not available"))))  

(defun org-projects-create-log-file (project-number)  
  "Create a new log file for PROJECT-NUMBER using the template."  
  (let ((log-path (org-projects-get-log-path project-number)))  
    (org-projects-ensure-template)  
    (copy-file org-projects-template-local-path log-path)  
    (with-current-buffer (find-file-noselect log-path)  
      (goto-char (point-min))  
      (while (re-search-forward "PROJECT-NUMBER" nil t)  
        (replace-match project-number))  
      (save-buffer))  
    log-path))  

;; Project discovery functions  
(defun org-projects-find-directories ()  
  "Find all project directories matching ####* pattern in home directory."  
  (directory-files "~" t "^[0-9]\\{4\\}.*$" t))  

(defun org-projects-extract-number (dir-path)  
  "Extract 4-digit project number from DIR-PATH."  
  (when (string-match "/\\([0-9]\\{4\\}\\)" dir-path)  
    (match-string 1 dir-path)))  

(defun org-projects-get-details ()  
  "Get list of projects with numbers and names."  
  (let ((projects (org-projects-find-directories)))  
    (mapcar (lambda (dir)  
              (cons (org-projects-extract-number dir)  
                    (file-name-nondirectory dir)))  
            projects)))  

(defun org-projects-select ()  
  "Prompt for project selection using completion."  
  (let* ((projects (org-projects-get-details))  
         (choices (mapcar (lambda (proj)  
                           (format "%s: %s"  
                                   (car proj)  
                                   (cdr proj)))  
                         projects))  
         (selection (completing-read "Select project: " choices nil t))  
         (project-num (car (split-string selection ":"))))  
    project-num))  

(defun org-projects-get-log-path (project-number)  
  "Get path to project log file for PROJECT-NUMBER."  
  (let ((project-dir  
         (car (directory-files "~" t  
                             (format "^%s" project-number)  
                             t))))  
    (expand-file-name (format "log%s.org" project-number)  
                     project-dir)))  

(defun org-projects-ensure-log-file (project-number)  
  "Ensure log file exists for PROJECT-NUMBER, creating if needed."  
  (let ((log-path (org-projects-get-log-path project-number)))  
    (unless (file-exists-p log-path)  
      (org-projects-create-log-file project-number))  
    log-path))  

;; Core TODO insertion function  
(defun org-projects-append-todo-to-tagged-headline (log-file todo-headline todo-body &optional tag)  
  "Append TODO to LOG-FILE with TODO-HEADLINE and TODO-BODY under TAG."  
  (let ((tag (or tag "appendtodos")))  
    (with-current-buffer (find-file-noselect log-file)  
      (save-excursion  
        (widen)  
        (goto-char (point-min))  
        (if (re-search-forward (concat "^\\*\\*\\* .*:" tag ":") nil t)  
            (progn  
              (org-end-of-subtree t)  
              (unless (bolp) (insert "\n"))  
              (insert (format "**** TODO %s\n%s\n" todo-headline todo-body))  
              (save-buffer)  
              (message "Added TODO: %s" todo-headline)  
              t)  
          (message "Could not find :%s: tag in %s" tag log-file)  
          nil)))))  

;; Interactive command  
;;;###autoload  
(defun add-todo ()  
  "Add a TODO item to a project log file."  
  (interactive)  
  (let* ((project-num (org-projects-select))  
         (log-file (org-projects-ensure-log-file project-num))  
         (todo-headline (read-string "TODO headline: "))  
         (todo-body (read-string "TODO description: ")))  
    
    (if (org-projects-append-todo-to-tagged-headline log-file todo-headline todo-body)  
        (progn  
          (find-file log-file)  
          (goto-char (point-min))  
          (re-search-forward (concat "TODO " (regexp-quote todo-headline)) nil t))  
      (message "Failed to add TODO"))))  

;; Create a command map for our functions  
(defvar org-projects-command-map  
  (let ((map (make-sparse-keymap)))  
    (define-key map "t" #'add-todo)  
    (define-key map "l" #'org-projects-list-projects)  
    (define-key map "o" #'org-projects-open-log)  
    map)  
  "Keymap for org-projects commands.")  

;; Additional utility functions  
(defun org-projects-list-projects ()  
  "Display a list of all projects."  
  (interactive)  
  (let ((projects (org-projects-get-details)))  
    (with-current-buffer (get-buffer-create "*Projects List*")  
      (erase-buffer)  
      (org-mode)  
      (insert "* Project List\n\n")  
      (dolist (proj projects)  
        (let* ((proj-num (car proj))  
               (log-file (org-projects-get-log-path proj-num))  
               (status (if (file-exists-p log-file)  
                          "[Log OK]"  
                        "[No Log]")))  
          (insert (format "** Project %s: %s %s\n"  
                         proj-num  
                         (cdr proj)  
                         status))))  
      (pop-to-buffer (current-buffer)))))  

(defun org-projects-open-log ()  
  "Open a project log file."  
  (interactive)  
  (let* ((project-num (org-projects-select))  
         (log-file (org-projects-ensure-log-file project-num)))  
    (find-file log-file)))  

;; Define the keymap binding directly  
;;;###autoload  
(defun org-projects-setup ()  
  "Set up org-projects keybindings and other configurations."  
  (interactive)  
  
  ;; Set up the direct keybindings for commonly used functions  
  (global-set-key (kbd "C-c j t") #'add-todo)  
  (global-set-key (kbd "C-c j l") #'org-projects-list-projects)  
  (global-set-key (kbd "C-c j o") #'org-projects-open-log)  
  
  ;; Also bind the prefix key to the command map for completeness  
  (global-set-key (kbd "C-c j") org-projects-command-map)  
  
  (message "org-projects setup complete. Use C-c j t to add TODOs."))  

(provide 'org-projects)  

;;; org-projects.el ends here  