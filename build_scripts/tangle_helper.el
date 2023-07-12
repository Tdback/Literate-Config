;; A command that automatically tangles the .org files in the dotfiles folder

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path
(if (not (file-directory-p "./.temp"))
    (message "Installing org-mode dependencies..."))
(require 'package)
(setq package-user-dir (expand-file-name "./.temp"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(require 'org)

(defcustom dotfiles-folder "~/.dotfiles/"
  "The folder where dotfiles and org-mode configuration files are stored."
  :type 'string
  :group 'dotfiles)

(defcustom dotfiles-org-files '("Alacritty.org" "Autostart.org" "Bspwm.org"
                                "Emacs.org" "Lockscreen.org" "Sxhkd.org"
                                "Polybar.org" "Rofi.org" "Starship.org")
  "This list of org-files under the `dotfiles-folder' which contain configuration
files that should be tangled."
  :type '(list string)
  :group 'dotfiles)

(defun td/dotfiles-tangle-org-file (&optional org-file)
  "Tangles a single .org file relative to the path in
dotfiles-folder. If no file is specified, tangle the current file
if it is an org-mode buffer inside of dotfiles-folder."
  (message "Tangling file: %s" org-file)
  ;; Suppress prompts and messages
  (let ((org-confirm-babel-evaluate nil)
        (message-log-max nil)
        (inhibit-message t))
    (org-babel-tangle-file (expand-file-name org-file dotfiles-folder))))

(defun td/dotfiles-tangle-org-files ()
  "Tangles all of the .org files in the paths specified by the
variable dotfiles-folder."
  (dolist (org-file dotfiles-org-files)
    (td/dotfiles-tangle-org-file org-file))
  (message "Dotfiles are up to date!"))

(td/dotfiles-tangle-org-files)
