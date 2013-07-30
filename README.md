i3-manager.el
=============

*Note: Emacs 24+ required.*

This project is a sketch of what may one day become a useful i3 manager
for emacs.

If you don't know wtf is i3, please follow the link below:

- http://i3wm.org/

The main goal is:

* Create named workspaces and provide tools to moving around them.
  - 'i3-manager-new-workspace
  - 'i3-manager-move-to-workspace
  - 'i3-manager-rename-workspace
* Find windows and jump to them
  - 'i3-manager-find-window
* Exec commands on specific workspaces and track them for future jumps
  - TODO
* Create a graphical tree representation of current i3 layout tree
  - TODO
* Learn Elisp =)


Dependencies
=============

* i3-emacs: https://github.com/vava/i3-emacs/

Install
=======

copy i3-manager.el to your emacs load path

and insert the line below to your .emacs or .emacs.d/init.el file:

(require 'i3-manager)

;; set the keybindings (*Modify as you want*)
(global-set-key (kbd "C-c C-3") 'i3-manager-move-to-workspace)
(global-set-key (kbd "C-c C-n") 'i3-manager-new-workspace)
(global-set-key (kbd "C-c C-w") 'i3-manager-find-window)

Contact
=======

For comments, suggestions or help contact me at:
     natel (at) secplus.com.br

*Long Live Emacs !*
