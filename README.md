i3-manager.el
=============

Draft of a i3 manager for EMACS

Install:

copy i3-manager.el to your emacs load path

and insert the line below to your .emacs or .emacs.d/init.el file:

(require 'i3-manager)

;; set the keybinding to workspace move
(global-set-key (kbd "C-c C-3") 'i3-manager-move-to-workspace)
(global-set-key (kbd "C-c C-n") 'i3-manager-new-workspace)

TODO:
* View i3 container tree in a 'tree-widget
* Implement 'i3-manager-move-to-window
* Implement 'i3-manager-rename-workspace
* Implement 'i3-manager-execute-on-workspace
* Others ...
