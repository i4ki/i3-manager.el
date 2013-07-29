;; i3-manager
;; Author: Tiago Natel de Moura <natel@secplus.com.br>
;;
;; Try to improve the i3 interface for emacs-hackers
;;

(require 'cl)
(require 'i3)
(require 'i3-integration)
(require 'tree-widget)

(defvar i3-manager-buffer-name "*i3-manager*"
  "Name of the i3-manager buffer")

(defun print-wspace (wspaces index)
  "Print workspace by INDEX in list of workspace WSPACES"
  (if (and (>= index 0) (< index (length wspaces)))
      (let* ((workspace (elt wspaces index))
             (output (i3-alist-value 'output workspace))
             (name (i3-alist-value 'name workspace)))
        (message "name: %s, output: %s" name output))
    nil))

(defun i3-manager-get-wspace (wspaces index)
  "Get workspace by INDEX in list of workspaces WSPACES.
Returns `nil` if not found"
  (if (and (>= index 0)
           (< index (length wspaces)))
      (elt wspaces index)
    nil))

(defun print-windows (windows)
  "Print all WINDOWS names"
  (let ((window (car windows))
        (next (cdr windows)))
    (message "name: %s" (i3-alist-value 'name window))
    (if next
        (print-windows next))))

;;;###autoload
(defun i3-manager ()
  "Start i3-mode buffer"
  (interactive)
  (switch-to-buffer i3-manager-buffer-name)
  (i3-manager-tree-widget))

(defun i3-manager-get-wspace-names ()
  (setq wspace-names (list))
  (setq i 0)
  (let ((wspaces (i3-get-workspaces)))
    (while (< i (length wspaces))
      (let ((workspace (elt wspaces i)))
        (setq wspace-names
              (append wspace-names
                      (list (i3-alist-value 'name workspace)))))
      (setq i (+ i 1))))
  wspace-names)

(defun i3-manager-move-to-workspace (&optional wspace-name)
  (interactive)
  (setq res nil)
  (if (not wspace-name)
      (setq wspace-name
            (ido-completing-read "Choose workspace: "
                                 (i3-manager-get-wspace-names))))
  (if wspace-name
      (setq res (i3-command 0 (format "workspace %s"
                               wspace-name))))
  (if res
      (princ res))
  res)

(defun i3-manager-new-workspace (&optional wspace-name)
  (interactive)
  (setq res nil)
  (if (not wspace-name)
      (setq wspace-name
            (read-string "Workspace name: ")))

  (if wspace-name
      (setq res (i3-command 0
                            (format "workspace %s" wspace-name))))

  (if res
      (princ res))
  res)

(defun i3-manager-get-wspace-of-window (name)
  (let ((wspaces (i3-get-tree-layout)))))

(defun i3-manager-move-to-window (&optional window-title)
  (interactive)
  (setq res nil)
  (if (not window-title)
      (setq window-title (ido-completing-read
                          "Choose window title: "
                          (i3-manager-get-window-names))))
  (if window-title
      (let (((wspace (i3-manager-get-wspace-of-window window-title)))
            (if wspace
                (i3-manager-move-to-workspace (i3-alist-value 'name
                                                              wspace))
              nil)))))

;;{{{  Widget tree
;; testing ...
(defun i3-manager-tree-widget (&optional theme)
         (interactive
          (list (if current-prefix-arg
                    (completing-read "Theme name: "
                                     '(("default" . "default")))
                  nil)))
         (kill-all-local-variables)
         (let ((inhibit-read-only t))
           (erase-buffer))
         (let ((all (overlay-lists)))
           (mapcar #'delete-overlay (car all))
           (mapcar #'delete-overlay (cdr all)))
         (tree-widget-set-theme theme)
         (widget-insert (format "%s. \n\n"
                                "i3 Manager Tree view"))
         (widget-create
          ;; Open this level
          'tree-widget :open t
          ;; Use a push button for this code
          :node '(push-button
                  :tag "Tree"
                  :format "%[%t%]\n"
                  :notify
                  (lambda (&rest ignore)
                    (message "This is the root node")))
          ;; Add subtrees (their nodes default to items)
          '(tree-widget :tag "1:main"
                        (tree-widget :tag "vertical"
                                     (item :tag "i4k@secplus ~")
                                     (item :tag "root@secplus ~")))
          '(tree-widget :tag "2:www"
                        (tree-widget :tag "horizontal"
                                     (item :tag "Google Chrome")
                                     (item :tag "Firefox")))))

(provide 'i3-manager)
