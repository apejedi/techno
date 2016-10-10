(require 'clomacs)

(clomacs-defun get-patterns techno.core/get-patterns)
(message (get-patterns))

(switch-to-buffer-other-window (get-buffer-create "tekno"))
