;; [ projectile

;; Also see http://batsov.com/projectile/

(use-package projectile
  :disabled
  :config
  (setq projectile-completion-system 'ivy
        projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired
        projectile-track-known-projects-automatically nil)
  (projectile-mode))

(defun projectile-available-p ()
  "Determine if we are anywhere where calling projectile functions makes sense."
  (or (null projectile-cached-project-root)
      (eq 'none projectile-cached-project-root)))

(defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

(global-set-key (kbd "C-c C-p") 'projectile-command-map)

(define-key 'projectile-command-map (kbd "h") 'hydra-projectile/body)

;; ]

(use-package treemacs-projectile
  :disabled
  :after treemacs projectile)

;; ]

