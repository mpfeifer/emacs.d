;;; hydras.el --- 
;; 
;; Description: All the hydras in one place
;; Author: Matthias
;; Keywords: emacs lisp init.el hydra
;; Dependencies: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; 1.0 (Tue Jul 16 15:39:24 2019): Initial release
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defhydra hydra-magit (:color blue :hint nil)
  "
History^^
------------------------------------------
for _f_ile"
  ("f" magit-log-buffer-file ))

(defhydra hydra-global-org (:color blue
                                   :hint nil)
  "
Timer^^        ^Clock^       
-----------------------------
s_t_art        _w_ clock in  
 _s_top        _o_ clock out 
_r_eset        _j_ clock goto
_p_rint        _m_ clock mru
               _c_ cancel clock
               _d_ display clock"
  ("t" org-timer-start)
  ("s" org-timer-stop)
  ;; Need to be at timer
  ("r" org-timer-set-timer)
  ;; Print timer value to buffer
  ("p" org-timer)
  ("w" (org-clock-in '(4)))
  ("o" org-clock-out)
  ;; Visit the clocked task from any buffer
  ("j" org-clock-goto)
  ("m" org-mru-clock-in)
  ("c" org-clock-cancel)
  ("d" org-clock-display))

(defhydra highlighting-hydra (:color blue :hint nil)
  "
Highlight                  Unhighlight
------------------------------------------------
_l_ines matching regexp      regular _e_xpression
_p_hrase                 
_r_egular expression
lines containing _E_rror     lines containing E_r_ror

"
  ("l" highlight-lines-matching-regexp)
  ("p" highlight-phrase)
  ("r" highlight-regexp)
  ("e" unhighlight-regexp)
  ("E" (call-interactively '(lambda ()
                              (interactive)
                              (highlight-lines-matching-regexp "ERROR" 'hi-pink))))
  ("r" (call-interactively '(lambda ()
                              (interactive)
                              (unhighlight-regexp "^.*ERROR.*$")))))

(defhydra hydra-yasnippets (:color blue
                                   :hint nil)
  "
Yasnippet - Yasnippet - Yasnippet - Yasnippet - Yasnippet
---------------------------------------------------------
_n_ew snippet for current mode    _e_dit existing snippet
_r_eload snippets                 _i_nsert snippet
"
  ("n" yas-new-snippet)
  ("e" yas-visit-snippet-file)
  ;; Need to be at timer
  ("r" yas-reload-all)
  ("i" yas-insert-snippet))

;;
;;
;; treemacs
;;
;;

(when (featurep 'treemacs)
  (defhydra hydra-treemacs (:color blue)
    "
Project/Workspace                Files
---------------------------------------------------------------------------------
_a_dd project to workspace         copy _p_ath at point
add and display current _p_roject  create _d_ir
copy project _r_oot                create _f_ile
create _w_orkspace                 _d_elete file/dir
collapse al_l_ projects
collapse ot_h_er projects
move project up (↑)
move project down (↓)
_r_emove project
"
    ("a" treemacs-add-project-to-workspace)
    ("p" treemacs-add-and-display-current-project)
    ("f" treemacs-create-file)
    ("d" treemacs-create-dir)
    ("r" treemacs-copy-project-root)
    ("w" treemacs-create-workspace)
    ("d" treemacs-delete)
    ("l" treemacs-collapse-all-projects)
    ("h" treemacs-collapse-other-projects)
    ("<down>" treemacs-move-project-down)
    ("<up>" treemacs-move-project-up)
    ("r" treemacs-remove-project-from-workspace))

  (define-key treemacs-mode-map (kbd "C-h C-m") 'hydra-treemacs/body))

;;
;;
;;
;;
;;

(defhydra ui-ops-hydra (:color blue :hint nil)
        "
Windows        Frames     Buffers         Moving            Appearance
----------------------------------------------------------------------
Split _r_ight    De_t_ach     _s_wap            j← k→             _S_tripe buffer mode 
Split _l_eft     _d_elete     r_o_tate          h↑ l↓
_f_it to Buffer  _m_ake       _e_diff
                          _R_evert
                          _M_inions Menu"
        ("d" mpx-delete-frame)
        ("e" ediff-this)
        ("f" fit-window-to-buffer)
        ("g" windmove-right)
        ("l" split-window-left)
        ("m" make-frame)
        ("o" rotate-windows)
        ("r" split-window-right)
        ("R" revert-buffer)
        ("s" swap-buffers)
        ("S" stripe-buffer-mode)
        ("t" detach-window)
        ("<left>" windmove-left)
        ("<up>" windmove-up)
        ("<down>" windmove-down)
        ("<right>" windmove-right)
        ("j" windmove-left)
        ("h" windmove-up)
        ("l" windmove-down)
        ("k" windmove-right)
        ("M" minions-minor-modes-menu))

(defhydra hydra-dired (:hint nil :color "#268bd2")
              "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
              ("\\" dired-do-ispell)
              ("(" dired-hide-details-mode)
              (")" dired-omit-mode)
              ("+" dired-create-directory)
              ("=" diredp-ediff)         ;; smart diff
              ("?" dired-summary)
              ("$" diredp-hide-subdir-nomove)
              ("A" dired-do-find-regexp)
              ("C" dired-do-copy)        ;; Copy all marked files
              ("D" dired-do-delete)
              ("E" dired-mark-extension)
              ("e" dired-ediff-files)
              ("F" dired-do-find-marked-files)
              ("G" dired-do-chgrp)
              ("g" revert-buffer)        ;; read all directories again (refresh)
              ("i" dired-maybe-insert-subdir)
              ("l" dired-do-redisplay)   ;; relist the marked or singel directory
              ("M" dired-do-chmod)
              ("m" dired-mark)
              ("O" dired-display-file)
              ("o" dired-find-file-other-window)
              ("Q" dired-do-find-regexp-and-replace)
              ("R" dired-do-rename)
              ("r" dired-do-rsynch)
              ("S" dired-do-symlink)
              ("s" dired-sort-toggle-or-edit)
              ("t" dired-toggle-marks)
              ("U" dired-unmark-all-marks)
              ("u" dired-unmark)
              ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
              ("w" dired-kill-subdir)
              ("Y" dired-do-relsymlink)
              ("z" diredp-compress-this-file)
              ("Z" dired-do-compress)
              ("q" nil)
              ("." nil :color blue))

(defhydra hydra-xml (:color blue :hint nil)
    "
xml mode hydra^^
----------------------------------------------
_p_: pretty print buffer

"
    ("p" xml-pretty-print-buffer))

(defhydra hydra-http (:color blue :hint nil)
    "
Know your HTTP well^^
----------------------------------------------
_h_: Headers    _m_: Methods
_r_: Relations  _s_: Status codes
"
    ("h" http-header)
    ("m" http-method)
    ("r" http-relation)
    ("s" http-status-code))

(defhydra hydra-global-scratch (:color blue :hint nil)
  "
Mode^^
----------------------------------------------
_t_ext-mode    _p_ython-mode        _o_rg-mode
_j_ava-mode    emacs-_l_isp-mode    _s_h-mode
jso_n_-mode    _x_ml-mode

"
  ("n" scratch-goto-json-mode)
  ("t" scratch-goto-text-mode)
  ("l" scratch-goto-emacs-lisp-mode)
  ("p" scratch-goto-python-mode)
  ("j" scratch-goto-java-mode)
  ("o" scratch-goto-org-mode)
  ("s" scratch-goto-shl-mode)
  ("x" scratch-goto-xml-mode))

(defhydra hydra-projectile (:color lightblue
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

  Find File          Search/Tags       Buffers              Cache                      Projects
---------------------------------------------------------------------------------------------------------------
  _F_: file            _t_: xref find tag  _i_: Ibuffer           _c_: cache clear             _w_: Start new website
 _ff_: file dwim                         _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur    _K_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                            ^^^^_z_: cache current
  _d_: find dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("F"   projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("t"   xref-find-definitions-other-window)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("o"   projectile-multi-occur)
  ("P"   projectile-switch-project "switch project")
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue)
  ("w" start-website))

(defun mpx-maven-hydra-wrapper ()
  (interactive)
  (if (and
       (featurep 'projectile)
       (projectile-project-root))
      (hydra-maven-projectile/body)
    (message "Projectile is not available. Calling maven hydra makes no sense.")))

(global-set-key (kbd "C-h C-p") 'hydra-projectile/body)

;;
;;
;;
;;
;;

(defhydra hydra-maven-projectile (:color lightblue :hint nil :exit t)
  "
PROJECTILE: %(projectile-project-root)


lifecycle     ^clean^          ^default^
^^^^^^^^-----------------------------------------------------------------
           _c_: clean       _v_: validate
                          _C_: compile
                          _t_: test
                          _p_: package     
                          _V_: verify
                          _i_: install
                          _d_: deploy        
"
  ("c" mpx-maven-clean)
  ("v" mpx-maven-validate)
  ("C" mpx-maven-compile)
  ("t" mpx-maven-test)
  ("p" mpx-maven-package)
  ("V" mpx-maven-verify)
  ("i" mpx-maven-install)
  ("d" mpx-maven-deploy))

;; And now they are put into some keymaps

(define-key org-mode-map (kbd "C-h o") 'hydra-global-org/body)

(define-key yas-minor-mode-map (kbd "C-h y") 'hydra-yasnippets/body)

(global-set-key (kbd "C-h C-u") 'ui-ops-hydra/body)

(define-key dired-mode-map (kbd "C-h C-h") 'hydra-dired/body)

(define-key nxml-mode-map (kbd "C-h C-m") 'hydra-xml/body)

(global-set-key (kbd "C-h C-h") 'hydra-http/body)

(global-set-key (kbd "C-h C-v") 'hydra-magit/body)  ;; C-h C-v - v for version control

(define-key logview-mode-map (kbd "C-h C-m") 'hydra-highlighting/body)
(global-set-key (kbd "C-h C-i") 'hydra-highlighting/body)

(global-set-key (kbd "C-h C-s") 'hydra-global-scratch/body)

(define-key java-mode-map (kbd "C-h C-m") 'mpx-maven-hydra-wrapper)

(provide 'hydras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hydras.el ends here
