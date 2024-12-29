;; I am using straight.el as my package manager.
;; It is recommended to disable package.el in the early-init.el file
;; to prevent package.el from loading packages automatically.
(setopt package-enable-at-startup nil)

;; Allow computing statistics for use-package.
;; You can check the stats by using M-x use-package-report
(setopt use-package-compute-statistics t)

