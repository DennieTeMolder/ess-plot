# ESS-plot
Display ESS/R plots in a dedicated Emacs window

![image](https://github.com/DennieTeMolder/ess-plot/assets/51680200/5c387d64-53ea-468f-9b84-26fc6f256cb5)

## Installation
### Emacs
Add the "ess-plot" folder to your `load-path`.

### Straight
Add the following to your `init.el`
```emacs-lisp
(use-package ess-plot
  :straight (ess-plot :type git
                      :host github
                      :repo "DennieTeMolder/ess-plot"
                      :files ("*.el" "etc"))
  :defer t)
```

### Doom
Add the following to your `package.el`:
```emacs-lisp
(package! ess-plot
  :recipe (:host github :repo "DennieTeMolder/ess-plot"))
```

And add the following to your `config.el`
```emacs-lisp
(use-package! ess-plot :defer t)
```

## Usage
Call `M-x ess-plot-toggle` to start redirecting plots.
Gg-plots should be redirected automatically, base-R plots require calling
`dev.flush()` to push the plot to the window. If the plot window was closed
call M-x `ess-plot-show` to redisplay the last plot. Plots are displayed in
PNG format thus plot history can be navigated using `image-mode` bindings
(i.e. `image-previous-file`).

## Limitations
 - After reloading the process the user needs to call `ess-plot-toggle` twice to restore functionality
 - Only implemented for the R dialect (help is welcome for others)
 - Can only be active for one process at a time

## Alternatives
- httpgd (R package) & `xwidget-webkit-browse-url`: This does provide a more
  RStudio like experience, allowing the user to zoom and export plots.
  However, in contrast to ESS-plot, this combination requires Emacs to
  be compiled with xwidget support and forces the user to install additional
  packages into their environment.
