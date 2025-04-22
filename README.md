# ESS-plot
Display ESS/R plots in a dedicated Emacs window

![image](https://github.com/DennieTeMolder/ess-plot/assets/51680200/5c387d64-53ea-468f-9b84-26fc6f256cb5)

## Installation
### Emacs
Add the "ess-plot" folder to your `load-path`. 

To enable ESS-plot in all R processes add the following to our `init.el`:
```emacs-lisp
(add-hook 'ess-r-post-run-hook #'ess-plot-on-startup-h')
```

Alternatively you can call `M-x ess-plot-toggle` to start redirecting plots for
that process.

### Straight
Add the following to your `init.el`:
```emacs-lisp
(use-package ess-plot
  :straight (ess-plot :type git :host github :repo "DennieTeMolder/ess-plot")
  :hook (ess-r-post-run . ess-plot-on-startup-h))
```

### Doom
Add the following to your `package.el`:
```emacs-lisp
(package! ess-plot
  :recipe (:host github :repo "DennieTeMolder/ess-plot"))
```

And add the following to your `config.el`
```emacs-lisp
(use-package! ess-plot
  :hook (ess-r-post-run . ess-plot-on-startup-h))
```

## Usage
When not using `ess-plot-on-startup-h`, call `M-x ess-plot-toggle` to start
redirecting plots for the current process. Gg-plots should be rendered
automatically, but base-R plots require calling `M-x ess-plot-show` or
`dev.flush()` in R to render the plot to the window. Plots are displayed in PNG
format thus plot history can be navigated using `image-mode` bindings (i.e.
`image-previous-file`). Calling `M-x ess-plot-hide` hides the plot window until
a new plot is generated. If the plot window was closed call `M-x ess-plot-show`
to re-display the last plot. Calling `ess-plot-toggle` again stops plots from
being redirected.

You can change the resolution and size of the next plot from inside of R. 
The code below restores the default settings:
```R
options(
  plot.width = 7,
  plot.height = 5,
  plot.units = "in",
  plot.res = 300
)
```

You can control if `ess-plot-toggle` will immediately create the plot window or
only when a new plot is rendered (default) by setting
`ess-plot-window-show-on-startup` to `t` or `nil` respectively. You can
customize how plot buffers are displayed by changing
`ess-plot-display-function`. Setting it to `display-buffer` will ensure the plot
window adheres more strictly to Emacs's window display rules.

## Limitations
 - Only implemented for the R dialect (help is welcome for others)

## Alternatives
- httpgd (R package) & `M-x xwidget-webkit-browse-url`: This does provide a more
  RStudio like experience, allowing the user to zoom and export plots. However,
  in contrast to ESS-plot, this combination requires Emacs to be compiled with
  xwidget support and requires installation of packages into the user's R
  environment.
