# NOTE this file is sourced by ess-plot--load
### Global variables ------
.ESS_PLOT_MASK. <- !isFALSE(getOption("ess_plot.mask_functions"))

### Functions ------
## Plotting ------
# Get index of current plotting device
.ess_plot_dev <- function() {
  filename <- getOption("ess_plot.file", default = 0L)
  dev_files <- sapply(.Devices, function(dev) {
    path <- attr(dev, "filepath", exact = TRUE)
    if (is.null(path))
      return(NA_character_)
    path
  })
  match(filename, dev_files, nomatch = 0L)
}

.ess_plot_is_current <- function() {
  .ess_plot_dev() == dev.cur()
}

.ess_plot_check_opts <- function() {
  stopifnot(is.numeric(unlist(base::options("plot.width", "plot.height", "plot.res"))))
  stopifnot(is.character(getOption("plot.units")))
}

.ess_plot_pdf_sync_opts <- function() {
  opts <- base::options("plot.width", "plot.height", "plot.units")
  if (opts$plot.units == "in") {
    grDevices::pdf.options(width = opts$plot.width, height = opts$plot.height)
  }
}

.ess_plot_new <- function() {
  if (.ess_plot_is_current())
    stop("There is already an open plotting device!")

  .ess_plot_check_opts()
  args <- base::options("plot.width", "plot.height", "plot.units", "plot.res")
  names(args) <- c("width", "height", "units", "res")

  filename <- tempfile(fileext = ".png")
  do.call(grDevices::png, c(list(filename = filename), args))
  base::options(ess_plot.file = filename)

  if (!.ess_plot_is_current())
    stop("Failed to create a plotting device!")

  dev.cur()
}

# NOTE used by ess-plot-start
.ess_plot_start <- function(plot_dir, width = 7, height = 5, units = "in", res = 300) {
  # Cleanup old graphics device if active
  if (.ess_plot_is_current()) {
    dev.off()
  } else if (dev.cur() > 1L) {
    stop("Another graphics devices is already active! Run `dev.off()` until the result is 1.")
  }

  # Set options required for plotting
  base::options(
    "ess_plot.dir" = normalizePath(plot_dir),
    "plot.width" = width,
    "plot.height" = height,
    "plot.units" = units,
    "plot.res" = res
  )
  .ess_plot_pdf_sync_opts()

  # Start initial device
  return_val <- .ess_plot_new()

  write_cmd <- if (.ESS_PLOT_MASK.) "dev.flush()" else ".ess_plot_show()"
  message(
    "Redirecting all graphics to PNG files. Use 'M-x ess-plot-toggle' to disable.",
    "\nCall `", write_cmd, "` to trigger displaying the plot."
  )

  invisible(return_val)
}

.ess_plot_stop <- function() {
  idx <- .ess_plot_dev()
  if (idx > 1L) {
    if (idx != dev.cur())
      warning("ESS-plot: device closed but another is still active.")
    dev.off(idx)
  }
}

.ess_plot_make_filename <- function() {
  plot_dir <- getOption("ess_plot.dir", default = "")
  if (!dir.exists(plot_dir)) {
    stop("ESS-plot directory does not exist: '", plot_dir, "'")
  }

  timestamp <- sub("\\.", "", format(Sys.time(), "%Y%m%d_%H%M%OS3"))
  session_id <- gsub(".*Rtmp([^/\\])", "R\\1", tempdir())
  file.path(plot_dir, paste0(timestamp, "_", session_id, ".png"))
}

# NOTE used by M-x ess-plot-show
.ess_plot_show <- function() {
  # Do noting if plots are not being redirected
  if (.ess_plot_dev() == 0L)
    return(invisible())
  if (!.ess_plot_is_current())
    stop("Cannot render plot to Emacs, another plotting device is active!")

  # Close plot to write to disk
  dev.off()

  # Move plot file to trigger the filewatcher
  plot_file <- getOption("ess_plot.file")
  if (file.exists(plot_file)) {
    file.rename(plot_file, .ess_plot_make_filename())
  }

  # Open a new plot
  invisible(.ess_plot_new())
}

.ess_plot_options <- function(..., .ess_plot_only = TRUE) {
  result <- base::options(...)
  if (is.null(names(list(...))))
    return(result)

  opts <- c("plot.width", "plot.height", "plot.units", "plot.res")
  if (.ess_plot_only && !all(...names() %in% opts))
    stop("The following options are not recognised: ", setdiff(...names(), opts))

  # Reset the graphics device if plotting options were changed
  if (any(names(result) %in% c("plot.width", "plot.height", "plot.units", "plot.res"))) {
    if (.ess_plot_is_current()) {
      .ess_plot_show()
    }
    if (any(names(result) %in% c("plot.width", "plot.height"))) {
      .ess_plot_pdf_sync_opts()
    }
  }
  invisible(result)
}

## Overrides ------
# NOTE: All of the functions should retain their original functionality.
# They should also keep working after dev.off() or when the ESSR_plot device is not active

if (.ESS_PLOT_MASK.) {
  dev.flush <- function(...) {
    if (.ess_plot_is_current()) {
      .ess_plot_show()
    } else {
      grDevices::dev.flush()
    }
  }

  options <- function(...) {
    .ess_plot_options(..., .ess_plot_only = FALSE)
  }

  print.ggplot <- function (...) {
    result <- ggplot2:::print.ggplot(...)
    if (.ess_plot_is_current()) {
      .ess_plot_show()
    }
    invisible(result)
  }

  ggsave <- function(filename,
                     plot = ggplot2::last_plot(),
                     device = NULL,
                     path = NULL,
                     scale = 1,
                     width = getOption("plot.width", NA),
                     height = getOption("plot.height", NA),
                     units = getOption("plot.units", "in"),
                     dpi = getOption("plot.res", 300),
                     limitsize = TRUE,
                     bg = NULL,
                     ...) {
    if (.ess_plot_dev() > 0L) {
      .ess_plot_check_opts()
    }
    ggplot2::ggsave(filename = filename, plot = plot, device = device, path = path,
                    scale = scale, width = width, height = height, units = units,
                    dpi = dpi, limitsize = limitsize, bg = bg, ...)
  }
}

## Environment management ------
.ess_plot_override_S3_method <- function(method, value) {
  env <- get(".__S3MethodsTable__.", envir = .BaseNamespaceEnv)
  if (exists(method, envir = env, inherits = FALSE))
    assign(method, value, envir = env)
  invisible()
}

# NOTE 'ESSR_plot' is referenced by .ess_plot_env_teardown() & M-x ess-plot-loaded-p
.ess_plot_env_attach <- function(env = NULL, warn.conflicts = TRUE) {
  pos <- match("ESSR_plot", search())
  if (is.null(env)) {
    if (is.na(pos))
      stop("ESSR_plot is not currently attached, provide the 'env' argument.")
    ESSR_plot <- as.environment(pos)
  } else {
    ESSR_plot <- env
  }

  if (!is.na(pos))
    detach(pos = pos)
  return_val <- attach(ESSR_plot, warn.conflicts = warn.conflicts)

  # NOTE this cannot be performed in the setup functions as it must be
  # re-triggered when ggplot2 is loaded
  if (.ESS_PLOT_MASK.)
    .ess_plot_override_S3_method("print.ggplot", print.ggplot)

  return(return_val)
}

.ess_plot_env_setup <- function(env = NULL) {
  .ess_plot_env_teardown()

  # Try to stay on top of the packages we are masking
  if (.ESS_PLOT_MASK.) {
    hook_fun <- function(...) {
      if ("ESSR_plot" %in% search())
        .ess_plot_env_attach(warn.conflicts = FALSE)
    }
    setHook(packageEvent("grDevices", "attach"), hook_fun)
    setHook(packageEvent("ggplot2", "attach"), hook_fun)
  }

  .ess_plot_env_attach(env)
}

# NOTE used by M-x ess-plot--unload
.ess_plot_env_teardown <- function(detach = FALSE) {
  # Stop current plotting device if it is active
  .ess_plot_stop()

  if (.ESS_PLOT_MASK.) {
    if (isNamespaceLoaded("ggplot2"))
      .ess_plot_override_S3_method("print.ggplot", ggplot2:::print.ggplot)
    setHook(packageEvent("grDevices", "attach"), NULL, "replace")
    setHook(packageEvent("ggplot2", "attach"), NULL, "replace")
  }

  if (detach)
    detach("ESSR_plot", character.only = TRUE)
  invisible(detach)
}


### Main ------
.ess_plot_env_setup(environment())
