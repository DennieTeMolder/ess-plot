### Functions ------
## Plotting ------
# Directory to which finalized plots are moved, to trigger Emacs' filewatcher
# NOTE used by M-x ess-plot-process-dir
.ESS_PLOT_DIR. <- file.path(normalizePath(tempdir(check = TRUE)), "session_plots")
if (!file.exists(.ESS_PLOT_DIR.)) dir.create(.ESS_PLOT_DIR.)

.ess_plot_make_filename <- function(n) {
  file.path(.ESS_PLOT_DIR., sprintf("%03d.png", n))
}

.ess_plot_files <- function() {
  list.files(.ESS_PLOT_DIR., pattern = "\\d{3}\\.png$", full.names = TRUE)
}

.ess_plot_create_counter <- function() {
  n <- length(.ess_plot_files())
  function(increment = FALSE) {
    if (increment)
      n <<- n + 1
    n
  }
}
.ess_plot_get_count <- .ess_plot_create_counter()

# NOTE used by M-x ess-plot-file-last
.ess_plot_file_last <- function() {
  n <- .ess_plot_get_count()
  if (n == 0)
    return(NULL)
  .ess_plot_make_filename(n)
}

# Get index of current plotting device
.ess_plot_dev <- function() {
  filename <- getOption("ess_plot.file", default = 0)
  dev_files <- sapply(.Devices, function(dev) {
    path <- attr(dev, "filepath", exact = TRUE)
    if (is.null(path))
      return(NA_character_)
    path
  })
  match(filename, dev_files, nomatch = 0)
}

# NOTE used by ess-plot-current-p
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

.ess_plot_start <- function(width = 7, height = 5, units = "in", res = 300, ...) {
  # Cleanup old graphics device if active
  if (.ess_plot_is_current()) {
    dev.off()
  } else if (dev.cur() > 1) {
    stop("Another graphics devices is already active! Run `dev.off()` until the result is 1.")
  } else {
    message("Redirecting all graphics to PNG files.\n",
            "Call `dev.flush()` to force write/display and `dev.off()` to disable.")
  }

  # Set options required for plotting
  base::options(
    "plot.width" = width,
    "plot.height" = height,
    "plot.units" = units,
    "plot.res" = res
  )
  .ess_plot_pdf_sync_opts()

  # Start initial device
  invisible(.ess_plot_new())
}

.ess_plot_show <- function() {
  # Do noting if plots are not being redirected
  if (.ess_plot_dev() == 0L)
    return(invisible())
  if (!.ess_plot_is_current())
    stop("Currently redirecting plots, but the active device is not targeting the right file!")

  # Close plot to write to disk
  dev.off()

  # Move plot file to trigger the filewatcher
  source_file <- getOption("ess_plot.file")
  if (file.exists(source_file)) {
    target_file <- .ess_plot_make_filename(.ess_plot_get_count(increment = TRUE))
    file.rename(source_file, target_file)
  }

  # Open a new plot
  invisible(.ess_plot_new())
}

## Overrides ------
# NOTE: All of the functions should retain their original functionality.
# They should also keep working after dev.off() or when the ESSR_plot device is not active

# NOTE used by M-x ess-plot-show
dev.flush <- function(...) {
  if (.ess_plot_is_current()) {
    .ess_plot_show()
  } else {
    grDevices::dev.flush()
  }
}

# REVIEW replace w/ grDevices::pdf.options()
options <- function(...) {
  result <- base::options(...)
  if (is.null(names(list(...))))
    return(result)
  # Reset the graphics device if plotting options were changed
  if (any(names(result) %in% c("plot.width", "plot.height", "plot.units", "plot.res"))) {
    .ess_plot_show()
    if (any(names(result) %in% c("plot.width", "plot.height"))) {
      .ess_plot_pdf_sync_opts()
    }
  }
  invisible(result)
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

## Environment management ------
.ess_plot_env <- function() {
  pos <- match("ESSR_plot", search())
  if (is.na(pos))
    stop("ESSR_plot is not currently attached. Call M-x ess-plot-load.")
  as.environment(pos)
}

.ess_plot_register_methods <- function() {
  .overrideS3method <- function(method, value) {
    env <- get(".__S3MethodsTable__.", envir = .BaseNamespaceEnv)
    if (exists(method, envir = env, inherits = FALSE))
      assign(method, value, envir = env)
    invisible()
  }
  .overrideS3method("print.ggplot", print.ggplot)
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
  attach(ESSR_plot, warn.conflicts = warn.conflicts)
  .ess_plot_register_methods()
}

.ess_plot_env_setup <- function(env = NULL) {
  .ess_plot_env_teardown()
  .ess_plot_env_attach(env)

  # Try to stay on top of the packages we are masking
  hook_fun <- function(...) {
    if ("ESSR_plot" %in% search())
      .ess_plot_env_attach(warn.conflicts = FALSE)
  }
  setHook(packageEvent("grDevices", "attach"), hook_fun)
  setHook(packageEvent("ggplot2", "attach"), hook_fun)

  .ess_plot_start()
}

# NOTE used by M-x ess-plot-unload
.ess_plot_env_teardown <- function(detach = FALSE) {
  setHook(packageEvent("grDevices", "attach"), NULL, 'replace')
  setHook(packageEvent("ggplot2", "attach"), NULL, 'replace')

  # Stop current plotting device if it is active
  which <- .ess_plot_dev()
  if (which > 1L) {
    if (which != dev.cur())
      warning("ESS-plot: device closed but another is still active.")
    dev.off(which)
  }

  if (detach)
    detach("ESSR_plot", character.only = TRUE)
  invisible(detach)
}


### Main ------
.ess_plot_env_setup(environment())
