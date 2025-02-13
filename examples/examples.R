# For M-x ess-plot-load
local(source('./ess-plot.R', local = TRUE))

# Alternative if Emacs is compiled w/ xwidgets support (https://github.com/nx10/httpgd)
## httpgd::hgd()
## httpgd::hgd_close()

library(ggplot2)
theme_set(theme_bw())

# Automatic ggplot
ggplot(economics, aes(date, unemploy)) + geom_line()

# Change plot size
options("plot.width" = 10)

ggplot(economics, aes(pop, unemploy)) + geom_point()

# ggsave uses the set options unless specified
(my_file <- tempfile(fileext = ".pdf"))
ggsave(my_file, height = 3)

# Reset
options("plot.width" = 7)

# Base R plots require calling dev.flush()
plot(economics$date, economics$unemploy)
dev.flush()

hist(economics$unemploy)
dev.flush()
