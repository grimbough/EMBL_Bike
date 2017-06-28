source("route_profile.R")

ids <- c("9372716", "9373393", "9373396", "9373398", "9373399")

plots <- list()
for(id in ids) {
    plots[[id]] <- profile_plot_route(segment_num = id)
}

do.call(gridExtra::grid.arrange, c(plots, ncol = 1))