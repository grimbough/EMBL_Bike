source("route_profile.R")
source("profile_plot.R")
source("whole_route.R")
library(cowplot)

ids <- c("9372716", "9373393", "9373396", "9373398", "9373399")

plots <- list()
for(id in ids) {
    plots[[id]] <- profile_plot_route(segment_num = id) +
        scale_fill_gradient2(limits = c(-10,10), high = "#006666", low = "orange", mid = "white" )
}

#pdf("/tmp/Rplots.pdf", paper = "a4")
do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
#dev.off()

do.call(gridExtra::grid.arrange, c(plots[1:4], ncol = 1))
do.call(gridExtra::grid.arrange, c(plots[5], ncol = 1, nrow = 4))

p1 <- plot_whole_route()
seg_ids <- c('643641', '767442', '683473', '10515277')
plots2 <- list()
for(seg_id in seg_ids) {
    plots2[[seg_id]] <- profile_plot(seg_id) +
        scale_fill_gradient2(limits = c(-11,11), high = "#006666", low = "orange", mid = "white" )
}

ggdraw() +
    draw_plot(p1, 0, .7, 1, .3) +
    draw_plot(plots2[[1]], 0, .35, .4, .35) +
    draw_plot(plots2[[2]], .4, .35, .6, .35) +
    draw_plot(plots2[[3]], 0, 0, .4, .35) +
    draw_plot(plots2[[4]], .4, 0, .6, .35)



ggdraw() +
    draw_plot(p1, x = 0, y = .75, width = 1, height = .25) +
    draw_plot(plots2[[1]], 0, .5, .5, .25) +
    draw_plot(plots2[[3]], .5, .5, .5, .25) +
    draw_plot(plots2[[2]], 0, .25, 1, .25) +
    draw_plot(plots2[[4]], 0, 0, 1, .25)

