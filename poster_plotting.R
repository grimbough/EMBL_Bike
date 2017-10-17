source("route_profile.R")
source("profile_plot.R")
source("whole_route.R")
library(cowplot)

segment_nums <- c("9373399", "9373398", "9373396", "9373393", "9372716")

poster_plot <- function(segment_num = '643641') {
    
    tmp_list <- list()
    for(seg in segment_nums) {
    
    seg_summary <- content(GET(url = paste0("https://www.strava.com/api/v3/routes/", seg),
                               config = add_headers(Authorization = "Bearer 1dae577174b2494adcfdca6a124fb303f56dc4ef")))
    
    
    url <- paste0("https://www.strava.com/api/v3/routes/", seg, "/streams")
    alt <- GET(url,
               config = add_headers(Authorization = "Bearer 1dae577174b2494adcfdca6a124fb303f56dc4ef")
    )
    
    distance <- unlist(content(alt)[[2]]$data)
    altitude <- unlist(content(alt)[[3]]$data)
    
    tab <- tibble(distance, altitude) %>% 
        group_by(km = distance %/% 1000) %>% 
        summarise(alt = min(altitude)) %>%
        rbind(c(max(distance) / 1000, altitude[length(altitude)])) %>%
        mutate(km_next = lead(km), alt_next = lead(alt)) %>%
        mutate(gradient = round(0.1 * (alt_next - alt)/(km_next - km), digits = 1)) %>%
        slice(-nrow(.))

    ids <- paste(seg, rownames(tab), sep = "_")
    values <- tibble(id = ids, gradient = tab$gradient)
    
    min_x <- floor(min(tab$alt)/100) * 100
    
    positions <- tibble(id = rep(rep(ids, each = 2), 2),
                        x = rep(sort(c(tab$km, tab$km_next)), 2),
                        y = c(rep(0, length(ids)*2), c(tab$alt, tab$alt_next)[ order(rep(tab$km, 2)) ] ),
                        #y = c(rep(min_x, length(ids)*2), c(tab$alt, tab$alt_next)[ order(rep(tab$km, 2)) ] ),
                        vertex = c(rep(1:2, length(ids)), rep(4:3, length(ids))))
    
    tmp_list[[ seg ]] <- full_join(values, positions, by = "id") %>%
        mutate(route = seg) %>%
        group_by(id) %>%
        arrange(vertex)
    }
    
    datapoly <- bind_rows(tmp_list)
    
    ## scale data
    datapoly <- group_by(datapoly, route) %>%
        mutate(x = x / max(x)) 
    
    colours <- RColorBrewer::brewer.pal(n = 6, name = "Reds")
    
    
    ## plotting
    ggplot(datapoly, aes(x = x, y = y3)) + 
        geom_polygon(data = filter(datapoly, route == segment_nums[1]),
                     aes(group = id), 
                         fill = colours[2], 
                         color = colours[2]) +
        geom_polygon(data = filter(datapoly, route == segment_nums[2]),
                     aes(group = id), 
                         fill = colours[3], 
                         color = colours[3]) +
        geom_polygon(data = filter(datapoly, route == segment_nums[3]),
                     aes(group = id), 
                     fill = colours[4], 
                     color = colours[4]) +
        geom_polygon(data = filter(datapoly, route == segment_nums[4]),
                     aes(group = id), 
                     fill = colours[5], 
                     color = colours[5]) +
        geom_polygon(data = filter(datapoly, route == segment_nums[5]),
                     aes(group = id), 
                     fill = colours[6], 
                     color = colours[6]) +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_rect(fill = colours[1]),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank()) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0))
    
    
    +
        ggtitle(seg_summary$name) +
        labs(subtitle = paste0("Average Gradient: ", seg_summary$average_grade, "%",
                               "\nLength: ", round(seg_summary$distance/1000, digits = 1), "km")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5),
              axis.text.y = element_text(size = 8),
              plot.title = element_text(family="Bookman", face="bold", size=16),
              plot.subtitle = element_text(family="AvantGarde")) +
        scale_x_continuous(breaks = function(x){ seq(from = floor(x[1]), to = ceiling(x[2]), by = 1) },
                           minor_breaks = NULL,
                           expand = c(0,0)) +
        scale_y_continuous(breaks = function(y){ seq(from = floor(y[1]), to = ceiling(y[2]), by = 100) },
                           minor_breaks = NULL,
                           limits = c(min(datapoly2$y), max(datapoly2$y) + 100),
                           expand = c(0,0)) +
        coord_fixed(ratio = 1/100) +
        scale_fill_gradient2(high = "red", mid = "white", low = "blue",
                             limits = c(-11,11)) +
        geom_label(data = tab, aes(x = km+0.45, y = alt_next, label = gradient),
                   label.padding = unit(0.1, "lines"),
                   size = 1)
    
    
}



profile_plot_route(segment_num = id) +
        scale_fill_gradient2(limits = c(-10,10), high = "#006666", low = "orange", mid = "white" )
}


