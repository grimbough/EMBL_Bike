source("route_profile.R")
source("profile_plot.R")
source("whole_route.R")
library(cowplot)

## EMBL ride
segment_nums <- c("9373399", "9373398", "9373396", "9373393", "9372716")

## RAID Sardinia
segment_nums <- c("10929236", "10929234", "10929232", "10929230", "10929229", "10929196")

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
                        vertex = c(rep(1:2, length(ids)), rep(4:3, length(ids))))
    
    tmp_list[[ seg ]] <- full_join(values, positions, by = "id") %>%
        mutate(route = seg) %>%
        group_by(id) %>%
        arrange(vertex)
    }
    
    datapoly <- bind_rows(tmp_list)
    
    ## scale data
    datapoly <- group_by(datapoly, route) %>%
        mutate(x = x / max(x), route_idx = match(route, rev(segment_nums))) #%>% mutate(y = y / max(y))
    
    for(i in 2:length(segment_nums)) {
        datapoly[which(datapoly$route_idx == i & datapoly$y != 0), "y"] <- datapoly[which(datapoly$route_idx == i & datapoly$y != 0), "y"] + 
            max(datapoly[which(datapoly$route_idx == (i-1)), "y"])
    }
    
    colours <- rev(RColorBrewer::brewer.pal(n = length(segment_nums)+1, name = "Greens"))
    
    
    ## plotting
    p1 <- ggplot(datapoly, aes(x = x, y = y)) 
    
    for(i in length(segment_nums):1) { 
        p1 <- p1 + geom_polygon(data = filter(datapoly, route_idx == i),
                                aes(group = id), 
                                fill = colours[i], 
                                color = colours[i])
    }
    p1 <- p1 + theme(axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 legend.position="none",
                 panel.background=element_rect(fill = colours[length(colours)]),
                 panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 plot.background=element_blank(),
                 plot.title = element_text(family="Bookman", face="bold", size=16)) +
        scale_x_continuous(expand = c(0,0)) +
        #scale_y_continuous(expand = c(0,0), limits = c(0, length(segment_nums)*1.1)) + 
        scale_y_continuous(expand = c(0,0), limits = c(0, max(datapoly$y)*1.1))
        #annotate(geom = "text", label = "RAID Sardinia", x = 0.5, y = length(segment_nums)*1.05, family="Bookman", size=12)
    
    return(p1)
    
}



profile_plot_route(segment_num = id) +
        scale_fill_gradient2(limits = c(-10,10), high = "#006666", low = "orange", mid = "white" )
}


