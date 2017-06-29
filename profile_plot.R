library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)


profile_plot <- function(segment_num = '643641') {
    
    seg_summary <- content(GET(url = paste0("https://www.strava.com/api/v3/segments/", segment_num),
                       config = add_headers(Authorization = "Bearer 1dae577174b2494adcfdca6a124fb303f56dc4ef")))
    

    url <- paste0("https://www.strava.com/api/v3/segments/", segment_num, "/streams/altitude")
    alt <- GET(url,
               config = add_headers(Authorization = "Bearer 1dae577174b2494adcfdca6a124fb303f56dc4ef")
               )
    
    distance <- unlist(content(alt)[[1]]$data)
    altitude <- unlist(content(alt)[[2]]$data)
    
    tab <- tibble(distance, altitude) %>% 
        group_by(km = distance %/% 1000) %>% 
        summarise(alt = min(altitude)) %>%
        rbind(c(max(distance) / 1000, max(altitude))) %>%
        mutate(km_next = lead(km), alt_next = lead(alt)) %>%
        mutate(gradient = round(0.1 * (alt_next - alt)/(km_next - km), digits = 1)) %>%
        slice(-nrow(.))
    
    ids <- rownames(tab)
    values <- tibble(id = ids, gradient = tab$gradient)
    
    min_x <- floor(min(tab$alt)/100) * 100
    
    positions <- tibble(id = rep(rep(ids, each = 2), 2),
                        x = rep(sort(c(tab$km, tab$km_next)), 2),
                        #y = c(rep(0, length(ids)*2), c(tab$alt, tab$alt_next)[ order(rep(tab$km, 2)) ] ),
                        y = c(rep(min_x, length(ids)*2), c(tab$alt, tab$alt_next)[ order(rep(tab$km, 2)) ] ),
                        vertex = c(rep(1:2, length(ids)), rep(4:3, length(ids))))
    
    datapoly2 <- full_join(values, positions, by = "id") %>%
        group_by(id) %>%
        arrange(vertex)
    
    ## plotting
    ggplot(datapoly2, aes(x = x, y = y)) + 
        geom_polygon(aes(fill = gradient, group = id), alpha = 0.9, color = "black") +
        ylab("Altitude (m)") +
        xlab("Distance (km)") +
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
