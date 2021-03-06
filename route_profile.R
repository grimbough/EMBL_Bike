library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)


profile_plot_route <- function(segment_num = '8734063') {
    
    seg_summary <- content(GET(url = paste0("https://www.strava.com/api/v3/routes/", segment_num),
                               config = add_headers(Authorization = "Bearer 1dae577174b2494adcfdca6a124fb303f56dc4ef")))
    
    
    url <- paste0("https://www.strava.com/api/v3/routes/", segment_num, "/streams")
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
    
    ## calculate the total ascent based on smoothed data
    height_meters <- mutate(tab, diff = alt_next - alt) %>% 
        mutate(up_down = ifelse(diff > 0, "ascent", "decent")) %>%
        group_by(up_down) %>% 
        summarise(total = ceiling(sum(diff)))
    
    ## plotting
    ggplot(datapoly2, aes(x = x, y = y)) + 
        geom_polygon(aes(fill = gradient, group = id), alpha = 0.9, color = "black",
                     size = 0.3) +
        ylab("Altitude (m)") +
        xlab("Distance (km)") +
        #ggtitle(seg_summary$name) +
        labs(title = seg_summary$name,
            subtitle = paste0("Smoothed Ascent:\t", height_meters$total[1], 
                              "m  |  Strava Ascent:\t", floor(seg_summary$elevation_gain), "m\n",
                               "Smoothed Descent:\t", abs(height_meters$total[2]), "m")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5),
              axis.text.y = element_text(size = 4),
              plot.title = element_text(family="Bookman", face="bold", size=16),
              plot.subtitle = element_text(family="AvantGarde")) +
        scale_x_continuous(breaks = function(x){ seq(from = floor(x[1]), to = ceiling(x[2]), by = 5) },
                           minor_breaks = function(x){ seq(from = floor(x[1]), to = ceiling(x[2]), by = 1) },
                           expand = c(0,0)) +
        scale_y_continuous(breaks = function(y){ seq(from = floor(y[1]), to = ceiling(y[2]), by = 100) },
                           minor_breaks = NULL,
                           limits = c(min(datapoly2$y), max(max(datapoly2$y), 100)),
                           expand = c(0,0)) +
        #coord_fixed(ratio = 1/100) +
        scale_fill_gradient2(limits = c(-10,10), high = "red", low = "blue")
    
    
}
