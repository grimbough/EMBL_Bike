source("route_profile.R")
source("profile_plot.R")
source("whole_route.R")
library(cowplot)
library(grid)


## EMBL ride
segment_nums <- c("9373399", "9373398", "9373396", "9373393", "9372716")

## RAID Sardinia
sardinia <- c("10929236", "10929234", "10929232", "10929230", "10929229", "10929196")
colours<-colorRampPalette(c("#E75480", "#FDD7E4"))(7)
colours<-colorRampPalette(c("hotpink3", "#FDD7E4"))(7)
colours <- colorRampPalette(c("#c64b85", "#FDD7E4"))(7)

## RAID Pyrenean
segment_nums <- c("10935793", "10935790", "10935786","10935783","10935780")

## Picos
picos <- c('10957118', '10957111','10957109','10957105','10957104','10957101')
colours <- rev(RColorBrewer::brewer.pal(n = length(picos)+1, name = "Reds"))

poster_plot <- function(segment_nums = NULL, output_file = "picos_label.pdf", title = "PICOS DE EUROPA", date = "September 2016", colours = colours, lower_border = TRUE) {
    
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
        
        ##any altitude below 0, set to 1
        altitude[which(altitude <= 0)] <- 1
        
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
        mutate(x = x / max(x), route_idx = match(route, rev(segment_nums))) %>% mutate(y = y / max(y))
    
    for(i in 2:length(segment_nums)) {
        datapoly[which(datapoly$route_idx == i & datapoly$y != 0), "y"] <- datapoly[which(datapoly$route_idx == i & datapoly$y != 0), "y"] + 
            max(datapoly[which(datapoly$route_idx == (i-1)), "y"])
    }
    
    if(lower_border) {
        datapoly$y[which(datapoly$y == 0)] <- -0.5
    }
    
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
                     plot.background=element_blank()#,
                     #aspect.ratio = 5/4
                     ) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), limits = c(min(datapoly$y), max(datapoly$y)*1.1))
    
    p1 <- p1 + theme(plot.margin = unit(rep(3, 4), units = "cm")) +
        geom_text(label = title, x = 0.5, y = -0.75, size = 18, family = "Sawasdee", fontface = "bold") + 
        geom_text(label = date, x = 0.92, y = -0.35, size = 6, family = "Sawasdee", col = colours[length(colours)])
    
    gt <- ggplot_gtable(ggplot_build(p1))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"

    ggsave(grid.draw(gt), filename = output_file, width = 40, height = 50, units = "cm")
    
    return(NULL)
    
}


