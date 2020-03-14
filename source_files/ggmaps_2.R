library(ggmap)
# source('source_files/schemes.R')
source('source_files/inset_maps_4.R')


# map1 <- get_map(location = c(lon = 37.764786, lat = 6.938531), 
#                 maptype = "terrain", zoom = 5)
# saveRDS(map1, 'data_files/ggmaps/horn_of_africa.rds')

map1 <- readRDS('data_files/ggmaps/horn_of_africa.rds')

map1<- ggmap(map1)+
  scale_x_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  scale_y_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  xlim(c(33, 50.5))+
  ylim(c(-0.5, 15))+
  geom_polygon(data = fortify(as(extent(Kisumu), 'SpatialPolygons')), 
               aes(long, lat, group=group), fill="NA", color="red")+
  geom_polygon(data = fortify(as(extent(Tigray), 'SpatialPolygons')), 
               aes(long, lat, group=group), fill="NA", color="red")+
  theme_minimal()+
  my_theme +
  theme(axis.text = element_text(size = 8),
        axis.title = element_blank(),
        plot.subtitle = element_text(size = 10, face = 'italic', color = 'blue'))+
  coord_equal()+
  # labs(subtitle='Reference map')+
  theme(axis.title = element_blank())+
  theme(plot.background = element_rect(fill = 'NA', colour = 'lightgrey'))+
  labs(subtitle = 'Reference map')

arrow_1 <- (extent(Tigray)@ymin + extent(Tigray)@ymax)/2

map1 <- map1+
  geom_segment(aes(x=extent(Tigray)@xmax, xend=Inf, y=arrow_1, yend=arrow_1), 
               arrow = arrow(length = unit(0.30, "cm"), type = "closed"),
               linejoin='mitre', lineend = 'butt', size = 0.46, color='red', lwd=0.25)+
  scalebar(x.min = min(map1$data$lon), x.max = max(map1$data$lon),
           y.min = min(map1$data$lat), y.max = max(map1$data$lat),
           dist = 300, dist_unit = "km",
           transform = TRUE, model = "WGS84", anchor=c(x=50, y=0), 
           height=0.015, st.dist=0.015, st.size=2, border.size =0.25)+
  north(x.min = min(map1$data$lon), x.max = max(map1$data$lon),
        y.min = min(map1$data$lat), y.max = max(map1$data$lat),
        scale = 0.15, anchor=c(x=51, y=5))
  

arrow_2 <- (extent(Kisumu)@ymin + extent(Kisumu)@ymax)/2

map1 <- map1+
  geom_segment(aes(x=extent(Kisumu)@xmin, xend=-Inf, y=arrow_2, yend=arrow_2), 
               arrow = arrow(length = unit(0.30, "cm"), type = "closed"),
               linejoin='mitre', lineend = 'butt', size = 0.45, color='red', lwd=0.25)

map3 <- ggmap(readRDS('data_files/ggmaps/spate_ggmap.rds'))+
  scale_x_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  scale_y_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  theme_minimal()+
  my_theme +
  theme(axis.text = element_text(size = 8),
        axis.title = element_blank(),
        plot.subtitle = element_text(size = 10, face = 'italic', color = 'blue'))+
  coord_equal()+
  theme(axis.title = element_blank())+
  # theme(plot.background = element_rect(fill = 'NA', colour = 'lightgrey'))+
  theme(plot.margin = margin(t=0, b=0))+
  labs(subtitle='Typical FBFS landscape in Tigray')
  
map3 <- map3 + 
  scalebar(x.min = min(map3$data$lon), x.max = max(map3$data$lon),
           y.min = min(map3$data$lat), y.max = max(map3$data$lat),
           dist = 100, dist_unit = "m", 
           transform = TRUE, model = "WGS84", anchor=c(x=39.5574, y=12.36986), 
           height=0.015, st.dist=0.015, st.size=2, st.color = 'white',
           border.size =0.25, box.fill = c("yellow", "white"))+
  north(x.min = min(map3$data$lon), x.max = max(map3$data$lon),
        y.min = min(map3$data$lat), y.max = max(map3$data$lat),
        scale = 0.15, anchor=c(x=39.557, y=12.37110))

map4 <- ggmap(readRDS('data_files/ggmaps/inundation_canal_ggmap.rds'))+
  scale_x_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  scale_y_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  theme_minimal()+
  my_theme +
  theme(axis.text = element_text(size = 8),
        axis.title = element_blank(),
        plot.subtitle = element_text(size = 10, face = 'italic', color = 'blue'))+
  coord_equal()+
  theme(axis.title = element_blank())+
  # theme(plot.background = element_rect(fill = 'NA', colour = 'lightgrey'))+
  theme(plot.margin = margin(t=0, b=0))+
  labs(subtitle='Typical FBFS landscape in Kisumu')

map4 <- map4 + 
  scalebar(x.min = min(map4$data$lon), x.max = max(map4$data$lon),
           y.min = min(map4$data$lat), y.max = max(map4$data$lat),
           dist = 100, dist_unit = "m",
           transform = TRUE, model = "WGS84", anchor=c(x=34.95215, y=-0.1461), 
           height=0.015, st.dist=0.015, st.size=2, st.color = 'white',
           border.size =0.25, box.fill = c("yellow", "white"))+
  north(x.min = min(map4$data$lon), x.max = max(map4$data$lon),
        y.min = min(map4$data$lat), y.max = max(map4$data$lat),
        scale = 0.15, anchor=c(x=34.9518, y=-0.1449))

ext_map3 <- data.frame(x=ggplot_build(map3)$layout$panel_scales_x[[1]]$range$range, 
                       y=ggplot_build(map3)$layout$panel_scales_y[[1]]$range$range)

ext_map4 <- data.frame(x=ggplot_build(map4)$layout$panel_scales_x[[1]]$range$range, 
                       y=ggplot_build(map4)$layout$panel_scales_y[[1]]$range$range)

# # as.data.frame(geosphere::centroid(Kisumu))
# inst_tigray <- inst_tigray +
#   geom_polygon(data = fortify(as(extent(ext_map3), 'SpatialPolygons')), 
#                aes(long, lat, group=group), fill="NA", color="red", lwd=0.5)+
#   geom_segment(aes(x=(extent(ext_map3)@xmin + extent(ext_map3)@xmax)/2, xend=(extent(ext_map3)@xmin + extent(ext_map3)@xmax)/2, y= extent(ext_map3)@ymin, yend=-Inf), 
#                arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
#                linejoin='mitre', lineend = 'butt', size = 0.25, color='red')


inst_tigray <- inst_tigray +
  geom_polygon(data = fortify(as(extent(ext_map3), 'SpatialPolygons')), 
               aes(long, lat, group=group), fill="NA", color="red", lwd=0.75)+
  geom_curve(aes(x=(extent(ext_map3)@xmin + extent(ext_map3)@xmax)/2, xend=((extent(ext_map3)@xmin + extent(ext_map3)@xmax)/2)+0.2457646,
                 y= extent(ext_map3)@ymax, yend=extent(ext_map3)@ymax),
             # arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
             size = 0.45, color='red', curvature = -2)+
  geom_segment(aes(x=(0.2457646+(extent(ext_map3)@xmin + extent(ext_map3)@xmax)/2), xend=(0.2457646+(extent(ext_map3)@xmin + extent(ext_map3)@xmax)/2), y= extent(ext_map3)@ymin, yend=-Inf), 
               arrow = arrow(length = unit(0.35, "cm"), type = "closed"),
               linejoin='mitre', lineend = 'butt', size = 0.45, color='red')
  

inst_kisumu <- inst_kisumu +
  geom_polygon(data = fortify(as(extent(ext_map4), 'SpatialPolygons')), 
               aes(long, lat, group=group), fill="NA", color="red", lwd=0.75)+
  geom_segment(aes(x=(extent(ext_map4)@xmin + extent(ext_map4)@xmax)/2, xend=(extent(ext_map4)@xmin + extent(ext_map4)@xmax)/2, y= extent(ext_map4)@ymin, yend=-Inf), 
               arrow = arrow(length = unit(0.35, "cm"), type = "closed"),
               linejoin='mitre', lineend = 'butt', size = 0.45, color='red')

map1 <- cbind(ggplotGrob(inst_kisumu), ggplotGrob(map1), ggplotGrob(inst_tigray), size = "last")

map2 <- cbind(ggplotGrob(map4), ggplotGrob(map3), size='last')

map2 <- arrangeGrob(map1, map2,
                    heights=unit(c(0.38, 0.7), c("null", "null"))
)


gb = grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
               gp=gpar(lwd=1, fill=NA, col="lightgray"))
map2 = gTree(children = gList(gb, map2))

map2 <- list(map2)

map2$top = grid::textGrob(" ",
                     # x=0.4, hjust=0, vjust = 0.5,
                     rot = 0,
                     gp = gpar(fontfamily='serif',fontsize=10,fontface="plain", col="black"))

map2$right = grid::textGrob(" ",
                       # x=0.4, hjust=0, vjust = 0.5,
                       rot = 0,
                       gp = gpar(fontfamily='serif',fontsize=10,fontface="plain", col="black"))

map2$bottom = grid::textGrob("Longitude",
                        # x=0.4, hjust=0, vjust = 0.5,
                        rot = 0,
                        gp = gpar(fontfamily='serif',fontsize=10,fontface="plain", col="black"))

map2$left = grid::textGrob("Latitude",
                      rot = 90,
                      gp = gpar(fontfamily='serif',fontsize=10,fontface="plain", col="black"))

map2 <- do.call(grid.arrange, map2)
map2 = gTree(children = gList(gb, map2))

# ggsave(map2,filename = 'figures/study_area_map.png', device = 'png', width=163, height = 131,units = 'mm', dpi = 300, scale=1)

ggsave(map2, filename = 'figures/study_area_map.png', device = 'png', width = 190, height = 151, units = 'mm', dpi = 300, scale=1)
## =====> pdf
ggsave(map2, filename = 'figures/study_area_map.pdf', device = 'pdf', width = 190, height = 151, units = 'mm',dpi = 300, scale=1)
