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
        plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue'))+
  coord_equal()+
  # labs(subtitle='Reference map')+
  theme(axis.title = element_blank())+
  theme(
    # # plot.margin = margin(t=0, b=0),
    # plot.margin = margin(b=0, l=0),
    # plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = 'NA', colour = 'lightgrey'),
  )

arrow_1 <- (extent(Tigray)@xmin + extent(Tigray)@xmax)/2

map1 <- map1+
  geom_segment(aes(x=arrow_1, xend=arrow_1, y=extent(Tigray)@ymax, yend=Inf), 
                  arrow = arrow(length = unit(0.10, "cm"), type = "closed"),
               linejoin='mitre', lineend = 'butt', size = 0.25, color='red', lwd=0.25)

arrow_2 <- (extent(Kisumu)@xmin + extent(Kisumu)@xmax)/2

map1 <- map1+
  geom_segment(aes(x=arrow_2, xend=arrow_2, y=extent(Kisumu)@ymin, yend=-Inf), 
               arrow = arrow(length = unit(0.10, "cm"), type = "closed"),
               linejoin='mitre', lineend = 'butt', size = 0.25, color='red', lwd=0.25)
# map3 <- data.frame(x= c(39.549691,  39.558858,  39.558730,  39.549640), y= c(12.375093,  12.375163,  12.370600, 12.370590))
# coordinates(map3) <- ~x+y
# proj4string(map3) <- "+proj=longlat +ellps=WGS84"
# ext <- extent(map3)

# # map3 <- get_map(location = c(lon = 39.642680, lat = 12.828424),
# #                 maptype = "satellite", zoom = 17)
# 
# map3 <- get_map(location = c(lon = 39.55423, lat = 12.37286),
#                 maptype = "satellite", zoom = 17)
# 
# saveRDS(map3, 'data_files/ggmaps/spate_ggmap.rds')


# map4 <- get_map(location = c(lon = 34.948958, lat = -0.143039),
#                 maptype = "satellite", zoom = 17)
# 
# saveRDS(map4, 'data_files/ggmaps/inundation_canal_ggmap.rds')
# 

map3 <- ggmap(readRDS('data_files/ggmaps/spate_ggmap.rds'))+
  scale_x_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  scale_y_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  theme_minimal()+
  my_theme +
  theme(axis.text = element_text(size = 8),
        axis.title = element_blank(),
        plot.subtitle = element_text(size = 10, face = 'italic', color = 'blue'))+
  coord_equal()+
  labs(subtitle='Typical FBFS landscape in Tigray')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=0, b=0))

map4 <- ggmap(readRDS('data_files/ggmaps/inundation_canal_ggmap.rds'))+
  scale_x_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  scale_y_continuous(expand = c(0,0), labels = function(x) sprintf("%.1f", x))+
  theme_minimal()+
  my_theme +
  theme(axis.text = element_text(size = 8),
        axis.title = element_blank(),
        plot.subtitle = element_text(size = 10, face = 'italic', color = 'blue'))+
  coord_equal()+
  labs(subtitle='Typical FBFS landscape in Kisumu')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=0, b=0))

data.frame(x=ggplot_build(map4)$layout$panel_scales_x[[1]]$range$range, 
           y=ggplot_build(map4)$layout$panel_scales_y[[1]]$range$range)

ext_map3 <- data.frame(x=ggplot_build(map3)$layout$panel_scales_x[[1]]$range$range, 
                              y=ggplot_build(map3)$layout$panel_scales_y[[1]]$range$range)

ext_map4 <- data.frame(x=ggplot_build(map4)$layout$panel_scales_x[[1]]$range$range, 
                              y=ggplot_build(map4)$layout$panel_scales_y[[1]]$range$range)

# as.data.frame(geosphere::centroid(Kisumu))
inst_tigray <- inst_tigray +
  geom_polygon(data = fortify(as(extent(ext_map3), 'SpatialPolygons')), 
               aes(long, lat, group=group), fill="NA", color="red", lwd=0.5)+
  geom_segment(aes(x=39.55767, xend=Inf, y=12.37286, yend=12.37286), 
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               linejoin='mitre', lineend = 'butt', size = 0.25, color='red')

inst_kisumu <- inst_kisumu +
  geom_polygon(data = fortify(as(extent(ext_map4), 'SpatialPolygons')), 
               aes(long, lat, group=group), fill="NA", color="red", lwd=0.5)+
  geom_segment(aes(x=34.95240, xend=Inf, y=-0.1430443, yend=-0.1430443), 
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               linejoin='mitre', lineend = 'butt', size = 0.25, color='red')


map2 <- grid.arrange(arrangeGrob(inst_tigray, nullGrob(), inst_kisumu,
                                heights=unit(c(0.995, 0.01, 0.905), c("null", "null", "null"))
))

# grid.arrange(map3, map1, map4, ncol=1)

# test <- grid.arrange(grid.arrange(map3, map1, map4, ncol=1), map2, ncol=2)
map1 <- arrangeGrob(inst_tigray, 
                   nullGrob(), 
                   map1, 
                   nullGrob(), 
                   inst_kisumu, 
                   heights=unit(c(0.995, 0.01, 1.05, 0.01, 0.905), c("null", "null", "null", "null", "null"))
)

map2 <- grid.arrange(map3, map4)
map2 <- grid.arrange(arrangeGrob(map1, map2, 
                                  layout_matrix = rbind(c(1,1,1,2,2,2,2), c(1,1,1,2,2,2,2)),
                                  bottom = grid::textGrob("Longitude",
                                                              # x=0.4, hjust=0, vjust = 0.5,
                                                              rot = 0,
                                                              gp = gpar(fontfamily='serif',fontsize=10,fontface="plain", col="black")),
                                  left = grid::textGrob("Latitude",
                                                            rot = 90,
                                                            gp = gpar(fontfamily='serif',fontsize=10,fontface="plain", col="black"))
                                  
))

gb = grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
               gp=gpar(lwd=1, fill=NA, col="lightgray"))
map2 = gTree(children = gList(gb, map2))

ggsave(map2, filename = 'figures/study_area_map.png', device = 'png', width = 130,height = 149, units = 'mm')
## =====> pdf
ggsave(map2, filename = 'figures/study_area_map.pdf', device = 'pdf', width = 130,height = 149, units = 'mm')
