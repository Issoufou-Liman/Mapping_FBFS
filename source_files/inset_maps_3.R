gb = grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
               gp=gpar(lwd=1, fill=NA, col="lightgray"))
## Spatially explicit representation of some of the patterns in the Boxplot ####
# The ggplot has shown various pattern. it would be nice to see them in space.
# In the following we will extract some of these patterns and present them spatially. 
source("source_files/north_arrow_and_scalebar.R")

Kenya_L0 <- getData("GADM", country="ken", level=0, path = 'data_files/shapefiles')
Kisumu <- readOGR('data_files/shapefiles/Kisumu.shp')
Kisumu_localities <- list(Kisumu=Kisumu, Kenya=Kenya_L0)
Kisumu_localities <- sapply(Kisumu_localities, function (i) {
  out=as.data.frame(coordinates(rgeos::gCentroid(i)))
  names(out)<- c('Long', 'Lat')
  out
}, simplify = FALSE
)
Kisumu_localities <- do.call(rbind, Kisumu_localities)
Kisumu_localities$Place <- rownames(Kisumu_localities)
ext <- expand.grid(as.list(as.data.frame(matrix(extent(Kisumu), nrow = 2, byrow = F))))
ext <- as(extent(Kisumu), 'SpatialPolygons')
inst0 <- ggplot()+
  geom_path(data=fortify(Kenya_L0), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  geom_polygon(data=fortify(Kisumu), aes(x=long, y= lat, group=group), size = 0.05, fill='red', colour='red')+
  geom_path(data=fortify(ext), aes(x=long, y= lat, group=group), size = 0.05, colour='red')+
  geom_text(data = Kisumu_localities[2, ], aes(x = Long, y = Lat, label = Place),
            position = position_nudge(y = -0.7),
            hjust=0.5, vjust=1, size=3)+
  theme_minimal()+
  my_theme +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(size = 8, face = "bold"),
    legend.text = element_text(family = 'serif', face = 'bold', size = 8),
    plot.title = element_text(size = 14, face = 'bold', color = 'black'),
    plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=1, r=1,b=-1, l=-1))
###########################
inst <- ggplot()+
  geom_polygon(data=fortify(Kisumu), aes(x=long, y= lat, group=group), size = 0.25, fill='white', colour='black')+
  scalebar(data=fortify(Kisumu), dist = 20, dist_unit = "km",
           transform = TRUE, model = "WGS84", anchor=c(x=34.9, y=-0.54), 
           height=0.03, st.dist=0.05, st.size=2.5, border.size =0.25)+
  north(data=fortify(Kisumu), scale = 0.32, anchor=c(x=35.44, y=0.03))+
  geom_text(data = Kisumu_localities[1, ], aes(x = Long, y = Lat, label = ""),
            position = position_nudge(y = -0.425),
            hjust=0.5, vjust=1, size=4, alpha=1)+ # used only to make rooms for inset maps
  geom_text(data = Kisumu_localities[1, ], aes(x = Long, y = Lat, label = "Kisumu County, Kenya"),
            position = position_nudge(y = 0.025),
            hjust=0.5, vjust=0, size=3)+
  theme_minimal()+
  my_theme +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12, face = 'bold'),
        strip.text = element_text(size = 8, face = "bold"),
        legend.text = element_text(family = 'serif', face = 'bold', size = 8),
        plot.background = element_rect(fill = 'lightgrey', colour = 'lightgrey'),
        plot.title = element_text(size = 14, face = 'bold', color = 'black'),
        plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  labs(subtitle='Referential')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=0, b=0))

inst_kisumu <- inst +
  annotation_custom(
    grob = ggplotGrob(inst0+
                        theme(panel.grid = element_blank(),
                              plot.background = element_rect(fill = 'white', colour = 'white'),
                              panel.border = element_blank()
                        )),
    xmin = 35.09,
    xmax = 35.35-0.01,
    ymin = -0.55,
    ymax = -0.3
  )
# inst_kisumu <- gTree(children = gList(grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
#                                          gp=gpar(lwd=1, fill="lightgray", col="lightgray")), ggplotGrob(inst)))

### Tigray #####
Ethiopia_L0 <- getData("GADM", country="ETH", level=0, path = 'data_files/shapefiles')
Tigray <- readOGR('data_files/shapefiles/Tigray.shp')
Tigray_localities <- list(Tigray=Tigray, Ethiopia=Ethiopia_L0)
Tigray_localities <- sapply(Tigray_localities, function (i) {
  out=as.data.frame(coordinates(rgeos::gCentroid(i)))
  names(out)<- c('Long', 'Lat')
  out
}, simplify = FALSE
)
Tigray_localities <- do.call(rbind, Tigray_localities)
Tigray_localities$Place <- rownames(Tigray_localities)
ext <- expand.grid(as.list(as.data.frame(matrix(extent(Tigray), nrow = 2, byrow = F))))
ext <- as(extent(Tigray), 'SpatialPolygons')
inst0 <- ggplot()+
  geom_path(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.05, fill='red', colour='red')+
  geom_path(data=fortify(ext), aes(x=long, y= lat, group=group), size = 0.05, colour='red')+
  geom_text(data = Tigray_localities[2, ], aes(x = Long, y = Lat, label = Place),
            position = position_nudge(y = -0.7),
            hjust=0.5, vjust=1, size=3)+
  theme_minimal()+
  my_theme +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(size = 8, face = "bold"),
    legend.text = element_text(family = 'serif', face = 'bold', size = 8),
    plot.title = element_text(size = 14, face = 'bold', color = 'black'),
    plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=1, r=1,b=-1, l=-1))
###########################

inst <- ggplot()+
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.25, fill='white', colour='black')+
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.25, fill='white', colour='black')+
  scalebar(data=fortify(Tigray), dist = 50, dist_unit = "km",
           transform = TRUE, model = "WGS84", anchor=c(x=38.5, y=12.25+0.2), 
           height=0.03, st.dist=0.05, st.size=2, border.size =0.25)+
  north(data=fortify(Tigray), scale = 0.2, anchor=c(x=37.25, y=14.85))+
  geom_text(data = Tigray_localities[1, ], aes(x = Long, y = Lat, label = "Tigray region, Ethiopia"),
            position = position_nudge(y = 0.05),
            hjust=0.5, vjust=0, size=3)+
  theme_minimal()+
  #my_line_theme+
  my_theme +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12, face = 'bold'),
        strip.text = element_text(size = 8, face = "bold"),
        legend.text = element_text(family = 'serif', face = 'bold', size = 8),
        plot.background = element_rect(fill = 'lightgrey', colour = 'lightgrey'),
        plot.title = element_text(size = 14, face = 'bold', color = 'black'),
        plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  # labs(x='Long', y='Lat')+
  labs(subtitle='Referential')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=0, b=0))

inst_tigray <- inst +
  annotation_custom(
    grob = ggplotGrob(inst0+
                        theme(panel.grid = element_blank(),
                              plot.background = element_rect(fill = 'white'),
                              panel.border = element_blank()
                        )),
    xmin = 37.5,
    xmax = 38.5,
    ymin = 12.25+0.2,
    ymax = 13.25+0.2
  )

# inst_tigray <- gTree(children = gList(grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
#                                          gp=gpar(lwd=1, fill="lightgray", col="lightgray")), ggplotGrob(inst)))