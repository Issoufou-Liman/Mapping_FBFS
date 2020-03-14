#################################################
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
inst00 <- ggplot()+
  geom_path(data=fortify(afr), aes(x=long, y= lat, group=group), size = 0.03, colour='black')+
  geom_polygon(data=fortify(Kenya_L0), aes(x=long, y= lat, group=group), fill='red', size = 0.03, colour='red')+
  theme_minimal()+
  #my_line_theme+
  my_theme +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank())+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()
###########################

inst01 <- ggplot()+
  geom_path(data=fortify(Kenya_L0), aes(x=long, y= lat, group=group), size = 0.03, colour='black')+
  geom_polygon(data=fortify(Kisumu), aes(x=long, y= lat, group=group), size = 0.03, fill='red', colour='red')+
  geom_path(data=fortify(ext), aes(x=long, y= lat, group=group), size = 0.03, colour='red')+
  
  geom_point(data = Kisumu_localities[2, ], aes(x = Long, y = Lat), size = 0.25)+
  theme_minimal()+
  my_theme +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank())+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()
###########################

inst_kisumu <- ggplot()+
  geom_polygon(data=fortify(Kisumu), aes(x=long, y= lat, group=group), size = 0.03, fill='white', colour='black')+
  scalebar(data=fortify(Kisumu), dist = 15, dist_unit = "km",
                 transform = TRUE, model = "WGS84", anchor=c(x=35.075, y=-0.55), 
                 height=0.03, st.dist=0.05, st.size=1, border.size =0.25/2)+
  north(data=fortify(Kisumu), scale = 0.32, anchor=c(x=35.44, y=0.03))+
  
  geom_point(data = Kisumu_localities[1, ], aes(x = Long, y = Lat), size = 0.25)+
  geom_text(data = Kisumu_localities[1, ], aes(x = Long, y = Lat, label = ""),
            position = position_nudge(y = -0.425),
            hjust=0.5, vjust=1, size=4, alpha=1)+ # used only to make rooms for inset maps
  my_theme +
  theme(panel.grid.major = element_line (size = 0.04),
        panel.grid.minor = element_line (size = 0.03),
        axis.text = element_text(size = 4),
        axis.title = element_blank(),
        plot.title = element_blank(),
        plot.margin = margin(t=0, r=0, b=0, l=0),
        plot.subtitle = element_text(size = 8, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()

#################################################
Ethiopia_L0 <- aggregate(Ethiopia_L1)
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

inst02 <- ggplot()+
  geom_path(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), size = 0.03, colour='black')+
  
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.03, fill='red', colour='red')+
  geom_path(data=fortify(ext), aes(x=long, y= lat, group=group), size = 0.03, colour='red')+
  
  geom_point(data = Tigray_localities[2, ], aes(x = Long, y = Lat), size = 0.25)+
  theme_minimal()+
  my_theme +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank())+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()
###########################
inst_tigray <- ggplot()+
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.03, fill='white', colour='black')+
  scalebar(data=fortify(Tigray), dist = 50, dist_unit = "km",
                 transform = TRUE, model = "WGS84", anchor=c(x=38.5, y=12.25+0.2), 
                 height=0.03, st.dist=0.05, st.size=1, border.size =0.25/2)+
  north(data=fortify(Tigray), scale = 0.2, anchor=c(x=37.25, y=14.85))+
  
  geom_point(data = Tigray_localities[1, ], aes(x = Long, y = Lat), size = 0.25)+
  my_theme +
  theme(panel.grid.major = element_line (size = 0.04),
        panel.grid.minor = element_line (size = 0.03),
        axis.text = element_text(size = 4),
        axis.title = element_blank(),
        plot.title = element_blank(),
        plot.margin = margin(t=0, r=0, b=0, l=0),
        plot.subtitle = element_text(size = 8, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()