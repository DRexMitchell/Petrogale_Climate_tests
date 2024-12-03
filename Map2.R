library(ggplot2)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
theme_set(theme_bw())

load("mapdata2.rda")

world <- ne_countries(scale = "medium", returnclass = "sf")
p <- ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  coord_sf(xlim = c(112, 154), ylim = c(-9, -45), expand = FALSE)

q <- p + theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue")); q

data <- as.data.frame(cbind(lon, lat))

r <- q + geom_point(data=data, aes(x=lon, y=lat), pch=icons, bg=species, size=exp(size/100)/15, alpha=1) + labs(x="", y=""); r

# polygons #
library(dplyr)
data <- as.data.frame(cbind(lon, lat, clade_groups))
hull <- data %>% group_by(clade_groups) %>% 
  slice(chull(lon, lat))

r + geom_polygon(data = hull, alpha = 0.2, 
                    aes(fill = c("blue", "red", "green", "white", "orange"),colour = c("blue", "red", "green", "white", "orange")))



data2 <- as.data.frame(cbind(lon_ord,lat_ord))

s <- r + geom_point(data=data2, aes(x=lon_ord, y=lat_ord), pch=icons_mn, bg=cols_mn, size=exp(size_ord/100)/15, alpha=1) + labs(x="", y=""); s

t <- s + annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"), style = north_arrow_fancy_orienteering) + annotation_scale(location = "bl", width_hint = 0.25); s

### Using shape and climate data ###

map2color <- function(x, pal, limits = range(x)){
  pal[findInterval(x, seq(limits[1], limits[2], length.out = length(pal) + 1), 
                   all.inside=TRUE)]
}
cont <- colorRampPalette(c("hotpink","darkblue"), interpolate = "linear")
colage <- cont(15)

cont <- colorRampPalette(c("darkblue","hotpink"), interpolate = "linear")
colage <- cont(15)
colmap <- map2color(x=rostrum_ord, pal=colage, limits=range(rostrum_ord))
r <- q + geom_point(data=data, aes(x=lon, y=lat), pch=21, bg=colmap, size=exp(size/100)/15, alpha=0.2) + labs(x="", y=""); r
colmap <- map2color(x=bullmn_ord, pal=colage, limits=range(lmkrat))
r <- q + geom_point(data=data2, aes(x=lon_ord, y=lat_ord), pch=21, bg=colmap, size=exp(size_ord/100)/10, alpha=1) + labs(x="", y=""); r

colmap <- map2color(x=plot1$RegScore, pal=colage, limits=range(plot1$RegScore))
r <- q + geom_point(data=data, aes(x=lon, y=lat), pch=21, bg=colmap, size=exp(size/100)/15, alpha=1) + labs(x="", y=""); r

s <- r + annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"), style = north_arrow_fancy_orienteering) + annotation_scale(location = "bl", width_hint = 0.25); s

### Preciptation Warmest Quarter ###

colmap <- map2color(x=climdata[,3], pal=colage, limits=range(climdata[,3]))
r <- q + geom_point(data=data, aes(x=lon, y=lat), pch=21, bg=colmap, size=exp(size/100)/15, alpha=0.2) + labs(x="", y=""); r

colmap <- map2color(x=bio3_ord, pal=colage, limits=range(climdata[,3]))
s <- r + geom_point(data=data2, aes(x=lon_ord, y=lat_ord), pch=21, bg=colmap, size=exp(size_ord/100)/15, alpha=1) + labs(x="", y=""); s

colmap <- map2color(x=plot1$RegScore, pal=colage, limits=range(plot1$RegScore))
r <- q + geom_point(data=data, aes(x=lon, y=lat), pch=21, bg=colmap, size=exp(size/100)/15, alpha=1) + labs(x="", y=""); r