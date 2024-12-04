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
  coord_sf(xlim = c(112, 154), ylim = c(-9, -45), expand = FALSE); p

q <- p + theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue")); q

data <- as.data.frame(cbind(lon, lat))

r <- q + geom_point(data=data, aes(x=lon, y=lat), pch=icons, bg=species, size=exp(size/100)/15, alpha=1) + labs(x="", y=""); r

# polygons #
library(tidyverse)

data <- as.data.frame(cbind(lon, lat, as.factor(clade_groups)))


hull <- data %>% group_by(V3) %>% 
  slice(chull(lon, lat))

r + geom_polygon(data=hull, aes(x=hull$lon, y=hull$lat), alpha = 0.2)



for(j in 1:nlevels(specs$species)) {edge_points <- rownames(PCA1$pc.scores[which(specs$species == levels(specs$species)[j]),])[
  chull(PCA1$pc.scores[which(specs$species == levels(specs$species)[j]), c(1,2)])]

polygon(PCA1$pc.scores[edge_points, c(1,2)], col = adjustcolor(colour[j],
                                                               alpha.f = 0.3) , border = colour[j])

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


plot(lat~lon, asp = TRUE, pch=icons, bg=species, size=exp(size/100)/15)

brach <- data[which(clade_groups == "brachyotis"), c(1,2)]
edge_points <- chull(brach)
polygon(brach[edge_points,], col = adjustcolor("#DC267F", alpha.f = 0.3) , border = "#DC267F")

pen <- data[which(clade_groups == "penicillata"), c(1,2)]
edge_points <- chull(pen)
polygon(pen[edge_points,], col = adjustcolor("#648FFF", alpha.f = 0.3) , border = "#648FFF")

lat_ <- data[which(clade_groups == "lateralis"), c(1,2)]
edge_points <- chull(lat_)
polygon(lat_[edge_points,], col = adjustcolor("#FFB000", alpha.f = 0.3) , border = "#FFB000")

xan <- data[which(clade_groups == "xanthopus"), c(1,2)]
edge_points <- chull(xan)
polygon(xan[edge_points,], col = adjustcolor("black", alpha.f = 0.3) , border = "black")

ppc <- data[which(clade_groups == "purpureicollis"), c(1,2)]
edge_points <- chull(ppc)
polygon(ppc[edge_points,], col = adjustcolor("#785EF0", alpha.f = 0.6) , border = "#785EF0")

roth <- data[which(clade_groups == "rothschildi"), c(1,2)]
edge_points <- chull(roth)
polygon(roth[edge_points,], col = adjustcolor("#785EF0", alpha.f = 0.6) , border = "#785EF0")


length(lat)