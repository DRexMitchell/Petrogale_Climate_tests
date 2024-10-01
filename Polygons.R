library(Morpho)
library(phytools)

load("polygons.rda")

#compute rostrum polygons relative to cranial size
#lmks are: 64,65,66,15,2,42,84,85
myarea_all <- c(1:354)
for (i in 1:354) {
  myarea_all[i] <- computeArea(shapedata[c(64,65,66,15,2,42,84,85),,i])
}
myarea_all <- unlist(myarea_all)
names(myarea_all) <- dimnames(shapedata)[[3]]
myarea_rat <- myarea_all/size

# obtain mean values and plot on phylogeny
rostrummn <- tapply(myarea_rat, names1, mean)
rostrum_ord <- rostrummn[matchy]
phyplot_rostrum <- contMap(tree, rostrum_ord)

# test OLS, PGLS, and physignals
rostrum_ord <- as.vector(rostrum_ord); names(rostrum_ord) <- names(climdata_ord[,1])
rostrum_poly <- procD.lm(rostrum_ord~log(size_ord), iter=999); summary(rostrum_poly)
rostrum_poly_pgls <- procD.pgls(rostrum_ord~log(size_ord), tree, iter=999); summary(rostrum_poly_pgls)
physignal(rostrum_ord, tree, iter=999)

# For all climate variables 1 to 19, test correlations and plot:
rostrum_clim1 <- procD.lm(rostrum_ord~climdata_ord[,1], iter=999); summary(rostrum_clim1)
rostrum_clim_phy1 <- procD.pgls(rostrum_ord~climdata_ord[,1], tree, iter=999); summary(rostrum_clim_phy1)
plot(rostrum_ord~climdata_ord[,12], pch=icons_mn, cex=exp(size_ord/100)/20, bg = cols_mn)


#compute mean bullae polygons, relative to cranial size
#lmks are: 97, 100, 99, 98, 101,102 or 78, 81, 80, 79, 82, 83
myarea_1 <- c(1:354)
myarea_2 <- c(1:354)
for (i in 1:354) {
  myarea_1[i] <- computeArea(shapedata[c(97, 100, 99, 98, 101,102),,i])
}
myarea_1 <- unlist(myarea_1)
names(myarea_1) <- dimnames(shapedata)[[3]]

for (i in 1:354) {
  myarea_2[i] <- computeArea(shapedata[c(78, 81, 80, 79, 82, 83),,i])
}
myarea_2 <- unlist(myarea_2)
names(myarea_2) <- dimnames(shapedata)[[3]]

myarea_all <- (myarea_1+myarea_2)/2
allarea_rat <- myarea_all/size

# obtain mean values and plot on phylogeny
bullaemn <- tapply(allarea_rat, names1, mean)
bullae_ord <- bullaemn[matchy]
phyplot_bullae <- contMap(tree, bullae_ord)
plot(setMap(phyplot_bullae, invert=T))

# test OLS, PGLS, and physignals
bullae_ord <- as.vector(bullae_ord); names(bullae_ord) <- names(climdata_ord[,1])
bullae_poly <- procD.lm(bullae_ord~log(size_ord), iter=999); summary(bullae_poly)
bullae_poly_pgls <- procD.pgls(bullae_ord~log(size_ord), tree, iter=999); summary(bullae_poly_pgls)
physignal(bullae_ord, tree, iter=999)
physignal(size_ord, tree, iter=999)

# For all climate variables 1 to 19, test correlations and plot:
bullae_clim <- procD.lm(bullae_ord~climdata_ord[,1], iter=999); summary(bullae_clim)
bullae_clim_phy <- procD.pgls(bullae_ord~climdata_ord[,1], tree, iter=999); summary(bullae_clim_phy)
plot(bullae_ord~climdata_ord[,1], pch=icons_mn, cex=exp(size_ord/100)/20, bg = cols_mn)
