library(geomorph)
library(landvR)
library(vegan)
library(phytools)

load("entire_sample.rda")

ind <- dimnames(shapedata)[[3]]
pet_sym <- bilat.symmetry(shapedata, ind=ind, land.pairs = sym, object.sym=T, curves=slide)
gpa <- gpagen(shapedata)
size <- gpa$Csize

allometry.model <- procD.lm(pet_sym$symm.shape~phy_dists_all,iter=999); summary(allometry.model)
allom_plot <- plot(allometry.model, type = "regression", predictor = log(size), reg.type = "RegScore", pch=icons, cex=exp(gpa$Csize/100)/40, bg = species)
plot(-1*allom_plot$RegScore~log(size), pch=icons, cex=exp(gpa$Csize/100)/40, bg = species, xlab = "log(cranial centroid size)", ylab = "shape score")

bergmann.model <- procD.lm(log(size)~lat, iter=999); summary(bergmann.model)
berg_plot <- plot(bergmann.model, type = "regression", predictor = lat, reg.type = "RegScore", pch=icons, cex=exp(gpa$Csize/100)/40, bg = species) 
plot(berg_plot$RegScore~lat, pch=icons, cex=exp(gpa$Csize/100)/40, bg = species, xlab = "Latitude", ylab = "log(Cranial size)")

# VARPART 

geodata <- cbind(lon, lat)
distances <- dist(geodata) # generates pairwise distances matrix
spatial.data<-pcnm(distances) # Principal Coordinate Neighbour Matrices conversion

length(spatial.data$values)
sum(spatial.data$values[1:8]/sum(spatial.data$values[1:152])) # Identify how many values equate to ~95% and isolate these
spat <- spatial.data$vectors[,1:8]

vif <- vifstep(climdata,th=3); vif 
# 5 variables remain after removing correlated climate variables

sym.mat <- two.d.array(pet_sym$symm.shape)
mod<-varpart(sym.mat,~log(size),~spat,~climdata[,c(2:3,8:9,18)]); mod
showvarparts(3)
plot(mod,digits = 2, Xnames = c('cranial size','geography', 'climate'), bg = c('red','orange', 'blue'))

full_model <- rda(sym.mat ~ log(size) + spat + climdata[,c(2:3,8:9,18)])
anova(full_model)
RsquareAdj(full_model)

full_size_fraction <- rda(sym.mat ~ log(gpa$Csize))
anova(full_size_fraction) 
RsquareAdj(full_size_fraction)

full_geo_fraction <- rda(sym.mat ~ spat)
anova(full_geo_fraction) 
RsquareAdj(full_geo_fraction)

full_clim_fraction <- rda(sym.mat ~ climdata[,c(2:3,8:9,18)])
anova(full_clim_fraction) 
RsquareAdj(full_clim_fraction)

pure_size_fraction <- rda(sym.mat ~ log(size) + Condition(climdata[,c(2:3,8:9,18)]) + Condition(spat))
anova(pure_size_fraction)
RsquareAdj(pure_size_fraction)

pure_geo_fraction <- rda(sym.mat~ spat + Condition(log(size)) + Condition(climdata[,c(2:3,8:9,18)]))
anova(pure_geo_fraction)
RsquareAdj(pure_geo_fraction)

pure_clim_fraction <- rda(sym.mat ~ climdata[,c(2:3,8:9,18)] + Condition(log(size)) + Condition(spat))
anova(pure_clim_fraction)
RsquareAdj(pure_clim_fraction)
