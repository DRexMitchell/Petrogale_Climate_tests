### Phylogenetic distances ###
library(ape)

phy_dists <- cophenetic.phylo(tree)


n <- c(20,20,6,4,20,19,26,20,25,10,7,14,18,16,17,17,16,19,19,20,21)
species_names <- c("P. assimilis", "P. brachyotis", "P. burbidgei", "P. coenensis", "P. concinna", "P. godmani", "P. herberti", "P. inornata", "P. l. centralis", "P. l. hacketti", "P. l. kimberleyensis", "P. l. lateralis",  "P. l. pearsoni", "P. mareeba", "P. penicillata", "P. persephone", "P. purpureicollis", "P. rothschildi", "P. sharmani", "P. wilkinsi", "P. xanthopus")

clade <- c("penicillata","brachyotis","brachyotis","penicillata","brachyotis","penicillata","penicillata","penicillata","lateralis","lateralis","lateralis","lateralis","lateralis","penicillata","penicillata","xanthopus","purpureicollis","rothschildi","penicillata","brachyotis","xanthopus") 

match_names <- match(species_names,rownames(phy_dists))
phy_dists_ord <- phy_dists[match_names,]
species_names_all <- rep(species_names, n)
clade_groups <- as.factor(rep(clade, n))

phy_dists_all <- array(NA, c(354,21))
for (i in 1:length(phy_dists[,1])) {
  phy_dists_all[,i] <- rep(phy_dists_ord[,i], n)
}

rownames(phy_dists_all) <- ind
colnames(phy_dists_all) <- species_names
phy_dists_all


sym.mat <- two.d.array(pet_sym$symm.shape)
mod<-varpart(sym.mat,~log(size),~phy_dists_all); mod
showvarparts(2)
plot(mod,digits = 2, Xnames = c('cranial size','phylogeny'), bg = c('red','blue'))

full_model <- rda(sym.mat ~ log(size) + phy_dists_all)
anova(full_model)
RsquareAdj(full_model)

full_size_fraction <- rda(sym.mat ~ log(gpa$Csize))
anova(full_size_fraction) 
RsquareAdj(full_size_fraction)

full_phy_fraction <- rda(sym.mat ~ phy_dists_all)
anova(full_phy_fraction) 
RsquareAdj(full_phy_fraction)

pure_size_fraction <- rda(sym.mat ~ log(size) + Condition(phy_dists_all))
anova(pure_size_fraction)
RsquareAdj(pure_size_fraction)

pure_phy_fraction <- rda(sym.mat ~ phy_dists_all + Condition(log(size)))
anova(pure_phy_fraction)
RsquareAdj(pure_phy_fraction)


sym.mat <- two.d.array(pet_sym$symm.shape)
mod<-varpart(sym.mat,~log(size),~spat,~climdata[,c(2:3,8:9,18)], ~phy_dists_all); mod
showvarparts(4)
plot(mod,digits = 2, Xnames = c('cranial size','geography', 'climate', 'phylogeny'), bg = c('red','orange', 'blue', 'green'))


sym.mat <- two.d.array(pet_sym$symm.shape)
mod<-varpart(myarea_all,~log(size),~spat,~climdata[,c(2:3,8:9,18)], ~phy_dists_all); mod
showvarparts(4)
plot(mod,digits = 2, Xnames = c('cranial size','geography', 'climate', 'phylogeny'), bg = c('red','orange', 'blue', 'green'))

mod<-varpart(myarea_all,~log(size),~spat,~climdata[,c(2:3,8:9,18)]); mod
showvarparts(3)
plot(mod,digits = 2, Xnames = c('cranial size','geography', 'climate'), bg = c('red','orange', 'blue'))


# Cranial allometry
allometry.model <- procD.lm(pet_sym$symm.shape~phy_dists_all,iter=999); summary(allometry.model)
allom_plot <- plot(allometry.model, type = "regression", predictor = log(size), reg.type = "RegScore", pch=icons, cex=exp(gpa$Csize/100)/40, bg = species)
plot(-1*allom_plot$RegScore~log(size), pch=icons, cex=exp(gpa$Csize/100)/40, bg = species, xlab = "log(cranial centroid size)", ylab = "shape score")

PGLS_allom <- procD.pgls(meanshapes_ord ~ log(size_ord), tree, iter=999); summary(PGLS_allom)

# meanshapes RETURNS ERROR OF COLLINEARITY

mn.mat <- two.d.array(meanshapes_ord)
mod<-varpart(mn.mat,~log(size_ord),~phy_dists); mod
showvarparts(2)
plot(mod,digits = 2, Xnames = c('cranial size','phylogeny'), bg = c('red','blue'))

### bullae climate

bulla.mod <- procD.lm(myarea_all/log(size)~climdata[,12]); summary(bulla.mod)
plot(allarea_rat~climdata[,12], pch=icons, cex=exp(gpa$Csize/100)/40, bg = species, xlab = "annual precipitation (mm)", ylab = "bullae size")

ann_rain <- climdata_ord[,12]; names(ann_rain) <- names(size_ord)
bullae_ord <- as.vector(bullae_ord); names(bullae_ord) <- names(size_ord)
bullae_pgls <- procD.pgls(bullae_ord ~ ann_rain, tree, iter=999); summary(bullae_pgls)

mod<-varpart(allarea_rat,~climdata[,12], ~phy_dists_all); mod
showvarparts(2)
plot(mod,digits = 2, Xnames = c('precipitation', 'phylogeny'), bg = c('blue','green'))

# meanshapes RETURNS ERROR OF COLLINEARITY
mod<-varpart(bullae_ord,~climdata_ord[,12],~phy_dists); mod
showvarparts(2)
plot(mod,digits = 2, Xnames = c('cranial size','phylogeny'), bg = c('red','blue'))

mod<-varpart(myarea_all,~log(size),~spat,~climdata[,c(2:3,8:9,18)], ~phy_dists_all); mod
showvarparts(4)
plot(mod,digits = 2, Xnames = c('cranial size','geography', 'climate', 'phylogeny'), bg = c('red','orange', 'blue', 'green'))

### residuals

allometry.model <- procD.lm(pet_sym$symm.shape~log(size),iter=999); summary(allometry.model)
mn <- mshape(pet_sym$symm.shape)
resid <- arrayspecs(allometry.model$residuals, 150, 3) 
resid_mn <- array(NA, c(150,3,354))
for (i in 1:354) {
  resid_mn[,,i] <- resid[,,i] + mn
}

allometry.model_resid <- procD.lm(resid_mn~log(size),iter=999); summary(allometry.model_resid)
allom_plot <- plot(allometry.model_resid, type = "regression", predictor = log(size), reg.type = "RegScore", pch=icons, cex=exp(gpa$Csize/100)/40, bg = species)
plot(allom_plot$RegScore~log(size), pch=icons, cex=exp(gpa$Csize/100)/40, bg = species, xlab = "log(cranial centroid size)", ylab = "shape score")

sym.mat_resid <- two.d.array(resid_mn)
mod<-varpart(sym.mat_resid,~log(size),~spat,~climdata[,c(2:3,8:9,18)], ~phy_dists_all); mod
showvarparts(4)
plot(mod,digits = 2, Xnames = c('cranial size','geography', 'climate', 'phylogeny'), bg = c('red','orange', 'blue', 'green'))

preds <- shape.predictor(allometry.model_resid$GM$fitted, x = allom_plot$PredLine, 
                         predmin = min(allom_plot$PredLine), 
                         predmax = max(allom_plot$PredLine))
differences <- coordinates.difference(coordinates = preds$predmax,
                                      reference = preds$predmin, type = "spherical")
procrustes.var.plot(preds$predmin, preds$predmax, col = heat.colors, col.val = differences[[1]][1:150], magnitude = 2, pt.size = 1)


### 
