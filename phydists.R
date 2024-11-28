### Phylogenetic distances ###
library(ape)

phy_dists <- cophenetic.phylo(tree)

n <- c(20,20,6,4,20,19,26,20,25,10,7,14,18,16,17,17,16,19,19,20,21)
species_names <- c("P. assimilis", "P. brachyotis", "P. burbidgei", "P. coenensis", "P. concinna", "P. godmani", "P. herberti", "P. inornata", "P. l. centralis", "P. l. hacketti", "P. l. kimberleyensis", "P. l. lateralis",  "P. l. pearsoni", "P. mareeba", "P. penicillata", "P. persephone", "P. purpureicollis", "P. rothschildi", "P. sharmani", "P. wilkinsi", "P. xanthopus")

match_names <- match(species_names,rownames(phy_dists))
phy_dists_ord <- phy_dists[match_names,]
species_names_all <- rep(names(n), n)

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

allarea_rat

mod<-varpart(allarea_rat,~log(size),~phy_dists_all); mod
showvarparts(2)
plot(mod,digits = 2, Xnames = c('cranial size','phylogeny'), bg = c('red','blue'))

sym.mat <- two.d.array(pet_sym$symm.shape)
mod<-varpart(sym.mat,~log(size),~spat,~climdata[,c(2:3,8:9,18)], ~phy_dists_all); mod
showvarparts(4)
plot(mod,digits = 2, Xnames = c('cranial size','geography', 'climate', 'phylogeny'), bg = c('red','orange', 'blue', 'hotpink'))


sym.mat <- two.d.array(pet_sym$symm.shape)
mod<-varpart(myarea_all,~log(size),~spat,~climdata[,c(2:3,8:9,18)], ~phy_dists_all); mod
showvarparts(4)
plot(mod,digits = 2, Xnames = c('cranial size','geography', 'climate', 'phylogeny'), bg = c('red','orange', 'blue', 'green'))

mod<-varpart(myarea_all,~log(size),~spat,~climdata[,c(2:3,8:9,18)]); mod
showvarparts(3)
plot(mod,digits = 2, Xnames = c('cranial size','geography', 'climate'), bg = c('red','orange', 'blue'))
