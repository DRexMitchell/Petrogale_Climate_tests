
library(geomorph)
library(landvR)
library(vegan)
library(phytools)
library(usdm)
library(rstudioapi)
library(mvMORPH)
library(qpcR)

load("mvgls.rda")

#which model of evolution does shape follow? Using Julien Clavel's mvMorph package
two_d_array <- list(array2d=two.d.array(meanshapes_ord))

BM_shapefit <- mvgls(array2d ~ 1 , tree, data = two_d_array, model="BM" , method="PL-LOOCV")
OU_shapefit <- mvgls(array2d ~ 1 , tree, data = two_d_array, model="OU" , method="PL-LOOCV")
EB_shapefit <- mvgls(array2d ~ 1 , tree, data = two_d_array, model="EB" , method="PL-LOOCV")

GIC_BM<- GIC(BM_shapefit)
GIC_OU <- GIC(OU_shapefit)
GIC_EB <- GIC(EB_shapefit)

#Compute vector of GICs
GICs=unlist(c(GIC_BM[2], GIC_OU[2], GIC_EB[2]))

#for tabulating later
GIC_shape <- unlist(c(GIC_BM[2], GIC_OU[2], GIC_EB[2]))

names(GICs) <- c("BM", "OU", "EB" )

#compute relative probabilities betweeen 0 and 1 by comparing GICS according to Burnham and Anderson (2002)
GICmin=GICs-min(GICs)
W_shape=exp(-0.5*GICmin)/sum(exp(-0.5*GICmin))

#OU is by far the preferred model
W_shape 

#Does it make much of a difference to the coordinates if we adjust for OU vs BM evolution? 

resid_BM <- arrayspecs(BM_shapefit$residuals, dim(meanshapes_ord)[[1]], 3)
resid_OU <- arrayspecs(OU_shapefit$residuals, dim(meanshapes_ord)[[1]], 3)

#The r-pls of these residuals is 1 - in other words, there seems to be next to no difference between the residuals of a BM and an OU model despite OU being the preferred model fit.
two.b.pls(resid_BM, resid_OU)

#mvgls

BM_climfit <- mvgls(array2d ~ climdata_ord[,2]+climdata_ord [,3]+ climdata_ord[,8]+climdata_ord [,9]+climdata_ord[,18], tree, data = two_d_array, model="BM" , method="PL-LOOCV")

OU_climfit <- mvgls(array2d ~ climdata_ord[,2]+climdata_ord [,3]+ climdata_ord[,8]+climdata_ord [,9]+climdata_ord[,18], tree, data = two_d_array, model="OU" , method="PL-LOOCV")

EB_climfit <- mvgls(array2d ~ climdata_ord[,2]+climdata_ord [,3]+ climdata_ord[,8]+climdata_ord [,9]+climdata_ord[,18], tree, data = two_d_array, model="EB" , method="PL-LOOCV")

No_phylo_climfit <- mvols(array2d ~ climdata_ord[,2]+climdata_ord [,3]+ climdata_ord[,8]+climdata_ord [,9]+climdata_ord[,18], data = two_d_array, method="PL-LOOCV")

GIC_BM<- GIC(BM_climfit)
GIC_OU <- GIC(OU_climfit)
GIC_EB <- GIC(EB_climfit)

#Compute vector of GICs - this also takes a little, ~1 minute
GICs=unlist(c(GIC_BM[2], GIC_OU[2], GIC_EB[2]))
#for tabling later
GIC_size <- unlist(c(GIC_BM[2], GIC_OU[2], GIC_EB[2]))
names(GICs) <- c("BM", "OU", "EB" )

#compute relative probabilities betweeen 0 and 1 by comparing AICS according to Burnham and Anderson (2002)

GICmin=GICs-min(GICs)
W_allom=exp(-0.5*GICmin)/sum(exp(-0.5*GICmin))
#OU is also strongly preferred here.
W_allom 

#But with small sample sizes, Type I error rates are high and common likelihood tests such as AIC can be biased (cooper et al. 2016)


#Does it make much of a difference to the coordinates if we adjust for OU vs BM evolution? 

#trying to do what I would usually do if I ran pgls:
resid_BM <- arrayspecs(BM_climfit$residuals, dim(meanshapes_ord)[[1]], 3)
resid_OU <- arrayspecs(OU_climfit$residuals, dim(meanshapes_ord)[[1]], 3)

#The r-pls of these residuals is 0.997. So not entirely 1 but I still suspect that the difference between BM and OU is pretty minor.
two.b.pls(resid_BM, resid_OU)

#Investigate models - here it runs a MANOVA.

MV_climate_OU   <- manova.gls(OU_climfit, nperm=1000, test="Pillai", verbose=TRUE)

MV_No_phylo_climfit <- manova.gls(No_phylo_climfit, nperm=1000, test="Pillai", verbose=TRUE)

save(MV_climate_OU, MV_No_phylo_climfit, file= "Mv_climate_OU_1000iter.rda")    
MV_climate_OU   <- manova.gls(OU_climfit, nperm=1000, test="Pillai", type="II", verbose=TRUE)

MV_No_phylo_climfit_II <- manova.gls(No_phylo_climfit, nperm=1000, test="Pillai", type="II", verbose=TRUE)

save(MV_climate_OU, MV_No_phylo_climfit, file= "Mv_climate_OU_1000iter_II.rda")


