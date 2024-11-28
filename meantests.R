library(geomorph)
library(landvR)
library(phytools)
library(usdm)

load("mean_climate_tests.rda")

# climate data tests - do for each variable

# Generate Procrustes OLS regression of climate variable and shape
bio1_mnshape <- procD.lm(meanshapes_ord ~ climdata_ord[,1],iter=999); summary(bio1_mnshape)

# Plot distribution
bio1_plot <- plot(bio1_mnshape, type = "regression", predictor = climdata_ord[,1], reg.type = "RegScore", pch=icons_mn, bg=cols_mn, cex=exp(size_ord/100)/20) 
plot(bio1_plot$RegScore~climdata_ord[,1], pch=icons_mn, cex=exp(size_ord/100)/20, bg = cols_mn, xlab = "Precipitation of Coldest Quarter", ylab = "Shape Score")

# Do phylogenetic generalised least squares test
PGLS_bio1 <- procD.pgls(meanshapes_ord ~ climdata_ord[,1], tree, iter=999); summary(PGLS_bio1)

# Test for phylogenetic signal of climate variable
physignal(climdata_ord[,1], tree, iter=999)

# Test correlation between predicted shape change and cranial size to assess influence of allometry.
OLS_bio1shapsesz <- procD.lm(bio1_plot$RegScore ~ log(size_ord), iter=999); summary(OLS_bio1shapsesz)

# Predict shape change associated with the climate variable.
preds <- shape.predictor(bio1_mnshape$GM$fitted, x = bio1_plot$PredLine, 
                         predmin = min(bio1_plot$PredLine), 
                         predmax = max(bio1_plot$PredLine))
differences <- coordinates.difference(coordinates = preds$predmax,
                                      reference = preds$predmin, type = "spherical")
procrustes.var.plot(preds$predmin, preds$predmax, col = heat.colors, col.val = differences[[1]][1:150], magnitude = 1, pt.size = 1)

