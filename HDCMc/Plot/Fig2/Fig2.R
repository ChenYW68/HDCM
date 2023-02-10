rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "HDCM")
maxtime <- 7
maxdist <- 5e6

Da.mod <- obs_PM25_2015w

Da.mod$Bias <- (Da.mod$REAL_PM25) - mean(Da.mod$REAL_PM25)  #- (Da.mod$sim50_CMAQ_PM25)


Da.mod <- dplyr::select(Da.mod, DATE_TIME, ID, LON_X, LAT_Y, Bias,
                        REAL_PM25, sim50_CMAQ_PM25) %>%
  setorderv(c("DATE_TIME", "ID"))
temp_part <- unique(Da.mod$DATE_TIME)
loc <- unique(Da.mod[, c("LON_X", "LAT_Y")])
mod_gridded <- SpatialPoints(coords = loc)
Da.long <- spacetime::STFDF(sp = mod_gridded
                            , time = temp_part
                            , data = Da.mod)

vv0 <- variogram(object = Bias ~ 1, # fixed effect component
                 data = Da.long, # July data
                 cressie = T,
                 covariogram = T,
                 # pseudo = -1,
                 width = 20000, # spatial bin (80 km)
                 cutoff = 320000, # consider pts < 1000 km apart
                 tlags = seq(0.01, maxtime + 0.01, 1)) # 0 days to 6 days
vv0$avgDist <- vv0$avgDist/1e3



vv <- vv0[!is.na(vv0$gamma), c(5, 7, 3)]

vv$timelag <- as.numeric(vv$timelag)
vv$avgDist <- as.numeric(vv$avgDist)
vv$gamma <- as.numeric(vv$gamma)

Contour.w <- reshape2::dcast(vv, timelag ~ avgDist, value.var = "gamma")


surf.w <- MBA::mba.surf(vv, no.X = 200, no.Y = 200,
                      extend = TRUE)$xyz.est


surf.w$z <- ifelse(surf.w$z < 0, 0, surf.w$z)
emp.z.w  <- as.matrix(Contour.w[, -1])
emp.z.w  <- ifelse(emp.z.w < 0, 0, emp.z.w)

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) #"Spectral" #RdYlGn
##########################################################################
#                      plot
##########################################################################
pdf(paste0(paste0("./figure/"), 'Fig2_a',".pdf"), width = 10, height =  8)
op <- par(mar = c(5, 5, 1, 1) + 1)
par(mgp = c(4.0, 1.2, 0.3), cex.axis = 1.8, cex.lab = 2, cex.main = 2.0)
filled.contour(x = surf.w$x, y = surf.w$y, z = surf.w$z,
               color.palette = myPalette,
               # ylab = "Spatial lags (km)",
               # xlab = "Temporal lags (days)",
               xlim = c(0, maxtime),
               ylim = c(0, 300),
               zlim = c(0, max(surf.w$z)),
               plot.title = title(main = "Winter of 2015",
                                  ylab = "Spatial lags (km)",
                                  xlab = "Temporal lags (days)"),
               plot.axes = {
                 # axis(1)
                 axis(1, at = seq(0, maxtime + 0.01, 1),
                      labels = seq(0, maxtime + 0.01, 1))
                 axis(2, at = seq(0, 310, 50), labels = seq(0, 310, 50))
                 # contour(x = surf$x, y = surf$y, z = surf$z,
                 #         add = TRUE, lwd = 2, labcex = 1.5)
                 contour(x = Contour.w$timelag,
                         y = as.numeric(colnames(Contour.w)[-1]),
                         z = emp.z.w,
                         add = TRUE, lwd = 2, labcex = 1.5)
               })
dev.off()


# summer
maxtime <- 7
maxdist <- 5e6

Da.mod <- obs_PM25_2015s

Da.mod$Bias <- (Da.mod$REAL_PM25) - mean(Da.mod$REAL_PM25)  #- (Da.mod$sim50_CMAQ_PM25)


Da.mod <- dplyr::select(Da.mod, DATE_TIME, ID, LON_X, LAT_Y, Bias,
                        REAL_PM25, sim50_CMAQ_PM25) %>%
  setorderv(c("DATE_TIME", "ID"))
temp_part <- unique(Da.mod$DATE_TIME)
loc <- unique(Da.mod[, c("LON_X", "LAT_Y")])
mod_gridded <- SpatialPoints(coords = loc)
Da.long <- spacetime::STFDF(sp = mod_gridded
                            , time = temp_part
                            , data = Da.mod)

vv0 <- variogram(object = Bias ~ 1, # fixed effect component
                 data = Da.long, # July data
                 cressie = T,
                 covariogram = T,
                 # pseudo = -1,
                 width = 20000, # spatial bin (80 km)
                 cutoff = 320000, # consider pts < 1000 km apart
                 tlags = seq(0.01, maxtime + 0.01, 1)) # 0 days to 6 days
vv0$avgDist <- vv0$avgDist/1e3



vv <- vv0[!is.na(vv0$gamma), c(5, 7, 3)]

vv$timelag <- as.numeric(vv$timelag)
vv$avgDist <- as.numeric(vv$avgDist)
vv$gamma <- as.numeric(vv$gamma)

Contour.s <- reshape2::dcast(vv, timelag ~ avgDist, value.var = "gamma")


surf.s <- MBA::mba.surf(vv, no.X = 200, no.Y = 200,
                      extend = TRUE)$xyz.est


surf.s$z <- ifelse(surf.s$z < 0, 0, surf.s$z)
emp.z.s <- as.matrix(Contour.s[, -1])
emp.z.s <- ifelse(emp.z.s < 0, 0, emp.z.s)

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) #"Spectral" #RdYlGn
##########################################################################
#                      plot
##########################################################################
pdf(paste0(paste0("./figure/"), 'Fig2_b',".pdf"), width = 10, height =  8)
# par(mfrow = c(1, 1))
# op <- par(mar = c(4, 4, 2, 1) + 0.5)
# par(cex = 1.25)
# par(mgp = c(3, 0.7, 0.3),  cex.axis = 1.5, cex.lab = 1.5)
op <- par(mar = c(5, 5, 1, 1) + 1)
par(mgp = c(4.0, 1.2, 0.3),  cex.axis = 1.8, cex.lab = 2, cex.main = 2.0)
filled.contour(x = surf.s$x, y = surf.s$y, z = surf.s$z,
               color.palette = myPalette,
               xlim = c(0, maxtime),
               ylim = c(0, 300),
               zlim = c(0, 1400),
               plot.title = title(main = "Summer of 2015",
                                  ylab = "Spatial lags (km)",
                                  xlab = "Temporal lags (days)"),
               plot.axes = {
                 # axis(1)
                 axis(1, at = seq(0, maxtime + 0.01, 1),
                      labels = seq(0, maxtime + 0.01, 1))
                 axis(2, at = seq(0, 310, 50), labels = seq(0, 310, 50))
                 contour(x = Contour.s$timelag,
                         y = as.numeric(colnames(Contour.s)[-1]),
                         z = emp.z.s,
                         add = TRUE, lwd = 2, labcex = 1.5)
               })
dev.off()
