source("./R/PSTVB_Packages.R")
load("./data/NAQPMS_CMAQ_Dataset_2015W.RData")
# Site <- NAQPMS_BTH_SITE
Site <- Site[distToNearestCAMQpoint <= 15]
sampel.coords <- as.matrix(Site[, c("LON", "LAT")])#as.matrix(da[, c("LON", "LAT")])
load("./data/CMAQ_BTH_SITE.RData")

load("./data/Large_BTH_map.RData")
Map_BTH <- fortify(larg_bth_map)
# mesh <- inla.mesh.2d(
#   #boundary = boundary
#   # loc.domain = global.coords
#   loc = sampel.coords[, 1:2],
#   max.edge = c(.4, .6), #0.3,0.7
#   offset = c(4e-1, 0.5), #0.4, 0.6
#   cutoff = .5,
# )
# # plot(mesh)
# # sampel.coords <- as.matrix(da[, c("LON", "LAT")])
# data <- as.data.frame(sampel.coords)
# coordinates(data) = ~ LON + LAT
#
#
# NAQPMS_CMAQ <- rbind(data.frame(LON = Site$LON,
#                                   LAT = Site$LAT,
#                                   Flag = "NAQPMS"),
#                        data.frame(LON = CMAQ_BTH_SITE$LON,
#                                   LAT = CMAQ_BTH_SITE$LAT,
#                                   Flag = "CMAQ"))
#
#
#
# label <- "da"
# p <- ggplot() +# gg(mesh) + #geom_sf(col = "black") +
#   # ggtitle(paste("Vertices: ", mesh$n)) +
#   geom_polygon(data = Map_BTH,
#                aes(x = long,y = lat, group = group),
#                colour = 'black',
#                fill = NA) +
#   # coord_sf(datum = st_crs(5880)) +
#   # gg(data, col = "gray10", size = 1, pch = "+" ) +
#   geom_point(NAQPMS_CMAQ, mapping = aes(x = LON, y = LAT, pch = Flag,  col = Flag),
#              size = 0.1) +
#   scale_shape_discrete(name = '',
#                         values = c(1, 19),
#                         labels = label) +
#   theme_bw() + #coord_fixed() +
#   labs(x =  "Longitude", y = "Latitude") +
#   theme(axis.text = element_text(size = 8, colour = "black"),
#         axis.title = element_text(size = 14, colour = "black")
#         # , legend.title = element_text(size = 12, colour = "black")
#         # , legend.text = element_text(size = 8, colour = "black")
#   )
# # plot(p)
# ggsave(
#   plot = p,
#   file = "./figure/large_bth_data.pdf",
#   width  = 10,
#   height = 6
# )

# pdf(paste0(paste0("./figure/"), 'large_bth_data',".pdf"), width = 10, height =  8)
# par(mfrow = c(1, 1))
# op <- par(mar = c(4, 4, 2, 1) + 0.5)
# par(cex = 1.25)
# par(mgp = c(3, 0.7, 0.3),  cex.axis = 1.5, cex.lab = 1.5)
# plot(C.china.county.map)
# points(Site$LON, Site$LAT, pch = "+", cex = 0.1)
# points(CMAQ_BTH_SITE$LON, CMAQ_BTH_SITE$LAT, pch = "*", cex = 0.1)
# dev.off()



areal.data <- as(larg_bth_map, "SpatialPolygons")
proj4string(areal.data) <- CRS("+proj=longlat +datum=WGS84")

pdf(paste0(paste0("./figure/"), 'FigS2',".pdf"), width = 10, height =  8)
# par(mfrow = c(1, 1))
op <- par(mar = c(4, 4, 0, 1) + 0.5)
# par(cex = 1.25)
par(mgp = c(3, 0.7, 0.3),  cex.axis = 1.2, cex.lab = 1.5)
plot(areal.data, #xlim = c(93, 135),
     #ylim = c(30, 50),
     lwd = 2, axes = F, las = 0,
     xlab = "Longitude",
     ylab = "Latitude")
axis(1,
     # col = "black",        # Axis line color
     # col.ticks = "black", # Ticks color
     # col.axis = "black",
     at = seq(95, 135, 5),
     labels  = paste0(seq(95, 135, 5), "°E"))
axis(2,
     # col = "black",        # Axis line color
     # col.ticks = "black", # Ticks color
     # col.axis = "black",
     at = seq(29, 55, 5),
     labels  = paste0(seq(29, 55, 5), "°N"))
points(Site$LON, Site$LAT, pch = "+", cex = 0.4, col = "black")
points(CMAQ_BTH_SITE$LON, CMAQ_BTH_SITE$LAT, pch = "*", cex = 0.2, col = "red")
legend(x = 100, y = 54,
       legend = c("Grid cells for the NAQPMS system", "Grid cells for the CMAQ system"),
       col = c("black", "red"),
       ncol = 1,
       # pch = as.numeric(c(".", 19)),
       pch = c("+", "*"),#as.numeric(c("+", "*")),
       # lty = c("blank", "solid"),
       # lwd = 0,
       bty = "n", cex = 1,
       pt.cex = 2.0)
dev.off()
# https://geocompr.robinlovelace.net/adv-map.html
# library(tmap)    # for static and interactive maps
# library(leaflet)
# library(sf)
# library(raster)
# library(dplyr)
# library(spData)
# library(spDataLarge)
