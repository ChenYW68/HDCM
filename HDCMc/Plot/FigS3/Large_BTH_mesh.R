# china.county.map <- rgdal::readOGR(dsn = path.expand(
#   paste0("./DataProcess/Gadm36_CHN_shp/."))
#   , layer = "gadm36_CHN_1"
#   , stringsAsFactors = FALSE
#   , encoding = "UTF-8"
#   , use_iconv = T)
#
# china.county.map <- china.county.map[china.county.map$NAME_1 %in%
#                                        c("Beijing", "Tianjin", "Hebei"
#                                          ,"Shandong", "Nei Mongol"
#                                          ,"Shanxi", "Henan","Liaoning"
#                                          , "Shaanxi", "Jilin", "Shanghai"
#                                          , "Jiangsu", "Heilongjiang", "Anhui"
#                                        ),]

load("./data/Large_BTH_map.RData")
load("./data/NAQPMS_CMAQ_Dataset_2015W.RData")
Site <- Site[distToNearestCAMQpoint <= 15]
Map = fortify(larg_bth_map)
max.edge = c(.35, .7)
offset = c(1e-1, 0.6)
cutoff = .23 #0.1
n.grps = 3
col = "blue"
size = 1
site.id = "ID"

scale = 5



max.edge = c(.21, .3) #0.3,0.7
offset = c(1e-1, 0.9) #0.4, 0.6
cutoff = 0.11



hjust = c(-0.5, 0.5, -0.5, 0.5)
Map_BTH <- fortify(Map)
Range.coords.x <- c(floor(min(Map_BTH[, 1])) + hjust[1],
                    ceiling(max((Map_BTH[, 1]))) + hjust[2])
Range.coords.y <- c(floor(min(Map_BTH[, 2])) + hjust[3],
                    ceiling(max((Map_BTH[, 2]))) + hjust[4])
sampel.coords <- as.matrix(Site[, c("LON", "LAT")])
Map_BTH <- sampel.coords
global.coords = as.matrix(Map_BTH[, 1:2])



mesh <- inla.mesh.2d(
  # boundary = boundary,
  loc.domain = global.coords,
  loc = sampel.coords[, 1:2],
  max.edge = max.edge, #0.4,0.6
  offset = offset, #0.4, 0.5
  cutoff = cutoff, # 0.5
)



grid.coords <- as.data.frame(mesh[["loc"]][, 1:2])
colnames(grid.coords) <- c("LON", "LAT")
xlim <- range(grid.coords$LON)
ylim <- range(grid.coords$LAT)

n.grps <- 3
n.clust <- n.grps^2
xlocs <- seq(xlim[1],xlim[2],length=n.grps+1)[-1]
xlocs <- xlocs-0.5*mean(diff(xlocs))
ylocs <- seq(ylim[1],ylim[2],length=n.grps+1)[-1]
ylocs <- ylocs-0.5*mean(diff(ylocs))
part.centroids <- expand.grid(xlocs,ylocs)#[-2



data <- as.data.frame(Site)
coordinates(data) = ~ LON + LAT

p <- ggplot() + inlabru::gg(mesh, int.color = "transparent") + #geom_sf(col = "black") +
  # ggtitle(paste("Vertices: ", mesh$n)) +
  geom_polygon(data = Map,
               aes(x = long,y = lat, group = group),
               colour = 'black',
               fill = NA) + geom_point(part.centroids,
                                       mapping = aes(x = Var1, y = Var2,
                                                     col = "red", shape = "19"),
               ) +
  scale_x_continuous(limits = Range.coords.x,
                     breaks = seq(Range.coords.x[1], Range.coords.x[2], scale + 0.5),
                     labels = paste0(seq(Range.coords.x[1], Range.coords.x[2], scale + 0.5), "° E")) +
  scale_y_continuous(limits = Range.coords.y,
                     breaks = seq(Range.coords.y[1],
                                  Range.coords.y[2], scale),
                     labels = (paste0(seq(Range.coords.y[1],
                                          Range.coords.y[2], scale), '° N'))) +
  geom_point(Site, mapping = aes(LON, LAT, col = "black",
                                 shape = "+"), size = 0.05) +
  # inlabru::gg(data, size = size, pch = "+") +
  geom_label(label.size = 0,
             aes(x = 100, y = 54,
                 label =  paste0("Vertices: ", mesh$n, " \n",
                                 "Triangles: ",
                                 length(mesh$graph$tv[,1]))),
             nudge_x = 0,
             # family = c("sans"),
             # fontface = 'bold',
             color = "black",
             size = 4) +
  scale_color_manual("", values = c("red", "black"),
                     breaks  = c("red", "black"),
                     label = c("The centroids of 9 subregions", "Grid cells from the NAQPMS outputs")) +
  scale_shape_manual("", values = c(19, 3),
                     breaks  = c("19", "+"),
                     label = c("The centroids of 9 subregions", "Grid cells from the NAQPMS outputs")) +
  theme_bw() + #coord_fixed() +
  labs(x =  "Longitude", y = "Latitude") +
  theme(axis.text = element_text(size = 11, colour = "black")
        , axis.title = element_text(size = 14, colour = "black")
        , legend.position = c(0.21, 0.85)
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , legend.background = element_rect(fill = "transparent")
        # , legend.background =
        # , legend.title = element_text(size = 12, colour = "black")
        , legend.text = element_text(size = 11, colour = "black")
  ) + guides(colour=guide_legend(override.aes = list(size = 2)))
ggsave(plot = p, height = 6, width = 8,
       file = './figure/FigS3.pdf')
