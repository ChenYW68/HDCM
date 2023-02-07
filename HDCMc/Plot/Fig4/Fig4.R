rm(list=ls())
source("./R/PSTVB_Packages.R")
library(CovUtil)
source("./R/CreateGrid.R")
source("./R/util.R")
load("./data/SiteData.RData")
load("./data/bth_map.RData")
obs_PM25_2015w <- obs_PM25_2015w
setDF(obs_PM25_2015w);setDF(Site);

INDX <- which(colnames(Site) %in% c("LON_X", "LAT_Y"))
Site <- spCoords.transform(Site[, -INDX], method = 1)
setDF(obs_PM25_2015w)
INDX <- which(colnames(obs_PM25_2015w) %in% c("LON_X", "LAT_Y"))
PM25_2015w <- spCoords.transform(obs_PM25_2015w[, -INDX],
                                 method = 1)



Ch <- 0.23
n.grps <- 2
H.basic.data <- CreateGrid(PM25_2015w,
                           sample.coords = Site,
                           Map = bth_map,
                           max.edge = c(.35, .65),
                           offset = c(1e-1, 0.5),
                           cutoff = 0.04,
                           distance.scale = 1e3,
                           n.grps = n.grps,
                           col = "black",
                           size = 2,
                           site.id = "ID",
                           factor = 1,
                           ch = Ch,
                           method = "Wendland",
                           distance.method = 1,
                           hjust = c(-1, 1, -1, 0.5),
                           way = 1)







grid.coords <- H.basic.data$grid.coords
mesh <- H.basic.data$mesh
Map <- bth_map
grid.coords$cluster <- as.factor(grid.coords$cluster)
p <- ggplot(data = grid.coords, aes(x = LON, y = LAT)) +
  geom_point(size = 1E-5)
p
grap <- mesh[["graph"]][["vv"]]
df <- NULL
for(i in 1:mesh$n){
  x <- grid.coords[i, ]
  y <- grid.coords[which(grap[i, ] == 1), ]
  # y <- y[y$cluster == x$cluster, ]

  df <- rbind(df, data.frame(x1 = x$LON, x2 = y$LON,
                             y1 = x$LAT, y2 = y$LAT))
}
p1 <- p + geom_segment(aes(x = x1, y = y1,
                           xend = x2,
                           yend = y2),
                       color = "gray80",
                       data = df)


p1 <- p1 +
  geom_polygon(data = Map, aes(x = long,y = lat, group = group),
               colour = 'black', fill = NA) +
  # coord_sf(datum = st_crs(5880)) +
  geom_point(data = Site, aes(x = LON, y = LAT)
             , shape = 20
             , col = "red", size = 1) +
  scale_x_continuous(limits = c(112.5, 120.5),
                     breaks = seq(113, 120.5, 1.5),
                     labels = paste0(seq(113, 120.5, 1.5), "° E")) +
  scale_y_continuous(limits = c(35.3, 43.3),
                     breaks = seq(35, 43.5, 2),
                     labels = paste0(seq(35, 43.2, 2), "° N"))+
  # geom_text(aes(x = 119.5, y = 37
  #               , label =  paste("Vertices: ", mesh$n, " \n",
  #                                "Triangles: ",
  #                                length(mesh$graph$tv[,1]))),
  #           color = "black", size = size[3]) +
  geom_label(label.size = 0,
             aes(x = 112.5, y = 43.3,
                 label =  "(a)"),
             color = "black",
             size = 7) +
  geom_label(label.size = 0,
             aes(x = 120.5, y = 36,
                 label =  paste("Vertices: ", mesh$n, " \n",
                                "Triangles: ",
                                length(mesh$graph$tv[,1]))),
             nudge_x = -1,
             # family = c("sans"),
             # fontface = 'bold',
             color = "black",
             size = 5) +
  geom_point(data = Site, aes(x = LON, y = LAT)
             , shape = 20
             , col = "red", size = 1) +
  theme_bw() + #coord_fixed() +
  labs(x =  "Longitude", y = "Latitude") +
  theme(axis.text = element_text(hjust = 0.7, size = 14, colour = "black")
        , axis.title = element_text(size = 16, colour = "black")
        , legend.position = c(0.82, 0.1)
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        # , legend.background = element_rect(colour = "transparent",
        #                                  fill = "transparent")
        # , legend.title = element_text(size = 12, colour = "black")
        # , legend.text = element_text(size = 8, colour = "black")
  ) + guides(colour=guide_legend(title = "Cluster: ", nrow = 1,
                                 byrow = TRUE,
                                 override.aes = list(size = 1.0)))


#--------------------------
grap <- mesh[["graph"]][["vv"]]
df <- NULL
for(i in 1:mesh$n){
  x <- grid.coords[i, ]
  y <- grid.coords[which(grap[i, ] == 1), ]
  y <- y[y$cluster == x$cluster, ]

  df <- rbind(df, data.frame(x1 = x$LON, x2 = y$LON,
                             y1 = x$LAT, y2 = y$LAT,
                             cluster = x$cluster))
}
p2 <- p + geom_segment(aes(x = x1, y = y1,
                          xend = x2, yend = y2,
                          colour = cluster),
                      data = df)


p2 <- p2 + scale_colour_manual(
  values = c("gray80", "gray50")) +
  geom_polygon(data = Map, aes(x = long,y = lat, group = group),
               colour = 'black', fill = NA) +
  # coord_sf(datum = st_crs(5880)) +
  geom_point(data = Site, aes(x = LON, y = LAT)
             , shape = 20
             , col = "red", size = 1) +
  scale_x_continuous(limits = c(112.5, 120.5),
                     breaks = seq(113, 120.5, 1.5),
                     labels = paste0(seq(113, 120.5, 1.5), "° E")) +
  scale_y_continuous(limits = c(35.3, 43.3),
                     breaks = seq(35, 43.5, 2),
                     labels = paste0(seq(35, 43.2, 2), "° N"))+
  # geom_text(aes(x = 119.5, y = 37
  #               , label =  paste("Vertices: ", mesh$n, " \n",
  #                                "Triangles: ",
  #                                length(mesh$graph$tv[,1]))),
  #           color = "black", size = size[3]) +
  geom_label(label.size = 0,
             aes(x = 112.5, y = 43.3,
                 label =  "(b)"),
             color = "black",
             size = 7) +
 # geom_label(label.size = 0,
 #               aes(x = 120.5, y = 37,
 #                   label =  paste("Vertices: ", mesh$n, " \n",
 #                                  "Triangles: ",
 #                                  length(mesh$graph$tv[,1]))),
 #               nudge_x = -1,
 #               # family = c("sans"),
 #               # fontface = 'bold',
 #               color = "black",
 #            size = 4) +
  geom_point(data = Site, aes(x = LON, y = LAT)
             , shape = 20
             , col = "red", size = 1) +
  theme_bw() + #coord_fixed() +
  labs(x =  "Longitude", y = "Latitude") +
  theme(axis.text = element_text(hjust = 0.7, size = 14, colour = "black")
        , axis.title = element_text(size = 16, colour = "black")
        , axis.ticks.y = element_blank()
        , legend.title = element_text(size = 14, colour = "black")
        , legend.text = element_text(size = 12, colour = "black")
        , legend.position = c(0.82, 0.1)
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        # , legend.background = element_rect(colour = "transparent",
        #                                  fill = "transparent")
        # , legend.title = element_text(size = 12, colour = "black")
        # , legend.text = element_text(size = 8, colour = "black")
  ) + guides(colour=guide_legend(title = "Cluster: ", nrow = 1,
                                 byrow = TRUE,
                                 override.aes = list(size = 1.0)))

ggsave(plot = p1, width = 7, height = 7, file = './figure/Fig4_a.pdf')
ggsave(plot = p2, width = 7, height = 7, file = './figure/Fig4_b.pdf')
