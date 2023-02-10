load("./Calibration/Calibrated_Data.RData")
setDT(After_Cali_PM25)
y.max <- ceiling(max(Real_PM25$PM25, After_Cali_PM25$PM25))
Da <- After_Cali_PM25
Real_PM25 <- Real_PM25[Method!= 3,]


Da$Method <- ordered(Da$Method, levels = c(1, 2, 3))

int <- 1e2
Label <- as_labeller(c(`1` = "Before calibration",
                       `2` = "After calibration"
                       ))
range(Real_PM25$PM25)
range(After_Cali_PM25$PM25)
######################################################################
######################################################################
{
  p1 <- ggplot() +
    geom_point(data = Da
               , aes(x = LON, y = LAT
                     , color = PM25
                     , fill =  PM25
               )
               , size = 0.8, pch = 20) +
    geom_point(data = Real_PM25
               , aes(x = LON, y = LAT
                     , color = PM25
                     , fill =  PM25
               )
               , size = 1.2, pch = 18) +
    facet_grid(Method ~ Date
               , labeller = labeller(Method = Label), switch = "both") +
    scale_x_continuous(limits = c(113.5, 120),
                       breaks = seq(113.5, 120, 2),
                       labels = paste0(seq(113.5, 120, 2), "° E")) +
    scale_y_continuous(limits = c(36, 42.6),
                       breaks = seq(36, 42.6, 2),
                       labels = paste0(seq(36, 42.6, 2), "° N"))+
    theme_bw() +
    theme(
      axis.text = element_blank()
      , axis.title = element_blank()
      , axis.ticks = element_blank()
      # rect = element_blank()
      # axis.text = element_text(size = 12, colour = "black")
      #     , axis.text.x = element_text(angle=90, hjust=1)
      #     , axis.title = element_text(size = 14, colour = "black")
      , legend.title = element_text(size = 10, colour = "black")
      , legend.text = element_text(size = 10, colour = "black")
      , legend.key.width = unit(7.8,"line")
      , legend.key.height = unit(1,"line")
      , panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , legend.position = "bottom"
      , legend.background = element_rect("transparent")
      , legend.margin= ggplot2::margin(t = 10, 0, 0, 0)
      # , legend.box.margin= margin(t=10,0,0,0)
      , legend.box.spacing = ggplot2::margin(t = -5,0,0,0)
      , strip.text = element_text(size = 10, colour = "black")
    ) + guides(fill = "none")#+ guides(col = "none")
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) #"Spectral" #RdYlGn
  sc <- scale_colour_gradientn(colours = myPalette(nrow(unique(After_Cali_PM25[After_Cali_PM25$Method == 1, c("LON", "LAT")]))), #
                               , limits = c(0, y.max),
                               name = expression(atop(atop(textstyle("PM"[2.5]),
                                                           textstyle(paste("(", mu, g, "/", m^{3},")"))), NA)),
                               breaks = c(0, round(c(seq(0, y.max - int, int)), 0)[-1], y.max),
                               labels = c(0, round(c(seq(0, y.max - int, int)), 0)[-1], y.max))

  p1 <- p1 + sc
}
ggsave(plot = p1, file = "./figure/Fig7.pdf",
       width = 9, height = 4, dpi = 300)
