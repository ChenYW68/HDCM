source("./R/PSTVB_Packages.R")

###################################################################
#                             HDCM
###################################################################
load("./Calibration/Result/HDCM_1_23_3.RData")
load("./data/SiteData.RData")

# in winter
# HDCM <- Tab[grepl("Fit_HDCM2_W", Tab)]
# load(paste0(file, "data/",HDCM))
# load(paste0(file, "data/", "train_HDCM2_W.RData"))
# y.fit <- test.HDCM(test = train, Ks = Re$Ks, PIU = Re$PIU, seed = 1234)
# y.fit <- (apply(y.fit, c(1, 2), quantile, prob = 0.5))



HDCM2_Residuals <- as.vector(wHDCM$Yts - wHDCM$Yts.Pred[2,,])
range(HDCM2_Residuals)
###################################################################
#                             CMAQ
###################################################################
x_range <- c(-200, 200)
density_range <- c(0, 0.04)

# In winter
{
  CMAQ_Residuals <- as.vector(obs_PM25_2015w$sim50_CMAQ_PM25 -
                                obs_PM25_2015w$REAL_PM25)
  range(CMAQ_Residuals)

  da <- rbind(data.frame(index = 1:length(CMAQ_Residuals),
                         error = CMAQ_Residuals,
                         method = "Before calibration"),
              data.frame(index = 1:length(CMAQ_Residuals),
                         error = HDCM2_Residuals,
                         method = "After calibration"))
  ggplot(da) + geom_density(aes(x = error, linetype = method))

  setDT(da)

  p <- ggplot(data = da, aes(colour = method, group = method, fill = method)) +
    geom_density(aes(error), alpha = 0.2, adjust = 3, size = 1)
  # facet_wrap(~ LAT_label, ncol = 4
  #            , labeller = labeller(LAT_label = Label)
  # )    #facet_grid
  p0 <- ggplot_build(p)
  da0 <- p0$data[[1]]
  lab <-  unique(da$method)
  da0$method = if_else(da0$group==1, lab[2],  lab[1])
  da0$method <- ordered(da0$method, levels = c("Before calibration",
                                               "After calibration"))
  label <- c("Before calibration", "After calibration")
}


{
  p1 <- ggplot(data = da0, aes(x = x, y = density)) +
    # geom_point(aes(shape = method), size = 5) +
    geom_line(aes(linetype = method, col = method), size = 1.5) +
    # scale_shape_manual(name = '', values =  c('*', '+'), labels = label) +
    scale_linetype_manual(name = '', values=  c("dashed", "solid"),
                          labels = label) +
    scale_color_manual(name = '', values =  c("black", "blue"),
                       labels = label)+
    geom_vline(xintercept = 0, col = "gray80", size = 0.8) +
    theme_bw() + ylim(density_range[1], density_range[2]) +
    scale_x_continuous(limits = c(x_range[1], x_range[2]),
                       breaks = seq(x_range[1], x_range[2],
                                    by = 2e2)) +
    geom_text(aes(x = -400, y = 0.04
                  , label =  "(a)"),
              # family = c("sans"),
              # fontface = 'bold',
              color = "black", size = 10) +
    labs(x = TeX("Error (μg/m^3)"), y = "Density") +
    theme(axis.text = element_text(size = 20, colour = "black")
          ,axis.text.x = element_text(hjust = 0.25, size = 20, colour = "black")
          , axis.title = element_text(size = 22, colour = "black")
          , legend.title = element_text(size = 20, colour = "black")
          , legend.text = element_text(size = 20, colour = "black")
          # , legend.title = element_blank()
          , legend.background = element_rect(colour = 'transparent'
                                             , fill = 'transparent')
          , legend.key.width = unit(5,"line")
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , legend.position = c(0.25, 0.6)
          # , legend.margin = margin(t = -0.1, unit='cm')
          , strip.text =  element_text(size = 16, colour = "black")) +
    guides(linetype = guide_legend(override.aes = list(size = 1.5),
                                   nrow = 2, byrow = TRUE))

}

da$method <- ordered(da$method, levels = c("Before calibration",
                                           "After calibration"))
{
  p2 <- ggplot(da, aes(x = index, y = error)) +
    geom_point(aes(shape = method, col = method), size = 4) +

    scale_color_manual(name = '', values =  c("black", "blue"), labels = label)+
    theme_bw() + ylim(x_range[1], x_range[2]) +
    geom_text(aes(x = 1, y = 400
                  , label =  "(b)"),
              # family = c("sans"),
              # fontface = 'bold',
              color = "black", size = 10) +
    scale_shape_manual(name = '', values =  c("o", "+"), labels = label) +
    labs(x = "Index", y = TeX("Error (μg/m^3)")) +
    theme(axis.text = element_text(size = 20, colour = "black")
          ,axis.text.x = element_text(hjust = 0.25, size = 20, colour = "black")
          , axis.title = element_text(size = 22, colour = "black")
          , legend.title = element_text(size = 20, colour = "black")
          , legend.text = element_text(size = 20, colour = "black")
          # , legend.title = element_blank()
          , legend.background = element_rect(colour = 'transparent'
                                             , fill = 'transparent')
          , legend.key.width = unit(5,"line")
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , legend.position = c(0.78, 0.95)
          # , legend.margin = margin(t = -0.1, unit='cm')
          , strip.text =  element_text(size = 16, colour = "black")) +
    guides(shape = guide_legend(override.aes = list(size = 10),
                                nrow = 1, byrow = TRUE))
  # p <- ggpubr::ggarrange(p1, p2, nrow = 1, ncol = 2)
  # p
}
p <- ggpubr::ggarrange(p1, p2, nrow = 1, ncol = 2)
##############################################################################
ggsave(plot = p, file = "./figure/Fig9.pdf", width = 18, height = 7, dpi = 300)
