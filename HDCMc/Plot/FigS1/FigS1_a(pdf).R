#########################################################################
rm(list=ls())
source("./R/PSTVB_Packages.R")
data("SiteData", package = "ADCM")
setDF(obs_PM25_2015s);
season <- "Summer"
Width <- c(7, 0.5, 0.2)
library(latex2exp)
file <- paste0("./figure/")
#########################################################################
#########################################################################
{
  PM25_CMAQ <- obs_PM25_2015s %>% setorder(ID) %>%
    dplyr::select(ID, CITY
                  , DATE_TIME
                  , YEAR, MONTH, DAY
                  , YEAR_MONTH
                  , REAL_PM25
                  , sim_CMAQ_PM25
                  , sim50_CMAQ_PM25
                  , LON, LAT
                  , LON_X, LAT_Y)
  setDF(PM25_CMAQ)
  # PM25_CMAQ$SQRT_BIAS25 = sqrt(PM25_CMAQ$REAL_PM25) - sqrt(PM25_CMAQ$sim_CMAQ_PM25)
  # PM25_CMAQ$LOG_BIAS25 = log10(PM25_CMAQ$REAL_PM25) - log10(PM25_CMAQ$sim_CMAQ_PM25)
  # PM25_CMAQ$BIAS25 = PM25_CMAQ$REAL_PM25 - PM25_CMAQ$sim_CMAQ_PM25
  setDT(PM25_CMAQ)
  #########################################################################
  # pm25_cmaq_data<- plyr::ddply(PM25_CMAQ
  #                        , .(CITY, SITEID, MONTH, DATE_TIME)
  #                        , plyr::summarize
  #                        , REAL_PM25_Avg = mean(REAL_PM25 , na.rm = TRUE)
  #                        , SQRT_REAL_PM25_Avg = mean(sqrt(REAL_PM25)
  #                                                    , na.rm = TRUE)
  #                        , LOG_REAL_PM25_Avg = mean(log(REAL_PM25)
  #                                                   , na.rm = TRUE)
  #                        , CMAQ_PM25_Avg = mean(CMAQ_PM25 , na.rm = TRUE)
  #                        , SQRT_CMAQ_PM25_Avg = mean(sqrt(CMAQ_PM25)
  #                                                    , na.rm = TRUE)
  #                        , LOG_CMAQ_PM25_Avg = mean(log(CMAQ_PM25)
  #                                                   , na.rm = TRUE))
  pm25_cmaq_data <- PM25_CMAQ
  #####################################################################
  #  correlation map
  #####################################################################

  x0 = pm25_cmaq_data$REAL_PM25
  y0 = pm25_cmaq_data$sim_CMAQ_PM25

  x1 = sqrt(pm25_cmaq_data$REAL_PM25)
  y1 = sqrt(pm25_cmaq_data$sim_CMAQ_PM25)

  x2 = log(pm25_cmaq_data$REAL_PM25)
  y2 = log(pm25_cmaq_data$sim_CMAQ_PM25)
  # par(mfrow = c(1, 3))
  # plot(x0, y0)
  # plot(x1, y1)
  # plot(x2, y2)

  r0 = round(cor(x0, y0), 3)
  r1 = round(cor(x1, y1), 3)
  r2 = round(cor(x2, y2), 3)

  da <- rbind(data.frame(CMAQ = x0, PM25 = y0, z = abs(x0 - y0),
                         r = r0, FLAG = "Original scale", GROUP = 1),
              data.frame(CMAQ = x1, PM25 = y1, z = abs(x1 - y1),
                         r = r1, FLAG = "Square root scale", GROUP = 2),
              data.frame(CMAQ = x2, PM25 = y2, z = abs(x2 - y2),
                         r = r2, FLAG = "Logarithmic scale", GROUP = 3))
  setDT(da)
}
################################################################
library(latex2exp)
size = c(23, 20, 20)


# g = 3
# Da <- da[GROUP ==g]
# P3 <- ggplot(data = Da) +
#   geom_point(aes(x= CMAQ, y = PM25, color = z), size = 1) +
#   facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
#   theme_bw() +
#   scale_colour_gradient(
#     limits = c(min(Da$CMAQ, Da$PM25), max(Da$CMAQ, Da$PM2))
#     , space = "Lab", low = 'green'
#     # , mid = 'green'
#     , high = 'red'
#     # colours =c(upper,lower, upper)
#     , na.value = "green") +
#   geom_label(x= quantile(Da$CMAQ)[1] + 1, y = max(Da$PM25),
#             label = paste0("R = ", min(Da$r)),
#             size = 5) +
#   labs(x= "", y = "") +
#   # labs(x = TeX("CMAQ PM2.5 ($μg/m^3$)"), y = TeX("Real PM2.5 ($μg/m^3$)")) +
#   theme(axis.text = element_text(size = 15, colour = "black")
#         , axis.title= element_text(size = 18, colour = "black")
#         # , legend.title = element_text(size = 10, colour = "black")
#         , legend.text= element_text(size = 15, colour = "black")
#         , legend.title = element_blank()
#         # , legend.position="top"
#         , legend.position = "none"
#         , legend.key.width = unit(1,"line")
#         , legend.key.height = unit(2,"line")
#         , strip.text =  element_text(size = 18, colour = "black")
#   )

#####################################################################
# library(cowplot)
# p <- cowplot::plot_grid(P1, P2, nrow = 1)
# ggsave(plot = p, paste0(file, 'FigS4_', season,".pdf"),
#        dpi= 500, width = 20, height = 10)
#####################################################################
#####################################################################
size = c(25, 23, 25)
g = 1
y.h = 0.19
Da <- da[GROUP ==g]
{
  P1 <- ggplot(Da, aes(PM25))  +
    geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1,
                   position="identity"
                   , binwidth = Width[1], colour = "black") +
    geom_label(x = 3, y = 0.19, label = paste0("(d)"),
               size = 12, label.size = 0) +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    theme_bw() +
    scale_x_continuous(limits = c(0, ceiling( max(Da$PM25)))) +
    scale_y_continuous(limits = c(0, y.h)) +
    # labs(x = " ", y = "Density") +
    labs(x = TeX("Observed PM$_{2.5}$ concentrations"), y = "Density") +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )

  g = 2
  Da <- da[GROUP ==g]
  P2 <- ggplot(Da, aes(PM25))  +
    geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1,
                   position="identity"
                   ,binwidth = Width[2], colour = "black") +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    geom_label(x = 0.2, y = 0.19,
               label = paste0("(e)"),
               size = 12, label.size = 0) +
    # geom_label(x= 12, y = 0.16,
    #            label = paste0("11/01/15~01/31/16"),
    #            size = 10, label.size = 0) +
    theme_bw() +
    scale_x_continuous(limits = c(0, ceiling(max(Da$PM25)))) +
    scale_y_continuous(limits = c(0, y.h)) +
    # labs(x = TeX("Observed PM$_{2.5}$ at different scales"), y = "") +
    labs(x = TeX("Observed PM$_{2.5}$ concentrations"), y = "Density") +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )
}

#####################################################################
g = 3
Da <- da[GROUP == g]
{
  P3 <- ggplot(Da, aes(PM25))  +
    geom_histogram(aes(y =..density../sum(..density..)), alpha=0.1,
                   position="identity"
                   , binwidth = Width[3], colour = "black")+
    geom_label(x = 0.3, y = 0.19, label = paste0("(f)"),
               size = 12, label.size = 0) +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    scale_x_continuous(limits = c(0, ceiling(max(Da$PM25)))
                       , breaks = seq(0, ceiling(max(Da$PM25)), 2)
                       , labels = seq(0, ceiling(max(Da$PM25)), 2)
                       , expand = c(0, 0)
    ) +
    theme_bw() +
    # labs(x = "", y = "") +
    labs(x = TeX("Observed PM$_{2.5}$ concentrations"), y = "Density") +
    scale_y_continuous(limits = c(0, y.h)) +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )
}
#####################################################################
#####################################################################
library(cowplot)
p <- cowplot::plot_grid(P1, P2, P3, nrow = 1)
#####################################################################
#####################################################################
ggsave(plot = p, paste0(file, "FigS1_a.pdf"),
       dpi = 300, width = 20, height = 6.5)
#####################################################################
#####################################################################
