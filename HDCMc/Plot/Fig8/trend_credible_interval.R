rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "HDCM")
load("./Full_Data_Model/all_HDCM_1_23_3.RData")
  
  
  
CityId <- unique(Site[, 1:2])
CityId$Id <- 1:nrow(CityId)
######################################################################################
City_Name <- "Hengshui"
CityId <- CityId[CityId$CITY %in% City_Name,]
Id <- CityId$Id
######################################################################################
#                             HDCM
######################################################################################
# in winter
REAL_PM25 <- as.data.frame(wHDCM[["true.Yts"]])
REAL_PM25$DATE_TIME <- as.Date(rownames(wHDCM[["true.Yts"]]))
REAL_PM25 <- gather(
            data = REAL_PM25,      #待转换的数据集名称
            key = "ID",       #转换后的分类字段名称（维度）
            value="PM25",    #转换后的度量值名称
            -DATE_TIME
          )

REAL_PM25$ID <- as.numeric(REAL_PM25$ID)


Fit.L25 <- as.data.frame(wHDCM[["Fitted.Yts"]][1,,])
Fit.L25$DATE_TIME <- as.Date(rownames(wHDCM[["Fitted.Yts"]][1,,]))
Fit.L25 <- gather(
  data = Fit.L25,      #待转换的数据集名称
  key = "ID",       #转换后的分类字段名称（维度）
  value="Fit.L25",    #转换后的度量值名称
  -DATE_TIME
)


Fit.Median <- as.data.frame(wHDCM[["Fitted.Yts"]][2,,])
Fit.Median$DATE_TIME <- as.Date(rownames(wHDCM[["Fitted.Yts"]][2,,]))
Fit.Median <- gather(
                data = Fit.Median,      #待转换的数据集名称
                key = "ID",       #转换后的分类字段名称（维度）
                value = "PM25",    #转换后的度量值名称
                -DATE_TIME
              )

Fit.U95 <- as.data.frame(wHDCM[["Fitted.Yts"]][3,,])
Fit.U95$DATE_TIME <- as.Date(rownames(wHDCM[["Fitted.Yts"]][3,,]))
Fit.U95 <- gather(
                  data = Fit.U95,      #待转换的数据集名称
                  key = "ID",       #转换后的分类字段名称（维度）
                  value="Fit.U95",    #转换后的度量值名称
                  -DATE_TIME
                )


Fit.Median <- Fit.Median %>% left_join(Fit.L25, by = c("DATE_TIME", "ID")) %>%
              left_join(Fit.U95, by = c("DATE_TIME", "ID")) %>%
  setorderv(c("ID", "DATE_TIME"))
Fit.Median$ID <- as.numeric(Fit.Median$ID)

Fit.Median <- Fit.Median #


REAL_PM25$Fit.L25 <- REAL_PM25$Fit.U95 <- REAL_PM25$PM25
REAL_PM25$Model <- "Observation"
Fit.Median$Model <- "HDCM"


CMAQ <- obs_PM25_2015w[, c(8, 2, 14)]
CMAQ$Model <- "CMAQ"
setnames(CMAQ, "sim_CMAQ_PM25", "PM25")
CMAQ$Fit.L25 <- CMAQ$Fit.U95  <- CMAQ$PM25




######################################################################################
######################################################################################
HDCM_fit <- rbind(REAL_PM25, Fit.Median, CMAQ) %>%
  left_join(as.data.frame(Site[, c(1, 4)]), by = c("ID")) %>%
  filter(month(DATE_TIME) %in% 12, day(DATE_TIME) %in% c(17:22))



HDCM_fit <- HDCM_fit[HDCM_fit$CITY %in% City_Name,]

range(HDCM_fit$Fit.U95)

######################################################################################


Da <- HDCM_fit %>% ddply(.(CITY, DATE_TIME, Model)
        , .fun = plyr::summarize
        , Fit.L25 = mean(Fit.L25, na.rm = TRUE)
        , Fit.U95 = mean(Fit.U95, na.rm = TRUE)
        , PM25 = mean(PM25, na.rm = TRUE)
        , .progress = "text")
######################################################################################
#             plot  credible  interval
######################################################################################
label <- c("Observation", "Before calibration", "After calibration with 95% interval")
Bcol <- c("black", "grey80", "grey50")
######################################################################################
Sys.setlocale("LC_TIME", "English")
######################################################################################
size = c(0.4, 0.8, 4)
UP = max(Da$Fit.U95, na.rm = T) + 10
scal = 150
time <- unique(Da$DATE_TIME)

alpha <- c("1", "2", "3")
Da$alpha <- ifelse(Da$Model %in% "HDCM", alpha[3],
                   ifelse(Da$Model %in% "CMAQ",
                          alpha[2], alpha[1]))
S = c("1", "2", "3")
Da$size <- ifelse(Da$Model %in% "HDCM", S[3],
                  ifelse(Da$Model %in% "CMAQ",
                         S[2], S[1]))

Da$Model <- ifelse(Da$Model %in% "HDCM", "HDCM",
                   ifelse(Da$Model %in% "CMAQ",
                          "CMAQ", "Observation"))


Da$Model <- ordered(Da$Model, levels = c("Observation", "CMAQ",
                                         "HDCM"))
ls <- c(1.3, 1.3, 1.3)
######################################################################
######################################################################
p <- ggplot(Da, aes(x = DATE_TIME, group  = Model)) +
  geom_ribbon(aes(ymin = Fit.L25, ymax = Fit.U95,
                  linetype = Model, fill = Model),
              alpha = 0.3, size = size[1]) +
  geom_line(aes(y = PM25, linetype = Model, col = Model,
                alpha = alpha, size = size)) +
  theme_bw() +
  scale_colour_manual(name = '', values = c("black",
                                            "gray50",
                                            "#43a2ca"),
                      labels = label) +
  scale_fill_manual(name = '', values = c("transparent",
                                          "transparent",
                                          "#43a2ca"),
                    labels = label) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_size_manual(values = ls) +
  scale_linetype_manual(name = '',
                        values = c("dashed",
                                 "twodash",
                                 "solid"),
                        labels = label) +
  scale_x_continuous(
    # expand = c(1e-5, 0),
    breaks  = unique(Da$DATE_TIME)
    , labels = c("Dec 17, 2015", "Dec 18, 2015",
                 "Dec 19, 2015", "Dec 20, 2015",
                 "Dec 21, 2015", "Dec 22, 2015"
    )
  ) +
  scale_y_continuous(limits = c(0, UP),
                     breaks  = seq(0, UP, scal),
                     labels = seq(0, UP, scal)) +
  labs( x = "Date", fill = "",
        y = latex2exp::TeX("PM$_{2.5}$ $( \\mu g/m^3 )$")) +
  theme(axis.text = element_text(size = 20, colour = "black")
        ,axis.text.x = element_text(hjust = 0.6, size = 20, colour = "black")
        , axis.title = element_text(size = 22, colour = "black")
        , legend.title = element_text(size = 20, colour = "black")
        , legend.text = element_text(size = 20, colour = "black")
        # , legend.title = element_blank()
        , legend.background = element_rect(colour = 'transparent'
                                           , fill = 'transparent')
        , legend.key.width = unit(5,"line")
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , legend.position = c(0.5, 0.9)
        # , legend.margin = margin(t = -0.1, unit='cm')
        , strip.text =  element_text(size = 16, colour = "black")) +
  guides(col = guide_legend(override.aes = list(size = ls),
                            nrow = 1, byrow = TRUE),
         alpha = "none", size = "none")
# p
ggsave(plot = p, file = "./figure/Fig8.pdf", width  = 18, height = 5)

# p











