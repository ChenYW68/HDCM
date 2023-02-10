# in winter
rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "HDCM")
file <- "./Result/"
######################################################################

######################################################################
Season <- "W"
City.Name <-  sort(as.character(unique(Site$CITY)))
Tab <- list.files(file)
if(length(Tab) == 0){
  cat("\n.............................\n")
  cat("Please first run the file \n\n 'Step1_run_all_model.R', ", "\n\n")
  cat("and then try again...", "\n\n")
  cat("\n.............................\n")
}else{

  ######################################################################
  load(paste0("./Result/HDCMw.RData"))
  cv.HDCM <- CVw_BTH
  load(paste0("./Result/HDCM2w.RData"))
  cv.HDCM2 <- CVw_BTH
  
  setDF(obs_PM25_2015w)
  setDF(Site)
  colnames(obs_PM25_2015w)
  CMAQ <- copy(obs_PM25_2015w)
  setnames(CMAQ, "sim_CMAQ_PM25", "PM25.Pred")
  colnames(CMAQ)
  colnames(obs_PM25_2015w)
  CMAQ$ID <- NULL
  # setDF(CMAQ)

  ######################################################################
  #  HDCM
  ######################################################################
  HDCM <- NULL
  for (r in 1:13) {
    cv.HDCM[[r]]$Pred <- cv.HDCM[[r]]$Predict.Yts 
    da <- as.data.frame(cv.HDCM[[r]]$Pred)
    da$DATE_TIME <- as.Date(rownames(cv.HDCM[[r]]$Pred))
    HDCM <- rbind(HDCM, gather(
      data = da,      
      key = "ID",      
      value="PM25.Pred" ,    
      -DATE_TIME 
    ))  
  }
  HDCM$ID <- as.numeric(HDCM$ID)
  
  index.1 <- which(colnames(Site) %in% c("ID", "LON", "LAT", "CITY"))
  index.2 <- which(colnames(CMAQ) %in% c("LON", "LAT", "DATE_TIME", "REAL_PM25"))
  
  HDCM$ID <- as.numeric(HDCM$ID)
 
  HDCM <- HDCM %>% left_join(Site[,index.1], by = c("ID")) %>%
    left_join(CMAQ[, index.2], by = c("LON", "LAT", "DATE_TIME"))
  HDCM$ID <- NULL
  HDCM$Errors <- HDCM$PM25.Pred - HDCM$REAL_PM25
  HDCM$Model <- "HDCM"
  setDT(HDCM)
  range(HDCM$Errors, na.rm = T)
  
  ######################################################################
  #  HDCM2
  ######################################################################
  HDCM2 <- NULL
  for (r in 1:13) {
    cv.HDCM2[[r]]$Pred <- cv.HDCM2[[r]]$Predict.Yts 
    da <- as.data.frame(cv.HDCM2[[r]]$Pred)
    da$DATE_TIME <- as.Date(rownames(cv.HDCM2[[r]]$Pred))
    HDCM2 <- rbind(HDCM2, gather(
      data = da,      
      key = "ID",      
      value="PM25.Pred" ,    
      -DATE_TIME 
    ))  
  }
  HDCM2$ID <- as.numeric(HDCM2$ID)
  
  index.1 <- which(colnames(Site) %in% c("ID", "LON", "LAT", "CITY"))
  index.2 <- which(colnames(CMAQ) %in% c("LON", "LAT", "DATE_TIME", "REAL_PM25"))
  
  HDCM2$ID <- as.numeric(HDCM2$ID)
  
  HDCM2 <- HDCM2 %>% left_join(Site[,index.1], by = c("ID")) %>%
    left_join(CMAQ[, index.2], by = c("LON", "LAT", "DATE_TIME"))
  HDCM2$ID <- NULL
  HDCM2$Errors <- HDCM2$PM25.Pred - HDCM2$REAL_PM25
  HDCM2$Model <- "HDCM2"
  setDT(HDCM2)
  range(HDCM2$Errors, na.rm = T)
  
  ######################################################################
  #  CMAQ
  ######################################################################
  CMAQ <- copy(obs_PM25_2015w)
  setnames(CMAQ, "sim_CMAQ_PM25", "PM25.Pred")
  setDF(CMAQ)
  index <- which(colnames(CMAQ) %in% colnames(HDCM)[1:6])
  CMAQ <- CMAQ[, index]
  
  CMAQ$Errors <- CMAQ$PM25.Pred - CMAQ$REAL_PM25
  CMAQ$Model <- "CMAQ"
  setDT(CMAQ)
  
  range(CMAQ$Errors, na.rm = T)
  
  ######################################################################
  #  UK
  ######################################################################
  table.oracle <- "./Result/pred_UKx_w_cv.xlsx"
  UK <- as.data.table(as.data.frame(readxl::read_excel(path = table.oracle)))
  colnames(UK)
  setDF(UK)
  index <- which(colnames(UK) %in% colnames(HDCM)[1:6])
  UK <- UK[, index]
  
  UK$Errors <- UK$PM25.Pred - UK$REAL_PM25
  UK$Model <- "UK"
  setDT(UK)
  
  range(UK$Errors, na.rm = T)
  
  
  ######################################################################
  #  RF
  ######################################################################
  table.oracle <- "./Result/pred_RFw_cv.xlsx"
  RF <- as.data.table(as.data.frame(readxl::read_excel(path = table.oracle)))
  setDF(RF)
  index <- which(colnames(RF) %in% colnames(HDCM)[1:6])
  RF <- RF[, index]
  
  RF$Errors <- RF$PM25.Pred - RF$REAL_PM25
  RF$Model <- "RF"
  setDT(RF)
  
  range(RF$Errors, na.rm = T)
  
  ######################################################################
  #  SVC
  ######################################################################
  table.oracle <- "./Result/pred_SVCw_cv.xlsx"
  SVC <- as.data.table(as.data.frame(readxl::read_excel(path = table.oracle)))
  setDF(SVC)
  index <- which(colnames(SVC) %in% colnames(HDCM)[1:6])
  SVC <- SVC[, index]
  
  SVC$Errors <- SVC$PM25.Pred - SVC$REAL_PM25
  SVC$Model <- "SVC"
  setDT(SVC)
  range(SVC$Errors, na.rm = T)
  
  
  ######################################################################
  #  STVC
  ######################################################################
  table.oracle <- "./Result/pred_STVCxz_w_cv.xlsx"
  STVC <- as.data.table(as.data.frame(readxl::read_excel(path = table.oracle)))
  colnames(STVC)
  setDF(STVC)
  index <- which(colnames(STVC) %in% colnames(HDCM)[1:6])
  STVC <- STVC[, index]
  
  STVC$Errors <- STVC$PM25.Pred - STVC$REAL_PM25
  STVC$Model <- "STVC"
  setDT(STVC)
  range(STVC$Errors, na.rm = T)
  
  
  ######################################################################
  #  STAR
  ######################################################################
  table.oracle <- "./Result/pred_INLAw_cv.xlsx"
  STAR <- as.data.table(as.data.frame(readxl::read_excel(path = table.oracle)))

  setDF(STAR)
  index <- which(colnames(STAR) %in% colnames(HDCM)[1:6])
  STAR <- STAR[, index]
  
  STAR$Errors <- STAR$PM25.Pred - STAR$REAL_PM25
  STAR$Model <- "STAR"
  setDT(STAR)
  range(STAR$Errors, na.rm = T)
  
  CMAQ$DATE_TIME <- as.Date(CMAQ$DATE_TIME)
  UK$DATE_TIME <- as.Date(UK$DATE_TIME)
  RF$DATE_TIME <- as.Date(RF$DATE_TIME)
  SVC$DATE_TIME <- as.Date(SVC$DATE_TIME)
  STVC$DATE_TIME <- as.Date(STVC$DATE_TIME)
  STAR$DATE_TIME <- as.Date(STAR$DATE_TIME)
  HDCM$DATE_TIME <- as.Date(HDCM$DATE_TIME)
  HDCM2$DATE_TIME <- as.Date(HDCM2$DATE_TIME)
  ######################################################################
  ######################################################################
  da <- rbind(CMAQ, UK, RF, SVC, STVC, STAR, HDCM, HDCM2)
  thres <- 200
  da <- da[!is.na(da$Errors), ]
  da <- da[abs(da$Errors) %>% between(0, thres), ]
  ######################################################################
  #  plot
  ######################################################################
  da$Model <- ordered(da$Model, levels = c("CMAQ", "UK", "RF",
                                           "SVC", 
                                           "STVC", "STAR", 
                                           "HDCM", "HDCM2"))
  p <- ggplot(data = da[da$Model %in%
                          c("CMAQ", "UK", "RF",
                            "SVC", 
                            "STVC", "STAR", 
                            "HDCM", "HDCM2"),], aes(colour = Model, 
                                                        group = Model, 
                                                        fill = Model)) +
    geom_density(aes(Errors), alpha = 0.2, adjust = 3, size = 1)
  # facet_wrap(~ LAT_label, ncol = 4
  #            , labeller = labeller(LAT_label = Label)
  # )    #facet_grid
  p0 <- ggplot_build(p)  
  winter.Residual <- p0$data[[1]]
  lab <-  unique(da$Model)
  winter.Residual$Model = if_else(winter.Residual$group==1, lab[1],
                                  if_else(winter.Residual$group==2, lab[2],
if_else(winter.Residual$group==3, lab[3],   
      if_else(winter.Residual$group==4, lab[4],
              if_else(winter.Residual$group==5, lab[5], 
                      if_else(winter.Residual$group==6, lab[6],
                              if_else(winter.Residual$group==7, lab[7],lab[8])))))))
  winter.Residual$Model <- ordered(winter.Residual$Model, levels = c("CMAQ", "UK", "RF", "SVC", 
                                                                     "STVC", "STAR", 
                                                                     "HDCM", "HDCM2"))
}

label <-  c("CMAQ", #"UK", "RF",
            "SVC", 
            "STVC", "STAR", 
            "HDCM",  TeX("HDCM$_2$"))
thres = 200
prob = 0.0145
winter.Residual <- winter.Residual[winter.Residual$Model %in% c("CMAQ", "SVC", 
                                                                "STVC", "STAR", 
                                                                "HDCM", "HDCM2"),
]

winter.Residual$Season = "Winter of 2015"
da <- rbind(winter.Residual)
# label <- c("CMAQ", "UK", "RF", "SVC", "HDCM")
pdf(file = "./figure/Fig5.pdf", width = 18, height = 12)
{
  ggplot(data = da, aes(x = x, y = density)) +
    # geom_point(aes(shape = Model), size = 2) +
    geom_line(aes(linetype = Model, col = Model), size = 2.2) +
    geom_hline(yintercept = 0.0, col = "gray80", size = 0.8) +
    # facet_wrap(~ Season, ncol = 1) +
    # scale_shape_manual(name = '', values =  c('', '*', '', '', '', ''), labels = label) +
    scale_linetype_manual(name = '', values=  c("longdash", "dotdash",
                                                "dotted", "dashed",
                                                "solid", "twodash"),
                          labels = label) +
    scale_color_manual(name = '', values =  c("red", "orange", "#4a1486",
                                              "#31a354", "black", "blue"),
                       labels = label)+
    geom_vline(xintercept = 0, col = "gray80", size = 0.8) +
    theme_bw() + ylim(c(0, prob)) + #xlim(-150, 150) +
    scale_x_continuous(limits=c(-thres, thres), breaks = sort(c(0, seq(-thres, thres, , 6))),
                       labels = sort(c(0, seq(-thres, thres, , 6)))) +
    #xlim(-thres, thres) +
    # geom_text(aes(x = -thres,#min(summer.Residual$x),
    #               y = prob,#max(summer.Residual$density)
    #               , label =  "(b)"),
    #           # family = c("sans"),
    #           fontface = 'bold',
    #           color = "black", size = 10) +
    labs(x = TeX("Prediction error (μg/$m^3$)"),
         y = "Density") +
    theme(axis.text = element_text(size = 40, colour = "black")
          # ,axis.text.x = element_text(hjust = 0.25, size = 35, colour = "black")
          , axis.title = element_text(size = 40, colour = "black")
          , legend.title = element_text(size = 30, colour = "black")
          , legend.text = element_text(size = 35, colour = "black")
          # , legend.title = element_blank()
          , legend.background = element_rect(colour = 'transparent'
                                             , fill = 'transparent')
          , legend.key.width = unit(7,"line")
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , legend.position =  c(0.78, 0.88)#"top"#
          # , legend.margin = margin(t = -0.1, unit='cm')
          , strip.text =  element_text(size = 30, colour = "black")) +
    guides(linetype = guide_legend(override.aes = list(size = 2.2),
                                   nrow = 3, byrow = TRUE))
}
dev.off()

















# pdf(file = "./figure/Fig4_predict_bias.pdf", width = 20, height = 8)
# {
#   p2 <- ggplot(data = winter.Residual, aes(x = x, y = density)) + 
#     # geom_point(aes(shape = Model), size = 5) +
#     geom_line(aes(linetype = Model, col = Model), size = 1.5) +
#     # scale_shape_manual(name = '', values =  c('*', '+'), labels = label) + 
#     scale_linetype_manual(name = '', values=  c("longdash", "dotdash",
#                                                 "dotted", "dashed",
#                                                 "solid", "twodash"),
#                           labels = label) +
#     scale_color_manual(name = '', values =  c("red", "blue", "#4a1486",
#                                               "#31a354", "black", "black"), 
#                        labels = label)+
#     geom_vline(xintercept = 0, col = "gray80", size = 0.8) +
#     theme_bw() + ylim(c(0, prob)) + xlim(-thres, thres) +
#     geom_text(aes(x = -thres,#min(summer.Residual$x), 
#                   y = prob,#max(summer.Residual$density)
#                   , label =  "(b)"),
#               # family = c("sans"),
#               fontface = 'bold',
#               color = "black", size = 10) +
#     labs(x = TeX("Error (μg/m^3)"),
#          y = "Density") +
#     theme(axis.text = element_text(size = 20, colour = "black")
#           ,axis.text.x = element_text(hjust = 0.25, size = 20, colour = "black") 
#           , axis.title = element_text(size = 22, colour = "black")
#           , legend.title = element_text(size = 20, colour = "black")
#           , legend.text = element_text(size = 20, colour = "black")
#           # , legend.title = element_blank()
#           , legend.background = element_rect(colour = 'transparent'
#                                              , fill = 'transparent')
#           , legend.key.width = unit(5,"line")
#           , panel.grid.major = element_blank()
#           , panel.grid.minor = element_blank()
#           , legend.position = c(0.25, 0.8)
#           # , legend.margin = margin(t = -0.1, unit='cm')
#           , strip.text =  element_text(size = 16, colour = "black")) +
#     guides(linetype = guide_legend(override.aes = list(size = 1.5),
#                                    nrow = 3, byrow = TRUE))
#   
#   
# }
# p2
# # p <- ggpubr::ggarrange(p1, p2, nrow = 1, ncol = 2)
# # p
# dev.off()

# summer.Residual$Season = "Summer of 2015"

















# pdf(file = "./figure/Fig4_predict_bias.pdf", width = 20, height = 8)
# {
#   p2 <- ggplot(data = winter.Residual, aes(x = x, y = density)) + 
#     # geom_point(aes(shape = Model), size = 5) +
#     geom_line(aes(linetype = Model, col = Model), size = 1.5) +
#     # scale_shape_manual(name = '', values =  c('*', '+'), labels = label) + 
#     scale_linetype_manual(name = '', values=  c("longdash", "dotdash",
#                                                 "dotted", "dashed",
#                                                 "solid", "twodash"),
#                           labels = label) +
#     scale_color_manual(name = '', values =  c("red", "blue", "#4a1486",
#                                               "#31a354", "black", "black"), 
#                        labels = label)+
#     geom_vline(xintercept = 0, col = "gray80", size = 0.8) +
#     theme_bw() + ylim(c(0, prob)) + xlim(-thres, thres) +
#     geom_text(aes(x = -thres,#min(summer.Residual$x), 
#                   y = prob,#max(summer.Residual$density)
#                   , label =  "(b)"),
#               # family = c("sans"),
#               fontface = 'bold',
#               color = "black", size = 10) +
#     labs(x = TeX("Error (μg/m^3)"),
#          y = "Density") +
#     theme(axis.text = element_text(size = 20, colour = "black")
#           ,axis.text.x = element_text(hjust = 0.25, size = 20, colour = "black") 
#           , axis.title = element_text(size = 22, colour = "black")
#           , legend.title = element_text(size = 20, colour = "black")
#           , legend.text = element_text(size = 20, colour = "black")
#           # , legend.title = element_blank()
#           , legend.background = element_rect(colour = 'transparent'
#                                              , fill = 'transparent')
#           , legend.key.width = unit(5,"line")
#           , panel.grid.major = element_blank()
#           , panel.grid.minor = element_blank()
#           , legend.position = c(0.25, 0.8)
#           # , legend.margin = margin(t = -0.1, unit='cm')
#           , strip.text =  element_text(size = 16, colour = "black")) +
#     guides(linetype = guide_legend(override.aes = list(size = 1.5),
#                                    nrow = 3, byrow = TRUE))
#   
#   
# }
# p2
# # p <- ggpubr::ggarrange(p1, p2, nrow = 1, ncol = 2)
# # p
# dev.off()

# summer.Residual$Season = "Summer of 2015"

