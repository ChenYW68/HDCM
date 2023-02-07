source("./R/util.R")

  load(paste0("./Result/WH_1_23_3.RData"))
  cv.HDCM <- CVw_BTH
  load(paste0("./Result/WH2_2_23_3.RData"))
  cv.HDCM2 <- CVw_BTH

  data("SiteData", package = "ADCM")
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


  City <- unique(HDCM$CITY)[7]

  HDCM <- HDCM[CITY == City,]
  HDCM2 <- HDCM2[CITY == City,]
  CMAQ <- CMAQ[CITY == City,]

  setDF(HDCM)
  setDF(HDCM2)
  setDF(CMAQ)
  ######################################################################
  pred <- HDCM[, "PM25.Pred"]
  obs <- HDCM[, "REAL_PM25"]
  # conditional.quantile(frcst, obs, main = "Sample Conditional Quantile Plot")
  Con.HDCM.Ens <- Cond.quantile(pred, obs,
                                city =  City,
                                model = "HDCM")

  pred <- HDCM2[, "PM25.Pred"]
  obs <- HDCM2[, "REAL_PM25"]
  # conditional.quantile(frcst, obs, main = "Sample Conditional Quantile Plot")
  Con.HDCM2.Ens <- Cond.quantile(pred, obs,
                                city =  City,
                                model = "HDCM2")


  pred <- CMAQ[, "PM25.Pred"]
  obs <- CMAQ[, "REAL_PM25"]
  # conditional.quantile(pred, obs, main = "Sample Conditional Quantile Plot")
  Con.CMAQ_PM25 <- Cond.quantile(pred, obs,
                                 city =  City,
                                 model = "CMAQ")


  # da1 <- rbind(Con.CMAQ_PM25$quan.data,
  #              # Con.HDCM1.Ens$quan.data,
  #              Con.HDCM.Ens$quan.data,
  #              # Con.SVC1$quan.data,
  #              Con.SVC$quan.data)
  # da2 <- rbind(Con.CMAQ_PM25$hist.data,
  #              Con.HDCM.Ens$hist.data,
  #              Con.SVC$hist.data)


  ######################################################################
  #                               plot
  ######################################################################
  library(ggsci)
  # load(paste0(file, CITY.Name, "_", month[1], "_cq.RData"))
  UP <- 430#max(da1$x, da1$y, na.rm = T) + 80
  da1 <- rbind(Con.CMAQ_PM25$quan.data,
               Con.HDCM.Ens$quan.data,
               Con.HDCM2.Ens$quan.data
               # Con.SVC1$quan.data,
               # Con.SVC$quan.data
  )
  label <- as.character(unique(da1$group))
  da1$group <- ordered(da1$group,
                       levels = label)
  # da1$method <- ifelse(da2$model%in% "SVC1", "1",
  #                     ifelse(da2$model%in% "SVC2", "3",
  #                            ifelse(da2$model%in% "HDCM1.Ens", "2",
  #                                   ifelse(da2$model%in% "HDCM2.Ens", "4", "0"))))
  da1$method <- ifelse(da1$Model%in% "CMAQ", 0,
                       ifelse(da1$Model%in% "HDCM", 1, 2))
  # da1$Model <- ifelse(da1$Model %in% "HDCM.Ens", "HDCM",
  #                     ifelse(da1$Model %in% "SVC", "SVC",
  #                            ifelse(da1$Model %in% "CMAQ", "CMAQ", "Observation")))

  da1$Model <- ordered(da1$Model, levels = c("CMAQ", "HDCM", "HDCM2"))

  da1$Model <- factor(da1$Model, levels =  c("CMAQ", "HDCM", "HDCM2"),
                       labels = c("CMAQ", "HDCM", TeX("HDCM$_2$")))
  # Label <- as_labeller(c(`CMAQ` = "CMAQ" , `HDCM` = "HDCM", `HDCM2` = TeX("HDCM$_2$")))
  # Label <- as_labeller(c(`0` = "CMAQ" , `1` = "SVC1"
  #                        , `2` = "HDCM1"
  #                        , `3` = "SVC2", `4` = "HDCM2"))

  size <- c(25, 23)
  ls = c(1.2, 0.8, 1.2)
  {
    p <- ggplot() +
      geom_line(da1, mapping =aes(x = x, y = y,
                                  col = group,
                                  group = flag,
                                  linetype = group,
                                  size = group))+
      geom_abline(intercept = 0, slope = 1, col = "gray", size = 0.5) +
      # geom_histogram(da2, binwidth = 70
      #                , position = "identity"
      #                ,mapping = aes(z, y=(..count..)/5
      #                )) +
      facet_wrap(~ Model, ncol = 3, labeller=label_parsed) +
      # facet_grid( ~ method, scales = "free"
      #            , labeller = labeller(method = Label)
      # )+
      scale_x_continuous(limits = c(0, UP),
                         expand = c(0, 0),
                         breaks  = seq(0, UP, 1.0e2),
                         labels = seq(0, UP, 1.0e2)) +
      scale_y_continuous(name = "",
                         limits = c(0, UP),
                         expand = c(0, 0),
                         breaks  = c(8),
                         position = "right",
                         labels = ""
                         #   function(y)
                         # { paste0(round(y*50, 0), "")
                         #   }
                         , sec.axis = sec_axis(~.,
                                               name = TeX("Observed PM$_{2.5}$ $( μg/m^3 )$"),
                                               breaks  = seq(0, UP, 1.0e2),
                                               labels = seq(0, UP, 1.0e2)
                                               # function(b) {
                                               #paste0(round(b,0), "")
                                               # }
                         )
      )  + scale_linetype_manual(name='',
                                 values=c("solid", "longdash", "dotted"),
                                 labels = label) +
      scale_size_manual(name='',
                        values = ls, labels = label) +
      scale_colour_manual(name='',
                          values = c("black", "black" , "black"), #, "#556B2F" , "blue"
                          labels = label)+
      theme_bw() +
      labs(x = TeX("Predicted PM$_{2.5}$ $( μg/m^3 )$"),
           y.left = TeX("Observed PM$_{2.5}$ $( μg/m^3 )$")) +
      theme(axis.text = element_text(size = size[2], colour = "black")
            , axis.text.x  = element_text(angle = 0)
            , axis.title = element_text(size = size[1], colour = "black")
            # , axis.title.y = element_text(size = size[1], colour = "black")
            , legend.title = element_text(size = size[1], colour = "black")
            , legend.text = element_text(size = size[2], colour = "black")
            # , legend.title = element_blank()
            , legend.background = element_rect(fill="transparent")
            , legend.key.width = unit(4.5,"line")
            , panel.grid.major = element_blank()
            , panel.grid.minor = element_blank()
            , legend.position = "top"#c(0.6, 0.1)
            , legend.margin = ggplot2::margin(t = 1, unit='cm')
            , axis.ticks.length.y.right = unit(-0, "cm")
            , strip.text =  element_text(size = size[2], colour = "black")
            # , axis.text.y.right  = element_text(vjust = -2,
            #                                     hjust = -300,
            #                              margin = margin(l = 15, r = 2))
      )  +  guides(linetype = guide_legend(override.aes =
                                             list(size = ls), nrow=1, byrow=TRUE))
  }

#---------------------------------------------------------------------------
# Fig5-Baoding:  Cond.quantile
# ######################################################################
# hdcm_tab <- c("HDCM", "W", CITY.Name)
# svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
# month = c(201511, 201512, 201601)
# p2 <- CondQuantile_Baoding_w(file = file, hdcm_tab = hdcm_tab,
#                              svc_tab = svc_tab, month = month,
#                              day = day, seed = 1234)
ggsave(plot = p, file = "./figure/Fig6.pdf",
       width  = 16, height = 7)
