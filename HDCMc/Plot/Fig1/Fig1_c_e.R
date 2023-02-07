rm(list=ls())
source("./R/PSTVB_Packages.R")
load("./data/SiteData.RData")

CITY.Name <- c("Zhangjiakou")
month <- c(201511, 201512, 201601)
Da1 <- obs_PM25_2015w %>%
  filter(YEAR_MONTH %in% month
         , CITY %in% CITY.Name
  ) %>%
  setorder(DATE_TIME, ID)

CITY.Name <- c("Beijing", "Hengshui")
month <- c(201506, 201507, 201508)
Da2 <- obs_PM25_2015s %>%
  filter(YEAR_MONTH %in% month
         , CITY %in% CITY.Name
  ) %>%
  setorder(DATE_TIME, ID)
Da1$Season <- 1
Da2$Season <- 2
PM25_CMAQ <- rbind(Da1)

PM25_CMAQ$FLAG = if_else(PM25_CMAQ$YEAR_MONTH %in% c(201511, 201512, 201601)
                         , "winter of 2015", "summer of 2015")
setDT(PM25_CMAQ)
PM25_CMAQ <- PM25_CMAQ[!is.na(PM25_CMAQ$REAL_PM25)]
da <- PM25_CMAQ %>%
  plyr::ddply(.(CITY, FLAG)
              , dplyr::summarize
              , Corr = round(mean(cor(sim_CMAQ_PM25, REAL_PM25)), 3)
              , LAT_label = round(mean(LAT), 2)
  )
Da <- PM25_CMAQ %>% left_join(da, by = c("CITY", "FLAG"))
setDT(Da)




Up <- max(Da$sim_CMAQ_PM25, Da$REAL_PM25, na.rm = T) + 20
Low <- 50

time <- as_labeller(c(`1` = "11/01/15 to 01/31/16",
                      `2` = "06/01/15 to 08/31/15"))


Da$type <- ifelse(Da$CITY %in% "Zhangjiakou", 1,
                  ifelse(Da$CITY %in% "Beijing", 2, 3))
Label <- as_labeller(c(`1` = "Zhangjiakou", `2` = "Beijing"
                       ,`3` = "Hengshui"))

size <- c(23, 20, 20)
p1 <- ggplot(data = Da) +
  facet_grid(type + Season~., scales = c("fixed")
             , labeller = labeller(Season = time, type = Label)) +   #facet_grid
  theme_bw()  +
  geom_abline(slope = 1, color = "gray", size = 1) +
  geom_abline(slope = 2, color = "gray",
              size = 0.8) +
  geom_abline(slope = 0.5, color = "gray",
              size = 0.8) +
  geom_point(aes(x= (REAL_PM25), y = (sim_CMAQ_PM25)),
             size = 1, col = "black") +
  scale_x_continuous(limits = c(0, Up)
                     , expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0, Up)
                     , expand = c(0, 0)
  ) +
  geom_text(y = 20,
            aes(x = 250, label = paste0("Corr = ", Corr)),
            size = 7, label.size = 18) +
  geom_text(x = (Up - Low)*1.23/2.10,
            y = Up*0.90,
            angle = 60,
            label = "k = 2",
            color = "gray",
            size = 6) +
  geom_text(x = (Up)*0.90,
            y = Up*0.86,
            angle = 43,
            label = "k = 1",
            size = 6) +
  geom_text(x = (Up)*0.88,
            y = Up*0.48,
            angle = 25,
            label = "k = 0.5",
            color = "gray",
            size = 6) +
  labs(x = "",y = "") +
  theme(axis.text = element_text(size = size[2], colour = "black")
        , axis.title.y = element_text(hjust = 0.5)
        , axis.title= element_text(size = size[1], colour = "black")
        # , legend.title = element_text(size = 10, colour = "black")
        , legend.text= element_text(size = size[3], colour = "black")
        , legend.title = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        # , legend.position="top"
        , legend.position = "none"
        , legend.key.width = unit(1,"line")
        , legend.key.height = unit(2,"line")
        , strip.text =  element_text(size = 20, colour = "black")
  )
ggsave(plot = p1, file = './figure/Fig1_c.png', dpi = 300, width  = 6, height = 5)



CITY.Name <- c("Beijing")#c("Beijing", "Hengshui")
month <- c(201506, 201507, 201508)
Da2 <- obs_PM25_2015s %>%
  filter(YEAR_MONTH %in% month
         , CITY %in% CITY.Name
  ) %>%
  setorder(DATE_TIME, ID)
Da1$Season <- 1
Da2$Season <- 2

PM25_CMAQ <- rbind(Da2)

PM25_CMAQ$FLAG = if_else(PM25_CMAQ$YEAR_MONTH %in% c(201511, 201512, 201601)
                         , "winter of 2015", "summer of 2015")
setDT(PM25_CMAQ)
PM25_CMAQ <- PM25_CMAQ[!is.na(PM25_CMAQ$REAL_PM25)]
da <- PM25_CMAQ %>%
  plyr::ddply(.(CITY, FLAG)
              , dplyr::summarize
              , Corr = round(mean(cor(sim_CMAQ_PM25, REAL_PM25)), 3)
              , LAT_label = round(mean(LAT), 2)
  )
Da <- PM25_CMAQ %>% left_join(da, by = c("CITY", "FLAG"))
setDT(Da)




Up <- max(Da$sim_CMAQ_PM25, Da$REAL_PM25, na.rm = T) + 20
Low <- 50

time <- as_labeller(c(`1` = "11/01/15 to 01/31/16",
                      `2` = "06/01/15 to 08/31/15"))


Da$type <- ifelse(Da$CITY %in% "Zhangjiakou", 1,
                  ifelse(Da$CITY %in% "Beijing", 2, 3))
Label <- as_labeller(c(`1` = "Zhangjiakou", `2` = "Beijing"
                       ,`3` = "Hengshui"))
p2 <- ggplot(data = Da) +
  facet_grid(type + Season~., scales = c("fixed")
             , labeller = labeller(Season = time, type = Label)) +   #facet_grid
  theme_bw()  +
  geom_abline(slope = 1, color = "gray", size = 1) +
  geom_abline(slope = 2, color = "gray",
              size = 0.8) +
  geom_abline(slope = 0.5, color = "gray",
              size = 0.8) +
  geom_point(aes(x= (REAL_PM25), y = (sim_CMAQ_PM25)),
             size = 1, col = "black") +
  scale_x_continuous(limits = c(0, Up)
                     , expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0, Up)
                     , expand = c(0, 0)
  ) +
  geom_text(y = 220,
            aes(x = 50, label = paste0("Corr = ", Corr)),
            size = 7, label.size = 18) +
  geom_text(x = (Up - Low)*1.3/2.10,
            y = Up*0.90,
            angle = 63,
            label = "k = 2",
            color = "gray",
            size = 6) +
  geom_text(x = (Up)*0.90,
            y = Up*0.86,
            angle = 43,
            label = "k = 1",
            size = 6) +
  geom_text(x = (Up)*0.88,
            y = Up*0.48,
            angle = 25,
            label = "k = 0.5",
            color = "gray",
            size = 6) +
  labs(x = "",
       y = TeX("CMAQ output PM$_{2.5}$ $( μg/m^3 )$")) +
  # labs(x = TeX("Observed PM$_{2.5}$ $( μg/m^3 )$"),
  #      y = TeX("CMAQ output PM$_{2.5}$ $( μg/m^3 )$")) +
  theme(axis.text = element_text(size = size[2], colour = "black")
        , axis.title.y = element_text(hjust = 0.5)
        , axis.title= element_text(size = size[1], colour = "black")
        # , legend.title = element_text(size = 10, colour = "black")
        , legend.text= element_text(size = size[3], colour = "black")
        , legend.title = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        # , legend.position="top"
        , legend.position = "none"
        , legend.key.width = unit(1,"line")
        , legend.key.height = unit(2,"line")
        , strip.text =  element_text(size = 20, colour = "black")
  )
ggsave(plot = p2, file = './figure/Fig1_d.png', dpi = 300, width  = 6, height = 5)


CITY.Name <- c("Hengshui")#c("Beijing", "Hengshui")
month <- c(201506, 201507, 201508)
Da2 <- obs_PM25_2015s %>%
  filter(YEAR_MONTH %in% month
         , CITY %in% CITY.Name
  ) %>%
  setorder(DATE_TIME, ID)
Da1$Season <- 1
Da2$Season <- 2

PM25_CMAQ <- rbind(Da2)

PM25_CMAQ$FLAG = if_else(PM25_CMAQ$YEAR_MONTH %in% c(201511, 201512, 201601)
                         , "winter of 2015", "summer of 2015")
setDT(PM25_CMAQ)
PM25_CMAQ <- PM25_CMAQ[!is.na(PM25_CMAQ$REAL_PM25)]
da <- PM25_CMAQ %>%
  plyr::ddply(.(CITY, FLAG)
              , dplyr::summarize
              , Corr = round(mean(cor(sim_CMAQ_PM25, REAL_PM25)), 3)
              , LAT_label = round(mean(LAT), 2)
  )
Da <- PM25_CMAQ %>% left_join(da, by = c("CITY", "FLAG"))
setDT(Da)




Up <- max(Da$sim_CMAQ_PM25, Da$REAL_PM25, na.rm = T) + 20
Low <- 50

time <- as_labeller(c(`1` = "11/01/15 to 01/31/16",
                      `2` = "06/01/15 to 08/31/15"))


Da$type <- ifelse(Da$CITY %in% "Zhangjiakou", 1,
                  ifelse(Da$CITY %in% "Beijing", 2, 3))
Label <- as_labeller(c(`1` = "Zhangjiakou", `2` = "Beijing"
                       ,`3` = "Hengshui"))
p3 <- ggplot(data = Da) +
  facet_grid(type + Season~., scales = c("fixed")
             , labeller = labeller(Season = time, type = Label)) +   #facet_grid
  theme_bw()  +
  geom_abline(slope = 1, color = "gray", size = 1) +
  geom_abline(slope = 2, color = "gray",
              size = 0.8) +
  geom_abline(slope = 0.5, color = "gray",
              size = 0.8) +
  geom_point(aes(x= (REAL_PM25), y = (sim_CMAQ_PM25)),
             size = 1, col = "black") +
  scale_x_continuous(limits = c(0, Up)
                     , expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0, Up)
                     , expand = c(0, 0)
  ) +
  geom_text(y = 190,
            aes(x = 40, label = paste0("Corr = ", Corr)),
            size = 7, label.size = 18) +
  geom_text(x = (Up - Low)*1.35/2.10,
            y = Up*0.90,
            angle = 60,
            label = "k = 2",
            color = "gray",
            size = 6) +
  geom_text(x = (Up)*0.90,
            y = Up*0.86,
            angle = 43,
            label = "k = 1",
            size = 6) +
  geom_text(x = (Up)*0.88,
            y = Up*0.48,
            angle = 25,
            label = "k = 0.5",
            color = "gray",
            size = 6) +
  labs(x = TeX("Observed PM$_{2.5}$ $( μg/m^3 )$"),
       y = "") +
  theme(axis.text = element_text(size = size[2], colour = "black")
        , axis.title.y = element_text(hjust = 0.5)
        , axis.title= element_text(size = size[1], colour = "black")
        # , legend.title = element_text(size = 10, colour = "black")
        , legend.text= element_text(size = size[3], colour = "black")
        , legend.title = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        # , legend.position="top"
        , legend.position = "none"
        , legend.key.width = unit(1,"line")
        , legend.key.height = unit(2,"line")
        , strip.text =  element_text(size = 20, colour = "black")
  )
ggsave(plot = p3, file = './figure/Fig1_e.png', dpi = 300, width  = 6, height = 5)

# pdf(file = "./figure/Fig1.png", width = 8, height = 16)
# p <- ggpubr::ggarrange(p1, p2, p3, nrow = 3, ncol = 1)
# p
# dev.off()
