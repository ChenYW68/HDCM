# save(Site, W_ts, Wts.test.Pred.mcmc, Wts.train.Pred.mcmc,
#      file = "./data/train_mcmc_ks.RData")
source("./R/PSTVB_Packages.R")
load("./data/bth_map.RData")
load("./Simulation/data/train_mcmc_ks.RData")
load("./Simulation/data/train_VB_EnKS.RData")
# Site
# W_ts
# Wts.test.Pred.mcmc
# Wts.train.Pred.mcmc

Wts.Data.MCMC <- Wts.Data.VB <- Wts.true <- Wts.sample <-NULL
for(t in 1:20){
  Wts.true <- rbind(Wts.true, data.frame(time = t, ID = Site$ID,
                                         Wts = as.vector(W_ts[t,])))

  ID <- Site[Site$Simu == "Train", 1]
  Wts.sample <- rbind(Wts.sample, data.frame(time = t, ID = ID,
                                             Wts = as.vector(W_ts[t, ID])))
  # MCMC-KS
  Wts.Data.MCMC <- rbind(Wts.Data.MCMC, data.frame(time = t, ID = as.numeric(dimnames(Wts.train.Pred.mcmc)[[3]]),
                                                   Wts = as.vector(Wts.train.Pred.mcmc[2, t, ])))

  Wts.Data.MCMC <- rbind(Wts.Data.MCMC, data.frame(time = t, ID = as.numeric(dimnames(Wts.test.Pred.mcmc)[[3]]),
                                                   Wts = as.vector(Wts.test.Pred.mcmc[2, t,])))

  # VB-EnKS

  Wts.Data.VB <- rbind(Wts.Data.VB, data.frame(time = t, ID = as.numeric(dimnames(Wts.train.Pred.vb)[[3]]),
                                               Wts = as.vector(Wts.train.Pred.vb[2, t, ])))

  Wts.Data.VB <- rbind(Wts.Data.VB, data.frame(time = t, ID = as.numeric(dimnames(Wts.test.Pred.vb)[[3]]),
                                               Wts = as.vector(Wts.test.Pred.vb[2, t,])))

}
Wts.true$Method <- 1
Wts.sample$Method <- 2
Wts.Data.MCMC$Method <- 3
Wts.Data.VB$Method <- 4




source("./R/util.R")
da <- setDF(rbind(Wts.true, Wts.sample, Wts.Data.MCMC, Wts.Data.VB)) %>% left_join(Site, by = "ID")
da$time_label <- paste0("time = ", da$time)
tt <- 1
# library(jcolors)

Map_BTH <- fortify(bth_map)
# p1 <- plot.sim(da[between(da$time, tt, tt + 5), ], var = "Wts",
#                size_point = c(2.0, 0.8), col_point = "blue",
#                size = 1.5, font.size = c(20, 14.5), pch = 20,
#                GeoMap = Map_BTH)
# ggsave(plot = p1, file = paste0("./figure/FigS7.pdf"),
#        width = 14, height = 10)


color.count <- nrow(unique(da[, c("LON", "LAT")]))
m <- c(floor(min(da[, "Wts"])), ceil(max(da[, "Wts"])))


p <- plot.sim(da[(between(da$time, tt, tt + 5)), ],
              var = "Wts",
              size_point = c(2.0, 0.8), col_point = "blue",
              size = 1.5, font.size = c(20, 18), pch = 20,
              GeoMap = Map_BTH, Geo_Color = 'gray80',
              color.count = color.count, m = m)
ggsave(plot = p, file = paste0("./figure/FigS7.pdf"),
       width = 14, height = 14)

p1 <- plot.sim(da[(between(da$time, tt, tt + 5))&(da$Method == 1), ],
               var = "Wts",
               size_point = c(2.0, 0.8), col_point = "blue",
               size = 1.5, font.size = c(20, 18), pch = 20,
               GeoMap = Map_BTH, Geo_Color = 'gray80',
               color.count = color.count, m = m)
ggsave(plot = p1, file = paste0("./figure/FigS7_a.pdf"),
       width = 16, height = 3)
# #
# #
da$Method <- ifelse(da$Method == 4, 5, da$Method)
p2 <- plot.sim(da[(between(da$time, tt, tt + 5))&(da$Method %in% c(2, 3, 5)), ],
               var = "Wts",
               size_point = c(2.0, 0.8), col_point = "blue",
               size = 1.5, font.size = c(20, 16), pch = 20,
               legend = T,
               GeoMap = Map_BTH, Geo_Color = 'gray80',
               color.count = color.count, m = m)
ggsave(plot = p2, file = paste0("./figure/FigS7_b.pdf"),
       width = 16, height = 8.5)
