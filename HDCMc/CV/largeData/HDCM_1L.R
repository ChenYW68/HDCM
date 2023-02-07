rm(list=ls())
source("./R/PSTVB_Packages.R")
source("./R/CreateGrid.R")
# source("./R/CreateHmatrix.R")
source("./R/Construct_TestTrain_Data.R")
source("./R/HDCM.R")
source("./R/VB_EnKS.R")
source("./R/VB.R")
source("./R/MEnKS.R")
source("./R/util.R")
# load("./data/CMAQ.RData")
# load("./data/GeoMap.RData")
load("./data/NAQPMS_CMAQ_Dataset_2015W.RData")
Site <- Site[distToNearestCAMQpoint <= 15]
set.seed(12345)


n.train <- 5000
train.id <- sample(Site$ID, n.train, replace = F)
cat("The number of training set:", length(train.id))

PM25_2015w <- NAQPMS_CMAQ_Dataset_2015W[distToNearestCAMQpoint <= 15]
Site$Flag <- ifelse(Site$ID %in% train.id, "train", "test")
PM25_2015w$Flag <- ifelse(PM25_2015w$ID %in% train.id, "train", "test")

temp <- Site[Site$Flag == "test", ]
plot(temp$LON, temp$LAT)

rm(NAQPMS_CMAQ_Dataset_2015W)
colnames(PM25_2015w)
# setnames(PM25_2015w, "PM25", "REAL_PM25")

setDF(Site);
INDX <- which(colnames(Site) %in% c("LON_X", "LAT_Y"))
Site <- ADCM::spCoords.transform(Site[, -INDX], method = 1)
setDF(PM25_2015w)
INDX <- which(colnames(PM25_2015w) %in% c("LON_X", "LAT_Y"))
PM25_2015w <- ADCM::spCoords.transform(PM25_2015w[, -INDX], method = 1)
######################################################################
#                   1. Create grid
######################################################################

load("./data/larg_bth_map.RData")
source("./R/CreateGrid.R")
Ch <- 0.05
n.grps <- 3
H.basic.data <- CreateGrid(PM25_2015w,
                           Site,
                           Map = fortify(larg_bth_map),
                           # max.edge = c(.21, .3), #0.3,0.7
                           # offset = c(1e-1, 0.9), #0.4, 0.6
                           # cutoff = 0.11,#.23, #0.1
                           max.edge = c(.35, .7), #0.3,0.7
                           offset = c(1e-1, 0.6), #0.4, 0.6
                           cutoff = .23,
                           # max.edge = c(.23, .4), #0.3,0.7
                           # offset = c(1e-1, 0.9), #0.4, 0.6
                           # cutoff = 0.3,
                           distance.scale = 1e3,
                           n.grps = n.grps,
                           col = "blue",
                           size = 1,
                           site.id = "ID",
                           factor = 1,
                           ch = Ch,
                           method = "indicator1",
                           response.label = "PM25",
                           distance.method = 1,
                           way = 2,
                           scale = 5) #indicator
H.basic.data$plot.grid
sum(H.basic.data$Grid.infor$summary$Knots.count)


ds <- max(H.basic.data$Grid.infor$summary$Hdist*0.23)
G <- H.basic.data$Grid.infor$summary$Hdist
apply(G<ds, 1, sum)/ncol(G)


# [1] 253.4651 247.3147 244.7844 236.5405 236.5972 242.4445 232.2080 238.5114
# [9] 236.6906 215.0532 223.4946 224.0213 225.3215 231.9834 230.0636 206.9150
cs <- 0.23
spTaper <- list()
for(g in 1:H.basic.data$Grid.infor$summary$res)
{
  spTaper$tuning[g] <- max(H.basic.data$Grid.infor$level[[g]]$BAUs.Dist*
                             H.basic.data$Grid.infor$level[[g]]$Max.Dist)*cs
}
print(spTaper$tuning)

# ggsave(plot = H.basic.data$plot.grid, height = 6, width = 8,
#        file = './figure/Fig_Large_Map.pdf')
H.basic.data$Grid.infor$summary$Knots.count
range(H.basic.data$Grid.infor$summary$Hdist)*0.05
# H.basic.data$Hs <- H.basic.data$Hs/rowSums(H.basic.data$Hs)
##########################################################################################
#                          3. Data for modeling
##########################################################################################
# YearMonth <- c(201511, 201512, 201601)
# PM25_2015w <- PM25_2015w %>% filter(YEAR_MONTH %in% YearMonth)
PM25_2015w <- PM25_2015w %>%
  dplyr::filter(
    # YEAR %in% year,
    between((as.Date(DATE_TIME)),
            (as.Date(paste0(2015, "-", "11-01"))),
            (as.Date(paste0(2015, "-", "11-30"))))#,
    # between(MONTH, 10, 12),
  )
colnames(PM25_2015w)
# #
# PM25_2015w[, c("sim_CMAQ_PM25", "sim_SPRESS")]=
#   sqrt(PM25_2015w[, c("sim_CMAQ_PM25", "sim_SPRESS")])

PM25_2015w[, c("sim_CMAQ_PM25")] <-
  sqrt(PM25_2015w[, c("sim_CMAQ_PM25")])

# PM25_2015w[, c("REAL_PM25")]=
#   sqrt(PM25_2015w[, c("REAL_PM25")])


DATE_TIME <- unique(PM25_2015w$DATE_TIME) %>% sort()
Nt <- length(DATE_TIME)
date.time <- data.frame(time.index = 1:Nt,
                        time.scale = seq(0, 1, , Nt),
                        time.scale.sin = sin(seq(0, 1, , Nt)/(0.03*pi)),
                        time.scale.cos = cos(seq(0, 1, , Nt)/(0.03*pi)),
                        DATE_TIME = DATE_TIME)
PM25_2015w <- PM25_2015w  %>% left_join(date.time, by = c("DATE_TIME"))

# PM25_2015w$Bias <- sqrt(PM25_2015w$REAL_PM25) - sqrt(obs_PM25_2015w$sim50_CMAQ_PM25)
# PM25_2015w$CMAQ_PM25  <- sqrt(PM25_2015w$sim50_CMAQ_PM25)
# construct datasets
# ADCM.Data <- ParYtsXts(Model_Base_Table_Update,
#                      include = list(YearMonth = YearMonth),
#                      X = c("CMAQ_PM25_30", "REAL_LON_WIND", "REAL_TEMP",
#                            "REAL_PRES", "REAL_DEWP", "REAL_LAT_WIND"))
source("./R/Construct_HDCM_Data.R")
ADCM.Data <- Construct_HDCM_Data(data = PM25_2015w,
                                 include = list(
                                   YEAR = c(2015, 2016),
                                   month_day = c("11-01", "1-31")
                                 ),
                                 Y = "PM25",#"Bias",#"REAL_PM25",
                                 X = c("sim_CMAQ_PM25"
                                       , "TEMP"
                                       , "WIND_X"
                                       , "WIND_Y"
                                       # , "time.index"
                                       # , "time.scale.sin"
                                       # , "time.scale.cos"
                                 ),
                                 standard = T,
                                 center = T,
                                 start.index = 1)
Vari <- var(sqrt(as.vector(ADCM.Data$Y_ts)))
# assign("scaled_variable", ADCM.Data$scaled_variable, envir = .GlobalEnv)
##########################################################################################
#-----------------------------------------------------------------------------------------
#                         4. Model setting
##########################################################################################
{
  ds <- 0.5*H.basic.data$Grid.infor$level[[1]]$Max.Dist
  # theta.2 <- c(1e2, 5e1, 0.5*max(H.basic.data$Grid.infor$level[[1]]$BAUs.Dist))

  theta.2 <- c(1e-1, 1E-3, 1E0)
  res.num <- H.basic.data$Grid.infor$summary$res
  # ADCM.Data$X_ts <- ADCM.Data$X_ts[-1,,]
  p1 = dim(ADCM.Data$X_ts)[1]
  #---------------------------------------------------------------------
  #                           4.1 Prior
  #---------------------------------------------------------------------
  prior <- list(
                beta = list(E_beta = rep(0, p1), sigma.sq = 1e5*diag(1, p1, p1))
              , obs.sigma.sq = list(a = 2, b = 1)
              , theta.1 = list(mu = rep(1e-3, res.num), sigma.sq =  rep(1e5, res.num))
              , theta.2 = list(a = rep(theta.2[2], res.num), b =  rep(theta.2[3], res.num))
              , zeta = list(a = rep(1e-3, res.num), b =  rep(1e0, res.num))
              , zeta0 = list(a = rep(1e-3, res.num), b =  rep(1e0, res.num))
              , proc.tau.sq = list(a = rep(2, res.num), b = rep(1, res.num))
              , proc0.tau.sq = list(a = rep(2, res.num), b = rep(1, res.num))
            )
  #---------------------------------------------------------------------
  #                        4.2 initialize  parameters
  #---------------------------------------------------------------------
  para <- list( beta = list(E_beta = c(8, 1, rep(0, p1 - 2))),
                  obs.sigma.sq = list(E_sigma.sq = 1, a = 2, b = 1)#1E-2
                , theta.1 = list(E_theta.1 = rep(1e-3, res.num))
                , theta.2 = list(E_theta.2 = rep(theta.2[1], res.num))
                , zeta = list(E_zeta = rep(1e-1, res.num))
                , zeta0 = list(E_zeta0 = rep(1e-1, res.num))
                , proc.tau.sq = list(E_tau.sq = rep(5e0, res.num))
                , proc0.tau.sq = list(E_tau.sq = rep(1E0, res.num))
              )
}
# proc.tau.sq: (10, 1); (100, 100)
##########################################################################################
#                          5. Model fitting and prediction
##########################################################################################
# ds <- min(H.basic.data$BAUs.Dist[row(H.basic.data$BAUs.Dist)!= col(H.basic.data$BAUs.Dist)])
library(profvis)
library(CovUtil)
source("./R/HDCM.R")
source("./R/VB_EnKS.R")
source("./R/util.R")
source("./R/VB.R")
source("./R/MEnKS.R")

g <- 1
Cs <- 8e-2
Cs <- 5e-2
# Cs <- 1e-3
# quantile(as.vector(H.basic.data$Grid.infor$level[[g]]$BAUs.Dist), Cs)*
#   H.basic.data$Grid.infor$level[[g]]$Max.Dist
#
# G <- as.matrix(H.basic.data$Grid.infor$level[[g]]$Adj.Mat)
# G <- ifelse(abs(G) > 0, 1, 0)
# mg <- nrow(G)
# dx <- as.vector(H.basic.data$Grid.infor$level[[g]]$BAUs.Dist)*
#   H.basic.data$Grid.infor$level[[g]]$Max.Dist/1800
# mean(dx[dx > 0])*8*sqrt(100/40)*
#   H.basic.data$Grid.infor$level[[g]]$Max.Dist
#
#
# min(dx[dx > 0])* H.basic.data$Grid.infor$level[[g]]$Max.Dist
# quantile(dx[dx > 0], c(0.01, 0.75))*8*sqrt(100/40)
#
# min(dx[dx > 0])*8*sqrt(100/40)
#
# max(dx[dx > 0])
#
# D <- fields::Wendland(H.basic.data$Grid.infor$level[[g]]$BAUs.Dist *
#                         H.basic.data$Grid.infor$level[[g]]$Max.Dist
#                       , theta =(H.basic.data$Grid.infor$level[[g]]$Max.Dist)*Cs
#                       , dimension = 1, k = 1)
#
# D <- ifelse(D > 0, 1, 0)
# apply(D, 2, sum)
#
# quantile(dx[dx > 0], c(0.01, 0.75))
#
#
# range(dx[dx > 0]*8*sqrt(100/40))
#
# median(dx[dx > 0])*8*sqrt(100/40)*
#   H.basic.data$Grid.infor$level[[g]]$Max.Dist*Cs
# # max(H.basic.data$Grid.infor$level[[g]]$BAUs.Dist) *
#   H.basic.data$Grid.infor$level[[g]]$Max.Dist*Cs


# m <- 10
# D <- H.basic.data$Grid.infor$level[[g]]$BAUs.Dist
# spTaper <- list()
# # theta <- vector()
# for(i in 1:mg){
#   D <- H.basic.data$Grid.infor$level[[g]]$BAUs.Dist
#   spTaper$taper[[g]] <- D
#
#   spTaper$tuning[[g]]$theta[i] <- (H.basic.data$Grid.infor$level[[g]]$Max.Dist)*
#     Rfast::nth(D[i, ], m, descending = F)
#
#   spTaper$taper[[g]][i, ] <- fields::Wendland(D[i, ] * H.basic.data$Grid.infor$level[[g]]$Max.Dist
#                                               , theta = spTaper$tuning[[g]]$theta[i]
#                                               , dimension = 1, k = 1)
#
# }

Ct <- 1
Ne <- 100
tab.1 <- strsplit(as.character(Ch), ".", fixed = TRUE)[[1]][2]
tab.2 <- strsplit(as.character(Cs), ".", fixed = TRUE)[[1]][2]
m <- sum(H.basic.data$Grid.infor$summary$Knots.count)
tab <- paste0("L", n.grps^2, "_", tab.1, "_", tab.2, "_", n.train, "_", m)
start.time <- Sys.time()
CV_T_Dist_W <- HDCM(Tab = tab,
                    Site = Site,
                    HDCM.Data = ADCM.Data,
                    H.basic.data = H.basic.data,
                    prior = prior,
                    ini.para = para,
                    CV = T,
                    verbose.VB = T,
                    verbose = TRUE,
                    Object = "Flag",
                    transf.Response = c("SQRT"),
                    Database = list(DSN = RODBC::odbcConnect("DSN_01",
                                     uid = "myname",
                                     pwd = "mypwd",
                                     believeNRows = FALSE,
                                     case = "toupper")),
                    response.scale = F,
                    save.Predict = F,
                    ensemble.size = Ne,
                    sample.size = 100,
                    # n.cores = 5,
                    factor = 1,
                    cs = Cs,
                    ct = Ct,
                    tol.real = 1e-4,
                    itMin = 5e1,
                    itMax = 5e1,
                    Obj.Seq = c(1:1))
end.time <- Sys.time()
print(end.time - start.time)
# test.id <- which(Site$ID %in% Site[Site$Flag == "test", 1])
# pred.Y_ts <- CV_T_Dist_W[[1]][["Pred"]]
# test.Y_ts <- ADCM.Data$Y_ts[, test.id]

