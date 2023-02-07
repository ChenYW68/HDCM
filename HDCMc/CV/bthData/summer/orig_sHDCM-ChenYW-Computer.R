rm(list=ls())
source("./R/PSTVB_Packages.R")
# data("SiteData", package = "AugstBase")
# data("GeoMap", package = "AugstBase")
# sourceCpp("./src/mathematics.cpp")
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
data("China_BTH_GeoMap", package = "ADCM")
data("SiteData", package = "ADCM")
setDF(obs_PM25_2015s);setDF(Site);

INDX <- which(colnames(Site) %in% c("LON_X", "LAT_Y"))
Site <- ADCM::spCoords.transform(Site[, -INDX], method = 1)
setDF(obs_PM25_2015s)

INDX <- which(colnames(obs_PM25_2015s) %in% c("LON_X", "LAT_Y"))
obs_PM25_2015s <- ADCM::spCoords.transform(obs_PM25_2015s[, -INDX],
                                           method = 1)

######################################################################
#                   1. Create grid
######################################################################
# china.county.map <- rgdal::readOGR(paste0("./DataProcess/Gadm36_CHN_shp/."),"gadm36_CHN_1"
#                                    , stringsAsFactors=FALSE
#                                    , encoding = "UTF-8"
#                                    , use_iconv = T)
# china.province.map <- china.province.map[china.province.map$NAME_1 == "Hebei" |
#                                            china.province.map$NAME_1 == "Beijing" |
#                                            china.province.map$NAME_1 == "Tianjin",]

# china.county.map <- rgdal::readOGR(dsn = path.expand(
#   paste0("./DataProcess/Gadm36_CHN_shp/."))
#                             , layer = "gadm36_CHN_2"
#                             , stringsAsFactors = FALSE
#                             , encoding = "UTF-8"
#                             , use_iconv = T)
# 
# bth_map <- china.county.map[china.county.map$NAME_1 %in%
#                                        c("Beijing", "Tianjin", "Hebei"
#                                        ),]
# save(bth_map, file = "./data/bth_map.RData")
# Boundary <- as.data.frame(t(bbox(china.county.map)))


# par(mfrow = c(1, 2))
# # plot(Map_BTH$long,Map_BTH$lat, cex = 0.1)
# plot(a$long,a$lat, cex = .1)

# plot(china.county.map)
# Map_BTH <- fortify(china.county.map)
# data("China_BTH_GeoMap", package = "ADCM")
load("./data/bth_map.RData")
source("./R/CreateGrid.R")
Ch <- 0.22
H.basic.data <- CreateGrid(obs_PM25_2015s, 
                           Site,
                           Map = bth_map,
                           max.edge = c(.35, .65), #0.3,0.7
                           offset = c(1e-1, 0.5), #0.4, 0.6
                           cutoff = 0.04,#.04, #0.1
                           # max.edge = c(.35, .65), #0.3,0.7
                           # offset = c(1e-1, 0.5), #0.4, 0.6
                           # cutoff = .05,
                           distance.scale = 1e3,
                           n.grps = 2,
                           col = "black", 
                           size = 2,
                           site.id = "ID",
                           factor = 1,
                           cs = Ch,
                           method = "indicator1",
                           distance.method = 1,
                           hjust = c(-1, 1, -1, 0.5),
                           way = 1) #indicator
H.basic.data$plot.grid
# ggsave(plot = H.basic.data$plot.grid, height = 6, width = 6,
#        file = './figure/Fig_Small_Map.pdf')
range(H.basic.data$Grid.infor$summary$Hdist)*0.15
H.basic.data$Grid.infor$summary$Knots.count
# H.basic.data$Hs <- H.basic.data$Hs/rowSums(H.basic.data$Hs)
rARPACK::eigs_sym(as.dgCMatrix.spam(H.basic.data$Grid.infor$level[[1]][["Adj.Mat"]]), 1)$values
# a = H.basic.data$Hs/rowSums(H.basic.data$Hs)
# b = H.basic.data$Hs[5,]/rowSums(H.basic.data$Hs)[5]
# all.equal(b, a[5,])
# points(part.centroids$Var1,part.centroids$Var2, col= "red", pch = 19)
##########################################################################################
#                       2. Mapping matrix: H
##########################################################################################
# H.basic.data <- CreateHmatrix(grid, method = c("indicator"), #indicator, INLA, Wendland
#                           site = Site, factor = 1, 
#                           cs = .15,  distance = F,
#                           distance.method = "geodetic:km" #geodetic:1000km
# )

# plot.Mat <- function(mat)
# {
#   mat %>% as.vector() %>% 
#     tibble(value = ., row = rep(1:nrow(mat), times = ncol(mat)),
#            col = rep(1: ncol(mat), each = nrow(mat))) %>%
#     ggplot(aes(x = row, y = col, fill = value)) +
#     geom_tile(size = 2) +
#     scale_fill_gradient(low = 'black',high = 'white')+
#     theme_minimal() +
#     theme(legend.position = 'none')
# }

##########################################################################################
#                          3. Data for modeling 
##########################################################################################
# YearMonth <- c(201506, 201507, 201508)
PM25_2015w <- obs_PM25_2015s #%>% filter(YEAR_MONTH %in% YearMonth)
# 
# CMAQ.cv <- Validation.Group.Region(data = PM25_2015w,
#                                 # sigma = Pred.sd,
#                                 col = c("REAL_PM25", 
#                                         "sim_CMAQ_PM25"),
#                                 by = "CITY")
# writexl::write_xlsx(CMAQ.cv, path = "./Result/CMAQ.cv.xlsx")
# # 
# PM25_2015w[, c("sim_CMAQ_PM25", "sim_SPRESS")]=
#   sqrt(PM25_2015w[, c("sim_CMAQ_PM25", "sim_SPRESS")])

# PM25_2015w[, c("sim50_CMAQ_PM25")] <-
#   sqrt(PM25_2015w[, c("sim50_CMAQ_PM25")])


DATE_TIME <- unique(PM25_2015w$DATE_TIME) %>% sort()
Nt <- length(DATE_TIME)
date.time <- data.frame(time.index = 1:Nt,
                        time.scale = seq(0, 1, , Nt),
                        time.scale.sin = sin(seq(0, 1, , Nt)/(0.03*pi)),
                        time.scale.cos = cos(seq(0, 1, , Nt)/(0.03*pi)),
                        DATE_TIME = DATE_TIME)
PM25_2015w <- PM25_2015w  %>% left_join(date.time, by = c("DATE_TIME"))

# PM25_2015w$Bias <- sqrt(PM25_2015w$REAL_PM25) - sqrt(obs_PM25_2015s$sim50_CMAQ_PM25) 
# PM25_2015w$CMAQ_PM25  <- sqrt(PM25_2015w$sim50_CMAQ_PM25) 
# construct datasets  

source("./R/Construct_ADCM_Data.R")
ADCM.Data <- Construct_ADCM_Data(data = PM25_2015w,
                                 include = list(YEAR = c(2015, 2015),
                                                month_day = c("6-01", "8-31")),
                                 Y = "REAL_PM25",#"Bias",#"REAL_PM25",
                                 X = c("sim50_CMAQ_PM25"
                                       , "sim_TEMP"
                                       , "sim_WIND_X"
                                       , "sim_WIND_Y"
                                       # , "sim_SPRESS" 
                                       # , "time.scale"
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
  ds <- 0.5*max(H.basic.data$Grid.infor$level[[1]]$Max.Dist)
  # theta.2 <- c(1e2, 5e1, 0.5*max(H.basic.data$Grid.infor$level[[1]]$BAUs.Dist))
  theta.2 <- c(1e-2, 1e-3, 1) #c(1e-2, 1e-3, 1)
  res.num <- H.basic.data$Grid.infor$summary$res
  # ADCM.Data$X_ts <- ADCM.Data$X_ts[-1,,]
  p1 = dim(ADCM.Data$X_ts)[1]
  #---------------------------------------------------------------------
  #                           4.1 Prior
  #---------------------------------------------------------------------
  prior <- list(
    beta = list(E_beta = rep(0, p1), sigma.sq = 1e5*diag(1, p1, p1))
    , obs.sigma.sq = list(a = 2, b = 1)
    , theta.1 = list(mu = rep(1e-4, res.num), sigma.sq =  rep(1e5, res.num))
    , theta.2 = list(a = rep(theta.2[2], res.num), b =  rep(theta.2[3], res.num))
    , zeta = list(a = rep(1e-3, res.num), b =  rep(2e1, res.num))
    , zeta0 = list(a = rep(1e-3, res.num), b =  rep(2e1, res.num)) 
    , proc.tau.sq = list(a = rep(2, res.num), b = rep(1, res.num))
    , proc0.tau.sq = list(a = rep(2, res.num), b = rep(1, res.num))
  )
  #---------------------------------------------------------------------
  #                        4.2 initialize  parameters
  #---------------------------------------------------------------------
  para <- list( beta = list(E_beta = c(50, 1, rep(0, p1 - 2))), # 6, 2
    obs.sigma.sq = list(E_sigma.sq = Vari, a = 2, b = 1)
    , theta.1 = list(E_theta.1 = rep(1e-3, res.num))
    , theta.2 = list(E_theta.2 = rep(theta.2[1], res.num))
    , zeta = list(E_zeta = rep(1e0, res.num))
    , zeta0 = list(E_zeta0 = rep(1e0, res.num)) 
    , proc.tau.sq = list(E_tau.sq = rep(2e1, res.num))
    , proc0.tau.sq = list(E_tau.sq = rep(1e0, res.num))
  )
}
##########################################################################################
#                          5. Model fitting and prediction
##########################################################################################
library(CovUtil)
source("./R/HDCM.R")
source("./R/VB_EnKS.R")
source("./R/VB.R")
source("./R/MEnKS.R")

g = 1
Cs <- 0.3
quantile(as.vector(H.basic.data$Grid.infor$level[[g]]$BAUs.Dist), Cs)*
  H.basic.data$Grid.infor$level[[g]]$Max.Dist

max(H.basic.data$Grid.infor$level[[g]]$BAUs.Dist) *
  H.basic.data$Grid.infor$level[[g]]$Max.Dist*0.3


Ct <- 1
Ne <- 100
tab.1 <- strsplit(as.character(Ch), ".", fixed = TRUE)[[1]][2]
tab.2 <- strsplit(as.character(Cs), ".", fixed = TRUE)[[1]][2]

tab <- paste0(tab.1, "_", tab.2, "_", Ct)

hdcm.table <- "HSO2_"
Obj.Seq <- 1:4
CVs_BTH <- HDCM(Tab = paste0(hdcm.table, tab),
                Site = Site,
                HDCM.Data = ADCM.Data,
                H.basic.data = H.basic.data,
                prior = prior,
                ini.para = para,
                CV = TRUE,
                verbose.VB = TRUE,
                verbose = TRUE,
                Object = "CITY",
                transf.Response = c("normal"),
                Database = list(DSN = RODBC::odbcConnect("DSN_01", 
                                                         uid = "myname", 
                                                         pwd = "mypwd", 
                                                         believeNRows = FALSE, 
                                                         case = "toupper")),
                response.scale = FALSE,
                save.Predict = TRUE,
                ensemble.size = Ne,
                # n.cores = 5,
                factor = 1,
                cs = Cs,
                ct = Ct,
                tol.real = 1e-4,
                itMin = 1e1,
                itMax = 2e2,
                Obj.Seq = Obj.Seq)

save(CVs_BTH, file = paste0("./Result/", hdcm.table, tab, "_", Obj.Seq[1], ".RData"))

