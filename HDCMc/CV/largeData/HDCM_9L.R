# install.packages("./LoadPackages//HDCM_1.0.zip", repos = NULL, type = "win.binary")
rm(list=ls())
source("./LoadPackages/RDependPackages.R")
load("./data/NAQPMS_CMAQ_Dataset_2015W.RData")
load("./data/Large_BTH_map.RData")
Site <- Site[distToNearestCAMQpoint <= 15]
set.seed(12345)
PM25_2015w <- NAQPMS_CMAQ_Dataset_2015W[distToNearestCAMQpoint <= 15]
#--------------------------------------------------------------------------------------
#--- Training and test datasets
n.train <- 5000
train.id <- sample(Site$ID, n.train, replace = F)
cat("The number of training set:", length(train.id))

Site$Flag <- ifelse(Site$ID %in% train.id, "train", "test")
PM25_2015w$Flag <- ifelse(PM25_2015w$ID %in% train.id, "train", "test")
#--------------------------------------------------------------------------------------
rm(NAQPMS_CMAQ_Dataset_2015W)

#--------------------------------------------------------------------------------------
# 1. Create a mesh through a triangulation scheme based on the ``INLA`` 
#    package (Lindgren and Rue, 2015), a spatial partitioning procedure is 
#    embedded in our triangulation scheme.
#--------------------------------------------------------------------------------------
Ch <- 0.05; R <- 3; Cs <- 5e-2; Ct <- 1; Ne <- 100
H.basic.data <- CreateGrid(PM25_2015w,
                           Site,
                           Map = fortify(larg_bth_map),
                           ## 2042
                           # max.edge = c(.35, .7),
                           # offset = c(1e-1, 0.6),
                           # cutoff = .23, # 0.5
                           ##3158
                           # max.edge = c(.23, .4), 
                           # offset = c(1e-1, 0.9), 
                           # cutoff = 0.3,
                           ## 10103
                           max.edge = c(.21, .3),
                           offset = c(1e-1, 0.9), 
                           cutoff = 0.11,
                           distance.scale = 1e3,
                           R.sqrt = R,
                           col = "blue",
                           size = 1,
                           site.id = "ID",
                           ch = Ch,
                           method = "Wendland",
                           response.label = "PM25",
                           distance.method = 1,
                           Grid = TRUE,
                           scale = 5)
H.basic.data$plot.grid
#######################################################################################
#--------------------------------------------------------------------------------------
#                               2. Data preparation for modeling
#--------------------------------------------------------------------------------------
#--Data are transformed via the square root transformation to stabilize the variance
PM25_2015w[, c("sim_CMAQ_PM25")] <- sqrt(PM25_2015w[, c("sim_CMAQ_PM25")])

#-- Sellect the time rang of data
PM25_2015w <- PM25_2015w %>% dplyr::filter(between(as.Date(DATE_TIME), 
                                                   as.Date(paste0(2015, "-", "11-01")),
                                                   as.Date(paste0(2015, "-", "11-30"))))

#-- Combine other variables with time variable
DATE_TIME <- unique(PM25_2015w$DATE_TIME) %>% sort()
Nt <- length(DATE_TIME)
date.time <- data.frame(time.index = 1:Nt,
                        time.scale = seq(0, 1, , Nt),
                        DATE_TIME = DATE_TIME)
PM25_2015w <- PM25_2015w  %>% left_join(date.time, by = c("DATE_TIME"))

#--Standardization
HDCM.Data <- Construct_HDCM_Data(data = PM25_2015w,
                                 include = list(YEAR = c(2015, 2016),
                                                month_day = c("11-01", "1-31")),
                                 Y = "PM25",
                                 X = c("sim_CMAQ_PM25", "TEMP", "WIND_X", "WIND_Y"),
                                 standard = T, center = T, start.index = 1)
#######################################################################################                              
#--------------------------------------------------------------------------------------
#                               3. Settings for the HDCM
#--------------------------------------------------------------------------------------
theta.2 <- c(1e-1, 1E-3, 1E0)
res.num <- H.basic.data$Grid.infor$summary$res
p <- dim(HDCM.Data$X_ts)[1]

#--3.1 Prior
prior <- list(
  beta = list(E_beta = rep(0, p), sigma.sq = 1e5*diag(1, p, p))
  , obs.sigma.sq = list(a = 2, b = 1)
  , theta.1 = list(mu = rep(1e-3, res.num), sigma.sq =  rep(1e5, res.num))
  , theta.2 = list(a = rep(theta.2[2], res.num), b =  rep(theta.2[3], res.num))
  , zeta = list(a = rep(1e-3, res.num), b =  rep(1e0, res.num))
  , zeta0 = list(a = rep(1e-3, res.num), b =  rep(1e0, res.num))
  , proc.tau.sq = list(a = rep(2, res.num), b = rep(1, res.num))
  , proc0.tau.sq = list(a = rep(2, res.num), b = rep(1, res.num))
)
#--3.2 Initialize  parameters
para <- list( beta = list(E_beta = c(8, 1, rep(0, p - 2))),
              obs.sigma.sq = list(E_sigma.sq = 1, a = 2, b = 1)
              , theta.1 = list(E_theta.1 = rep(1e-3, res.num))
              , theta.2 = list(E_theta.2 = rep(theta.2[1], res.num))
              , zeta = list(E_zeta = rep(1e-1, res.num))
              , zeta0 = list(E_zeta0 = rep(1e-1, res.num))
              , proc.tau.sq = list(E_tau.sq = rep(5e0, res.num))
              , proc0.tau.sq = list(E_tau.sq = rep(1E0, res.num))
)
#--------------------------------------------------------------------------------------
#                               4.  Fitting and predictions
#--------------------------------------------------------------------------------------
#-- A name for a list of all objects that will be saved
tab.1 <- strsplit(as.character(Ch), ".", fixed = TRUE)[[1]][2]
tab.2 <- strsplit(as.character(Cs), ".", fixed = TRUE)[[1]][2]
m <- sum(H.basic.data$Grid.infor$summary$Knots.count)
tab <- paste0("L", R^2, "_", tab.1, "_", tab.2, "_", n.train, "_", m)

#--Oracle
# Oracle.infor <- list(DSN = RODBC::odbcConnect("DSN_01",
#  uid = "myname",
#  pwd = "mypwd",
#  believeNRows = FALSE,
#  case = "toupper")),
Oracle.infor <- NULL                                  
start.time <- Sys.time()
CV_Ranalysis <- HDCM(Tab = tab,
                     Site = Site,
                     HDCM.Data = HDCM.Data,
                     H.basic.data = H.basic.data,
                     prior = prior,
                     ini.para = para,
                     CV = TRUE,
                     verbose.VB = TRUE,
                     verbose = TRUE,
                     Object = "Flag",
                     transf.Response = c("SQRT"),
                     Database = Oracle.infor, 
                     save.Predict = TRUE,
                     ensemble.size = Ne,
                     n.cores = 10,
                     cs = Cs,
                     ct = Ct,
                     tol.real = 1e-4,
                     itMin = 1e1,
                     itMax = 5e1)
end.time <- Sys.time()
print(end.time - start.time)



