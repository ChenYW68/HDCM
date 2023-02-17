# install.packages("./LoadPackages//HDCM_1.0.zip",
#                  repos = NULL,
#                  type = "win.binary")
rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "HDCM")
data("China_BTH_GeoMap", package = "HDCM")
setDF(obs_PM25_2015w);setDF(Site);

#--------------------------------------------------------------------------------------
DATE_TIME <- unique(obs_PM25_2015w$DATE_TIME) %>% sort()
Nt <- length(DATE_TIME)
date.time <- data.frame(time.index = 1:Nt,
                        time.scale = seq(0, 1, , Nt),
                        time.scale.sin = sin(seq(0, 1, , Nt)/(0.03*pi)),
                        time.scale.cos = cos(seq(0, 1, , Nt)/(0.03*pi)),
                        DATE_TIME = DATE_TIME)
PM25_2015w <- obs_PM25_2015w  %>% left_join(date.time, by = c("DATE_TIME"))
#--------------------------------------------------------------------------------------
hdcm.table <- "HDCMw_"
Ch <- 0.23; R <- 1; Cs <- 0.3; Ct <- 1; Ne <- 100
Obj.Seq <- 1:13
#--------------------------------------------------------------------------------------
# 1. Create a mesh through a triangulation scheme based on the ``INLA`` 
#    package (Lindgren and Rue, 2015), a spatial partitioning procedure is 
#    embedded in our triangulation scheme.
#--------------------------------------------------------------------------------------
H.basic.data <- CreateGrid(PM25_2015w,
                           sample.coords = Site,
                           Map = BTH_City_Map,
                           max.edge = c(.35, .65),
                           offset = c(1e-1, 0.5),
                           cutoff = 0.04,
                           distance.scale = 1e3,
                           R.sqrt = R,
                           col = "black",
                           size = 2,
                           site.id = "ID",
                           factor = 1,
                           ch = Ch,
                           method = "indicator1",
                           distance.method = 1,
                           hjust = c(-1, 1, -1, 0.5)) 
H.basic.data$plot.grid
#######################################################################################
#--------------------------------------------------------------------------------------
#                               2. Data preparation for modeling
#--------------------------------------------------------------------------------------
#--Data are transformed via the square root transformation to stabilize the variance
PM25_2015w[, c("sim50_CMAQ_PM25")] <- sqrt(PM25_2015w[, c("sim50_CMAQ_PM25")])
HDCM.Data <- Construct_HDCM_Data(data = PM25_2015w,
                                 include = list(YEAR = c(2015, 2016),
                                                month_day = c("11-01", "1-31")),
                                 Y = "REAL_PM25",
                                 X = c("sim50_CMAQ_PM25"
                                       , "sim_TEMP"
                                       , "sim_WIND_X"
                                       , "sim_WIND_Y"
                                 ),
                                 standard = TRUE,
                                 center = TRUE,
                                 start.index = 1)
#######################################################################################                              
#--------------------------------------------------------------------------------------
#                               3. Settings for the HDCM
#--------------------------------------------------------------------------------------{
{
  theta.2 <- c(1e-1, 1e-3, 1)
  res.num <- H.basic.data$Grid.infor$summary$res
  p <- dim(HDCM.Data$X_ts)[1]
  #--3.1 Prior
  prior <- list(
    beta = list(E_beta = rep(0, p), sigma.sq = 1e5*diag(1, p, p))
    , obs.sigma.sq = list(a = 2, b = 1)
    , theta.1 = list(mu = rep(1e-4, res.num), sigma.sq =  rep(1e5, res.num))
    , theta.2 = list(a = rep(theta.2[2], res.num), b =  rep(theta.2[3], res.num))
    , zeta = list(a = rep(1e-3, res.num), b =  rep(2e1, res.num))
    , zeta0 = list(a = rep(1e-3, res.num), b =  rep(2e1, res.num))
    , proc.tau.sq = list(a = rep(2, res.num), b = rep(1, res.num))
    , proc0.tau.sq = list(a = rep(2, res.num), b = rep(1, res.num))
  )
  #--3.2 Initialize  parameters
  para <- list( beta = list(E_beta = c(7.0, 2.5, rep(0, p - 2))),
                obs.sigma.sq = list(E_sigma.sq = 1, a = 2, b = 1)
                , theta.1 = list(E_theta.1 = rep(1e-3, res.num))
                , theta.2 = list(E_theta.2 = rep(theta.2[1], res.num))
                , zeta = list(E_zeta = rep(1e0, res.num))
                , zeta0 = list(E_zeta0 = rep(1e0, res.num))
                , proc.tau.sq = list(E_tau.sq = rep(5e0, res.num))
                , proc0.tau.sq = list(E_tau.sq = rep(1, res.num))
  )
}
#--------------------------------------------------------------------------------------
#                               4.  Fitting and predictions
#--------------------------------------------------------------------------------------
#-- A name for a list of all objects that will be saved
tab.1 <- strsplit(as.character(Ch), ".", fixed = TRUE)[[1]][2]
tab.2 <- strsplit(as.character(Cs), ".", fixed = TRUE)[[1]][2]

m <- sum(H.basic.data$Grid.infor$summary$Knots.count)
# tab <- paste0(tab.1, "_", tab.2, "_", Ct)
tab <- paste0(R^2, "_", tab.1, "_", tab.2, "_", m)
#--Oracle
# Oracle.infor <- list(DSN = RODBC::odbcConnect("DSN_01",
#  uid = "myname",
#  pwd = "mypwd",
#  believeNRows = FALSE,
#  case = "toupper")),
Oracle.infor <- NULL  
CVw_BTH <- HDCM(Tab = paste0(hdcm.table, tab),
                Site = Site,
                HDCM.Data = HDCM.Data,
                H.basic.data = H.basic.data,
                prior = prior,
                ini.para = para,
                CV = T,
                verbose.VB = TRUE,
                verbose = TRUE,
                Object = "CITY",
                transf.Response = c("SQRT"),
                Database = Oracle.infor,
                response.scale = FALSE,
                save.Predict = TRUE,
                ensemble.size = Ne,
                plot = FALSE,
                # n.cores = 5,
                factor = 1,
                cs = Cs,
                ct = Ct,
                tol.real = 1e-3,
                itMin = 1e1,
                itMax = 1e2,
                Obj.Seq = Obj.Seq)
# save(CVw_BTH, file = paste0("./CV/Result/", hdcm.table, tab, "_", Obj.Seq[1], ".RData"))
