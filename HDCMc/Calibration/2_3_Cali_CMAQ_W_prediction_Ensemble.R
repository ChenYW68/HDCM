# rm(list=ls())
###################################################################
source("./R/PSTVB_Packages.R")
load("./data/China_GeoMap.RData")
load("./data/SiteData.RData")
load("./data/CMAQ_BTH_SITE.RData")
# load("./data/CMAQ_PM25_COM.RData")
CMAQ_Site <- CMAQ_BTH_SITE[distToNearestProvince <= 0,]
###################################################################
###################################################################
load(paste0("./Calibration/Result/Cali_HDCM_W.RData"))



file <- "./Calibration/Result/"
Tab <- list.files(file)
# HDCM <- Tab[grepl(paste0("Fit"), Tab) & grepl(paste0("_W_"), Tab)]
HDCM <- Tab[grepl(paste0("Fit"), Tab)]
load(paste0(file, HDCM[1]))
###################################################################
###################################################################
Year <- c(2015, 2015)
# month_day <- c("11-26", "12-01")
month_day <- c("12-17", "12-22")
###################################################################
###################################################################
setDT(obs_PM25_2015w)
DATE <- unique(obs_PM25_2015w[YEAR_MONTH %in%
                                c(201511, 201512, 201601), .(DATE_TIME)]) %>%
  as.data.frame()
date.index <- data.frame(time.index = 1:length(DATE$DATE_TIME),
                         time.scale = seq(0, 1, , length(DATE$DATE_TIME)),
                         DATE_TIME = DATE$DATE_TIME,
                         YEAR = year(DATE$DATE_TIME),
                         MONTH = month(DATE$DATE_TIME),
                         DAY = day(DATE$DATE_TIME)) %>%
  dplyr::filter(
    # YEAR %in% year,
    between((as.Date(DATE_TIME)),
            (as.Date(paste0(Year[1], "-", month_day[1]))),
            (as.Date(paste0(Year[2], "-", month_day[2]))))#,
    # between(MONTH, 10, 12),
  )



###################################################################
###################################################################
# XX_ts <- Cali_Data_Inf$X_ts
# XX_ts[2,,] <- matrix(sqrt(as.vector(Cali_Data_Inf$X_ts[2,,])),
#                      nrow = nrow(Cali_Data_Inf$X_ts[2,,]),
#                      ncol = Cali_Data_Inf$Nt)
###################################################################
time.index <- unique(Cali_Data_Inf$CMAQ_Cali$time.index)
CMAQ_Cali <- Cali_Data_Inf$CMAQ_Cali
CMAQ_Cali$Cali.pred <- 0
CMAQ_Cali$W_ts <- 0
CMAQ_Cali$var <- 0
# CMAQ_Cali$sd2 <- 0
setDT(CMAQ_Cali)
para_est <- wHDCM_Fitting$Para.dist$parameter
###################################################################
###################################################################
n.pred <- nrow(Cali_Data_Inf$Hs)
ensemble.size <- dim(wHDCM_Fitting$Ks$EnXs)[3]
############################################################
############################################################
# CMAQ_ID <- CMAQ_Cali[CMAQ_Cali$T_index == 1, ]$CMAQ_ID
N.CMAQ <- Cali_Data_Inf$CMAQ_N
Nt <- length(time.index)
############################################################
y.Pred.ens <- Wts.ens <- array(0, dim = c(Nt, n.pred, ensemble.size),
                                  dimnames = list(unique(Cali_Data_Inf$CMAQ_Cali$DATE_TIME),
                                                  1:n.pred,
                                                  paste0("En_",
                                                         1:ensemble.size)))


t1 <- proc.time()
test.H.vt <- mcmapply(t = time.index, SIMPLIFY = F,
                      FUN = function(t)
                        Matrix::crossprod(x = t(Cali_Data_Inf$Hs),
                                          y = wHDCM_Fitting$Ks$EnXs[t + 1, ,])
                      , mc.cores = 1)


beta.sample <- mvnfast::rmvn(ensemble.size, mu = para_est$beta$E_beta,
                             sigma = para_est$beta$sigma.sq)

Xts.beta.ens <- mcmapply(t = seq_len(Nt), SIMPLIFY = F,
                         FUN = function(t)
                           Matrix::crossprod(x = t(beta.sample),
                                             y = Cali_Data_Inf$X_ts[, t, ])
                         , mc.cores = 1)
time.index0 <- time.index
library(matrixStats)
for(t in 1:Nt){
        Wts.ens[t,, ] <- as.matrix(test.H.vt[[t]])
        y.Pred.ens[t, , ] <- t(Xts.beta.ens[[t]]) +  Wts.ens[t,, ] +
          rnorm(n.pred * ensemble.size, 0,
                sqrt(para_est$obs.sigma.sq$E_sigma.sq))
        CMAQ_Cali[time.index == time.index0[t], ("Cali.pred"):= rowMedians(y.Pred.ens[t, , ])]
        CMAQ_Cali[time.index == time.index0[t], ("W_ts"):= rowMedians(Wts.ens[t, , ])]
        # CMAQ_Cali[time.index == time.index[t], ("var"):= colVar(t(y.Pred.ens[t, , ]))]
}
t2 <- proc.time()
cat("Runing time from HDCM model: \n")
print(t2 - t1)
cat("\n ........................ \n")

# CMAQ_Cali$Cali.pred <- CMAQ_Cali$Cali.pred^2
############################################################
############################################################
setDT(CMAQ_Cali)
Cali_CMAQ_PM25 <-  CMAQ_Cali %>%
  ddply(.(CMAQ_ID, LON, LAT, DATE_TIME,
          YEAR, MONTH, DAY, YEAR_MONTH, time.index)
        , .fun = plyr::summarize
        , CMAQ_PM25 = mean(CMAQ_PM25, na.rm = TRUE)
        , PM25 = mean(Cali.pred, na.rm = TRUE)
        , W_ts = mean(W_ts, na.rm = TRUE)
        , .progress = "text") #%>% left_join(Y.pred.sd,
                                            # by = c("LON", "LAT",
                                            #        "CMAQ_ID",
                                            #        "T_index")
                                            # )
Cali_CMAQ_PM25$Method <- 2
Cali_CMAQ_PM25$PM25 <- Cali_CMAQ_PM25$PM25^2

orig_CMAQ_PM25 <- Cali_CMAQ_PM25[, c("LON", "LAT", "DATE_TIME", "CMAQ_PM25", "Method")]
orig_CMAQ_PM25$Method <- 1

Cali_CMAQ_PM25 <- Cali_CMAQ_PM25[, c("LON", "LAT", "DATE_TIME", "PM25", "Method")]
setnames(orig_CMAQ_PM25, "CMAQ_PM25", "PM25")


range(orig_CMAQ_PM25$PM25)
range(Cali_CMAQ_PM25$PM25)


###################################################################
###################################################################
# Da <- rbind(orig_CMAQ_PM25, Cali_CMAQ_PM25)
###################################################################
#              the real spatialtemporal data
###################################################################
# load("./dataModel_Base_Table_Update.RData")
###################################################################
{
  Real_PM25 <- obs_PM25_2015w %>%
    dplyr::filter(
      # YEAR %in% year,
      between((as.Date(DATE_TIME)),
              (as.Date(paste0(Year[1], "-", month_day[1]))),
              (as.Date(paste0(Year[2], "-", month_day[2]))))
    )%>%
    setorder(ID, DATE_TIME) %>%
    ddply(.(CITY, YEAR_MONTH, DATE_TIME) #, LON, LAT
          , plyr::summarize
          , PM25 = mean(REAL_PM25, na.rm = T)
          # , CMAQ_PM25 = median(sim_CMAQ_PM25, na.rm = T)
          , LON = mean(LON, na.rm = T)
          , LAT = mean(LAT, na.rm = T)
    )
  setDT(Real_PM25)
  #####################################################################
  R1 <- Real_PM25[, c("LON", "LAT", "DATE_TIME", "PM25")]
  R1$Method <- 1
  R3 <- R2 <- R1
  R2$Method <- 2
  R3$Method <- 3
  # R2 <- Real_PM25[, c("LON", "LAT", "DATE_TIME", "CMAQ_PM25")]
  # setnames(R2, "CMAQ_PM25", "PM25")
  # R2$Method <- 2
  # R3$Method <- 3

}
Real_PM25 <- rbind(R1, R2, R3)
After_Cali_PM25 <- rbind(orig_CMAQ_PM25, Cali_CMAQ_PM25)
###################################################################
###################################################################
colnames(After_Cali_PM25)
# After_Cali_PM25$Date = paste0("Nov", " ",
#                  day(After_Cali_PM25$DATE_TIME), ", ",
#                  "2015")
#
# Real_PM25$Date = paste0("Dec", " ",
#                               day(Real_PM25$DATE_TIME), ", ",
#                               "2015")

After_Cali_PM25$Date = paste0(ifelse(month(After_Cali_PM25$DATE_TIME) == 11, "Nov",
                        ifelse(month(After_Cali_PM25$DATE_TIME) == 12, "Dec",
                               "Jun")), " ",
                 day(After_Cali_PM25$DATE_TIME), ", ",
                 year(After_Cali_PM25$DATE_TIME))
Real_PM25$Date = paste0(ifelse(month(Real_PM25$DATE_TIME) == 11, "Nov",
                           ifelse(month(Real_PM25$DATE_TIME) == 12, "Dec",
                                  "Jun")), " ",
                    day(Real_PM25$DATE_TIME), ", ",
                    year(Real_PM25$DATE_TIME))




After_Cali_PM25$Date <- ordered(After_Cali_PM25$Date, levels = unique(After_Cali_PM25$Date))
Real_PM25$Date <- ordered(Real_PM25$Date, levels = unique(Real_PM25$Date))

After_Cali_PM25$Method <- ordered(After_Cali_PM25$Method, levels = c(1, 2))
Real_PM25$Method <- ordered(Real_PM25$Method, levels  = c(1, 2, 3))

save(After_Cali_PM25, Real_PM25, file = "./Calibration/Result/Calibrated_Data.RData")
