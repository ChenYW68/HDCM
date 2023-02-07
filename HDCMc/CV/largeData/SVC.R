rm(list=ls())
source("./R/PSTVB_Packages.R")
source("./R/util.R")

load("./data/NAQPMS_CMAQ_Dataset_2015W.RData")
Site <- Site[distToNearestCAMQpoint <= 15]
set.seed(12345)

n.train <- 5000
train.id <- sample(Site$ID, n.train, replace = F)
cat("The number of training set:", length(train.id))

PM25_2015w <- NAQPMS_CMAQ_Dataset_2015W[distToNearestCAMQpoint <= 15]
Site$Flag <- ifelse(Site$ID %in% train.id, "train", "test")
PM25_2015w$Flag <- ifelse(PM25_2015w$ID %in% train.id, "train", "test")

{  
  PM25_2015w <- PM25_2015w %>% setorderv(c("Flag", "ID","DATE_TIME"))
  setDF(PM25_2015w)
  ##################################################################
  ###################################################################
  #                           1. Data loading
  ###################################################################
  
  Flag.Name <- as.character(unique(PM25_2015w$Flag))
  DATE_TIME <- unique(PM25_2015w$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  PM25_2015w <- PM25_2015w  %>% left_join(date.time,  by = c("DATE_TIME"))
  PM25_2015w[, c("PM25", "sim_CMAQ_PM25")] <-
    sqrt(PM25_2015w[, c("PM25", "sim_CMAQ_PM25")])
  
  Covariate <- c("sim_CMAQ_PM25" 
                 # , "TEMP"
                 # , "WIND_X"
                 # , "WIND_Y"
                 
  );
  
  # Cova <- which(base::colnames(PM25_2015w) %in% Covariate)
  # if(length(Covariate) > 1){
  #   for(k in 1:(length(Covariate)))
  #   {
  #     PM25_2015w[, Cova[k]] = scale(center = T, as.vector(
  #       PM25_2015w[, Cova[k]]))[, 1]
  #   }}
  setDF(PM25_2015w)
  Cov.Index <- which(base::colnames(PM25_2015w) %in% Covariate)
  if(length(Cov.Index) > 13){
    mean_covariates <- apply(PM25_2015w[, Cov.Index], 2, mean)
    sd_covariates <- apply(PM25_2015w[, Cov.Index], 2, sd)
    PM25_2015w[, Cov.Index] <- scale(PM25_2015w[, Cov.Index],
                                     center = mean_covariates,
                                     scale = sd_covariates)
  }
  
}

PM25_2015w <- PM25_2015w %>%
  dplyr::filter(
    # YEAR %in% year,
    between((as.Date(DATE_TIME)), 
            (as.Date(paste0(2015, "-", "11-01"))), 
            (as.Date(paste0(2015, "-", "11-01"))))#,
    # between(MONTH, 10, 12),
  )

# ###################################################################
# #                           2. Model
# ###################################################################
region <- unique(PM25_2015w$Flag)
###################################################################
#                       SVC model
###################################################################
{
  p <- length(Covariate) + 1
  # set parameters 
  {
    n.samples <- 1e4
    
    starting <- list("phi" = 1e-4, "sigma.sq" = 1
                     , "tau.sq" = .1, "nu" = 0.5,
                     'beta.starting'= c(8, 1, rep(0, p - 2)))
    tun <- 0.1
    tuning <- list("phi" = 1e-8, "nu" = 1e-2, "sigma.sq" = tun
                   , "tau.sq" = tun, 'beta' = c(tun, tun))
    
    priors.1 <- list("beta.Norm" = list(rep(0, p), diag(1e5, p)),
                     "phi.Unif" = c(1/1E7, 1), #1/2e2, 1/1e1
                     "sigma.sq.IG" = c(2e0, 1e0),
                     "nu.Unif" = c(0.1, 3), #1/2e2, 1/1e1
                     "tau.sq.IG" = c(2, 1e0))
    
    cov.model <- "exponential"
    
    n.report <- 5e3
    verbose <- T
  }
  
  
  region <- sort(as.character(unique(PM25_2015w$Flag)))
  region_num <- 1:length(region)
  setDT(PM25_2015w)
  
  Ens <- ceil(0.5*n.samples)
  
  colNames <- c("Flag","LON", "LAT", "DATE_TIME",
                "YEAR_MONTH", "YEAR","MONTH","DAY",
                "PM25")
  start.time <- Sys.time()
  for(r in 1){
    
    year_range <- unique(PM25_2015w$YEAR)
    tem <- PM25_2015w[Flag %in% region[r], "ID"]
    
    for(Year in year_range)
    {
      Base_Table <- PM25_2015w[YEAR == Year,]
      month_range <- unique(Base_Table$MONTH)
      for(Month in month_range)
      {
        Base_Tab <- Base_Table[MONTH == Month,]
        day_range <- unique(Base_Tab$DAY)
        for(Day in day_range)
        {
          set.seed(1234)
          cat("\n\n   the ", r, "th region: ", region[r], "!!!\n\n")
          cat("   year: ", Year, "; month: ", Month, "; day: ", Day," \n\n")
          cat("...................SVC.................\n\n")
          # Database
          Da.mod <- Base_Tab[Flag %in% "train" & DAY == Day, ]
          Da.pre <- Base_Tab[Flag %nin% "train" & DAY == Day, ]
          Da.pre$PM25 <- Da.pre$PM25^2
          setDF(Da.mod);setDF(Da.pre);
          Y <- Da.mod[, "PM25"]
          X <- as.matrix(cbind(Da.mod[, c(Covariate)]))
          # colnames(X) <- c("intercept", Covariate)
          colnames(X) <- c(Covariate)
          coords <- as.matrix(Da.mod[, c("LON_X", "LAT_Y")])
          
          n.samples <- 10
          fit.start.time <- Sys.time()
          m.1 <- spLM(Y ~ (X), coords = coords
                      , starting = starting, tuning = tuning
                      , priors = priors.1, cov.model = cov.model
                      , n.samples = n.samples, verbose = verbose
                      , n.report = 1e0)
          # View parameter estimates
          # m.2 <- spRecover(m.1, start = Ens + 1, verbose = T, 
          #                  n.omp.threads = 8)
          fit.end.time <- Sys.time()
          print(fit.end.time - fit.start.time)
          #9300000
          Ens <- 5
          # round(summary(m.2$p.theta.recover.samples)$quantiles, 2)
          X <- as.matrix((Da.pre[, c(Covariate)]))
          X <- cbind(1, X)
          coords <- as.matrix(Da.pre[, c("LON_X", "LAT_Y")])
          cat("   year: ", Year, "; month: ", Month, "; day: ", Day," \n\n")
          
          pred.start.time <- Sys.time()
          Y.pred <- spPredict(m.1, pred.covars = X,
                              pred.coords = coords,
                              start = Ens + 1,
                              verbose = T, 
                              n.report = 1e0,
                              n.omp.threads = 1)
          pred.end.time <- Sys.time()
          print(pred.end.time - pred.start.time)
          # 53.63485
          Y.pred$p.y.predictive.samples <- ifelse(Y.pred$p.y.predictive.samples < 0, 0, 
                           Y.pred$p.y.predictive.samples^2)
          
          
          y.pred <- apply(Y.pred$p.y.predictive.samples, 1, quant)
          Pred.sd <- apply(Y.pred$p.y.predictive.samples, 1, sd)
          
    
          
          PM25.U95 <-  y.pred[3, ]#^2
          PM25.Pred <- y.pred[2, ]#^2
          PM25.L25 <-  y.pred[1, ]#^2
          
          
          
          Coverage <- mean(PM25.L25 < Da.pre$PM25 & PM25.U95 > Da.pre$PM25)
          
         
          spT <- spT_validation(z = Da.pre$PM25, 
                                zhat = PM25.Pred, 
                                sigma = Pred.sd,
                                zhat.Ens = NULL, 
                                names = F, CC = F)#[c(1, 4)]
          print(spT)
          
          if(r == region_num[1] & Year == year_range[1]  &
             Month == month_range[1] & Day == day_range[1])
          {
            SVC <- data.frame(Da.pre[, colNames], 
                              PM25.L25 = PM25.L25, 
                              PM25.Pred = PM25.Pred,
                              PM25.U95 = PM25.U95,
                              Pred.sd = Pred.sd,
                              Coverage = Coverage,
                              INS = spT["INS"])
            
          }else{
            SVC <- rbind(SVC, data.frame(Da.pre[, colNames], 
                                         PM25.L25 = PM25.L25, 
                                         PM25.Pred = PM25.Pred, 
                                         PM25.U95 = PM25.U95,
                                         Pred.sd = Pred.sd,
                                         Coverage = Coverage,
                                         INS = spT["INS"]))
          }
          temp0 <- Validation.Group.Region(SVC, sigma = SVC$Pred.sd,
                                           col = c("PM25", "PM25.Pred"),
                                           by = "Flag")
          cat("\n.............................\n")
          print(temp0)
          
          print(mean(SVC$PM25.L25 < SVC$PM25 & SVC$PM25.U95 > SVC$PM25))
        }
        cat("\n.............................\n")
      }
    }
    
    temp0 <- Validation.Group.Region(SVC, sigma = SVC$Pred.sd, 
                                     col = c("PM25", "PM25.Pred"),
                                     by = "Flag")
    cat("\n.............................\n")
    print(temp0)
   
  }
}
end.time <- Sys.time()
print(end.time - start.time)

da <- plyr::ddply(SVC
                  , .(Flag) #, DAY, DATE_TIME
                  , plyr::summarize
                  , Coverage = mean(Coverage)
                  # , INS = mean(INS)
                  , .progress = "text")
setnames(temp0, "Group", "Flag")
temp <- temp0 %>% left_join(da, by = "Flag")
temp$Coverage <- round(ifelse(is.na(temp$Coverage), 
                              mean(temp$Coverage, na.rm = T),
                              temp$Coverage), 4)
temp$INS <- round(ifelse(is.na(temp$INS), mean(temp$INS, na.rm = T),
                         temp$INS), 4)
temp
# writexl::write_xlsx(temp, path = "./Result/large_SVC_cv.xlsx")





###################################################################
#                       full model
###################################################################

# Covariate = c("sim_CMAQ_PM25" 
#               # , "time.index"
#               # , "sim_TEMP"
#               # , "sim_RHUID"
#               # , "sim_SPRESS"
#               # , "sim_WIND_Y"
#               # , "sim_WIND_X"
# );

# load("./data/BTH/PM25_2015w.RData")
# start_time.1 <- proc.time()
# for(t in 1:92){
#   Da.mod <- PM25_2015w[TIME == t, ]
#   Da.mod$PM25 = if_else(is.na(Da.mod$PM25)
#                              , Da.mod$NA.Kriging
#                              , Da.mod$PM25)
#   # predict
#   load(paste0("./3_Calibration/Cali_ADCM_W.RData"))
#   CMAQ_Cali <- Cali_Data_Inf$CMAQ_Cali
#   Cova <- which(base::colnames(CMAQ_Cali) %in% Covariate)
#   CMAQ_Cali[, c("sim_CMAQ_PM25")]=
#     sqrt(CMAQ_Cali[,  c("sim_CMAQ_PM25")])
#   
#   # if(length(Covariate) > 1){
#   #   for(k in 2:(length(Covariate)))
#   #   {
#   #     PM25_2015w[, Cova[k]] = scale(center = T, as.vector(
#   #       PM25_2015w[, Cova[k]]))[, 1]
#   #   }}
#   
#   Da.pre <- CMAQ_Cali[DATE_TIME == as.Date("2015-11-26"),]
#   
#   setDF(Da.mod);setDF(Da.pre);
#   Y = Da.mod[, "PM25"]
#   X = as.matrix(cbind(Da.mod[, c(Covariate)]))
#   # colnames(X) <- c("intercept", Covariate)
#   colnames(X) <- c(Covariate)
#   coords <- as.matrix(Da.mod[, c("LON_X", "LAT_Y")])
#   # model fitting
#   
#   m.1 <- spLM(sqrt(Y) ~ (X), coords = coords
#               , starting = starting, tuning = tuning
#               , priors = priors.1, cov.model = cov.model
#               , n.samples = n.samples, verbose = T
#               , n.report = n.report)
# }
# 
# end_time.1 <- proc.time()
# print(end_time.1 - start_time.1)

# View parameter estimates
# m.2 <- spRecover(m.1, start = Ens + 1, verbose = FALSE)
# beta <- round(summary(m.2$p.beta.recover.samples
# )$quantiles, 2) %>% as.data.frame()
# 
# beta <- as.data.frame(t(beta[, 3]))
# colnames(beta) <- c("Intercept", Covariate)
# 
# start_time.2 <- proc.time()
# X <-  as.matrix((Da.pre[, c(Covariate)]))
# X <- cbind(1, X)
# coords <- as.matrix(Da.pre[, c("LON_X", "LAT_Y")])
# y.pred <- spPredict(m.1, pred.covars = X,
#                     pred.coords = coords,
#                     start = Ens + 1,
#                     verbose = T,
#                     n.omp.threads = 8)
# end_time.2 <- proc.time()
# print(end_time.2 - start_time.2)