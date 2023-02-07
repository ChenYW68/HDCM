rm(list=ls())
source("./R/PSTVB_Packages.R")
source("./R/util.R")
data("SiteData", package = "ADCM")
{
  
  PM25_2015w <- obs_PM25_2015s %>% setorderv(c("CITY", "ID","DATE_TIME"))
  setDF(PM25_2015w)
  ##################################################################
  ###################################################################
  #                           1. Data loading
  ###################################################################
  
  City.Name <- as.character(unique(PM25_2015w$CITY))
  setDF(PM25_2015w)
  DATE_TIME <- unique(PM25_2015w$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  PM25_2015w <- PM25_2015w  %>% left_join(date.time,  by = c("DATE_TIME"))
  
  
  
  PM25_2015w[, c("REAL_PM25", "sim50_CMAQ_PM25")] <-
    sqrt(PM25_2015w[, c("REAL_PM25", "sim50_CMAQ_PM25")])
  
  Covariate <- c("sim50_CMAQ_PM25" 
                 # , "sim_TEMP"
                 # # , "sim_SPRESS"
                 # , "sim_WIND_X"
                 # , "sim_WIND_Y"
                 # , "time.index"
                 
  );
  
  setDF(PM25_2015w)
  Cov.Index <- which(base::colnames(PM25_2015w) %in% Covariate)
  if(length(Cov.Index) > 2){
    mean_covariates <- apply(PM25_2015w[, Cov.Index], 2, mean)
    sd_covariates <- apply(PM25_2015w[, Cov.Index], 2, sd)
    PM25_2015w[, Cov.Index] <- scale(PM25_2015w[, Cov.Index],
                                     center = mean_covariates,
                                     scale = sd_covariates)
  }
  
}

# ###################################################################
# #                           2. Model
# ###################################################################
region <- unique(PM25_2015w$CITY)
###################################################################
#                       SVC model
###################################################################
{
  p = length(Covariate) + 1
  # set parameters 
  {
    n.samples <- 1e4
    
    starting <- list("phi" = 1e-6, "sigma.sq" = 1
                     , "tau.sq" = .1, "nu" = 0.5,
                     'beta.starting'= c(7, 2.5, rep(0, p - 2)))
    tun <- 0.1
    tuning <- list("phi" = 1e-8, "nu" = 1e-2, "sigma.sq" = tun
                   , "tau.sq" = tun, 'beta' = c(tun, tun))
    
    priors.1 <- list("beta.Norm" = list(rep(0, p), diag(1e5, p)),
                     "phi.Unif" = c(1/1E7, 1/1e4), #1/2e2, 1/1e1
                     "sigma.sq.IG" = c(2e0, 1e0),
                     "nu.Unif" = c(0.1, 3), #1/2e2, 1/1e1
                     "tau.sq.IG" = c(2, 1e0))
    
    cov.model <- "exponential"
    
    n.report <- 5000
    verbose <- F
  }
  
  
  region <- sort(as.character(unique(PM25_2015w$CITY)))
  region_num <- 1:length(region)
  setDT(PM25_2015w)
  
  Ens <- ceil(0.5*n.samples)
  
  colNames <- c("CITY","LON", "LAT", "DATE_TIME",
                "YEAR_MONTH", "YEAR","MONTH","DAY",
                "REAL_PM25")
  for(r in region_num)
  {
    
    year_range <- unique(PM25_2015w$YEAR)
    tem <- PM25_2015w[CITY %in% region[r], "ID"]
    
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
          cat(paste0("\n\n   the ", r, "th region: ", region[r], "!!!\n\n"))
          cat(paste0("   year: ", Year, "; month: ", Month, "; day: ", Day," \n\n"))
          cat(paste0("...................SVC.................\n\n"))
          # Database
          Da.mod <- Base_Tab[CITY %nin% region[r] & DAY == Day, ]
          Da.pre <- Base_Tab[CITY %in% region[r] & DAY == Day, ]
          Da.pre$REAL_PM25 <- Da.pre$REAL_PM25^2
          setDF(Da.mod);setDF(Da.pre);
          Y <- Da.mod[, "REAL_PM25"]
          X <- as.matrix(cbind(Da.mod[, c(Covariate)]))
          # colnames(X) <- c("intercept", Covariate)
          colnames(X) <- c(Covariate)
          coords <- as.matrix(Da.mod[, c("LON_X", "LAT_Y")])
          m.1 <- spLM(Y ~ (X), coords = coords
                      , starting = starting, tuning = tuning
                      , priors = priors.1, cov.model = cov.model
                      , n.samples = n.samples, verbose = verbose
                      , n.report = n.report)
          # View parameter estimates
          m.2 <- spRecover(m.1, start = Ens + 1, verbose = FALSE)
          round(summary(m.2$p.theta.recover.samples
          )$quantiles, 2)
          X <- as.matrix((Da.pre[, c(Covariate)]))
          X <- cbind(1, X)
          coords <- as.matrix(Da.pre[, c("LON_X", "LAT_Y")])
          Y.pred <- spPredict(m.1, pred.covars = X,
                              pred.coords = coords,
                              start = Ens + 1,
                              verbose = F)
          
          Y.pred$p.y.predictive.samples <- ifelse(
                        Y.pred$p.y.predictive.samples < 0, 0, 
                        Y.pred$p.y.predictive.samples^2)
          
          
          y.pred <- apply(Y.pred$p.y.predictive.samples, 1, quant)
          Pred.sd <- apply(Y.pred$p.y.predictive.samples, 1, sd)
          
          # Pred.sd  <- 2*y.pred[2, ]*Pred.sd
          
          
          # y.pred <- ifelse(y.pred < 0, 0, y.pred)
          
          PM25.U95 <-  y.pred[3, ]
          PM25.Pred <- y.pred[2, ]
          PM25.L25 <-  y.pred[1, ]
          
          # interval_range <- rep(95, length(Da.pre$REAL_PM25))
          # alpha <- (100 - interval_range) / 100
          # lower <- qnorm(alpha / 2, sqrt(PM25.Pred))#quantile(PM25.Pred, probs = alpha / 2)#qnorm(alpha / 2, Da.pre$REAL_PM25)
          # upper <- qnorm((1 - alpha / 2), sqrt(PM25.Pred))#quantile(PM25.Pred, probs = (1 - alpha / 2))#qnorm((1 - alpha / 2), Da.pre$REAL_PM25)
          # 
          # INS <-  scoringutils::interval_score(
          #   true_values = Da.pre$REAL_PM25,
          #   lower = lower,
          #   upper = upper,
          #   interval_range = 95,
          #   weigh = TRUE,
          #   separate_results = FALSE
          # )
          
          Coverage <- mean(PM25.L25 < Da.pre$REAL_PM25 & PM25.U95 > Da.pre$REAL_PM25)
          
          
          spT <- spT_validation(z = Da.pre$REAL_PM25, 
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
                              Coverage = Coverage)
            
          }else{
            SVC <- rbind(SVC, data.frame(Da.pre[, colNames], 
                                         PM25.L25 = PM25.L25, 
                                         PM25.Pred = PM25.Pred, 
                                         PM25.U95 = PM25.U95,
                                         Pred.sd = Pred.sd,
                                         Coverage = Coverage))
          }
          temp0 <- Validation.Group.Region(SVC, sigma = SVC$Pred.sd,
                                           col = c("REAL_PM25", "PM25.Pred"),
                                           by = "CITY")
          cat("\n.............................\n")
          print(temp0)
          
          print(mean(SVC$PM25.L25 < SVC$REAL_PM25 & SVC$PM25.U95 > SVC$REAL_PM25))
        }
        cat("\n.............................\n")
      }
    }
    
    temp0 <- Validation.Group.Region(SVC, sigma = SVC$Pred.sd, 
                                     col = c("REAL_PM25", "PM25.Pred"),
                                     by = "CITY")
    cat("\n.............................\n")
    print(temp0)
    
  }
}
temp0$CVG <- NULL
da <- plyr::ddply(SVC
                  , .(CITY) #, DAY, DATE_TIME
                  , plyr::summarize
                  , Coverage = mean(Coverage)
                  , .progress = "text")
setnames(temp0, "Group", "CITY")
temp <- temp0 %>% left_join(da, by = "CITY")
temp$Coverage <- round(ifelse(is.na(temp$Coverage), 
                              mean(temp$Coverage, na.rm = T),
                              temp$Coverage), 4)
# temp$INS <- round(ifelse(is.na(temp$INS), mean(temp$INS, na.rm = T),
#                          temp$INS), 4)
temp
# writexl::write_xlsx(temp[, c(1:3, 6, 5)], path = "./Result/orig_SVCs_cv.xlsx")
# writexl::write_xlsx(SVC, path = "./Result/orig_pred_SVCs_cv.xlsx")

# writexl::write_xlsx(temp[, c(1:3, 6, 5)], path = "./Result/SVCs_cv.xlsx")
# writexl::write_xlsx(SVC, path = "./Result/pred_SVCs_cv.xlsx")