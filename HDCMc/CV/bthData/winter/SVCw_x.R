rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "ADCM")
{

  MODE_BASE_TABLE <- obs_PM25_2015w %>% setorderv(c("CITY", "ID","DATE_TIME"))
  setDF(MODE_BASE_TABLE)
  ##################################################################
  ###################################################################
  #                           1. Data loading
  ###################################################################

  City.Name <- as.character(unique(MODE_BASE_TABLE$CITY))
  setDF(MODE_BASE_TABLE)
  DATE_TIME <- unique(MODE_BASE_TABLE$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  MODE_BASE_TABLE <- MODE_BASE_TABLE  %>% left_join(date.time,  by = c("DATE_TIME"))
  MODE_BASE_TABLE[, c("REAL_PM25")] <- sqrt(MODE_BASE_TABLE[,  c("REAL_PM25")])
  MODE_BASE_TABLE[, c("sim50_CMAQ_PM25")] <- sqrt(MODE_BASE_TABLE[,  c("sim50_CMAQ_PM25")])

  Covariate = c("sim50_CMAQ_PM25");

  setDF(MODE_BASE_TABLE)
  Cov.Index <- which(base::colnames(MODE_BASE_TABLE) %in% Covariate)
  if(length(Cov.Index) > 1){
    mean_covariates <- apply(MODE_BASE_TABLE[, Cov.Index], 2, mean)
    sd_covariates <- apply(MODE_BASE_TABLE[, Cov.Index], 2, sd)
    MODE_BASE_TABLE[, Cov.Index] <- scale(MODE_BASE_TABLE[, Cov.Index],
                                          center = mean_covariates,
                                          scale = sd_covariates)
  }
}

# ###################################################################
# #                           2. Model
# ###################################################################
region <- unique(MODE_BASE_TABLE$CITY)
###################################################################
#                       SVC model
###################################################################
{
  p <- length(Covariate) + 1
  # set parameters
  {
    n.samples <- 1e5

    starting <- list("phi" = 1e-6, "sigma.sq" = 1
                     , "tau.sq" = .1, "nu" = 0.5,
                     'beta.starting'= c(0, 0, rep(0, p - 2)))
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


  region <- sort(as.character(unique(MODE_BASE_TABLE$CITY)))
  region_num <- 1:length(region)
  setDT(MODE_BASE_TABLE)

  Ens <- ceil(0.5*n.samples)

  colNames <- c("CITY","LON", "LAT", "DATE_TIME",
                "YEAR_MONTH", "YEAR","MONTH","DAY",
                "REAL_PM25")
  for(r in region_num)
  {

    year_range <- unique(MODE_BASE_TABLE$YEAR)
    tem <- MODE_BASE_TABLE[CITY %in% region[r], "ID"]

    for(Year in year_range)
    {
      Base_Table <- MODE_BASE_TABLE[YEAR == Year,]
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
          Da.mod <- Base_Tab[CITY %nin% region[r] & DAY == Day, ]
          Da.pre <- Base_Tab[CITY %in% region[r] & DAY == Day, ]

          setDF(Da.mod);setDF(Da.pre);
          Y = Da.mod[, "REAL_PM25"]
          X = as.matrix(cbind(Da.mod[, c(Covariate)]))
          # colnames(X) <- c("intercept", Covariate)
          colnames(X) <- c(Covariate)
          coords <- as.matrix(Da.mod[, c("LON_X", "LAT_Y")])


          m.1 <- spLM(Y ~ (X), coords = coords
                      , starting = starting, tuning = tuning
                      , priors = priors.1, cov.model = cov.model
                      , n.samples = n.samples, verbose = verbose
                      , n.report = n.report)
          # View parameter estimates
          # m.2 <- spRecover(m.1, start = Ens + 1, verbose = FALSE)
          # round(summary(m.2$p.theta.recover.samples
          #               )$quantiles, 2)
          X <-  as.matrix((Da.pre[, c(Covariate)]))
          X <- cbind(1, X)
          coords <- as.matrix(Da.pre[, c("LON_X", "LAT_Y")])
          y.pred <- spPredict(m.1, pred.covars = X,
                              pred.coords = coords,
                              start = Ens + 1,
                              verbose = F)
          y.pred <- apply(y.pred$p.y.predictive.samples, 1,quant)

          temp.U95 <-  y.pred[3, ]^2
          temp.Pred <- y.pred[2, ]^2
          temp.L25 <-  y.pred[1, ]^2

          Da.pre$REAL_PM25 <- Da.pre$REAL_PM25^2
          spT <- spT_validation(Da.pre$REAL_PM25, temp.Pred,
                                sigma = NA, zhat.Ens = NULL,
                                names = F, CC = F)[c(1, 4)]
          print(spT)

          if(r == region_num[1] & Year == year_range[1]  &
             Month == month_range[1] & Day == day_range[1])
          {
            SVC <- data.frame(Da.pre[, colNames],
                              PM25.L25 = temp.L25,
                              PM25.Pred = temp.Pred,
                              PM25.U95 = temp.U95)

          }else{
            SVC <- rbind(SVC, data.frame(Da.pre[, colNames]
                                         , PM25.L25 = temp.L25
                                         , PM25.Pred = temp.Pred
                                         , PM25.U95 = temp.U95))
          }
          temp1 <- Validation.Group.Region(SVC,
                                           col = c("REAL_PM25", "PM25.Pred"),
                                           by = "CITY")
          cat("\n.............................\n")
          print(temp1)
        }
        cat("\n.............................\n")
      }
    }

    temp0 <- Validation.Group.Region(SVC, col = c("REAL_PM25", "PM25.Pred"),
                                     by = "CITY")
    cat("\n.............................\n")
    print(temp0)
  }
}
# writexl::write_xlsx(temp0, path = "./data/SVCx_cv.xlsx")
writexl::write_xlsx(temp0, path = "./Result/SVCx_w_cv.xlsx")
writexl::write_xlsx(SVC, path = "./Result/pred_SVCx_w_cv.xlsx")
