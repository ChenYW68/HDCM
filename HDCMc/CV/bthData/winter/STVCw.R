rm(list=ls())
source("./R/PSTVB_Packages.R")
data("SiteData", package = "ADCM")
source("./R/util.R")
library(spTDyn)
{
  PM25_2015w <- obs_PM25_2015w  %>% setorderv(c("CITY", "ID","DATE_TIME"))
  setDF(PM25_2015w)
  ##################################################################
  ###################################################################
  #                           1. Data loading
  ###################################################################

  setDF(PM25_2015w)
  DATE_TIME <- unique(PM25_2015w$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  PM25_2015w <- PM25_2015w  %>% left_join(date.time,  by = c("DATE_TIME"))
  PM25_2015w[, c("REAL_PM25", "sim50_CMAQ_PM25")] <-
    sqrt(PM25_2015w[, c("REAL_PM25", "sim50_CMAQ_PM25")])

  Covariate = c("sim50_CMAQ_PM25"
                , "time.index"
                , "sim_TEMP"
                , "sim_SPRESS"
                , "sim_WIND_Y"
                , "sim_WIND_X"
  );



  # Cova <- which(base::colnames(PM25_2015w) %in% Covariate)
  # if(length(Covariate) > 1){
  #   for(k in 2:(length(Covariate)))
  #   {
  #     PM25_2015w[, Cova[k]] = scale(center = T, as.vector(
  #       PM25_2015w[, Cova[k]]))[, 1]
  #   }}

  setDF(PM25_2015w)
  Cov.Index <- which(base::colnames(PM25_2015w) %in% Covariate)
  if(length(Cov.Index) > 1){
    mean_covariates <- apply(PM25_2015w[, Cov.Index], 2, mean)
    sd_covariates <- apply(PM25_2015w[, Cov.Index], 2, sd)
    PM25_2015w[, Cov.Index] <- scale(PM25_2015w[, Cov.Index],
                                     center = mean_covariates,
                                     scale = sd_covariates)
  }
}
region <- sort(as.character(unique(PM25_2015w$CITY)))
region_num <- 1:length(region)
rmse <- matrix(0, nrow = 13, ncol = 2)

colNames <- c("CITY","LON", "LAT", "DATE_TIME",
              "YEAR_MONTH", "YEAR","MONTH","DAY",
              "REAL_PM25")

setDT(PM25_2015w)
t0 <- Sys.time()
for(r  in region_num)
{

  set.seed(1234)
  cat(paste0("\n\n   The ", r, "th region: ", region[r], "!!!\n\n"))
  Da.mod <- PM25_2015w[CITY %nin% region[r], ]
  Da.pre <- PM25_2015w[CITY %in% region[r], ]
  setDF(Da.mod);setDF(Da.pre);
  Da.mod$intercept <- 1
  Da.pre$intercept <- 1

  # model
  post.gp <- spTDyn::GibbsDyn(REAL_PM25 ~ 1 + sim50_CMAQ_PM25 +
                                tp(intercept) +
                                # tp(sim50_CMAQ_PM25) +
                                # (time.index) +
                                (sim_TEMP) +
                                (sim_WIND_X) +
                                (sim_WIND_Y)
                              , coords =~LON_X + LAT_Y
                              , nBurn = 5e3
                              , nItr = 1e4
                              , data = Da.mod
                              , report = 5
                              , distance.method = "geodetic:mile"
                              , prior = priors(inv.var.prior = Gamm(a = 2, b = 1),
                                               beta.prior = Norm(0, 1E4),
                                               rho.prior = Norm(0, 1E4))
                              , initials = initials(rhotp = .1, phi = 1e-3)
                              , scale.transform = "NONE"
                              , spatial.decay = decay(
                                distribution = Unif(1/1e5, 1e0),
                                tuning = 1E-5)
  )
  Da.pre$REAL_PM25 <- Da.pre$REAL_PM25^2


  Y.pred <- predict(post.gp, newdata = Da.pre,
                     newcoords =~LON_X + LAT_Y)

  Y.pred$pred.samples <- ifelse(Y.pred$pred.samples < 0, 0,
                                Y.pred$pred.samples^2)


  y.pred <- apply(Y.pred$pred.samples, 1, quant)
  Pred.sd <- apply(Y.pred$pred.samples, 1, sd)

  PM25.U95 <-  y.pred[3, ]#^2
  PM25.Pred <- y.pred[2, ]#^2
  PM25.L25 <-  y.pred[1, ]

  Coverage <- mean(PM25.L25 < Da.pre$REAL_PM25 & PM25.U95 > Da.pre$REAL_PM25)


  spT <- spT_validation(z = Da.pre$REAL_PM25,
                        zhat = PM25.Pred,
                        sigma = NA,
                        zhat.Ens = NULL,
                        names = F, CC = F)#[c(1, 4)]
  print(spT)

  spT <- as.vector(spT)
  RMSE2 <- spT[1]
  Coef2 <- spT[2]
  FAC2.2 <- spT[3]
  CRPS2 <- spT[4]


  {cat(paste0("Testing object: ", region[r], "\n"))
    cat(paste0("Root mean squared error (RMSE) = ", RMSE2, "; \n"
               , "Continuous rank probability score (CRPS)  = ", CRPS2, "; \n"
               , "Fraction of predictions within a factor of two (FAC2) = ", FAC2.2,"; \n"
               , "Pearson correlation between the prediction and real data = ", Coef2,"; \n"))
  }

  if(r == region_num[1]){

    STVC <- data.frame(Da.pre[, colNames]
                       , PM25.L25 = PM25.L25
                       , PM25.Pred = PM25.Pred
                       , PM25.U95 = PM25.U95
                       , Pred.sd = Pred.sd
                       , Coverage = Coverage
                       , INS = spT["INS"]
    )
    #Beta <- beta

  }else{
    STVC <- rbind(STVC,
                  data.frame(Da.pre[, colNames]
                             , PM25.L25 = PM25.L25
                             , PM25.Pred = PM25.Pred
                             , PM25.U95 = PM25.U95
                             , Pred.sd = Pred.sd
                             , Coverage = Coverage
                             , INS = spT["INS"]
                  ))
    #Beta <- rbind(Beta, beta)
  }
  # Beta <- as.data.frame(Beta)
  #

  temp0 <- Validation.Group.Region(STVC, Sigma = NA,
                                   col = c("REAL_PM25",
                                           "PM25.Pred"),
                                   by = "CITY")
  cat("\n.............................\n")
  print(temp0)


}
temp0$CVG <- NULL
da <- plyr::ddply(STVC
                  , .(CITY) #, DAY, DATE_TIME
                  , plyr::summarize
                  , Coverage = mean(Coverage)
                  # , INS = mean(INS)
                  , .progress = "text")
setnames(temp0, "Group", "CITY")
temp <- temp0 %>% left_join(da, by = "CITY")
temp$Coverage <- round(ifelse(is.na(temp$Coverage),
                              mean(temp$Coverage, na.rm = T),
                              temp$Coverage), 4)
temp
writexl::write_xlsx(temp, path = "./Result/STVCxz_w_cv.xlsx")
writexl::write_xlsx(STVC, path = "./Result/pred_STVCxz_w_cv.xlsx")

