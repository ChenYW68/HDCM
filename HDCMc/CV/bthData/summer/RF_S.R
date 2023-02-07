rm(list=ls())
source("./R/PSTVB_Packages.R")
data("SiteData", package = "ADCM")
source("./R/util.R")
###################################################################
#                           1. Data loading
###################################################################
{
  PM25_2015s <- obs_PM25_2015s %>% setorderv(c("CITY", "ID","DATE_TIME"))
  setDT(PM25_2015s)
  
  colnames(PM25_2015s)
  # setnames(PM25_2015s, "REAL_PM25", "REAL_PM25")
  
  setDF(Site); 
  INDX <- which(colnames(Site) %in% c("LON_X", "LAT_Y"))
  Site <- ADCM::spCoords.transform(Site[, -INDX], method = 1)
  setDF(PM25_2015s)
  INDX <- which(colnames(PM25_2015s) %in% c("LON_X", "LAT_Y"))
  PM25_2015s <- ADCM::spCoords.transform(PM25_2015s[, -INDX],
                                         method = 1)
  
  
  DATE_TIME <- unique(PM25_2015s$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  PM25_2015s <- PM25_2015s  %>% left_join(date.time,  by = c("DATE_TIME"))
  
  
  City.Name <-  sort(as.character(unique(PM25_2015s$CITY)))
}

PM25_2015s[, c("REAL_PM25", "sim50_CMAQ_PM25")] <-
  sqrt(PM25_2015s[, c("REAL_PM25", "sim50_CMAQ_PM25")])


Covariate <- c("sim50_CMAQ_PM25"
               , "sim_TEMP"
               # , "SPRESS"
               , "sim_WIND_X"
               , "sim_WIND_Y"
               , "time.index"
);
setDF(PM25_2015s)
Cov.Index <- which(base::colnames(PM25_2015s) %in% Covariate)
if(length(Cov.Index) > 2){
  mean_covariates <- apply(PM25_2015s[, Cov.Index], 2, mean)
  sd_covariates <- apply(PM25_2015s[, Cov.Index], 2, sd)
  PM25_2015s[, Cov.Index] <- scale(PM25_2015s[, Cov.Index],
                                   center = mean_covariates,
                                   scale = sd_covariates)
}


colNames <- c("CITY", "LON", "LAT", "DATE_TIME", "YEAR_MONTH",
              "YEAR", "MONTH", "DAY", "REAL_PM25", "ID")
setDT(PM25_2015s)
for(City in 1:length(City.Name))
{
  cat(paste0("\n\n   The ", City, "th region: ", City.Name[City], "!!!\n\n"))
  set.seed(1234)
  # data
  Da.mod <- PM25_2015s[CITY %nin% City.Name[City], ]
  Da.pre <- PM25_2015s[CITY %in% City.Name[City], ]
  setDF(Da.mod);
  setDF(Da.pre)
  # Da.mod$REAL_PM25 <- (Da.mod$REAL_PM25)
  # Da.pre$REAL_PM25 <- (Da.pre$REAL_PM25)
  
  INDX <- which(colnames(Da.mod) %in% c(Covariate, "REAL_PM25"))
  Da.pre0 <- Da.pre
  Da.mod <- Da.mod[, INDX]
  Da.pre <- Da.pre[, INDX]
  colnames(Da.pre)
  Da.pre0$REAL_PM25 <- Da.pre0$REAL_PM25^2
  # model
  # rf <- randomForest((REAL_PM25)~., data = Da.mod, 
  #                    importance=TRUE, ntree = 500,
  #                    do.trace = F) 
  # # test
  # 
  # PM25.Pred <-  predict(rf, 
  #                       newdata = Da.pre[, -which(base::colnames(Da.pre) == 
  #                                                   "REAL_PM25")], 
  #                       predict.all=TRUE) %>% 
  #   as.vector()
  
  # pred.rf.int <- apply(PM25.Pred$individual, 1, function(x) {
  #   c(mean(x) + c(-1, 1) * sd(x), 
  #     quantile(x, c(0.025, 0.975)))
  # })
  # 
  # t(pred.rf.int)
  # library(rfinterval)
  source("./R/util.R")
  output <- randomForest.Model(REAL_PM25~., train_data = Da.mod, 
                               test_data = Da.pre[, -which(base::colnames(Da.pre) == 
                                                             "REAL_PM25")],
                               method = c("quantreg"), #, "split-conformal", "quantreg"
                               symmetry = TRUE, alpha = 0.05)
  
  
  PM25.Pred.sd <- as.vector(output$Pred.sd)*2*output$testPred
  
  PM25.Pred <- ifelse(output$testPred < 0, 0, output$testPred^2)
  PM25.L25 <- ifelse(output$quantreg_interval$lower < 0, 0, 
                     output$quantreg_interval$lower^2)
  PM25.U95 <- ifelse(output$quantreg_interval$upper < 0, 0, 
                     output$quantreg_interval$upper^2)
  
  
  
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
  
  # PM25.U95 <-  output$oob_interval$upper^2
  # PM25.Pred <- output$testPred^2
  # PM25.L25 <-  output$quantreg_interval$lo^2
  
  # mean(PM25.L25 < y & PM25.U95 > y)
  
  # mean(output$oob_interval$lo^2 < y & output$oob_interval$up^2 > y)
  # mean(output$sc_interval$lo^2 < y & output$sc_interval$up^2 > y)
  
  spT <- spT_validation(z = Da.pre0$REAL_PM25, 
                        zhat = PM25.Pred, 
                        sigma = PM25.Pred.sd,
                        zhat.Ens = NULL, 
                        names = F, CC = F)#[c(1, 4)]
  print(spT)
  Coverage <- mean(PM25.L25 < Da.pre0$REAL_PM25 & PM25.U95 > Da.pre0$REAL_PM25)
  
  
  # PM25.Pred <- PM25.Pred^2
  # PM25.Pred <- ifelse(PM25.Pred > 800, 800, PM25.Pred)
  
  INDX <- which(colnames(Da.pre0) %in% colNames)
  if(City == 1){
    
    RF <- data.frame(Da.pre0[, INDX], 
                     PM25.Pred = PM25.Pred,
                     low.Pred = PM25.L25, 
                     upp.Pred = PM25.U95,
                     PM25.Pred.sd = PM25.Pred.sd,
                     Coverage = Coverage)
  }else{
    RF <- rbind(RF, data.frame(Da.pre0[, INDX], 
                               PM25.Pred = PM25.Pred,
                               low.Pred = PM25.L25, 
                               upp.Pred = PM25.U95,
                               PM25.Pred.sd = PM25.Pred.sd,
                               Coverage = Coverage)
    )
  }
  cat("....................RF..................\n\n")
  cat("       the ", City, "th City: ", City.Name[City], "\n\n")
  cat("....................RF..................\n\n")
  temp0 <- Validation.Group.Region(RF, sigma = RF$PM25.Pred.sd, 
                                   col = c("REAL_PM25", "PM25.Pred"),
                                   by = "CITY")
  cat("\n.............................\n")
  print(temp0)
  # cat("........................................\n\n")
} 
temp0$CVG <- NULL
setnames(temp0, "Group", "CITY")
temp <- temp0 %>% left_join(distinct(RF[, c("CITY", "Coverage")]),
                            by = "CITY")

temp$Coverage <- round(ifelse(is.na(temp$Coverage), mean(temp$Coverage, na.rm = T),
                              temp$Coverage), 4)
temp
# writexl::write_xlsx(temp, path = "./Result/orig_RFs_cv.xlsx")
# writexl::write_xlsx(RF, path = "./Result/orig_pred_RFs_cv.xlsx")

writexl::write_xlsx(temp[, c(1:3, 6, 5)], path = "./Result/RFs_cv.xlsx")
writexl::write_xlsx(RF, path = "./Result/pred_RFs_cv.xlsx")
