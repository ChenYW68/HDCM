rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "HDCM")
###################################################################
#                           1. Data loading
###################################################################
{
  PM25_2015w <- obs_PM25_2015w %>% setorderv(c("CITY", "ID","DATE_TIME"))
  setDT(PM25_2015w)

  colnames(PM25_2015w)
  # setnames(PM25_2015w, "REAL_PM25", "REAL_PM25")
  
  setDF(Site); 
  INDX <- which(colnames(Site) %in% c("LON_X", "LAT_Y"))
  Site <- ADCM::spCoords.transform(Site[, -INDX], method = 1)
  setDF(PM25_2015w)
  INDX <- which(colnames(PM25_2015w) %in% c("LON_X", "LAT_Y"))
  PM25_2015w <- ADCM::spCoords.transform(PM25_2015w[, -INDX],
                                         method = 1)
  
  
  DATE_TIME <- unique(PM25_2015w$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  PM25_2015w <- PM25_2015w  %>% left_join(date.time,  by = c("DATE_TIME"))
  
  
  City.Name <-  sort(as.character(unique(PM25_2015w$CITY)))
}
PM25_2015w[, c("REAL_PM25", "sim50_CMAQ_PM25")] <-
  sqrt(PM25_2015w[, c("REAL_PM25", "sim50_CMAQ_PM25")])


Covariate <- c("sim50_CMAQ_PM25"
               , "sim_TEMP"
               , "sim_WIND_X"
               , "sim_WIND_Y"
               , "time.index"
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


colNames <- c("CITY", "LON", "LAT", "DATE_TIME", "YEAR_MONTH",
              "YEAR", "MONTH", "DAY", "REAL_PM25", "ID")
setDT(PM25_2015w)
for(City in 1:length(City.Name))
{
  cat(paste0("\n\n   The ", City, "th region: ", City.Name[City], "!!!\n\n"))
  set.seed(1234)
  # data
  Da.mod <- PM25_2015w[CITY %nin% City.Name[City], ]
  Da.pre <- PM25_2015w[CITY %in% City.Name[City], ]
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
  output <- randomForest.Model(REAL_PM25~., train_data = Da.mod, 
                       test_data = Da.pre[, -which(base::colnames(Da.pre) == 
                                                     "REAL_PM25")],
                       method = c("quantreg"), #, "split-conformal", "quantreg"
                       symmetry = TRUE, alpha = 0.05)

  
  PM25.Pred.sd <- 2*output$testPred*as.vector(output$Pred.sd)#sd(output$oob_error)
 
  PM25.Pred <- ifelse(output$testPred < 0, 0, output$testPred^2)
  PM25.L25 <- ifelse(output$quantreg_interval$lower < 0, 0, 
                     output$quantreg_interval$lower^2)
  PM25.U95 <- ifelse(output$quantreg_interval$upper < 0, 0, 
                     output$quantreg_interval$upper^2)
  

  
  spT <- spT_validation(z = Da.pre0$REAL_PM25, 
                        zhat = PM25.Pred, 
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
                     PM25.Pred.sd = PM25.Pred.sd)
  }else{
    RF <- rbind(RF, data.frame(Da.pre0[, INDX], 
                               PM25.Pred = PM25.Pred,
                               low.Pred = PM25.L25, 
                               upp.Pred = PM25.U95,
                               PM25.Pred.sd = PM25.Pred.sd)
    )
  }
  cat("....................RF..................\n\n")
  cat("       the ", City, "th City: ", City.Name[City], "\n\n")
  cat("....................RF..................\n\n")
  temp0 <- Validation.Group.Region(RF, col = c("REAL_PM25", "PM25.Pred"),
                                         by = "CITY")
  cat("\n.............................\n")
  print(temp0)
  # cat("........................................\n\n")
} 

# writexl::write_xlsx(temp, path = "./Result/RFw_cv.xlsx")
writexl::write_xlsx(RF, path = "./Result/pred_RFw_cv.xlsx")

