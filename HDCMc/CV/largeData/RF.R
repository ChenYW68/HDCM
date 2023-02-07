source("./R/PSTVB_Packages.R")
###################################################################
#                           1. Data loading
###################################################################
{
  load("./data/NAQPMS_CMAQ_Dataset_2015W.RData")
  Site <- Site[distToNearestCAMQpoint <= 15]
  set.seed(12345)
  
  
  n.train <- 5000
  train.id <- sample(Site$ID, n.train, replace = F)
  cat("The number of training set:", length(train.id))
  
  PM25_2015w <- NAQPMS_CMAQ_Dataset_2015W[distToNearestCAMQpoint <= 15]
  Site$Flag <- ifelse(Site$ID %in% train.id, "train", "test")
  PM25_2015w$Flag <- ifelse(PM25_2015w$ID %in% train.id, "train", "test")
  temp <- Site[Site$Flag == "test", ]
  plot(temp$LON, temp$LAT)
  
  rm(NAQPMS_CMAQ_Dataset_2015W)
  PM25_2015w <- PM25_2015w %>%
    dplyr::filter(
      # YEAR %in% year,
      between((as.Date(DATE_TIME)), 
              (as.Date(paste0(2015, "-", "11-01"))), 
              (as.Date(paste0(2015, "-", "11-30"))))#,
      # between(MONTH, 10, 12),
    )
  colnames(PM25_2015w)
  # setnames(PM25_2015w, "PM25", "REAL_PM25")
  
  setDF(Site); 
  INDX <- which(colnames(Site) %in% c("LON_X", "LAT_Y"))
  Site <- ADCM::spCoords.transform(Site[, -INDX], method = 1)
  setDF(PM25_2015w)
  INDX <- which(colnames(PM25_2015w) %in% c("LON_X", "LAT_Y"))
  PM25_2015w <- ADCM::spCoords.transform(PM25_2015w[, -INDX],
                                         method = 1)
  
  City.Name <-  sort(as.character(unique(PM25_2015w$Flag)))
}
PM25_2015w[, c("PM25", "sim_CMAQ_PM25")] <-
  sqrt(PM25_2015w[, c("PM25", "sim_CMAQ_PM25")])


Covariate <- c("sim_CMAQ_PM25"
               , "TEMP"
               , "WIND_X"
               , "WIND_Y"
               # , "time.scale"
);
# fmla <- as.formula(paste0("sqrt(PM25)~", paste(Covariate, collapse = "+")))
Cov.Index <- which(base::colnames(PM25_2015w) %in% Covariate)
# PM25_2015w[, c("PM25", "sim_CMAQ_PM25")] <- 
#   sqrt(PM25_2015w[,  c("PM25", "sim_CMAQ_PM25")])


# if(length(Covariate) > 1){
#   for(k in 1:(length(Covariate)))
#   {
#     PM25_2015w[, Cov.Index[k]] = scale(center = T, as.vector(
#       PM25_2015w[, Cov.Index[k]]))[, 1]
#   }}

setDF(PM25_2015w)
Cov.Index <- which(base::colnames(PM25_2015w) %in% Covariate)
if(length(Cov.Index) > 2){
  mean_covariates <- apply(PM25_2015w[, Cov.Index], 2, mean)
  sd_covariates <- apply(PM25_2015w[, Cov.Index], 2, sd)
  PM25_2015w[, Cov.Index] <- scale(PM25_2015w[, Cov.Index],
                                   center = mean_covariates,
                                   scale = sd_covariates)
}
colNames <- c("nearestProvince", "nearestProvinceEn", 
              "LON", "LAT", "DATE_TIME", "YEAR_MONTH",
              "YEAR", "MONTH", "DAY", "PM25", "ID")
setDT(PM25_2015w)
start.time <- proc.time()
for(City in 1:1)
{
  cat(paste0("\n\n   The ", City, "th region: ", City.Name[City], "!!!\n\n"))
  set.seed(1234)
  # data
  Da.mod <- PM25_2015w[Flag %nin% City.Name[City], ]
  Da.pre <- PM25_2015w[Flag %in% City.Name[City], ]
  setDF(Da.mod);
  setDF(Da.pre)
  Da.mod$PM25 <- (Da.mod$PM25)
  Da.pre$PM25 <- (Da.pre$PM25)
  
  INDX <- which(colnames(Da.mod) %in% c(Covariate, "PM25"))
  Da.pre0 <- Da.pre
  Da.mod <- Da.mod[, INDX]
  Da.pre <- Da.pre[, INDX]
  Da.pre0$PM25 <- Da.pre0$PM25^2
  # model
  # rf <- randomForest((PM25)~., data = Da.mod, importance=TRUE, ntree = 500,
  #                    do.trace = TRUE) 
  # # test
  # PM25.Pred <-  predict(rf, newdata = Da.pre[, -which(base::colnames(Da.pre) == "PM25")]) %>% as.vector()
  # PM25.Pred <- PM25.Pred
  # PM25.Pred <- ifelse(PM25.Pred > 800, 800, PM25.Pred)
  
  # library(rfinterval)
  source("./R/util.R")
  output <- randomForest.Model(PM25~., train_data = Da.mod, 
                       test_data = Da.pre[, -which(base::colnames(Da.pre) == 
                                                     "PM25")],
                       method = c("quantreg"),
                       symmetry = TRUE, alpha = 0.05)
  
  PM25.Pred.sd <- 2*output$testPred*as.vector(output$Pred.sd)#sd(output$oob_error)
  
  PM25.Pred <- ifelse(output$testPred < 0, 0, output$testPred^2)
  PM25.L25 <- ifelse(output$quantreg_interval$lower < 0, 0, 
                     output$quantreg_interval$lower^2)
  PM25.U95 <- ifelse(output$quantreg_interval$upper < 0, 0, 
                     output$quantreg_interval$upper^2)
  
  
  spT <- spT_validation(z = Da.pre0$PM25, 
                        zhat = PM25.Pred, 
                        sigma = PM25.Pred.sd,
                        zhat.Ens = NULL, 
                        names = F, CC = F)#[c(1, 4)]
  print(spT)
 
  Coverage <- mean(PM25.L25 < Da.pre0$PM25 & PM25.U95 > Da.pre0$PM25)
  
  # interval_range <- rep(95, length(Da.pre$PM25))
  # alpha <- (100 - interval_range) / 100
  # lower <- qnorm(alpha / 2, sqrt(PM25.Pred))#quantile(PM25.Pred, probs = alpha / 2)#qnorm(alpha / 2, Da.pre$REAL_PM25)
  # upper <- qnorm((1 - alpha / 2), sqrt(PM25.Pred))#quantile(PM25.Pred, probs = (1 - alpha / 2))#qnorm((1 - alpha / 2), Da.pre$REAL_PM25)
  # 
  # INS <-  scoringutils::interval_score(
  #   true_values = Da.pre$PM25,
  #   lower = lower,
  #   upper = upper,
  #   interval_range = 95,
  #   weigh = TRUE,
  #   separate_results = FALSE
  # )
  INDX <- which(colnames(Da.pre0) %in% colNames)
  if(City == 1)
  {
    
    RF <- data.frame(Da.pre0[, INDX], 
                     PM25.Pred = PM25.Pred,
                     Coverage = Coverage,
                     PM25.Pred.sd = PM25.Pred.sd,
                     Coverage = Coverage,
                     INS = spT["INS"])
  }else{
    RF <- rbind(RF, data.frame(Da.pre0[, INDX], 
                               PM25.Pred = PM25.Pred,
                               Coverage = Coverage,
                               PM25.Pred.sd = PM25.Pred.sd,
                               Coverage = Coverage,
                               INS = spT["INS"])
    )
  }
  cat("....................RF..................\n\n")
  cat("       the ", City, "th City: ", City.Name[City], "\n\n")
  cat("....................RF..................\n\n")
  spT <- spT_validation(Da.pre0[, "PM25"], sigma = RF$PM25.Pred.sd, 
                              PM25.Pred, NULL, F, CC = F)
  print(spT)
  # cat("........................................\n\n")
} 
end.time <- proc.time()
print(end.time - start.time)



spT$Coverage <- mean(RF$Coverage)
spT
# writexl::write_xlsx(temp, path = "./Result/RF_cv.xlsx")
