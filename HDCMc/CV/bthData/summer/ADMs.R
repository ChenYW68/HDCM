rm(list = ls())
source("./R/PSTVB_Packages.R")
source("./R/util.R")
data("SiteData", package = "ADCM")
{
  MODE_BASE_TABLE <- obs_PM25_2015s  %>% setorderv(c("CITY", "ID","DATE_TIME"))
  setDF(MODE_BASE_TABLE)
  ##################################################################
  ###################################################################
  #                           1. Data loading
  ###################################################################
  
  setDF(MODE_BASE_TABLE)
  DATE_TIME <- unique(MODE_BASE_TABLE$DATE_TIME) %>% sort()
  Nt <- length(DATE_TIME)
  date.time <- data.frame(time.index = 1:Nt,
                          time.scale = seq(0, 1, , Nt),
                          DATE_TIME = DATE_TIME)
  MODE_BASE_TABLE <- MODE_BASE_TABLE  %>% left_join(date.time,  by = c("DATE_TIME"))

  
  MODE_BASE_TABLE[, c("REAL_PM25", "sim50_CMAQ_PM25")] <-
    sqrt(MODE_BASE_TABLE[, c("REAL_PM25", "sim50_CMAQ_PM25")])
  
  Covariate <- c("sim50_CMAQ_PM25" 
                 , "sim_TEMP"
                 # , "sim_SPRESS"
                 , "sim_WIND_X"
                 , "sim_WIND_Y"
                 , "LON_X"
                 , "LAT_Y"
                 # , "time.index"
                 
              );
  
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

region <- sort(as.character(unique(MODE_BASE_TABLE$CITY)))
region_num <- 1:length(region)

colNames <- c("CITY","LON", "LAT", "DATE_TIME",
              "YEAR_MONTH", "YEAR","MONTH","DAY",
              "REAL_PM25")
setDF(MODE_BASE_TABLE)
bs <- "tp"
t0 <- Sys.time()
k <- 9
for(r  in region_num)#
{
  print(r)
  
  Da.mod <- MODE_BASE_TABLE[MODE_BASE_TABLE$CITY %nin% region[r],]
  Da.pre <- MODE_BASE_TABLE[MODE_BASE_TABLE$CITY %in% region[r],]
  
  
  fitres <- mgcv::gam(REAL_PM25 ~ sim50_CMAQ_PM25 + 
                        s(LON_X, LAT_Y, time.index, k = 100, bs = "tp") +
                        # s(time.index, k = k, bs = bs, m = 2) +
                        s(sim_TEMP, k = k, bs = bs, m = 2) +
                        # s(sim_SPRESS, k = k, bs = bs, m = 2) +
                        s(sim_WIND_X, k = k, bs = bs, m = 2) +
                        s(sim_WIND_Y, k = k, bs = bs, m = 2),
                      drop.intercept = F,
                      data = Da.mod)
  
  summary(fitres)
  
  pred_value <- predict(fitres, newdata = Da.pre, se.fit = T, type = "response")
  #print(pred.gp)
  # rmse[i, ] <- ADCM::spT_validation(DataValPred$REAL_PM25^2,
  #                                   as.vector(pred_value^2), NULL, F, CC = F)[c(1, 4)]
  

  
  
  Pred.sd <-  (as.numeric(pred_value$se.fit))* 2*ifelse(pred_value$fit < 0, 1E-3, pred_value$fit)
  PM25.Pred <- ifelse(pred_value$fit < 0, 0, pred_value$fit^2)
  
  PM25.L25 <- PM25.Pred + 
    qnorm(0.025)*Pred.sd#*testPred
  PM25.U95 <- PM25.Pred + 
    qnorm(0.975)*Pred.sd#*testPred
  
  
  PM25.L25 <- ifelse(PM25.L25 < 0, 0, PM25.L25)
  PM25.U95 <- ifelse(PM25.U95 < 0, 0, PM25.U95)
  
  
  Da.pre$REAL_PM25 <- Da.pre$REAL_PM25^2
  
  spT <- spT_validation(z = Da.pre$REAL_PM25, 
                        zhat = as.vector(PM25.Pred), 
                        sigma = NA,
                        zhat.Ens = NULL, 
                        names = F, CC = F)#[c(1, 4)]
  print(spT)
  
  
  if(r == region_num[1])
  {
    ADM <- data.frame(Da.pre[, colNames], 
                      PM25.L25 = PM25.L25, 
                      PM25.Pred = PM25.Pred,
                      PM25.U95 = PM25.U95,
                      Pred.sd = Pred.sd)
    
  }else{
    ADM <- rbind(ADM, data.frame(Da.pre[, colNames], 
                                 PM25.L25 = PM25.L25, 
                                 PM25.Pred = PM25.Pred, 
                                 PM25.U95 = PM25.U95,
                                 Pred.sd = Pred.sd))
  }
  temp0 <- Validation.Group.Region(ADM, Sigma = NA,
                                   col = c("REAL_PM25", "PM25.Pred"),
                                   by = "CITY")
  cat("\n.............................\n")
  print(temp0)
  
  # sqrt(sum((pred_value^2-DataValPred$REAL_PM25^2)^2)/length(DataValPred$REAL_PM25))
  
  print(Sys.time() - t0)
  t0 <- Sys.time()
  
}
temp0$CVG <- NULL
setDT(ADM)
# save(UK, file = paste0(file, "/UK_W.RData"))
# temp1 <- ADCM::Validation.Group.Region(UK, 
#                                col = c("REAL_PM25", 
#                                        "PM25.Pred"), 
#                                by = "CITY") 
# da <- plyr::ddply(ADM
#                   , .(CITY) #, DAY, DATE_TIME
#                   , plyr::summarize
#                   # , Coverage = mean(Coverage)
#                   # , INS = mean(INS)
#                   , .progress = "text")
# setnames(temp0, "Group", "CITY")
# temp <- temp0 %>% left_join(da, by = "CITY")
# temp$Coverage <- round(ifelse(is.na(temp$Coverage), mean(temp$Coverage, na.rm = T),
#                               temp$Coverage), 4)
# temp$INS <- round(ifelse(is.na(temp$INS), mean(temp$INS, na.rm = T),
#                               temp$INS), 4)
temp0
writexl::write_xlsx(temp0[, c(1:3, 6, 5)], path = "./Result/ADMs_cv.xlsx")
# writexl::write_xlsx(ADM, path = "./Result/pred_ADMs_cv.xlsx")
