rm(list=ls())
source("./R/PSTVB_Packages.R")
source("./R/util.R")

load("./data/NAQPMS_CMAQ_Dataset_2015W.RData")
Site <- Site[distToNearestCAMQpoint <= 15]
set.seed(12345)

####################################################################
n.train <- 2500
train.id <- sample(Site$ID, n.train, replace = F)
cat("The number of training set:", length(train.id))

PM25_2015w <- NAQPMS_CMAQ_Dataset_2015W[distToNearestCAMQpoint <= 15]
Site$Flag <- ifelse(Site$ID %in% train.id, "train", "test")
PM25_2015w$Flag <- ifelse(PM25_2015w$ID %in% train.id, "train", "test")

{
  setDT(PM25_2015w)
  ##################################################################
  ###################################################################
  #                           1. Data loading
  ###################################################################
  
  City.Name <- sort(as.character(unique(PM25_2015w$CITY)))

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


setDT(PM25_2015w)
colNames <- c("CITY","LON", "LAT", "DATE_TIME",
              "YEAR_MONTH", "YEAR","MONTH","DAY",
              "REAL_PM25")


###################################################################
#                           1. Universal Kriging
###################################################################
# Universal Kriging
setDT(PM25_2015w)
{
  for( City in 1:length(City.Name))
  {
    set.seed(1234)
    year_range <- unique(PM25_2015w$YEAR)
    # Matrix with the estimated daily covariance parameters
    cov.pars.daily <- NULL
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
          cat("   the ", City, "th City: ", City.Name[City], "!!!\n\n")
          cat("   year:", Year, "; month: ", Month, "; day: ", Day,". \n\n")
          cat("...................UK.................\n\n")
          # data preparation 
          Da.mod <- Base_Tab[CITY %nin% City.Name[City] & DAY == Day, ]
          Da.pre <- Base_Tab[CITY %in% City.Name[City] & DAY == Day, ]
          setDF(Da.mod);setDF(Da.pre)
          # This first loop is to estimate the covariance parameters of the exponential covariance
          
          # geodata object with observed daily PM concentration and daily CMAQ output
          pm.ukrig.df <- data.frame(cbind(Da.mod[, "LON_X"],
                                          Da.mod[, "LAT_Y"],
                                          (Da.mod[, "REAL_PM25"]),
                                          Da.mod[, Covariate]))
          pm.ukrig.geo <- as.geodata(pm.ukrig.df,
                                     coords.col = c(1,2), 
                                     data.col = c(3:ncol(pm.ukrig.df)))
          
          # estimation of the covariance parameters via REML
          # pm.ukrig.reml <- likfit(geodata = pm.ukrig.geo,
          #                         coord = pm.ukrig.geo$coords,
          #                         data = pm.ukrig.geo$data[,1],
          #                         trend = ~pm.ukrig.geo$data[, -1],
          #                         cov.model = "exponential",
          #                         ini = c(1, 100.0), 
          #                         nugget= 1, 
          #                         fix.nug = FALSE,
          #                         lik.met = "REML",
          #                         lambda = 1)
          likFun <- function(lik.met = "REML"){
            tryCatch(
              expr = {
                likfit(geodata = pm.ukrig.geo,
                                        coord = pm.ukrig.geo$coords,
                                        data = pm.ukrig.geo$data[,1],
                                        trend = ~pm.ukrig.geo$data[, -1],
                                        cov.model = "exponential",
                                        ini = c(1E1, 100.0), 
                                        nugget= 1E-2, 
                                        fix.nug = FALSE,
                                        lik.met = lik.met,
                                        lambda = 1, 
                       messages = F)
              },
              error = function(e){
                message('Caught an error!')
                print(e)
              },
              warning = function(w){
                message('Caught an warning!')
                print(w)
              },
              finally = {
                message('success!')
              }
            )    
          }
          pm.ukrig.reml <- likFun(lik.met = "REML")
          if(length(pm.ukrig.reml) == 2){
            pm.ukrig.reml <- likFun(lik.met = "ML")
          }
          
          cov.pars.daily <- rbind(cov.pars.daily, 
                                  data.frame(sigmasq = pm.ukrig.reml$sigmasq,
                                             phi = pm.ukrig.reml$phi,
                                             tausq = pm.ukrig.reml$tausq))
          
          
        }
      }
    }
    cov.pars.uk <- as.numeric(apply(cov.pars.daily, 2, mean))
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
          cat("   the ", City, "th City: ", City.Name[City], "!!!\n\n")
          cat("   year:", Year, "; month: ", Month, "; day: ", Day,". \n\n")
          cat("...................UK.................\n\n")
          # data preparation 
          Da.mod <- Base_Tab[CITY %nin% City.Name[City] & DAY == Day, ]
          Da.pre <- Base_Tab[CITY %in% City.Name[City] & DAY == Day, ]
          # Da.mod$PM25 = (Da.mod$PM25)
          Da.pre$REAL_PM25 <- Da.pre$REAL_PM25^2
          setDF(Da.mod);setDF(Da.pre)
          # Da.mod$CMAQ_PM25_30 = (Da.mod$CMAQ_PM25_30)
          # Da.pre$CMAQ_PM25_30 = (Da.pre$CMAQ_PM25_30)
          # Da.mod <- Da.mod[, c(Covariate, "REAL_PM25", "LON_X", "LAT_Y")]
          # Da.pre <- Da.pre[, c(Covariate, "REAL_PM25", "LON_X", "LAT_Y")]
          
          # geodata object with observed daily PM concentration and daily CMAQ output
          pm.ukrig.df <- data.frame(cbind(Da.mod[, "LON_X"],
                                          Da.mod[, "LAT_Y"],
                                          (Da.mod[, "REAL_PM25"]),
                                          Da.mod[, Covariate]))
          pm.ukrig.train.geo <- as.geodata(pm.ukrig.df,
                                           coords.col=c(1,2), 
                                           data.col=c(3:ncol(pm.ukrig.df)))
          pm.ukrig.test.df <- data.frame(cbind(Da.pre[, "LON_X"],
                                               Da.pre[, "LAT_Y"],
                                               (Da.pre[, "REAL_PM25"]),
                                               Da.pre[, Covariate]))
          pm.ukrig.test.geo <- as.geodata(pm.ukrig.test.df,
                                          coords.col=c(1,2), 
                                          data.col=c(3:ncol(pm.ukrig.test.df)))
          
          
          # Specifying all the options for Universal Kriging
          kc.uk.control <- krige.control(type.krige="ok",
                                         trend.d=~pm.ukrig.train.geo$data[, -1],
                                         trend.l=~pm.ukrig.test.geo$data[, -1],
                                         cov.model="exponential",
                                         cov.pars=c(cov.pars.uk[1],cov.pars.uk[2]),
                                         nugget=cov.pars.uk[3],
                                         lambda=1)
          
          # Predicting at test sites via Universal Kriging
          pred.uk.day <- krige.conv(pm.ukrig.train.geo, 
                                    coords=pm.ukrig.train.geo$coords,
                                    data=pm.ukrig.train.geo$data[,1],
                                    locations=pm.ukrig.test.geo$coords,
                                    krige=kc.uk.control)
          
          
          testPred <- ifelse(pred.uk.day$predict < 0, 0, pred.uk.day$predict)
          PM25.Pred.sd <- 2*testPred * sqrt(as.numeric(pred.uk.day$krige.var))
          PM25.Pred <- testPred^2
          
          PM25.L25 <- PM25.Pred + 
            qnorm(0.025)*PM25.Pred.sd#*testPred
          PM25.U95 <- PM25.Pred + 
            qnorm(0.975)*PM25.Pred.sd#*testPred
          
          
          PM25.L25 <- ifelse(PM25.L25 < 0, 0, PM25.L25)
          PM25.U95 <- ifelse(PM25.U95 < 0, 0, PM25.U95)
          
          # IGN <- verification::crps(Da.pre$REAL_PM25,
          #                           cbind(PM25.Pred, (PM25.Pred)))$IGN
          # interval_range <- rep(95, length(Da.pre$REAL_PM25))
          # alpha <- (100 - interval_range) / 100
          # lower <- qnorm(alpha / 2, sqrt(PM25.Pred))#quantile(PM25.Pred, probs = alpha / 2)#qnorm(alpha / 2, Da.pre$REAL_PM25)
          # upper <- qnorm((1 - alpha / 2), sqrt(PM25.Pred))#quantile(PM25.Pred, probs = (1 - alpha / 2))#qnorm((1 - alpha / 2), Da.pre$REAL_PM25)
          # 
          # INS <-  scoringutils::interval_score(
          #   true_values = sqrt(Da.pre$REAL_PM25),
          #   lower = lower,
          #   upper = upper,
          #   interval_range = 95,
          #   weigh = TRUE,
          #   separate_results = FALSE
          # )
          spT <- spT_validation(z = Da.pre$REAL_PM25, 
                                zhat = PM25.Pred, 
                                sigma = PM25.Pred.sd,
                                zhat.Ens = NULL, 
                                names = F, CC = F)#[c(1, 4)]
          print(spT)
          Coverage <- mean(PM25.L25 < Da.pre$REAL_PM25 & PM25.U95 > Da.pre$REAL_PM25)
          
          
          # Kriging.Fit
          if(City == 1 & Year == year_range[1]  & Month == month_range[1] & Day == day_range[1])
          {
            
            UK <- data.frame(Da.pre[, colNames], PM25.Pred = PM25.Pred,
                             low.Pred = PM25.L25, upp.Pred = PM25.U95,
                             PM25.Pred.sd = PM25.Pred.sd,
                             Coverage = Coverage,
                             INS = spT["INS"]
            )
          }else{
            UK <- rbind(UK
                        , data.frame(Da.pre[, colNames], PM25.Pred = PM25.Pred,
                                     low.Pred = PM25.L25, upp.Pred = PM25.U95,
                                     PM25.Pred.sd = PM25.Pred.sd,
                                     Coverage = Coverage,
                                     INS = spT["INS"])
            )
          }
          temp0 <- Validation.Group.Region(UK, sigma = UK$PM25.Pred.sd, 
                                           col = c("REAL_PM25", "PM25.Pred"),
                                           by = "CITY")
          cat("\n.............................\n")
          print(temp0)
          cat(".....................UK....................\n\n")
        }
      }
    }
  }
}
setDT(UK)
# save(UK, file = paste0(file, "/UK_W.RData"))
# temp1 <- ADCM::Validation.Group.Region(UK, 
#                                col = c("REAL_PM25", 
#                                        "PM25.Pred"), 
#                                by = "CITY") 
da <- plyr::ddply(UK
                    , .(CITY) #, DAY, DATE_TIME
                    , plyr::summarize
                    , Coverage = mean(Coverage)
                    # , INS = mean(INS)
                    , .progress = "text")
setnames(temp0, "Group", "CITY")
temp <- temp0 %>% left_join(da, by = "CITY")
temp$Coverage <- round(ifelse(is.na(temp$Coverage), mean(temp$Coverage, na.rm = T),
                        temp$Coverage), 4)
temp$INS <- round(ifelse(is.na(temp$INS), mean(temp$INS, na.rm = T),
                              temp$INS), 4)
temp
writexl::write_xlsx(temp, path = "./Result/UK_cv.xlsx")
