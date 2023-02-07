rm(list=ls())
source("./R/PSTVB_Packages.R")
source("./R/util.R")
load("./data/NAQPMS_CMAQ_Dataset_2015W.RData")
Site <- Site[distToNearestCAMQpoint <= 15]
set.seed(12345)


n.train <- 2500
train.id <- sample(Site$ID, n.train, replace = F)
cat("The number of training set:", length(train.id))

PM25_2015w <- NAQPMS_CMAQ_Dataset_2015W[distToNearestCAMQpoint <= 15]
Site$Flag <- ifelse(Site$ID %in% train.id, "train", "test")
PM25_2015w$Flag <- ifelse(PM25_2015w$ID %in% train.id, "train", "test")

{
  PM25_2015w <- PM25_2015w %>% setorderv(c("Flag", "ID","DATE_TIME")) 
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
  PM25_2015w[, c("PM25", "sim_CMAQ_PM25")] <-
    sqrt(PM25_2015w[, c("PM25", "sim_CMAQ_PM25")])
  
  Covariate = c("sim_CMAQ_PM25" 
                , "TEMP"
                , "WIND_X"
                , "WIND_Y"
  );
  
  PM25_2015w <- PM25_2015w %>%
    dplyr::filter(
      # YEAR %in% year,
      between((as.Date(DATE_TIME)), 
              (as.Date(paste0(2015, "-", "11-01"))), 
              (as.Date(paste0(2015, "-", "11-30"))))#,
      # between(MONTH, 10, 12),
    )
  
  # Cova <- which(base::colnames(PM25_2015w) %in% Covariate)
  # if(length(Covariate) > 1){
  #   for(k in 2:(length(Covariate)))
  #   {
  #     PM25_2015w[, Cova[k]] = scale(center = T, as.vector(
  #       PM25_2015w[, Cova[k]]))[, 1]
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
}
region <- sort(as.character(unique(PM25_2015w$Flag)))
region_num <- 1:length(region)

colNames <- c("CITY","LON", "LAT", "DATE_TIME",
              "YEAR_MONTH", "YEAR","MONTH","DAY",
              "PM25")

setDT(PM25_2015w)
start.time <- Sys.time()
for(r  in 1)
{
  
  set.seed(1234)
  cat(paste0("The ", r, "th region: ", region[r], "!!!\n\n"))
  Da.mod <- PM25_2015w[Flag %in% "train", ]
  Da.pre <- PM25_2015w[Flag %nin% "train", ]
  setDF(Da.mod);setDF(Da.pre);
  Da.mod$intercept <- 1
  Da.pre$intercept <- 1
  
  # model
  post.gp <- spTDyn::GibbsDyn(PM25 ~ 1 + sim_CMAQ_PM25 +
                                # tp(intercept) +
                                tp(sim_CMAQ_PM25) +
                                (TEMP) +
                                (WIND_X) +
                                (WIND_Y)
                              , coords =~LON_X + LAT_Y
                              , nBurn = 5e3
                              , nItr = 1e4
                              , data = Da.mod
                              , report = 1E4
                              , distance.method = "geodetic:mile"
                              , prior = priors(inv.var.prior = Gamm(a = 2, b = 1),
                                               beta.prior = Norm(0, 1E4), 
                                               rho.prior = Norm(0, 1E4))
                              , initials = initials(rhotp = .1,
                                                    phi = 1e-5)
                              , scale.transform = "NONE"
                              , spatial.decay = decay(
                                distribution = Unif(1/1e7, 1e0), 
                                tuning = 1E-8)
  )
  Da.pre$PM25 <- Da.pre$PM25^2
  
  
  Y.pred <- predict(post.gp, newdata = Da.pre, newcoords =~LON_X + LAT_Y)

  Y.pred$pred.samples <- ifelse(Y.pred$pred.samples < 0, 0, 
                                Y.pred$pred.samples^2)
  
  
  y.pred <- apply(Y.pred$pred.samples, 1, quant)
  Pred.sd <- apply(Y.pred$pred.samples, 1, sd)
  
  PM25.U95 <-  y.pred[3, ]#^2
  PM25.Pred <- y.pred[2, ]#^2
  PM25.L25 <-  y.pred[1, ]
  
  Coverage <- mean(PM25.L25 < Da.pre$PM25 & PM25.U95 > Da.pre$PM25)
  
  
  spT <- spT_validation(z = Da.pre$PM25, 
                        zhat = PM25.Pred, 
                        sigma = Pred.sd,
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
  
  temp0 <- Validation.Group.Region(STVC, sigma = STVC$Pred.sd, 
                                   col = c("PM25",
                                           "PM25.Pred"),
                                   by = "CITY")
  cat("\n.............................\n")
  print(temp0)
  
  
}
end.time <- Sys.time()
print(end.time - start.time)




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
temp$INS <- round(ifelse(is.na(temp$INS), 
                         mean(temp$INS, na.rm = T),
                         temp$INS), 4)
temp
writexl::write_xlsx(temp, path = "./Result/large_STVC_cv.xlsx")


