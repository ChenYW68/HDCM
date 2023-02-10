rm(list=ls())
source("./LoadPackages/RDependPackages.R")
data("SiteData", package = "HDCM")
data("China_BTH_GeoMap", package = "HDCM")
setDF(obs_PM25_2015w);setDF(Site);
# library(brinla)



DATE_TIME <- unique(obs_PM25_2015w$DATE_TIME) %>% sort()
Nt <- length(DATE_TIME)
date.time <- data.frame(time.index = 1:Nt,
                        time.scale = seq(0, 1, , Nt),
                        time.scale.sin = sin(seq(0, 1, , Nt)/(0.03*pi)),
                        time.scale.cos = cos(seq(0, 1, , Nt)/(0.03*pi)),
                        DATE_TIME = DATE_TIME)
PM25_2015w <- obs_PM25_2015w  %>% left_join(date.time, by = c("DATE_TIME"))



Map_BTH <- fortify(BTH_City_Map) 
setnames(Map_BTH, c("long", "lat"), c("LON", "LAT"))
setDF(Map_BTH)
Map_BTH <- ADCM::spCoords.transform(Map_BTH, method = 1)
colnames(Map_BTH)
global.coords = as.matrix(Map_BTH[, c("LON", "LAT")])


mesh <- inla.mesh.2d(
  loc.domain = global.coords,
  loc = Site[, c("LON", "LAT")],
  max.edge = c(.35, .65), 
  offset = c(1e-1, 0.5), 
  cutoff = 0.04,
)
data <- as.data.frame(Site)
coordinates(data) = ~ LON + LAT
p <- ggplot() + gg(mesh) + 
  ggtitle(paste("Vertices: ", mesh$n)) +
  geom_polygon(data = Map_BTH,
               aes(x = LON, y = LAT, group = group),
               colour = 'black',
               fill = NA) +
  inlabru::gg(data, col = "red", size = 0.1, pch = "+" ) +
  theme_bw() + 
  labs(x =  "longitude", y = "latitude") +
  theme(axis.text = element_text(size = 8, colour = "black")
        , axis.title = element_text(size = 14, colour = "black")
        # , legend.title = element_text(size = 12, colour = "black")
        # , legend.text = element_text(size = 8, colour = "black")
  )
p

## ################################
## Make the SPDE object and the formula
## ################################
##--- Construct the SPDE object
# spde = inla.spde2.matern(mesh=mesh)


Nt <- length(unique(PM25_2015w$time.index))
region <- sort(as.character(unique(PM25_2015w$CITY)))
region_num <- 1:length(region)
setDF(PM25_2015w)


Covariate <- c("sim50_CMAQ_PM25" 
              , "sim_TEMP"
              , "sim_WIND_X"
              , "sim_WIND_Y"
              , "time.scale"
);


Cov.Index <- which(base::colnames(PM25_2015w) %in% Covariate)
PM25_2015w[, c("REAL_PM25", "sim50_CMAQ_PM25")] <- 
  sqrt(PM25_2015w[,  c("REAL_PM25", "sim50_CMAQ_PM25")])


setDF(PM25_2015w)
Cov.Index <- which(base::colnames(PM25_2015w) %in% Covariate)
if(length(Cov.Index) > 2){
  mean_covariates <- apply(PM25_2015w[, Cov.Index], 2, mean)
  sd_covariates <- apply(PM25_2015w[, Cov.Index], 2, sd)
  PM25_2015w[, Cov.Index] <- scale(PM25_2015w[, Cov.Index],
                                   center = mean_covariates,
                                   scale = sd_covariates)
}


colNames <- c("CITY","LON", "LAT", "DATE_TIME",
              "YEAR_MONTH", "YEAR","MONTH","DAY",
              "REAL_PM25")

setDT(PM25_2015w)
for(r in region_num){
  cat(paste0("\n\n   The ", r, "th region: ", region[r], "!!!\n\n"))
  Da.mod <- PM25_2015w[CITY %nin% region[r], ]
  Da.pre <- PM25_2015w[CITY %in% region[r], ]
  colnames(Da.mod)
  setDF(Da.mod);setDF(Da.pre);
  #-------------------------------------------------------------------
  #-- Create A matrices for the estimation part --#
  n <- length(unique(Da.mod$ID))
  A.est =
    inla.spde.make.A(mesh,
                     loc = cbind(Da.mod$LON, Da.mod$LAT),
                     group = Da.mod$time.index,
                     n.group = Nt)

  #-------------------------------------------------------------------
  field.indices = inla.spde.make.index("field",
                         n.spde = mesh$n,
                         n.group = Nt) 
  
  stack.est =  inla.stack(data = list(PM25 = Da.mod$REAL_PM25),
               A = list(A.est, 1),
               effects = list(c(field.indices,
                         list(Intercept = 1)),
                         list(Da.mod[, Cov.Index])),
               tag = "est")
  
  
  # test
  A.pred =
    inla.spde.make.A(mesh, loc = cbind(Da.pre$LON, Da.pre$LAT),
                     group= Da.pre$time.index, n.group = Nt)
  
  stack.pred =   inla.stack(data = list(PM25 = NA),
               A = list(A.pred, 1),
               effects = list(c(field.indices,
                         list(Intercept = 1)),
                         list(Da.pre[, Cov.Index])),
               tag = "pred")
  
  #-- Create the "full" stack object (estimation + prediction)
  stack = inla.stack(stack.est, stack.pred)
  
  spde <- inla.spde2.matern(mesh, alpha = 1, constr = FALSE)
  

  formula <- (PM25 ~ 1 + sim50_CMAQ_PM25 + 
                sim_TEMP + sim_WIND_X +
                sim_WIND_Y +
                f(field, model = spde, group = field.group, 
                  control.group = list(model = "ar1")))
  
  t1 <- proc.time()
  mod <- inla(formula,
         data = inla.stack.data(stack, spde = spde),
         family = "gaussian",
         control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
         control.compute = list(openmp.strategy = "large"),
         keep = FALSE, 
         verbose = FALSE, 
         num.threads = 1:1,
         control.inla = list(reordering = "metis", 
                             strategy = "gaussian",
                             int.strategy = "ccd"
                             ))
  t2 <- proc.time()
  cat("\n\nEstimation of INLA takes time: \n")
  print(t2 - t1)
  
  print(summary(mod))
  ## ############################
  ## Mapping
  ## ############################
  ##--- Posterior mean of the linear predictor
  index.pred <- inla.stack.index(stack, "pred")$data
  # View(mod)
  pred.mean <- mod$summary.linear.predictor[index.pred, "mean"]
  Pred.sd <- mod$summary.linear.predictor[index.pred, "sd"]
  
  PM25.U95 <- mod$summary.linear.predictor[index.pred, "0.975quant"] #+ 
    # mod$summary.random$field[index.pred, "0.975quant"]
  PM25.Pred <- mod$summary.linear.predictor[index.pred, "0.5quant"] #+ 
    # mod$summary.random$field[index.pred, "0.5quant"]
  PM25.L25 <-  mod$summary.linear.predictor[index.pred, "0.025quant"] #+ 
    # mod$summary.random$field[index.pred, "0.025quant"]
  
  
  Pred.sd <- 2*pred.mean*as.vector(Pred.sd)
  Da.pre$REAL_PM25 <- Da.pre$REAL_PM25^2
  
  PM25.U95 <- ifelse(PM25.U95 < 0, 0, PM25.U95^2)
  PM25.Pred <- ifelse(PM25.Pred < 0, 0, PM25.Pred^2)
  PM25.L25 <- ifelse(PM25.L25 < 0, 0, PM25.L25^2)

  
  spT <- spT_validation(z = Da.pre$REAL_PM25, 
                        zhat = PM25.Pred, 
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
    
    Inla <- data.frame(Da.pre[, colNames]
                      , PM25.L25 = PM25.L25
                      , PM25.Pred = PM25.Pred
                      , PM25.U95 = PM25.U95
                      , Pred.sd = Pred.sd
    #Beta <- beta
    
  }else{
    Inla <- rbind(Inla, 
                 data.frame(Da.pre[, colNames]
                              , PM25.L25 = PM25.L25
                              , PM25.Pred = PM25.Pred
                              , PM25.U95 = PM25.U95
                              , Pred.sd = Pred.sd
                 ))
    #Beta <- rbind(Beta, beta)
  }
  # Beta <- as.data.frame(Beta)
  # 
 
  temp0 <- Validation.Group.Region(Inla, 
                                   col = c("REAL_PM25",
                                           "PM25.Pred"),
                                   by = "CITY")
  cat("\n.............................\n")
  print(temp0)
}

# writexl::write_xlsx(temp, path = "./Result/INLAw_cv.xlsx")
writexl::write_xlsx(Inla, path = "./Result/pred_INLAw_cv.xlsx")

