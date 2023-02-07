rm(list=ls())
source("./R/PSTVB_Packages.R")
source("./R/util.R")
data("China_BTH_GeoMap", package = "ADCM")
data("SiteData", package = "ADCM")
setDF(obs_PM25_2015w);setDF(Site);

INDX <- which(colnames(Site) %in% c("LON_X", "LAT_Y"))
Site <- ADCM::spCoords.transform(Site[, -INDX], method = 1)
setDF(obs_PM25_2015w)
INDX <- which(colnames(obs_PM25_2015w) %in% c("LON_X", "LAT_Y"))
PM25_2015w <- ADCM::spCoords.transform(obs_PM25_2015w[, -INDX],
                                           method = 1)


DATE_TIME <- unique(PM25_2015w$DATE_TIME) %>% sort()
Nt <- length(DATE_TIME)
date.time <- data.frame(time.index = 1:Nt,
                        time.scale = seq(0, 1, , Nt),
                        time.scale.sin = sin(seq(0, 1, , Nt)/(0.03*pi)),
                        time.scale.cos = cos(seq(0, 1, , Nt)/(0.03*pi)),
                        DATE_TIME = DATE_TIME)
PM25_2015w <- PM25_2015w  %>% left_join(date.time, by = c("DATE_TIME"))


load("./data/bth_map.RData")
Map_BTH <- fortify(bth_map) 
setnames(Map_BTH, c("long", "lat"), c("LON", "LAT"))
setDF(Map_BTH)
Map_BTH <- ADCM::spCoords.transform(Map_BTH, method = 1)
colnames(Map_BTH)
global.coords = as.matrix(Map_BTH[, c("LON", "LAT")])


mesh <- inla.mesh.2d(
  #boundary = boundary
  loc.domain = global.coords,
  loc = Site[, c("LON", "LAT")],
  # max.edge = c(.35, .65), #0.3,0.7
  # offset = c(1e-1, 0.5), #0.4, 0.6
  # cutoff = .05 # 0.5
  max.edge = c(.35, .65), #0.3,0.7
  offset = c(1e-1, 0.5), #0.4, 0.6
  cutoff = 0.04,
)
data <- as.data.frame(Site)
coordinates(data) = ~ LON + LAT
p <- ggplot() + gg(mesh) + #geom_sf(col = "black") +
  ggtitle(paste("Vertices: ", mesh$n)) +
  geom_polygon(data = Map_BTH,
               aes(x = LON, y = LAT, group = group),
               colour = 'black',
               fill = NA) +
  # coord_sf(datum = st_crs(5880)) +
  inlabru::gg(data, col = "red", size = 0.1, pch = "+" ) +
  theme_bw() + #coord_fixed() +
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
              # , "sim_RHUID"
              # , "sim_SPRESS"
              , "sim_WIND_Y"
              , "sim_WIND_X"
              # , "time.index"
              , "time.scale"
);
# fmla <- as.formula(paste0("sqrt(PM25)~", paste(Covariate, collapse = "+")))
Cov.Index <- which(base::colnames(PM25_2015w) %in% Covariate)
PM25_2015w[, c("REAL_PM25", "sim50_CMAQ_PM25")] <- 
  sqrt(PM25_2015w[,  c("REAL_PM25", "sim50_CMAQ_PM25")])


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

library(brinla)
# ?bri.band.ggplot

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
  
  
  
  prior.range <- c(max(c(diff(range(Da.mod$LON)),
                         diff(range(Da.mod$LAT)))) * 1/20, 3)
  ## Medium sd, relative half-spread factor
  prior.sigma <- c(sd(Da.mod$REAL_PM25, na.rm=TRUE) / 2, 4)
  
  
  spde.alpha <- 1
  ## lognormal prior
  ## Centre the parameterisation at range=1, sigma=1
  nu <- spde.alpha - 1
  kappa.zero <- sqrt(8*nu) / 1
  tau.zero <- (gamma(nu) / (gamma(spde.alpha) * 4*pi * kappa.zero^(2*nu)) )^0.5 / 1
  ## sigma^2 = Gamma(0.5)/( Gamma(1.5) (4\pi)^(dim/2) * kappa^(2*0.5) * tau^2 )
  ## tau = [ Gamma(0.5)/( Gamma(1.5) (4\pi)^(dim/2) * kappa^(2*0.5) ) ]^0.5 / sigma
  ## kappa = sqrt(8*0.5) / range
  ## tau = [ Gamma(0.5)/( Gamma(1.5) (4\pi)^(dim/2) * (sqrt(8*0.5)/range)^(2*0.5) ) ]^0.5 / sigma
  B.tau   <- cbind(log(tau.zero), nu, -1)
  B.kappa <- cbind(log(kappa.zero), -1, 0)
  
  theta.prior.mean <- log(c(prior.range[1], prior.sigma[2]))
  ## Half-width of prior prediction interval, on log-scale: 2*sd = log(rel)
  theta.prior.prec <- 4 / log(c(prior.range[2], prior.sigma[2]))^2
  spde <- inla.spde2.matern(mesh, alpha = spde.alpha,
                             # B.tau = B.tau, B.kappa = B.kappa,
                             # theta.prior.mean = theta.prior.mean,
                             # theta.prior.prec = theta.prior.prec,
                             constr = FALSE)
  
  hyper.range.initial <- log(prior.range[1])
  hyper.sigma.initial <- log(prior.sigma[1])
  
  # spde$f$hyper.default$theta1$initial <- hyper.range.initial
  # # spde$f$hyper.default$theta2$initial <- hyper.sigma.initial
  
  
  #-- Define the formula --#-1 + Intercept +
  formula <- (PM25 ~ 1 + sim50_CMAQ_PM25 + 
                sim_TEMP + sim_WIND_X +
                sim_WIND_Y + #time.scale +
                # f(time.scale, model = "rw1", scale.model = TRUE) +
                # f(sim_WIND_X, sim50_CMAQ_PM25, model = "iid") +
                # f(inla.group(REAL_TEMP, n = 1e2), model="rw2", scale.model = TRUE) + +
                # f(inla.group(REAL_DEWP, n = 1e2), model="rw2", scale.model = TRUE) +
                # f(inla.group(REAL_LON_WIND, n = 1e2), 
                #   model="rw2", scale.model = TRUE) +
                # f(inla.group(REAL_LAT_WIND, n = 1e2), 
                #   model="rw2", scale.model = TRUE) +
                f(field, model = spde, group = field.group, 
                  control.group = list(model = "ar1")))
  
  t1 <- proc.time()
  mod <- inla(formula,
         data = inla.stack.data(stack, spde = spde),
         family = "gaussian",
         # control.family = list(hyper = list(theta = list(initial = 2))),
         control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
         control.compute = list(openmp.strategy = "large"),
         # control.mode=list(theta=mod.mode$mode$theta, restart=FALSE),
         keep = FALSE, 
         verbose = FALSE, 
         num.threads = 1:1,
         control.inla = list(reordering = "metis", 
                             # tolerance = 1e-5, 
                             # numint.maxfeval = 1E2,
                             # numint.relerr = 1e-2,
                             # numint.abserr = 1e-3,
                             # h = 1e-5,
                             strategy = "gaussian",
                             int.strategy = "ccd"#,
                             # force.diagonal = TRUE,
                             # stupid.search = FALSE
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
  pred.mean <-  mod$summary.linear.predictor[index.pred, "mean"]
  Pred.sd <- mod$summary.linear.predictor[index.pred, "sd"]
  
  PM25.U95 <-  mod$summary.linear.predictor[index.pred, "0.975quant"] #+ 
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
  Coverage <- mean(PM25.L25 < Da.pre$REAL_PM25 & PM25.U95 > Da.pre$REAL_PM25)
  # lp_grid_sd[index.mountains]=NA
  # PM25.Pred <- ifelse(PM25.Pred^2 >= 800, 800, PM25.Pred^2)
  
  
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
    
    Inla <- data.frame(Da.pre[, colNames]
                      , PM25.L25 = PM25.L25
                      , PM25.Pred = PM25.Pred
                      , PM25.U95 = PM25.U95
                      , Pred.sd = Pred.sd
                      , Coverage = Coverage
                      , INS = spT["INS"]
    )
    #Beta <- beta
    
  }else{
    Inla <- rbind(Inla, 
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
 
  temp0 <- Validation.Group.Region(Inla, sigma = Inla$Pred.sd, 
                                   col = c("REAL_PM25",
                                           "PM25.Pred"),
                                   by = "CITY")
  cat("\n.............................\n")
  print(temp0)
}
temp0$CVG <- NULL
da <- plyr::ddply(Inla
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
# temp$INS <- round(ifelse(is.na(temp$INS), 
#                               mean(temp$INS, na.rm = T),
#                               temp$INS), 4)
temp
# writexl::write_xlsx(temp, path = "./Result/INLAw_cv.xlsx")
# writexl::write_xlsx(Inla, path = "./Result/pred_INLAw_cv.xlsx")

