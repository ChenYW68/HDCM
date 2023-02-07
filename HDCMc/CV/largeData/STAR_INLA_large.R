rm(list=ls())
source("./R/PSTVB_Packages.R")
source("./R/util.R")
load("./data/NAQPMS_CMAQ_Dataset_2015W.RData")
Site <- Site[distToNearestCAMQpoint <= 15]
set.seed(12345)


n.train <- 5000
train.id <- sample(Site$ID, n.train, replace = F)
cat("The number of training set:", length(train.id))

PM25_2015w <- NAQPMS_CMAQ_Dataset_2015W[distToNearestCAMQpoint <= 15]
Site$Flag <- ifelse(Site$ID %in% train.id, "train", "test")
PM25_2015w$Flag <- ifelse(PM25_2015w$ID %in% train.id, "train", "test")
rm(NAQPMS_CMAQ_Dataset_2015W)
# YearMonth <- c(201511)
# PM25_2015w <- obs_PM25_2015w %>% filter(YEAR_MONTH %in% YearMonth)
PM25_2015w <- PM25_2015w %>%
  dplyr::filter(
    # YEAR %in% year,
    between((as.Date(DATE_TIME)),
            (as.Date(paste0(2015, "-", "11-01"))),
            (as.Date(paste0(2015, "-", "11-30"))))#,
    # between(MONTH, 10, 12),
  )
colnames(PM25_2015w)
range(PM25_2015w$PM25)

setDF(PM25_2015w);setDF(Site);

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
                        time.scale.sin = sin(seq(0, 1, , Nt)/(0.03*pi)),
                        time.scale.cos = cos(seq(0, 1, , Nt)/(0.03*pi)),
                        DATE_TIME = DATE_TIME)
PM25_2015w <- PM25_2015w  %>% left_join(date.time, by = c("DATE_TIME"))
# china.county.map <- rgdal::readOGR(paste0("./DataProcess/Gadm36_CHN_shp/."),"gadm36_CHN_1"
#                                    , stringsAsFactors=FALSE
#                                    , encoding = "UTF-8"
#                                    , use_iconv = T)
# china.county.map <- china.county.map[china.county.map$NAME_1 %in%
#                                        c("Beijing", "Tianjin", "Hebei"
#                                          ,"Shandong", "Nei Mongol"
#                                          ,"Shanxi", "Henan","Liaoning"
#                                          , "Shaanxi", "Jilin", "Shanghai"
#                                          , "Jiangsu", "Heilongjiang", "Anhui"
#                                        ),]

load("./data/Large_BTH_map.RData")
Map_BTH <- fortify(larg_bth_map)
setnames(Map_BTH, c("long", "lat"), c("LON", "LAT"))
global.coords = as.matrix(Site[, c("LON", "LAT")])
mesh <- inla.mesh.2d(
                    #boundary = boundary
                    loc.domain = global.coords,
                    loc = Site[, c("LON", "LAT")],#[Site$Flag == "train", c("LON", "LAT")],
                    ## 10103
                    max.edge = c(.21, .3), #0.3,0.7
                    offset = c(1e-1, 0.9), #0.4, 0.6
                    cutoff = 0.11,
                    ## 2042
                    # max.edge = c(.35, .7), #0.3,0.7
                    # offset = c(1e-1, 0.6), #0.4, 0.6
                    # cutoff = .23 # 0.5
                    ##3158
                    # max.edge = c(.23, .4), #0.3,0.7
                    # offset = c(1e-1, 0.9), #0.4, 0.6
                    # cutoff = 0.3
                  )
mesh$n


data <- as.data.frame(Site[Site$Flag == "train", c("LON", "LAT")])
coordinates(data) = ~ LON + LAT
p <- ggplot() + inlabru::gg(mesh) + geom_sf(col = "black") +
  ggtitle(paste("Vertices: ", mesh$n)) +
  geom_polygon(data = Map_BTH,
               aes(x = LON, y = LAT, group = group),
               colour = 'black',
               fill = NA) +
  # coord_sf(datum = st_crs(5880)) +
  inlabru::gg(data, col = "red", size = 2, pch = "+" ) +
  theme_bw() + #coord_fixed() +
  labs(x =  "Longitude", y = "Latitude") +
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
region <- sort(as.character(unique(PM25_2015w$Flag)))
region_num <- 1:length(region)
setDF(PM25_2015w)


Covariate <- c("sim_CMAQ_PM25"
               , "TEMP"
               , "WIND_X"
               , "WIND_Y"
               , "time.scale"
);
# fmla <- as.formula(paste0("sqrt(PM25)~", paste(Covariate, collapse = "+")))
Cov.Index <- which(base::colnames(PM25_2015w) %in% Covariate)
PM25_2015w[, c("PM25", "sim_CMAQ_PM25")] <-
  sqrt(PM25_2015w[,  c("PM25", "sim_CMAQ_PM25")])


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

d <- rdist(Site[, 2:3], Site[, 2:3])
colNames <- c("Flag", "LON", "LAT", "DATE_TIME",
              "YEAR_MONTH", "YEAR", "MONTH", "DAY",
              "PM25")

#------------------------------------------------------
start.time <- Sys.time()
for(r in 1){
  setDT(PM25_2015w)
  cat(paste0("\n\n   The ", r, "th region: ", region[r], "!!!\n\n"))
  Da.mod <- PM25_2015w[Flag %in% "train", ]
  Da.pre <- PM25_2015w[Flag %nin% "train", ]
  colnames(Da.mod)
  setDF(Da.mod);setDF(Da.pre);
  #-------------------------------------------------------------------
  #-- Create A matrices for the estimation part --#
  n <- length(unique(Da.mod$ID))
  A.est <- inla.spde.make.A(mesh,
                     loc = cbind(Da.mod$LON, Da.mod$LAT),
                     group = Da.mod$time.index,
                     n.group = Nt)

  #-------------------------------------------------------------------
  field.indices <- inla.spde.make.index("field",
                                       n.spde = mesh$n,
                                       n.group = Nt)

  stack.est <-  inla.stack(data = list(PM25 = Da.mod$PM25),
                          A = list(A.est, 1),
                          effects = list(c(field.indices,
                                           list(Intercept = 1)),
                                         list(Da.mod[, Cov.Index])),
                          tag = "est")


  # test
  A.pred <- inla.spde.make.A(mesh, loc = cbind(Da.pre$LON, Da.pre$LAT),
                     group = Da.pre$time.index, n.group = Nt)

  stack.pred <- inla.stack(data = list(PM25 = NA),
                            A = list(A.pred, 1),
                            effects = list(c(field.indices,
                                             list(Intercept = 1)),
                                           list(Da.pre[, Cov.Index])),
                            tag = "pred")

  #-- Create the "full" stack object (estimation + prediction)
  stack <- inla.stack(stack.est, stack.pred)
  # stack = inla.stack(stack.est)

  prior.range <- c(1e-10, 10)
  # prior.range <- c(max(c(diff(range(Da.mod$LON)),
  #                        diff(range(Da.mod$LAT)))) * 1/20, 10)
  ## Medium sd, relative half-spread factor
  prior.sigma <- c(sd(Da.mod$PM25, na.rm = TRUE) / 2, 1)


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
                            # prior.range.nominal = theta.prior.mean,
                            # theta.prior.mean = theta.prior.mean,
                            # theta.prior.prec = theta.prior.prec,
                            constr = FALSE)

  hyper.range.initial <- log(prior.range[1])
  hyper.sigma.initial <- log(prior.sigma[1])

  # spde$f$hyper.default$theta1$initial <- hyper.range.initial
  # spde$f$hyper.default$theta2$initial <- hyper.sigma.initial


  #-- Define the formula --#-1 + Intercept +
  formula <- (PM25 ~ 1 + sim_CMAQ_PM25 +
                TEMP + WIND_X +
                WIND_Y + #time.scale +
                # f(time, CMAQ_PM25_30, model="rw2", scale.model = TRUE) +
                # f(inla.group(REAL_TEMP, n = 1e2), model="rw2", scale.model = TRUE) + +
                # f(inla.group(REAL_DEWP, n = 1e2), model="rw2", scale.model = TRUE) +
                # f(inla.group(REAL_LON_WIND, n = 1e2),
                #   model="rw2", scale.model = TRUE) +
                # f(inla.group(REAL_LAT_WIND, n = 1e2),
                #   model="rw2", scale.model = TRUE) +
                f(field, model = spde, group = field.group,
                  control.group = list(model = "ar1"))
              )

  t1 <- proc.time()
  mod <- inla(formula,
              data = inla.stack.data(stack, spde = spde),
              family = "gaussian",
              # control.family = list(hyper = list(theta = list(initial = 2))),
              control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
              control.compute = list(openmp.strategy = "large"),
              # control.mode=list( theta = log(rep(1e1, 4)), restart=FALSE),
              # keep = FALSE,
              verbose = TRUE,
              num.threads = 1:1,
              # control.fixed = list(prec.intercept = 1),
              control.inla = list(reordering = "metis",
                                  # reordering = "amdc",
                                  tolerance = 1e-3,
                                  # numint.maxfeval = 1e2,
                                  numint.relerr = 1e-3,
                                  # numint.abserr = 1e-2,
                                  # tolerance = 1e-2,
                                  # tolerance.f = 1e-2,
                                  # tolerance.g = 1e-2,
                                  # tolerance.x = 1e-2,
                                  # tolerance.step = 1e-2,
                                  # stupid.search.max.iter = 10,
                                  # optimise.strategy = "plain",
                                  # # h = 1e-2,
                                  optimiser = "gsl",
                                  strategy  = "gaussian",
                                  int.strategy = "ccd",
                                  # force.diagonal = T,
                                  stupid.search = F
                                  )
              )
  t2 <- proc.time()
  cat("\n\nEstimation of INLA takes time: \n")
  print(t2 - t1)

  print(summary(mod))

  output.field <- inla.spde2.result(inla=mod,
               name="field",
               spde=spde, do.transf=TRUE)
  inla.emarginal(function(x) x,
                 output.field$marginals.kappa[[1]])

  inla.emarginal(function(x) x,
                 output.field$marginals.variance.nominal[[1]])
  inla.emarginal(function(x) x,
                output.field$marginals.range.nominal[[1]])



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
  PM25.L25 <-  mod$summary.linear.predictor[index.pred, "0.025quant"]

  Pred.sd <- 2*pred.mean*as.vector(Pred.sd)
  Da.pre$PM25 <- Da.pre$PM25^2

  PM25.U95 <- ifelse(PM25.U95 < 0, 0, PM25.U95^2)
  PM25.Pred <- ifelse(PM25.Pred < 0, 0, PM25.Pred^2)
  PM25.L25 <- ifelse(PM25.L25 < 0, 0, PM25.L25^2)
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

    Inla <- data.frame(Da.pre[, colNames]
                       , PM25.L25 = PM25.L25
                       , PM25.Pred = PM25.Pred
                       , PM25.U95 = PM25.U95
                       , Pred.sd = Pred.sd
                       , Coverage = Coverage
                       # , INS = spT["INS"]
    )

  }else{
    Inla <- rbind(Inla,
                  data.frame(Da.pre[, colNames]
                             , PM25.L25 = PM25.L25
                             , PM25.Pred = PM25.Pred
                             , PM25.U95 = PM25.U95
                             , Pred.sd = Pred.sd
                             , Coverage = Coverage
                             # , INS = spT["INS"]
                  ))
  }
  # Beta <- as.data.frame(Beta)
  #

  temp0 <- Validation.Group.Region(Inla, sigma = Inla$Pred.sd,
                                   col = c("PM25", "PM25.Pred"),
                                   by = "Flag")
  cat("\n.............................\n")
  print(temp0)
}
end.time <- Sys.time()
print(end.time - start.time)
writexl::write_xlsx(temp0, path = paste0("./Result/STARxz_INLAw_large_",
                                         mesh$n,"_cv.xlsx"))
writexl::write_xlsx(Inla, path = paste0("./Result/pred_STARxz_INLAw_large_",
                                        mesh$n,"_cv.xlsx"))
