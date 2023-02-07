#####################################################################
#####################################################################
# load("./data/CMAQ_Site.Rdata")
# load("./data/Site.RData")
# data("SiteData", package = "stBase")
# data("GeoMap", package = "stBase")
file <- "./Calibration/Result/"
source("./R/CreateGrid.R")
source("./R/Construct_HDCM_Data.R")
load("./data/bth_map.RData")
load(paste0(file, "/pred.location.grid.output.RData"))
load("./data/SiteData.RData")
#####################################################################
Year <- rep(2015, 2)
# month_day <- c("11-26", "12-01")
month_day <- c("12-17", "12-22")
###########################################################################
# YearMonth <- c(201511)
# Day <- 25:30#27:31#
#####################################################################
Cali.Data <- pred.location.grid.output %>%
  dplyr::filter(
    # YEAR %in% year,
    between((as.Date(DATE_TIME)),
            (as.Date(paste0(Year[1], "-", month_day[1]))),
            (as.Date(paste0(Year[2], "-", month_day[2]))))#,
    # between(MONTH, 10, 12),
  ) %>% setorderv(c("sample.id", "DATE_TIME"))

DATE_TIME <- unique(seq(as.Date("2015/11/1"), as.Date("2016/1/31"), by = "day")) %>%
  sort()
Nt <- length(DATE_TIME)
date.time <- data.frame(time.index = 1:Nt,
                        time.scale = seq(0, 1, , Nt),
                        DATE_TIME = DATE_TIME)
Cali.Data <- Cali.Data  %>% left_join(date.time, by = c("DATE_TIME"))
#####################################################################
#  H
#####################################################################


pred.coords <- distinct(Cali.Data[, c("sample.id", "sample.lon", "sample.lat",
                               "LON_X", "LAT_Y")])
setnames(pred.coords, c(1:3), c("ID", "LON", "LAT"))
setnames(Cali.Data, "sample.id", c("ID"))
source("./R/CreateGrid.R")

H.basic.data <- CreateGrid(Cali.Data,
                           sample.coords = Site,
                           pred.coords = pred.coords,
                           Map = bth_map,
                           max.edge = c(.35, .65), #0.3,0.7
                           offset = c(1e-1, 0.5), #0.4, 0.6
                           cutoff = 0.04,
                           distance.scale = 1e3,
                           n.grps = 1,
                           col = "black",
                           size = 2,
                           site.id = "ID",
                           factor = 1,
                           ch = 0.23,
                           method = "indicator1",
                           distance.method = 1,
                           hjust = c(-1, 1, -1, 0.5),
                           way = 1) #indicator


file <- "./Calibration/Result/"
Tab <- list.files(file)
# HDCM <- Tab[grepl(paste0("Fit"), Tab) & grepl(paste0("_W_"), Tab)]
HDCM <- Tab[grepl(paste0("Fit"), Tab)]
load(paste0(file, HDCM[1]))
# Cali.Data$sim50_CMAQ_PM25 <- sqrt(Cali.Data$sim50_CMAQ_PM25)
HDCM.Data <- Construct_HDCM_Data(data = Cali.Data,
                                 include = list(YEAR = Year, month_day = month_day),
                                 siteid = "ID",
                                 Y = "CMAQ_PM25",
                                 X = c("sim50_CMAQ_PM25"
                                       , "sim_TEMP"
                                       , "sim_WIND_X"
                                       , "sim_WIND_Y"
                                 ),
                                 start.time = min(Cali.Data$time.index) - 1,
                                 standard = T,
                                 center = T,
                                 start.index = 1,
                                 scaled_variable = wHDCM_Fitting[["data"]][["scaled_variable"]]
                                 )


#####################################################################
Cali_Data_Inf <- list(Hs = H.basic.data$Hs
                      , Nt = length(unique(Cali.Data$DATE_TIME))
                      , X_ts = HDCM.Data$X_ts
                      , Nc = length(unique(Cali.Data$sample.id))
                      , CMAQ_N = length(unique(Cali.Data$sample.id))/Nc
                      , CMAQ_Cali = Cali.Data
                    )
save(Cali_Data_Inf, file = "./Calibration/Result/Cali_HDCM_W.RData")
#####################################################################
