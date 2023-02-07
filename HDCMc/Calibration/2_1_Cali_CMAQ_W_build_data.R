rm(list=ls())
# library(stBase)
###########################################################################
# Day <- 26:31#27:31#
Year <- c(2015, 2015)
month_day <- c("12-17", "12-22")
# month_day <- c("11-26", "12-01")
Nc <- 50
###########################################################################
file <- "./Calibration/"
###########################################################################
source("./R/PSTVB_Packages.R")
source("./R/util.R")
# 1. randomly generate U spatial points around s by binary uniform distribution


load("./data/CMAQ_BTH_SITE.RData")
load("./data/CMAQ_PM25_COM.RData")

CMAQ_Site <- CMAQ_BTH_SITE[distToNearestProvince <= 100,]
CMAQ_PM25_COM <- CMAQ_PM25_COM[LON %in% CMAQ_Site$LON & LAT%in%CMAQ_Site$LAT,]

CMAQ_PM25_COM <- CMAQ_PM25_COM %>%
  dplyr::filter(
    # YEAR %in% year,
    between(as.Date(DATE_TIME),
            (as.Date(paste0(Year[1], "-", month_day[1]))),
            (as.Date(paste0(Year[2], "-", month_day[2]))))#,
    # between(MONTH, 10, 12),
  )
# CMAQ_Site <-  spCoords.transform(setDF(distinct(CMAQ_PM25_COM[Flag==1, c(1, 4:5)])),
#                                  method = 1)
# setDT(CMAQ_Site)

Grid.Data <- unique(CMAQ_PM25_COM[, c("LON", "LAT")])
Grid.Data <- CMAQ_PM25_COM[, -c("LON_X", "LAT_Y")] %>% left_join(
  spCoords.transform(as.data.frame(Grid.Data),
                                col = c("LON", "LAT"),
                                method = 1) #%>% setorderv("CMAQ_ID", 1)
            , by = c("LON", "LAT"))


r1 <- range(abs(diff(CMAQ_Site$LON)))[1]/5  #0.0026855
r2 <- range(abs(diff(CMAQ_Site$LAT)))[1]/5  #0.08362579
###########################################################################
###########################################################################
CMAQ_Site <- CMAQ_BTH_SITE[distToNearestProvince <= 0,]

Predict.Location <- NULL
for(s in 1:nrow(CMAQ_Site))
{
  Predict.Location <- rbind(Predict.Location, data.frame(sample.id  = (Nc*(s - 1) + 1):(Nc*(s)),
                                 sample.lon = runif(Nc, CMAQ_Site$LON[s] - r1, CMAQ_Site$LON[s] + r1),
                    sample.lat = runif(Nc, CMAQ_Site$LAT[s] - r2, CMAQ_Site$LAT[s] + r2),
                    LON = CMAQ_Site$LON[s],
                    LAT = CMAQ_Site$LAT[s]#,
                    # LON_X = CMAQ_Site$LON_X[s],
                    # LAT_Y = CMAQ_Site$LAT_Y[s]
                    ))
  if(s %% 500 == 0){
    cat("s = ", s, "\n")
  }
}
# setnames(CMAQ_Site_total, "CMAQ_ID", "ID")
# plot(Predict.Location[Predict.Location$Grid.Bs == 1, 1:2])
# points(cmaq_site[1, 2:3], col = "red")

# 2. transform the coordinate
Predict.Coor <- spCoords.transform(Predict.Location,
                                             col = c("sample.lon", "sample.lat"),
                                             colname = c("LON_X", "LAT_Y"),
                                             method = 1)

# connect predicted location to cmaq grid
Grid.Coord <- unique(Grid.Data[, c("LON", "LAT", "LON_X", "LAT_Y")])
# setnames(Grid.Coord, "CMAQ_ID", "ID")
# CMAQ
rm(Predict.Location)

Grid_point_CMAQ_Data <- Cali_from_grid_prePoint(Predict.Coor = Predict.Coor,
                                           Grid.Coord = Grid.Coord,
                                           Grid.Data = Grid.Data,
                                           neighbor = 50,
                                           col_var = c("LON_X", "LAT_Y"),
                                           distance.scale = 1e3,
                                           from.var = colnames(Grid.Data)[c(3)],
                                           to.var = paste0("sim50_",
                                                           colnames(Grid.Data)[c(3)]))


Grid_point_CMAQ_Data$sim50_CMAQ_PM25 <- sqrt(Grid_point_CMAQ_Data$sim50_CMAQ_PM25)

#-----------------------------------------------------------------------------------
load(file = "./data/NAQPMS_DATA.RData")
load(file = "./data/NAQPMS_BTH_SITE.RData")
NAQPMS_Site <- NAQPMS_BTH_SITE[distToNearestProvince <= 100,]
NAQPMS_DATA <- spCoords.transform(setDF(NAQPMS_DATA), method = 1)
NAQPMS_DATA <- NAQPMS_DATA[(NAQPMS_DATA$LON %in% NAQPMS_Site$LON) &
                             (NAQPMS_DATA$LAT%in%NAQPMS_Site$LAT),] %>%
  dplyr::filter(
    # YEAR %in% year,
    between((as.Date(DATE_TIME)),
            (as.Date(paste0(Year[1], "-", month_day[1]))),
            (as.Date(paste0(Year[2], "-", month_day[2]))))#,
    # between(MONTH, 10, 12),
  )
# %>%
#   filter(YEAR_MONTH %in% c(YearMonth),
#          day(DATE_TIME) %in% c(Day))

setDT(NAQPMS_Site); setDT(NAQPMS_DATA);
# distinct(NAQPMS_DATA[, 6:7])
Grid_point_CAQRA_Data <- Cali_from_grid_prePoint(Predict.Coor = Predict.Coor,
                                           Grid.Coord = NAQPMS_Site,
                                           Grid.Data = NAQPMS_DATA,
                                           neighbor = 15,
                                           col_var = c("LON_X", "LAT_Y"),
                                           distance.scale = 1e3,
                                           from.var = colnames(NAQPMS_DATA)[c(14:18)],
                                           to.var = paste0("sim_",
                                                           colnames(NAQPMS_DATA)[c(14:18)]))


pred.location.grid.output <- Grid_point_CAQRA_Data %>%
  left_join(Grid_point_CMAQ_Data[, c("sample.id", "DATE_TIME", "sim50_CMAQ_PM25")],
            by = c("sample.id", "DATE_TIME"))
pred.location.grid.output$sim_TEMP <- pred.location.grid.output$sim_TEMP - 273.15
# load("./data/Base_CAQRA_Table.RData")
# colnames(Base_CAQRA_Table)
# setnames(CMAQ_Site_total, 1, "CMAQ_ID")
# pred.location.grid.output <- as.data.table(pred.location.grid.output) %>%
#   left_join(unique(Base_CAQRA_Table[, c("DATE_TIME", "TIME", "time_label")]),
#   by = c("DATE_TIME")) %>% left_join(CMAQ_Site_total[, c(1:3)],
#                                      by = c("CMAQ_ID", "DATE_TIME"))
# load("./data/CMAQ_PM25_COM.RData")
pred.location.grid.output$ID <- NULL
pred.location.grid.output <- pred.location.grid.output %>%
  left_join(CMAQ_PM25_COM[, c("CMAQ_ID", "LON", "LAT", "DATE_TIME",  "CMAQ_PM25")],
                                        by = c("DATE_TIME", "LON", "LAT"))
rm(CMAQ_PM25_COM)
save(pred.location.grid.output,
     file = paste0(file, "/Result/pred.location.grid.output.RData"))
