# options(rgdal_show_exportToProj4_warnings="none")
# # 1 loading packages ---------------------------------------
packages <- c("RandomFields",
              "data.table",
              "ggplot2",
              "plyr",
              "parallel",
              "sqldf", 
              "randomForest", 
              "spBayes", 
              "latex2exp",
              "gstat", 
              "ggtext", 
              "numDeriv",
              "lubridate", 
              "dplyr", 
              "INLA", 
              "rgeos", 
              "Hmisc", 
              "MASS", 
              "inlabru",
              "tidyr", 
              "RColorBrewer", 
              "progress",
              "RODBC",
              "fields", 
              "rgdal", 
              "cowplot",
              "invgamma",
              "geoR",
              "ranger", 
              "MBA", 
              "scoringRules",
              "Rcpp", 
              "writexl", 
              "inlabru",
              "SpecsVerification", 
              "ggmap",
              "ranger", 
              "verification",
              "mapproj", 
              "sp",
              "mvnfast", 
              "rgdal", 
              "scoringutils", 
              "spTDyn",
              "HDCM",
              "rARPACK"
              ) 
# ,'MASS'
# 2  library
for(i in 1:length(packages))
{
  if(!lapply(packages[i], require,
             character.only = TRUE)[[1]])
  {
    install.packages(packages[i])
    # library(packages[i])
    lapply(packages[i], require,
           character.only = TRUE)
  }else{lapply(packages[i], require,
               character.only = TRUE)}
}
# x=lapply(packages, require, character.only = TRUE)
# rm(list=ls())
rm(i, packages)


