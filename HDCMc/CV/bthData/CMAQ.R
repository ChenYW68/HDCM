source("./R/PSTVB_Packages.R")
source("./R/util.R")
data("SiteData", package = "ADCM")
setDF(obs_PM25_2015s)
colnames(obs_PM25_2015s)
spT_validation()
# View(obs_PM25_2015s)
result = plyr::ddply(obs_PM25_2015s,
                     .(CITY, ID),
                     plyr::summarize,
                     Mean = mean(sim_CMAQ_PM25),
                     sigma = sd(sim_CMAQ_PM25),
                     .progress = "text") 
# View(result)

da <- obs_PM25_2015s %>% left_join(result, by = c("CITY", "ID"))

CMAQ.cv <- Validation.Group.Region(data = da,
                                sigma = da$sigma,
                                col = c("REAL_PM25", "sim_CMAQ_PM25"),
                                by = "CITY")

writexl::write_xlsx(CMAQ.cv, path = "./Result/CMAQs.cv.xlsx")

result = plyr::ddply(obs_PM25_2015w,
                     .(CITY, ID),
                     plyr::summarize,
                     Mean = mean(sim_CMAQ_PM25),
                     sigma = sd(sim_CMAQ_PM25),
                     .progress = "text") 
# View(result)

da <- obs_PM25_2015w %>% left_join(result, by = c("CITY", "ID"))

CMAQ.cv <- Validation.Group.Region(data = da,
                                   sigma = da$sigma,
                                   col = c("REAL_PM25", "sim_CMAQ_PM25"),
                                   by = "CITY")

writexl::write_xlsx(CMAQ.cv, path = "./Result/CMAQw.cv.xlsx")