library(RODBC)
library(data.table)
DSN_01 <- odbcConnect("DSN_01", uid = "myname", pwd = "mypwd"
                      , believeNRows = FALSE, case = "toupper")

Table <- "SEN_"
Tab <- sqlTables(DSN_01, schema = "MYNAME")
Table <- Tab[grepl(toupper(Table), Tab$TABLE_NAME), 3]

source("./R/util.R")
sensit.Re <- NULL
for(i in 1:length(Table)){
  str <- strsplit(Table[i], "_")[[1]]
  print(Table[i])
  print(str)

  ELAPSED <- sqlQuery(DSN_01,  paste0("select ELAPSED from ", Table[i], " order by ITER"),
                      errors = F)


  ELAPSED <- mean(as.numeric(ELAPSED$ELAPSED[-1], na.rm = F))

  sensit.Re <- rbind(sensit.Re, data.frame(Ch = as.numeric(paste0("0.", str[2])),
                                           Cs =  ifelse(str[3] == "NA", 1, as.numeric(paste0("0.",
                                                                                             str[3]))),
                                           Ct = as.numeric(str[4]),
                                           Ne = as.numeric(str[5]),
                                           tail(sqlQuery(DSN_01,
                                                         paste0("select ITER, FITTING_RMSE, TESTING_RMSE from ",
                                                                Table[i], " order by ITER"),
                                                         errors = F), 1),
                                           elapsed = ELAPSED))
}
colnames(sensit.Re)
# sensit.Re <- data.frame(sensit.Re)
# Cs <- factor(sensit.Re$Cs,levels = as.factor(sensit.Re$Cs))
# Ct <- factor(sensit.Re$Ct,levels = as.factor(sensit.Re$Ct))
# ggplot(sensit.Re, aes(factor(Cs, levels = Cs),
#                 factor(y,levels = Ct), #定义x，y轴顺序，防止被默认改变
#                 fill = TESTING_RMSE))+  #根据相关性值填充颜色
#   geom_tile()+  #色块函数
#   scale_fill_gradient2(low = 'Blue',mid = 'white',high ='red'
#                        # ,limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1)
#                        )+
#   labs(x=NULL,y=NULL)+
#   theme_bw(base_size = 15)
setDT(sensit.Re)

# op <- par(mar = c(4, 4, 2, 1) + 0.5)
# par(cex = 1.25)
# par(mgp = c(3, 0.7, 0.3),  cex.axis = 1.5, cex.lab = 1.5)
ch <- sort(unique(sensit.Re$Ch))
ch.list <- chunk(ch, 3)
for(j in 1:length(ch.list)){
  pdf(paste0(paste0("./figure/"), "FigS4_tuning_train_", j,".pdf"), width = 18, height = 10)
  par(mfrow = c(3, 4))

  for(i in 1:length(ch.list[[j]])){
    for(e in sort(as.numeric(unique(sensit.Re$Ne)))){

      da <- sensit.Re[(Ne == e) & (Ch == ch.list[[j]][i]), c(2, 3, 6)] %>% dcast(Cs ~ Ct,
                                                                                 value.var = "FITTING_RMSE",
                                                                                 fun.aggregate = mean)
      print(da)
      da0 <- as.matrix(da[, -1])

      op <- par(mar = c(4, 8, 3, 1))
      # brk<- c(floor(min(sensit.Re$FITTING_RMSE)), seq(floor(min(sensit.Re$FITTING_RMSE)),
      #           ceiling(max(sensit.Re$FITTING_RMSE)),, 10),
      #         ceiling(max(sensit.Re$FITTING_RMSE))) %>% unique()

      # brk<- quantile( c(sensit.Re$FITTING_RMSE))

      # myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) #"Spectral" #RdYlGn
      # col <- tim.colors(length(brk) - 1)
      # par(cex = 1.25)
      par(mgp = c(3, 1.2, 0.5), cex.main = 2.5, cex.axis = 1.5, cex.lab = 2.5)
      fields::image.plot(x = da$Cs,
                         y = as.numeric(colnames(da)[-1]),
                         z = da0,
                         xlab = TeX("$c_s$"),
                         ylab = TeX("$c_t$"),
                         xlim = c(.1, 1),
                         ylim = c(1, 5),
                         zlim = c(floor(min(sensit.Re$FITTING_RMSE)),
                                  ceiling(max(sensit.Re$FITTING_RMSE))),
                         # col= col,
                         # breaks = brk,
                         # lab.breaks= names(brk),
                         legend.lab = ifelse(e > 30, "", TeX("RMSE ($μg/m^3$)")),
                         axes=F,
                         legend.mar = 12,
                         legend.line= 3.5,
                         legend.shrink = 1,
                         legend.width = 2
      )
      Ne <- as.character(e)
      axis(1, at = seq(.1, 1, 0.1), labels = seq(.1, 1, 0.1))
      axis(2, at = seq(1, 5, 1), labels = seq(1, 5, 1))
      title(main = eval(bquote(expression(paste("c"['h'], " = ", .(ch.list[[j]][i]), ", N"['e'], " = ", .(Ne))))))
    }
  }
  dev.off()
}



#   theme_bw(base_size = 15)
setDT(sensit.Re)
# pdf(paste0(paste0("./figure/"), "tuning_sensitivity_test",".pdf"), width = 15, height =  12)
# par(mfrow = c(3, 2))
# op <- par(mar = c(4, 4, 2, 1) + 0.5)
# par(cex = 1.25)
# par(mgp = c(3, 0.7, 0.3),  cex.axis = 1.5, cex.lab = 1.5)

# pdf(paste0(paste0("./figure/"), "tuning_test",".pdf"), width = 16, height = 6)
# par(mfrow = c(3, 4))
ch <- sort(unique(sensit.Re$Ch))
ch.list <- chunk(ch, 3)
for(j in 1:length(ch.list)){
  pdf(paste0(paste0("./figure/"), "FigS5_tuning_test_", j,".pdf"), width = 18, height = 10)
  par(mfrow = c(3, 4))
  for(i in 1:length(ch.list[[j]])){
    for(e in sort(as.numeric(unique(sensit.Re$Ne)))){

      da <- sensit.Re[(Ne == e) & (Ch == ch.list[[j]][i]), c(2, 3, 7)] %>% dcast(Cs ~ Ct,
                                                                                 value.var = "TESTING_RMSE",
                                                                                 fun.aggregate = mean
      )
      print(da)
      da0 <- as.matrix(da[, -1])

      op <- par(mar = c(4, 8, 3, 1))
      # par(cex = 1.25)
      par(mgp = c(3, 1.2, 0.5), cex.main = 2.5, cex.axis = 1.5, cex.lab = 2.5)
      fields::image.plot(x = da$Cs,
                         y = as.numeric(colnames(da)[-1]),
                         z = da0,
                         xlab = TeX("$c_s$"),
                         ylab = TeX("$c_t$"),
                         xlim = c(.1, 1),
                         ylim = c(1, 5),
                         zlim = c(floor(min(sensit.Re$TESTING_RMSE)),
                                  ceiling(max(sensit.Re$TESTING_RMSE))),
                         legend.lab = ifelse(e > 30, "", TeX("RMSE ($μg/m^3$)")),
                         axes=F,
                         legend.mar = 12,
                         legend.line= 3.5,
                         legend.shrink = 1,
                         legend.width = 2
      )
      Ne <- as.character(e)
      axis(1, at = seq(.1, 1, 0.1), labels = seq(.1, 1, 0.1))
      axis(2, at = seq(1, 5, 1), labels = seq(1, 5, 1))

      title(main = eval(bquote(expression(paste("c"['h'], " = ", .(ch.list[[j]][i]), ", N"['e'], " = ", .(Ne))))))
    }
  }
  dev.off()
}



pdf(paste0(paste0("./figure/"), "FigS6_tuning_time",".pdf"), width = 20, height = 4)
par(mfrow = c(1, 4))
ch <- unique(sensit.Re$Ch)

source("./R/PSTVB_Packages.R")
sensit.Re.Da <- plyr::ddply(sensit.Re
                            , .(Cs, Ct, Ne)
                            , plyr::summarize
                            , elapsed = mean(elapsed)
                            , .progress = "text")

setDT(sensit.Re.Da)
# for(i in 1:length(ch)){
for(e in sort(as.numeric(unique(sensit.Re$Ne)))){

  da <- sensit.Re.Da[(Ne == e), ] %>% dcast(Cs ~ Ct, value.var = "elapsed",
                                            fun.aggregate = mean)
  print(da)
  da0 <- as.matrix(da[, -1])

  op <- par(mar = c(4, 6, 3, 1))
  # par(cex = 1.25)
  par(mgp = c(3, 1.2, 0.5), cex.main = 2.5, cex.axis = 1.5, cex.lab = 2.5)
  fields::image.plot(x = da$Cs,
                     y = as.numeric(colnames(da)[-1]),
                     z = da0,
                     xlab = TeX("$c_s$"),
                     ylab = TeX("$c_t$"),
                     xlim = c(.1, 1),
                     ylim = c(1, 5),
                     zlim = c(floor(min(sensit.Re.Da$elapsed)),
                              ceiling(max(sensit.Re.Da$elapsed))),
                     legend.lab = ifelse(e > 30, "", TeX("Average time (seconds) on each iteration")),
                     axes=F,
                     legend.mar = 12,
                     legend.line= 3.5,
                     legend.shrink = 1,
                     legend.width = 2
  )
  Ne <- as.character(e)
  axis(1, at = seq(.1, 1, 0.1), labels = seq(.1, 1, 0.1))
  axis(2, at = seq(1, 5, 1), labels = seq(1, 5, 1))

  title(main = eval(bquote(expression(paste("N"['e'], " = ", .(Ne))))))
  # }
}
dev.off()
