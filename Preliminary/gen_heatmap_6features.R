jpeg("heatmap_daily_6features.jpg",width=1600,height=1200)
heatmap.2(as.matrix(daily.df[,2:7]),trace = "none",margins = c(17,5))
dev.off()