daily.df["color.cvd"] <- ifelse(daily.df["class.cvd"],"red","blue")

#draw scatterplot3d for cardiovascular diseases based on averages
svg("scatterplot3d_avg_daily_cvd.svg",width = 10 , height = 8)
scatterplot3d(x=daily.df[,2], y = daily.df[,4],z=daily.df[,6],color = daily.df[,"color.cvd"],xlab = "daily activity count average (count per minute)", ylab="daily step count average (step per minute)", zlab="daily a1 proportion average")
legend("topright",c("TRUE","FALSE"),col=c("red","blue"),pch=1)
title("3d scatter plot of cardiovascular diseases based on average activity and step counts and a1 proportion")
dev.off()

#draw scatterplot3d for cardiovascular diseases based on stds
svg("scatterplot3d_std_daily_cvd.svg",width = 10 , height = 8)
scatterplot3d(x=daily.df[,3], y = daily.df[,5],z=daily.df[,7],color = daily.df[,"color.cvd"],xlab = "daily activity count std (count per minute)", ylab="daily step count std (step per minute)", zlab="daily a1 proportion std")
legend("topright",c("TRUE","FALSE"),col=c("red","blue"),pch=1)
title("3d scatter plot of cardiovascular diseases based on std of activity and step counts and a1 proportion")
dev.off()



daily.df["color.mob"] <- ifelse(daily.df["class.mobilityImpaired"],"red","blue")

#draw scatterplot3d for mobility impairment based on averages
svg("scatterplot3d_avg_daily_mob.svg",width = 10 , height = 8)
scatterplot3d(x=daily.df[,2], y = daily.df[,4],z=daily.df[,6],color = daily.df[,"color.mob"],xlab = "daily activity count average (count per minute)", ylab="daily step count average (step per minute)", zlab="daily a1 proportion average")
legend("topright",c("TRUE","FALSE"),col=c("red","blue"),pch=1)
title("3d scatter plot of mobility impairment based on average activity and step counts and a1 proportion")
dev.off()

#draw scatterplot3d for mobility impairment based on stds
svg("scatterplot3d_std_daily_mob.svg",width = 10 , height = 8)
scatterplot3d(x=daily.df[,3], y = daily.df[,5],z=daily.df[,7],color = daily.df[,"color.mob"],xlab = "daily activity count std (count per minute)", ylab="daily step count std (step per minute)", zlab="daily a1 proportion std")
legend("topright",c("TRUE","FALSE"),col=c("red","blue"),pch=1)
title("3d scatter plot of mobility impairment based on std of activity and step counts and a1 proportion")
dev.off()



daily.df["color.cog"] <- ifelse(daily.df["class.cognitionImpaired"],"red","blue")

#draw scatterplot3d for cognition impairment based on averages
svg("scatterplot3d_avg_daily_cog.svg",width = 10 , height = 8)
scatterplot3d(x=daily.df[,2], y = daily.df[,4],z=daily.df[,6],color = daily.df[,"color.cog"],xlab = "daily activity count average (count per minute)", ylab="daily step count average (step per minute)", zlab="daily a1 proportion average")
legend("topright",c("TRUE","FALSE"),col=c("red","blue"),pch=1)
title("3d scatter plot of cognition impairment based on average activity and step counts and a1 proportion")
dev.off()

#draw scatterplot3d for cognition impairment based on stds
svg("scatterplot3d_std_daily_cog.svg",width = 10 , height = 8)
scatterplot3d(x=daily.df[,3], y = daily.df[,5],z=daily.df[,7],color = daily.df[,"color.cog"],xlab = "daily activity count std (count per minute)", ylab="daily step count std (step per minute)", zlab="daily a1 proportion std")
legend("topright",c("TRUE","FALSE"),col=c("red","blue"),pch=1)
title("3d scatter plot of cognition impairment based on std of activity and step counts and a1 proportion")
dev.off()
