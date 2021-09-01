#Digitize Figure 2 from Heithaus and Dill 2002

library(digitize)

# x2 <- digitize("fig2_2002.png")
# x2$seasoncover <- rep(c("warm shallow", "warm deep", "cold97 shallow", "cold97 deep", "cold99 shallow", "cold99 deep"),each = 3)
# x2$season <- unlist(lapply(strsplit(x2$seasoncover, " "), "[[", 1))
# x2$cover <- unlist(lapply(strsplit(x2$seasoncover, " "), "[[", 2))
# write.csv(x2, "fig2_heithaus2002.csv", row.names = FALSE)

x2 <- read.csv("fig2_heithaus2002.csv")

#put data from figure in dataframe

biomass <- x2[c(2,5,8,11,14,17),-1]
names(biomass)[1] <- "biomass"  
biomass$error <- c()

biomass[1,"error"] <- mean(abs(x2$y[1]-x2$y[2]), abs(x2$y[3]-x2$y[2]))
biomass[2, "error"] <- mean(abs(x2$y[4]-x2$y[5]), abs(x2$y[6]-x2$y[5]))
biomass[3, "error"] <- mean(abs(x2$y[7]-x2$y[8]), abs(x2$y[9]-x2$y[8]))
biomass[4, "error"] <- mean(abs(x2$y[10]-x2$y[11]), abs(x2$y[12]-x2$y[11]))
biomass[5, "error"] <- mean(abs(x2$y[13]-x2$y[14]), abs(x2$y[15]-x2$y[14]))
biomass[6, "error"] <- mean(abs(x2$y[16]-x2$y[17]), abs(x2$y[18]-x2$y[17]))

plotCIgraph <- function(loc, value, lower, upper, wiskwidth, color = "grey", linewidth = 2) {
  w <- wiskwidth / 2
  segments(x0 = loc, x1 = loc, y0 = lower, y1 = upper, col = color,
           lwd = linewidth)
  segments(x0 = loc - w, x1 = loc + w, y0 = upper, y1 = upper,
           col = color, lwd = linewidth) # upper whiskers
  segments(x0 = loc - w, x1 = loc + w, y0 = lower, y1 = lower,
           col = color, lwd = linewidth) # lower whiskers
}

#plot data to make sure it looks right
windows()

bp <- barplot(biomass$biomass, 
              width = 0.5, space = 1, 
              ylim = c(0,4000), names.arg = biomass$seasoncover)

plotCIgraph(bp, biomass$biomass, 
            biomass$biomass - biomass$error, 
            biomass$biomass + biomass$error, 
            0.1, color = "black", 1)

#Here let's just take the ratio for each season and average

warm <- biomass$biomass[1]/biomass$biomass[2]

cold97 <- biomass$biomass[3]/biomass$biomass[4]

cold99 <- biomass$biomass[5]/biomass$biomass[6]

mean(c(warm, cold97, cold99))

#ratio is 5 (3-9)











