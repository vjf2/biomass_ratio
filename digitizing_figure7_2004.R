##Digitize and calculate proxies for relative prey availabilty 

library(digitize)

#fig 7 Heithaus 2004

# x <- digitize("fig 7_2004.png")
# x$depthcover <- rep(c("DY", "SY", "SN", "DN"), each=3)
# write.csv(x, "fig7_Heithaus2004.csv", row.names = FALSE)

x <- read.csv("fig7_Heithaus2004.csv")

#estimate 95% region 

biomass <- data.frame(depthcover=c("DY", "SY", "SN", "DN"), 
                      logbiomass=x[c(2,5,8,11),"y"], 
                      logerror=numeric(4))

biomass[1,"logerror"] <- mean(abs(x$y[1]-x$y[2]), abs(x$y[3]-x$y[2]))
biomass[2, "logerror"] <- mean(abs(x$y[4]-x$y[5]), abs(x$y[6]-x$y[5]))
biomass[3, "logerror"] <- mean(abs(x$y[7]-x$y[8]), abs(x$y[9]-x$y[8]))
biomass[4, "logerror"] <- mean(abs(x$y[10]-x$y[11]), abs(x$y[12]-x$y[11]))

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

bp <- barplot(biomass$logbiomass, 
              width = 0.5, space = 1, 
              ylim = c(0,3.5), names.arg = biomass$depthcover)

plotCIgraph(bp, biomass$logbiomass, 
            biomass$logbiomass - biomass$logerror, 
            biomass$logbiomass + biomass$logerror, 
            0.1, color = "black", 1)

#Add sample size to calculate standard deviation

biomass$samplesize <- c(37, 198, 40, 373)

#transform data

biomass$rawbiomass <- (10^biomass$logbiomass) - 1
biomass$rawerror <- (10^(biomass$logbiomass + biomass$logerror) - 10^(biomass$logbiomass - biomass$logerror))/2

#back calculate st dev
biomass$stdev <- (biomass$rawerror * sqrt(biomass$samplesize))/1.96

#average_biomass_ratio

average_seagrass <- (biomass$rawbiomass[1]*1 + biomass$rawbiomass[2]*9)/10 

average_nonseagrass <- (biomass$rawbiomass[3]*1 + biomass$rawbiomass[4]*9)/10 

#ratio
average_seagrass/average_nonseagrass

#combine the error evenly
# 0.5 * (biomass$rawerror[1]^2 + biomass$rawerror[2]^2)^(1/2)

#approximate the weighted error because I can't think of the proper formula

set.seed(1)

approx_sd_seagrass <-sd(c(rnorm(37, mean = 663, sd=1325),rnorm(198, mean=192, sd=564)))
approx_error_seagrass <- ((1.96 * approx_sd_seagrass) / sqrt(37+198))

approx_sd_nonseagrass <-sd(c(rnorm(40, mean = 86, sd=432), rnorm(373, mean=22, sd=70)))
approx_error_nonseagrass <- ((1.96 * approx_sd_nonseagrass) / sqrt(40+373))

#average seagrass 239.3413 +/- 91
#average nonseagrass 28.63952 +/- 14

#highest ratio

(average_seagrass + approx_error_seagrass)/(average_nonseagrass - approx_error_nonseagrass) 

(average_seagrass - approx_error_seagrass)/(average_nonseagrass + approx_error_nonseagrass)

#ratio is 8 (range 3-23)






