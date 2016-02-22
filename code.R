#install.packages("ggmap")
#install.packages("maptools")
#install.packages("maps")
library("ggmap")
library("maptools")
library("maps")

library(RNetCDF)
fname = "data/NOAA_Daily_phi_500mb.nc"
fid = open.nc(fname)
print.nc(fid)
dat = read.nc(fid)

# make a simple plot
#map("world", fill=TRUE, col="white", ylim=c(-60, 90), mar=c(0,0,0,0))
data("wrld_simpl")
lon = dat$X - 180
lat = at$Y
phi = dat$phi[,,1]
quartz(width=9, height=6)
image(lon, rev(lat), phi, xlab="Longitude", ylab="Latitude")
#box()
#contour(x, y, phi, add=TRUE, levels = seq(90, 200, by = 5))
title(main="Phi")
plot(wrld_simpl, add=TRUE)

close.nc(fid)