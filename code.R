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

################## make a simple plot
#map("world", fill=TRUE, col="white", ylim=c(-60, 90), mar=c(0,0,0,0))
data("wrld_simpl")
lon = dat$X - 180
lat = at$Y
phi = dat$phi[,,1]
quartz(width=11, height=4)
image(lon, rev(lat), phi, xlab="Longitude", ylab="Latitude")
#box()
#contour(x, y, phi, add=TRUE, levels = seq(90, 200, by = 5))
title(main="Phi")
plot(wrld_simpl, add=TRUE)


################## try ggplot



################# make an animation
# get an years data
lon = dat$X - 180
lat = at$Y

for (i in c(1:10)) {
  plot.new()
  pdf(file=sprintf("temp%02d.pdf", i), width=9, height=4)
  phi = dat$phi[,,i]
  #quartz(width=11, height=4)
  image(lon, rev(lat), phi, xlab="Longitude", ylab="Latitude")
  title(main="Phi")
  plot(wrld_simpl, add=TRUE)
  dev.off()
}
system("convert -background white -alpha remove -layers OptimizePlus -delay 80 *.pdf animate.gif")
file.remove(list.files(pattern="pdf"))

close.nc(fid)