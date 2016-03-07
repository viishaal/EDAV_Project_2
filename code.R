#install.packages("ggmap")
#install.packages("maptools")
#install.packages("maps")
library("ggmap")
library("maptools")
library("maps")
library(RNetCDF)
require(reshape2)

fname = "data/NOAA_Daily_phi_500mb.nc"
fid = open.nc(fname)
print.nc(fid)
dat = read.nc(fid)

################## make a simple plot
#map("world", fill=TRUE, col="white", ylim=c(-60, 90), mar=c(0,0,0,0))
data("wrld_simpl")
lon = dat$X - 180
lat = dat$Y
phi = dat$phi[,,1]
plot.new()
quartz(width=9, height=6)
#rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
#filled.contour(x=lon, y=rev(lat), z=phi, color.palette=rgb.palette, #levels=numeric,
#               plot.title=title(main="Phi", xlab='Longitude [°]', ylab='Latitude [°]'),
#               plot.axes={axis(1); axis(2);map('world', add=TRUE);grid()})

#bwr = colorRampPalette(c("blue", "white", "red"))
image(lon, rev(lat), phi, xlab="Longitude", ylab="Latitude", xlim=c(-180,180), ylim=c(-90,90))
 #     , col=terrain.colors(100))
#map("world", add = TRUE)
#box()
#contour(x, y, phi, add=TRUE, levels = seq(90, 200, by = 5))
#title(main="Phi")
plot(wrld_simpl, add=TRUE)
#map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0), add=T)
#mapWorld <- borders("world", colour="gray50", fill="gray50")
#mapWorld


################## try ggplot
df = data.frame(phi)
names(df) = lat
df$lon= lon
mdata = melt(df, id=c("lon"))
names(mdata) = c("lon", "lat", "x")
mdata$x = as.numeric(mdata$x)
mdata$lon = as.numeric(mdata$lon)
mdata$lat = as.numeric(as.character(mdata$lat))

wr <- map_data("world")
# Prepare a map of World
wrmap <- ggplot(wr, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  geom_point(data=mdata, inherit.aes=FALSE, aes(x=lon, y=lat, fill=x), size=3, shape=4) +
  #ggplot(mdata, inherit.aes=F, aes(lon, lat))
  scale_fill_gradient("Phi", limits=c(4500,6000)) +
  #scale_fill_brewer()
  theme_bw() +
  coord_equal()

wrmap

################# make an animation
# get an years data
lon = dat$X - 180
lat = dat$Y
a=16120
b=16130
for (i in c(a:b)) {
  plot.new()
  pdf(file=sprintf("temp%02d.pdf", i), width=9, height=4)
  phi = dat$phi[,,i]
  #quartz(width=11, height=4)
  #image(lon, rev(lat), phi, xlab="Longitude", ylab="Latitude")
  #title(main="Phi")
  #plot(wrld_simpl, add=TRUE)
  rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
  filled.contour(x=lon, y=rev(lat), z=phi, color.palette=rgb.palette, #levels=numeric,
                 plot.title=title(main="Phi", xlab='Longitude [°]', ylab='Latitude [°]'),
                 plot.axes={axis(1); axis(2);map('world', add=TRUE);grid()})
  dev.off()
}
system("convert -background white -alpha remove -layers OptimizePlus -delay 80 *.pdf thai_flood.gif")
file.remove(list.files(pattern="pdf"))


###### use animation package

library(animation)

#Set delay between frames when replaying
ani.options(interval = 0.05, ani.dev="png", ani.height=300, ani.width=900)
saveGIF({
  lon = dat$X - 180
  lat = dat$Y
  #a=16120
  #b=16130
  a = 15755
  b = 15765
  
  min_z = Inf
  max_z = 0
  for (i in c(a:b)) {
    phi = dat$phi[,,i]
    if (min_z > min(phi)) {min_z = min(phi)}
    if (max_z < max(phi)) {max_z = max(phi)}
  }
  min_z
  max_z
  rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
  for (i in c(a:b)) {
    phi = dat$phi[,,i]
    filled.contour(x=lon, y=rev(lat), z=phi, zlim=c(min_z,max_z), color.palette=rgb.palette, #levels=numeric,
                   plot.title=title(main="Phi", xlab='Longitude [°]', ylab='Latitude [°]'),
                   plot.axes={axis(1); axis(2);map('world', add=TRUE);grid()})
  }
},movie.name = "turkey_no_flood.gif")


saveVideo({
  #layout(matrix(c(1, rep(2, 5)), 6, 1))
  #par(mar=c(4,4,2,1) + 0.1)

  lon = dat$X - 180
  lat = dat$Y
  #a=16120
  #b=16130
  a = 15755
  b = 15765
  
  for (i in c(a:b)) {
    #plot.new()
    #pdf(file=sprintf("temp%02d.pdf", i), width=9, height=4)
    phi = dat$phi[,,i]
    rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
    filled.contour(x=lon, y=rev(lat), z=phi, color.palette=rgb.palette, #levels=numeric,
                   plot.title=title(main="Phi", xlab='Longitude [°]', ylab='Latitude [°]'),
                   plot.axes={axis(1); axis(2);map('world', add=TRUE);grid()})
    #dev.off()
  }
},video.name = "test_png.mp4", other.opts = "-b 1000k -s 1280x800")


close.nc(fid)