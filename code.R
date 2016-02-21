library(RNetCDF)
fname = "data/NOAA_Daily_phi_500mb.nc"
fid = open.nc(fname)
print.nc(fid)
dat = read.nc(fid)

close.nc(fid)