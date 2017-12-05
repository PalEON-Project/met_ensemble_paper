# Looking at SIPNET output from the met ensemble

path.dat <- "~/Desktop/Research/met_ensemble_paper/SIPNET_runs/"

# load(file.path(path.dat, "SIPNET_MIROC-ESM_004", "ensemble.ts.1000016441.AGB.850.2015.Rdata"))
# summary(ensemble.ts)

# Getting a yearly time series
dat.yr <- data.frame(year=850:2015, GPP=NA, NPP=NA, LAI=NA, AGB=NA, TotSoilCarb=NA)
for(i in 1:nrow(dat.yr)){
  ncT <- ncdf4::nc_open(file.path(path.dat, "SIPNET_MIROC-ESM_004", "out/1001814484", paste0(dat.yr$year[i], ".nc")))
  
  for(v in names(dat.yr)[2:ncol(dat.yr)]){
    dat.yr[i, v] <- mean(ncdf4::ncvar_get(ncT, v))
  }
  ncdf4::nc_close(ncT)
}

summary(dat.yr)


dat.yr2 <- stack(dat.yr[,c("GPP", "NPP", "LAI", "AGB", "TotSoilCarb")])
dat.yr2$year <- dat.yr$year
summary(dat.yr2)

library(ggplot2) 
png("../figures/SIPNET_Test.png", height=6, width=8, units = "in", res=180)
ggplot(dat=dat.yr2) + 
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x=year, y=values))
dev.off()