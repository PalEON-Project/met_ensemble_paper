# Looking at SIPNET output from the met ensemble
library(ggplot2) 

path.dat <- "~/Desktop/Research/met_ensemble_paper/SIPNET_runs/"

# load(file.path(path.dat, "SIPNET_MIROC-ESM_004", "ensemble.ts.1000016441.AGB.850.2015.Rdata"))
# summary(ensemble.ts)

vars.extract <- c("GPP", "NPP", "LAI", "AGB", "TotSoilCarb")

# Getting a yearly time series
ens.mems <- dir(path.dat)
dat.all <- data.frame()
for(ens in ens.mems){
  dat.yr <- data.frame(GCM=stringr::str_split(ens, "_")[[1]][1], ens=ens, year=850:2015, GPP=NA, NPP=NA, LAI=NA, AGB=NA, TotSoilCarb=NA)
  for(i in 1:nrow(dat.yr)){
    
    if(!file.exists(file.path(path.dat, ens, paste0(dat.yr$year[i], ".nc")))) next 
    ncT <- ncdf4::nc_open(file.path(path.dat, ens, paste0(dat.yr$year[i], ".nc")))
    
    for(v in vars.extract){
      dat.yr[i, v] <- mean(ncdf4::ncvar_get(ncT, v))
    }
    ncdf4::nc_close(ncT)
  }
  
  dat.all <- rbind(dat.all, dat.yr)
}


for(ens in unique(dat.all$ens)){
  for(v in vars.extract){
    dat.all[dat.all$ens==ens,paste0(v, ".100")] <- zoo::rollapply(dat.all[dat.all$ens==ens,v], 100, FUN=mean, fill=NA)
  }
}
summary(dat.all)

dat.yr2 <- stack(dat.all[,vars.extract])
dat.yr2$smoothed <- stack(dat.all[,paste0(vars.extract, ".100")])[,1]
dat.yr2$year <- dat.all$year
dat.yr2$ens <- dat.all$ens
summary(dat.yr2)

png("../figures/SIPNET_Test.png", height=6, width=8, units = "in", res=180)
ggplot(dat=dat.yr2) + 
  facet_wrap(~ind, scales="free_y", ncol=2) +
  geom_line(aes(x=year, y=smoothed, color=ens), alpha=0.8) +
  theme_bw()
dev.off()