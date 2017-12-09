# --------------------------------------------------------
# Looking at met versus ecosystem ensemble variability
# --------------------------------------------------------
# What we need to do
# 1. Pull Ecosystem data & Met data for each ensemble member (annual means)
#    - ecosys vars: GPP, NPP, NEE, LAI, AGB, SoilCarb
#    - met vars: Tair, Precip, SWDOWN, LWDOWN, Qair, Press, Wind
# 2. Calculate mean, SD, and CV for each variable for each year
#    - start with total ensemble, then break down by GCM & sub-members
# --------------------------------------------------------

library(ggplot2)

# --------------------------------------------------------
# 1. Pull Ecosystem data & Met data for each ensemble member (annual means)
# --------------------------------------------------------
vars.ecosys <- c("GPP", "NPP", "NEE", "LAI", "AGB", "TotSoilCarb")
vars.met <- c("air_temperature", "precipitation_flux", "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")
splice.points=c(1850, 1901, 2010)

# path.met <- "~/Desktop/Research/met_ensembles/data/met_ensembles/HARVARD.v5/"
# path.base <- "~/Desktop/Research/met_ensemble_paper/"
path.met <- "~/met_ensemble/data/met_ensembles/HARVARD.v5/"
path.base <- "~/met_ensemble_paper"
path.sipnet <- file.path(path.base, "SIPNET_runs")

# Lets only look at the met for which we have ecosystem model simulations completed
ens.all <- dir(path.sipnet) 
dat.all <- data.frame()
pb <- txtProgressBar(min=0, max=length(ens.all)*length(850:2015), style=3)
pb.ind=1
for(ens in ens.all){
  GCM <- stringr::str_split(ens, "_")[[1]][1]
  ens.day <- stringr::str_split(ens, "[.]")[[1]][1]
  dat.tmp <- data.frame(GCM=GCM, ens.day=ens.day, ens.hr=ens, year=850:2015)
  
  
  for(yr in 1:nrow(dat.tmp)){
    if(file.exists(file.path(path.sipnet, ens, paste0(dat.tmp$year[yr], ".nc")))){
      nc.eco <- ncdf4::nc_open(file.path(path.sipnet, ens, paste0(dat.tmp$year[yr], ".nc")))
      for(v in vars.ecosys){
        dat.tmp[yr, v] <- mean(ncdf4::ncvar_get(nc.eco, v))
      }
      ncdf4::nc_close(nc.eco)
      
    } else {
      for(v in vars.ecosys){
        dat.tmp[yr, v] <- NA
      }
    }
    
    
    if(file.exists(file.path(path.met, "1hr/ensembles", GCM, ens, paste0(ens, ".", stringr::str_pad(dat.tmp$year[yr], 4, pad="0"), ".nc")))){
      nc.met <- ncdf4::nc_open(file.path(path.met, "1hr/ensembles", GCM, ens, paste0(ens, ".", stringr::str_pad(dat.tmp$year[yr], 4, pad="0"), ".nc")))
      for(v in vars.met){
        dat.tmp[yr, v] <- mean(ncdf4::ncvar_get(nc.met, v))
      }
      ncdf4::nc_close(nc.met)
      
    } else {
      for(v in vars.met){
        dat.tmp[yr, v] <- NA
      }
      
    }
    
    setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
  }
  
  dat.all <- rbind(dat.all, dat.tmp)
  
}
summary(dat.all)

# Save the output just to save time the next time around
write.csv(dat.all, file.path(path.base, "data", "Ensembles_AnnualMeans.csv"), row.names=F)
# --------------------------------------------------------

