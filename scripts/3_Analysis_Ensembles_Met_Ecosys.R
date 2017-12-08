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


# --------------------------------------------------------
# 2. Calculate mean, SD, and CV for each variable for each year
# --------------------------------------------------------
# ens.stats.yr <- array(dim=c(length(850:2015), length(vars.ecosys)+length(vars.met), 3))
# dimnames(ens.stats.yr) <-list(year= 850:2015, var=c(vars.ecosys, vars.met), stat=c("mean", "SD", "CV"))


ens.mean <- aggregate(dat.all[,c(vars.ecosys, vars.met)], by=list(dat.all$year), FUN=mean)
ens.lwr  <- aggregate(dat.all[,c(vars.ecosys, vars.met)], by=list(dat.all$year), FUN=quantile, 0.025)
ens.upr  <- aggregate(dat.all[,c(vars.ecosys, vars.met)], by=list(dat.all$year), FUN=quantile, 0.975)
ens.sd  <- aggregate(dat.all[,c(vars.ecosys, vars.met)], by=list(dat.all$year), FUN=sd)

# Convert temp to Celcius rather than Kelvin
ens.mean$air_temperature <- ens.mean$air_temperature - 273.15
ens.lwr$air_temperature <- ens.lwr$air_temperature - 273.15
ens.upr$air_temperature <- ens.upr$air_temperature - 273.15

ens.CV <- ens.mean; 
ens.CV[,2:ncol(ens.CV)] <- ens.sd[,2:ncol(ens.sd)]/ens.mean[,2:ncol(ens.mean)]*100

names(ens.mean)[1] <- names(ens.sd)[1] <- names(ens.CV)[1] <- "year"
summary(ens.mean)
summary(ens.sd)
summary(ens.CV)

ens.stats.yr <- stack(ens.mean[,2:ncol(ens.mean)])
names(ens.stats.yr) <- c("mean", "var")
ens.stats.yr$year <- ens.mean$year
ens.stats.yr$lower <- stack(ens.lwr[,2:ncol(ens.mean)])[,1]
ens.stats.yr$upper <- stack(ens.upr[,2:ncol(ens.mean)])[,1]
ens.stats.yr$SD <- stack(ens.sd[,2:ncol(ens.mean)])[,1]
ens.stats.yr$CV <- stack(ens.CV[,2:ncol(ens.mean)])[,1]
ens.stats.yr$type <- as.factor(ifelse(ens.stats.yr$var %in% vars.ecosys, "Ecosystem", "Driver"))
summary(ens.stats.yr)

# Adding smoothing to the figures
fact.smooth = 10
for(v in unique(ens.stats.yr$var)){
  ens.stats.yr[ens.stats.yr$var==v, "mean.smooth"] <- zoo::rollapply(ens.stats.yr[ens.stats.yr$var==v, "mean"], fact.smooth, mean, fill=NA)
  ens.stats.yr[ens.stats.yr$var==v, "lower.smooth"] <- zoo::rollapply(ens.stats.yr[ens.stats.yr$var==v, "lower"], fact.smooth, mean, fill=NA)
  ens.stats.yr[ens.stats.yr$var==v, "upper.smooth"] <- zoo::rollapply(ens.stats.yr[ens.stats.yr$var==v, "upper"], fact.smooth, mean, fill=NA)
  ens.stats.yr[ens.stats.yr$var==v, "SD.smooth"] <- zoo::rollapply(ens.stats.yr[ens.stats.yr$var==v, "SD"], fact.smooth, mean, fill=NA)
  ens.stats.yr[ens.stats.yr$var==v, "CV.smooth"] <- zoo::rollapply(ens.stats.yr[ens.stats.yr$var==v, "CV"], fact.smooth, mean, fill=NA)
}


png(file.path(path.base, "figures", "TimeSeries_values_1800-2015_raw.png"), height=6, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "air_temperature", "precipitation_flux"),]) +
    facet_grid(var~., scales="free_y") +
    geom_ribbon(aes(x=year, ymin=lower, ymax=upper, fill=var), alpha=0.5) +
    geom_line(aes(x=year, y=mean, color=var), size=1) +
    geom_vline(xintercept=splice.points, linetype="dashed") +
    scale_fill_manual(values=c("green4", "purple4", "coral2", "dodgerblue3")) +
    scale_color_manual(values=c("green4", "purple4", "coral2", "dodgerblue3")) +
    guides(color=F, fill=F) +
    # scale_x_continuous(expand=c(0,0)) +
    coord_cartesian(xlim=c(1800, 2015), expand=0) +
    theme_bw()
)
dev.off()

png(file.path(path.base, "figures", "TimeSeries_values_0850-2015_smooth.png"), height=6, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "air_temperature", "precipitation_flux"),]) +
    facet_grid(var~., scales="free_y") +
    # geom_ribbon(aes(x=year, ymin=mean.smooth-SD.smooth, ymax=mean.smooth+SD.smooth), alpha=0.5) +
    geom_ribbon(aes(x=year, ymin=lower.smooth, ymax=upper.smooth, fill=var), alpha=0.5) +
    geom_line(aes(x=year, y=mean.smooth, color=var)) +
    geom_vline(xintercept=splice.points, linetype="dashed", size=0.5) +
    scale_fill_manual(values=c("green4", "purple4", "coral2", "dodgerblue3")) +
    scale_color_manual(values=c("green4", "purple4", "coral2", "dodgerblue3")) +
    guides(color=F, fill=F) +
    # scale_x_continuous(expand=c(0,0)) +
    coord_cartesian(expand=0) +
    theme_bw()
)
dev.off()

png(file.path(path.base, "figures", "TimeSeries_CV_0850-2015_smooth.png"), height=6, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "air_temperature", "precipitation_flux"),]) +
    # facet_grid(var~., scales="free_y") +
    geom_line(aes(x=year, y=CV.smooth, color=var, alpha=type)) +
    geom_vline(xintercept=splice.points+fact.smooth/2, linetype="dashed") +
    scale_color_manual(values=c("green4", "purple4", "coral2", "dodgerblue3")) +
    scale_alpha_manual(values=c(0.5, 1)) +
    scale_x_continuous(expand=c(0,0)) +
    guides(alpha=F) +
    # coord_cartesian(xlim=c(1800, 2015)) +
    theme_bw() +
    theme(legend.position="top")
)
dev.off()

png(file.path(path.base, "figures", "TimeSeries_CV_1800-2015_raw.png"), height=6, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "air_temperature", "precipitation_flux"),]) +
    # facet_grid(type~., scales="free_y") +
    geom_line(aes(x=year, y=CV, color=var, alpha=type)) +
    geom_vline(xintercept=splice.points, linetype="dashed") +
    scale_color_manual(values=c("green4", "purple4", "coral2", "dodgerblue3")) +
    scale_alpha_manual(values=c(0.5, 1)) +
    guides(alpha=F) +
    scale_x_continuous(expand=c(0,0)) +
    coord_cartesian(xlim=c(1800, 2015)) +
    theme_bw() +
    theme(legend.position="top")
)
dev.off()

# ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "NPP", "LAI", "AGB", "TotSoilCarb"),]) +
#   facet_grid(.~type) +
#   # geom_ribbon(aes(x=year, ymin=mean-SD, ymax=mean+SD)) +
#   geom_line(aes(x=year, y=CV.smooth, color=var)) +
#   geom_vline(xintercept=splice.points+fact.smooth/2, linetype="dashed") +
#   theme_bw()
# 
# ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c(vars.met),]) +
#   facet_grid(.~type) +
#   # geom_ribbon(aes(x=year, ymin=mean-SD, ymax=mean+SD)) +
#   geom_line(aes(x=year, y=CV.smooth, color=var)) +
#   geom_vline(xintercept=splice.points+fact.smooth/2, linetype="dashed") +
#   theme_bw()

# --------------------------------------------------------

