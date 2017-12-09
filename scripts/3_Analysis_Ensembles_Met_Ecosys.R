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


path.google <- "~/Google Drive/PalEON_Met_Ensembles/"
dat.all <- read.csv(file.path(path.google, "data", "Ensembles_AnnualMeans.csv"))
summary(dat.all)

vars.flux <- c("precipitation_flux", "GPP", "NPP", "NEE")
sec2yr <- 60*60*24*365.25 # 60 s/min * 60 min/hr * 24 hr/day * 365.25 day/yr
dat.all[,vars.flux] <- dat.all[,vars.flux] * sec2yr

dat.all$air_temperature <- dat.all$air_temperature-273.15
summary(dat.all)
# --------------------------------------------------------


# --------------------------------------------------------
# 2. Calculate mean, SD, and CV for each variable for each year
#     - graph trends through time
# --------------------------------------------------------
# ens.stats.yr <- array(dim=c(length(850:2015), length(vars.ecosys)+length(vars.met), 3))
# dimnames(ens.stats.yr) <-list(year= 850:2015, var=c(vars.ecosys, vars.met), stat=c("mean", "SD", "CV"))


ens.mean <- aggregate(dat.all[,c(vars.ecosys, vars.met)], by=list(dat.all$year), FUN=mean, na.rm=T)
ens.lwr  <- aggregate(dat.all[,c(vars.ecosys, vars.met)], by=list(dat.all$year), FUN=quantile, 0.025, na.rm=T)
ens.upr  <- aggregate(dat.all[,c(vars.ecosys, vars.met)], by=list(dat.all$year), FUN=quantile, 0.975, na.rm=T)
ens.sd  <- aggregate(dat.all[,c(vars.ecosys, vars.met)], by=list(dat.all$year), FUN=sd, na.rm=T)

# Convert temp to Celcius rather than Kelvin
ens.mean$air_temperature <- ens.mean$air_temperature
ens.lwr$air_temperature <- ens.lwr$air_temperature
ens.upr$air_temperature <- ens.upr$air_temperature

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

ens.stats.yr$var <- car::recode(ens.stats.yr$var, "'air_temperature'='Temp'; 
                                                   'precipitation_flux'='Precip'; 
                                                   'surface_downwelling_shortwave_flux_in_air'='SW Rad'; 
                                                   'surface_downwelling_longwave_flux_in_air' ='LW Rad'; 
                                                   'air_pressure'='Press'; 
                                                   'specific_humidity'='Humidity';
                                                   'wind_speed'='Wind'")
ens.stats.yr$var <- factor(ens.stats.yr$var, levels=c("Temp", "Precip", "SW Rad.", "LW Rad.", "Press", "Humidity", "Wind", vars.ecosys))

png(file.path(path.google, "figures", "TimeSeries_values_1800-2015_raw.png"), height=8, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "Temp", "Precip") & ens.stats.yr$year>=1800,]) +
    facet_grid(var~., scales="free_y") +
    geom_ribbon(aes(x=year, ymin=lower, ymax=upper, fill=var), alpha=0.5) +
    geom_line(aes(x=year, y=mean, color=var), size=1.5) +
    geom_vline(xintercept=splice.points, linetype="dashed", size=2) +
    scale_fill_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    scale_color_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    scale_x_continuous(expand=c(0,0)) +
    guides(color=F, fill=F) +
    labs(x="Year") +
    scale_x_continuous(expand=c(0,0)) +
    coord_cartesian(xlim=c(1800, 2015)) +
    theme_bw() +
    theme(axis.text=element_text(size=rel(2)),
          axis.title.x = element_text(size=rel(2.5), face="bold"),
          axis.title.y = element_blank(),
          strip.text = element_text(size=rel(2), face="bold"))
)
dev.off()

png(file.path(path.google, "figures", "TimeSeries_values_0850-2015_smooth.png"), height=8, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "Temp", "Precip"),]) +
    facet_grid(var~., scales="free_y") +
    # geom_ribbon(aes(x=year, ymin=mean.smooth-SD.smooth, ymax=mean.smooth+SD.smooth), alpha=0.5) +
    geom_ribbon(aes(x=year, ymin=lower.smooth, ymax=upper.smooth, fill=var), alpha=0.5) +
    geom_line(aes(x=year, y=mean.smooth, color=var), size=1.75) +
    geom_vline(xintercept=splice.points, linetype="solid", size=5, alpha=0.25) +
    scale_fill_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    scale_color_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    guides(color=F, fill=F) +
    # scale_x_continuous(expand=c(0,0)) +
    coord_cartesian(expand=0) +
    theme_bw()
)
dev.off()

png(file.path(path.google, "figures", "TimeSeries_values_1800-2015_smooth.png"), height=8, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "Temp", "Precip") & ens.stats.yr$year>=1800,]) +
    facet_grid(var~., scales="free_y") +
    # geom_ribbon(aes(x=year, ymin=mean.smooth-SD.smooth, ymax=mean.smooth+SD.smooth), alpha=0.5) +
    geom_ribbon(aes(x=year, ymin=lower.smooth, ymax=upper.smooth, fill=var), alpha=0.5) +
    geom_line(aes(x=year, y=mean.smooth, color=var), size=1.75) +
    geom_vline(xintercept=splice.points, linetype="solid", size=10, alpha=0.25) +
    scale_fill_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    scale_color_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    guides(color=F, fill=F) +
    labs(x="Year") +
    scale_x_continuous(expand=c(0,0)) +
    coord_cartesian(xlim=c(1800, 2015)) +
    theme_bw() +
    theme(axis.text=element_text(size=rel(2)),
          axis.title.x = element_text(size=rel(2.5), face="bold"),
          axis.title.y = element_blank(),
          strip.text = element_text(size=rel(2), face="bold"))
)
dev.off()

png(file.path(path.google, "figures", "TimeSeries_CV_0850-2015_smooth.png"), height=8, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "Temp", "Precip"),]) +
    # facet_grid(var~., scales="free_y") +
    geom_line(aes(x=year, y=CV.smooth, color=var, alpha=type)) +
    geom_vline(xintercept=splice.points, linetype="solid", size=5, alpha=0.25) +
    scale_color_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    scale_alpha_manual(values=c(0.5, 1)) +
    scale_x_continuous(expand=c(0,0)) +
    guides(alpha=F) +
    # coord_cartesian(xlim=c(1800, 2015)) +
    theme_bw() +
    theme(legend.position="top")
)
dev.off()

png(file.path(path.google, "figures", "TimeSeries_CV_1800-2015_smooth.png"), height=8, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "Temp", "Precip"),]) +
    # facet_grid(var~., scales="free_y") +
    geom_line(aes(x=year, y=CV.smooth, color=var, alpha=type), size=3) +
    geom_vline(xintercept=splice.points, linetype="solid", size=17, alpha=0.25) +
    scale_color_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    scale_alpha_manual(values=c(0.5, 1)) +
    scale_x_continuous(expand=c(0,0)) +
    labs(x="Year", y="CV") +
    guides(alpha=F) +
    coord_cartesian(xlim=c(1800, 2015)) +
    theme_bw() +
    # theme(exis.text) +
    theme(axis.text=element_text(size=rel(2)),
          axis.title.x = element_text(size=rel(2.5), face="bold"),
          axis.title.y = element_text(size=rel(2.5), face="bold"),
          strip.text = element_text(size=rel(2), face="bold")) +
    theme(legend.position="top",
          legend.text=element_text(size=rel(2)),
          legend.key.size=unit(2, "lines"),
          legend.title=element_blank())
)
dev.off()


png(file.path(path.google, "figures", "TimeSeries_CV_1800-2015_raw.png"), height=8, width=15, unit="in", res=220)
print(
  ggplot(dat=ens.stats.yr[ens.stats.yr$var %in% c("GPP", "AGB", "Temp", "Precip") & ens.stats.yr$year>=1800,]) +
    # facet_grid(type~., scales="free_y") +
    geom_line(aes(x=year, y=CV, color=var, alpha=type, size=type)) +
    geom_vline(xintercept=splice.points, linetype="dashed", size=2) +
    scale_fill_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    scale_color_manual(values=c("coral2", "dodgerblue3", "green4", "purple4")) +
    scale_alpha_manual(values=c(0.5, 1)) +
    scale_size_manual(values=c(1.5, 3)) +
    guides(alpha=F, size=F, color=guide_legend(override.aes=list(size=3))) +
    labs(x="Year") +
    scale_x_continuous(expand=c(0,0)) +
    coord_cartesian(xlim=c(1800, 2015)) +
    theme_bw() +
    theme(legend.position="top",
          legend.text=element_text(size=rel(2)),
          legend.key.size=unit(2, "lines"),
          legend.title=element_blank()) +
    theme(axis.text=element_text(size=rel(2)),
          axis.title.x = element_text(size=rel(2.5), face="bold"),
          axis.title.y = element_blank(),
          strip.text = element_text(size=rel(2), face="bold"))
)
dev.off()

# --------------------------------------------------------


# --------------------------------------------------------
# Looking at met vs. model ensemble spread
# --------------------------------------------------------
summary(dat.all)
summary(ens.CV)

ggplot(data=ens.CV) +
  geom_point(aes(x=air_temperature, y=GPP))

ggplot(data=ens.CV) +
  geom_point(aes(x=precipitation_flux, y=GPP))

ggplot(data=ens.CV) +
  geom_point(aes(x=air_temperature, y=AGB))

# --------------------------------------------------------


# --------------------------------------------------------
# Trying to figure out the effects of short- versus long-term variation 
# on ecosystem dynamics
# 
# Workflow
# 1. Calculate ensemble member deviation from the mean for each year
# 2. Identify years/ensemble members where a model is outside of its normal
#    relation
# 3. Compare the ecosystem model relation in the year of & X years after that deviation
#    - use someting similar to SEA to look at magnitude and lags of these deviations
# --------------------------------------------------------
summary(dat.all)

dat.all[,paste0(c(vars.ecosys, vars.met), ".dev")] <- NA
for(yr in 850:2015){
  for(v in c(vars.ecosys, vars.met)){
    dat.all[dat.all$year==yr, paste0(v, ".dev")] <- dat.all[dat.all$year==yr, v] - mean(dat.all[dat.all$year==yr, v])
  }
  # yr.means <- apply(dat.all[dat.all$year==yr,c(vars.ecosys, vars.met)], 2, mean, na.rm=T)
  # dat.all[dat.all$year==yr,paste0(c(vars.ecosys, vars.met), ".dev")] <- dat.all[dat.all$year==yr, c(vars.ecosys, vars.met)]-yr.means
}
summary(dat.all)

ens.stats.mean <- aggregate(dat.all[,paste0(c(vars.ecosys, vars.met), ".dev")], 
                            by=dat.all[,c("GCM", "ens.day", "ens.hr")], 
                            FUN=mean)
ens.stats.sd   <- aggregate(dat.all[,paste0(c(vars.ecosys, vars.met), ".dev")], 
                            by=dat.all[,c("GCM", "ens.day", "ens.hr")], 
                            FUN=sd)
ens.stats.min   <- aggregate(dat.all[,paste0(c(vars.ecosys, vars.met), ".dev")], 
                            by=dat.all[,c("GCM", "ens.day", "ens.hr")], 
                            FUN=min)
ens.stats.max   <- aggregate(dat.all[,paste0(c(vars.ecosys, vars.met), ".dev")], 
                             by=dat.all[,c("GCM", "ens.day", "ens.hr")], 
                             FUN=max)
# summary(ens.stats)

ens.stats.mean[,c("GCM", "ens.day", "ens.hr", "air_temperature.dev", "precipitation_flux.dev", "GPP.dev", "AGB.dev")]
ens.stats.sd  [,c("GCM", "ens.day", "ens.hr", "air_temperature.dev", "precipitation_flux.dev", "GPP.dev", "AGB.dev")]
ens.stats.min [,c("GCM", "ens.day", "ens.hr", "air_temperature.dev", "precipitation_flux.dev", "GPP.dev", "AGB.dev")]
ens.stats.max [,c("GCM", "ens.day", "ens.hr", "air_temperature.dev", "precipitation_flux.dev", "GPP.dev", "AGB.dev")]

# Just spot checking how many SD the max dev for MIROC_ESM_004.03 is from its mean
ens.extreme <- data.frame()
for(ens in unique(ens.stats$ens.hr)){
  under <- which(dat.all[dat.all$ens.hr==ens, "air_temperature.dev"] <= ens.stats.mean[ens.stats.mean$ens.hr==ens, "air_temperature.dev"] - 3*ens.stats.sd[ens.stats.sd$ens.hr==ens, "air_temperature.dev"])  
  over  <- which(dat.all[dat.all$ens.hr==ens, "air_temperature.dev"] >= ens.stats.mean[ens.stats.mean$ens.hr==ens, "air_temperature.dev"] + 3*ens.stats.sd[ens.stats.sd$ens.hr==ens, "air_temperature.dev"] )

  # Converting indices to years
  under <- dat.all[dat.all$ens.hr==ens, "year"][under]
  over  <- dat.all[dat.all$ens.hr==ens, "year"][over]
  
  dat.tmp <- data.frame(ens.hr=ens, type=c(rep("over", length(over)), rep("under", length(under))),
                        yr.anom=c(over, under))

  ens.extreme <- rbind(ens.extreme, dat.tmp)
}
# names(ens.extreme) <- c("ens", "under", "over")
summary(ens.extreme)

# Setting up a data frame that pulls these anomalous years and the preceeding and following 5
yrs.lag = 3
dat.sea <- data.frame()
for(i in 1:nrow(ens.extreme)){
  dat.tmp <- data.frame(ens.extreme[i,])
  for(j in -yrs.lag:yrs.lag){
    dat.tmp[,paste0("p", j)] <- dat.all[dat.all$ens.hr==dat.tmp$ens.hr & dat.all$year==dat.tmp$yr.anom+j,"GPP.dev"]
  }
  
  dat.sea <- rbind(dat.sea, dat.tmp)
}
summary(dat.sea)
summary(dat.sea[dat.sea$type=="over",])

# --------------------------------------------------------
# 