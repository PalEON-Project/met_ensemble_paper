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
vars.ecosys <- c("GPP", "NPP", "NEE", "LAI", "AGB", "TotSoilCarb", "SoilMoist", "tair")
vars.met <- c("air_temperature", "precipitation_flux", "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")
splice.points=c(1850, 1901, 2010)


path.google <- "~/Google Drive/PalEON_Met_Ensembles/"
# dat.all <- read.csv(file.path(path.google, "data", "Ensembles_AnnualMeans.csv"))
# summary(dat.all)

path.ed <- file.path(path.google, "data/ED_runs/3_spin_finish")
# path.ed <- file.path(path.google, "data/ED_runs/1_spin_initial")

ens.ed <- dir(path.ed)
ens.ed <- ens.ed[ens.ed!="Icon\r"]

dat.ed <- data.frame()
for(ens in 1:length(ens.ed)){
  files.ens <- dir(file.path(path.ed, ens.ed[ens]))
  files.ens <- files.ens[files.ens!="Icon\r"]
  dat.ens <- data.frame()
  for(i in 1:length(files.ens)){
    ncT <- ncdf4::nc_open(file.path(path.ed, ens.ed[ens], files.ens[i]))
    ntime <- ncdf4::ncvar_get(ncT, "time")
    
    dat.tmp <- data.frame(matrix(nrow=length(ntime), ncol=length(vars.ecosys)))
    names(dat.tmp) <- vars.ecosys
    for(v in vars.ecosys){
      if(v=="SoilMoist"){
        dat.tmp[,v] <- ncdf4::ncvar_get(ncT, v)[9,]
      } else {
        dat.tmp[,v] <- ncdf4::ncvar_get(ncT, v)
      }
      
    }
    
    ncdf4::nc_close(ncT)
    # if(i>1 & nrow(dat.tmp)<1200) stop("Missing years!")
    dat.ens <- rbind(dat.ens, dat.tmp)
  }
  dat.ens$ens <- substr(ens.ed[ens], 1, nchar(ens.ed[ens])-7)
  dat.ens$month <- c(6:12, rep(1:12, length.out=nrow(dat.ens)-7))
  dat.ens$year  <- c(rep(0, 7), rep(1:((nrow(dat.ens)-7)/12), each=12))
  
  dat.ed <- rbind(dat.ed, dat.ens)
}

# plot(dat.ed[1:(12*30), "tair"], type="l")
# plot(dat.ed[1:(12*30), "AGB"], type="l")
# plot(dat.ed[1:(12*30), "Fire"], type="l")

dat.stack <- stack(dat.ed[, vars.ecosys])
dat.stack$ens <- dat.ed$ens
dat.stack$year <- dat.ed$year
dat.stack$month <- dat.ed$month


dat.ed.yr <- aggregate(dat.stack[,"values"], dat.stack[,c("ens", "year", "ind")], FUN=mean)

# dat.stack <- stack(dat.ed.yr[, vars.ecosys])
# dat.stack$ens <- dat.ed.yr$ens
# dat.stack$year <- dat.ed.yr$year

dat.stack$ind <- factor(dat.stack$ind, levels=vars.ecosys)
# ggplot(data=dat.stack[dat.stack$year>max(dat.stack$year)-50,]) +
ggplot(data=dat.stack[dat.stack$year<50,]) +
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x=year+month/12-1/24, y=values, color=ens)) +
  # geom_vline(xintercept=seq((max(dat.stack$year)-50), max(dat.stack$year), by=30)) +
  theme_bw() +
  theme(legend.position="top")

ggplot(data=dat.ed.yr[dat.ed.yr$year>max(dat.stack$year)-50,]) +
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x=year, y=x, color=ens)) +
  # geom_vline(xintercept=seq(900, max(dat.ed.yr$year), by=30)) +
  theme_bw() +
  theme(legend.position="top")

ggplot(data=dat.stack) +
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x=year, y=values, color=ens)) +
  coord_cartesian(xlim=c(850, 900), expand=0) +
  theme_bw() +
  theme(legend.position="top")

ggplot(data=dat.stack) +
  facet_wrap(~ind, scales="free_y") +
  geom_line(aes(x=year, y=values, color=ens)) +
  coord_cartesian(xlim=c(1850, 2015), expand=0) +
  theme_bw() +
  theme(legend.position="top")

plot(dat.ens$AGB, type="l")
