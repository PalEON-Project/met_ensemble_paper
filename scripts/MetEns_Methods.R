# Create some simplified before & after images of the met downscaling for AGU 2017
library(ggplot2)
path.google <- "~/Google Drive/PalEON_Met_Ensembles/"
path.debias <- file.path(path.google, "data/HARVARD.v5/day")
# dir(path.debias)

splice.points=c(1850, 1901, 2010)

ann.raw <- read.csv(file.path(path.debias, "Met_Raw_Annual.csv"))
doy.raw <- read.csv(file.path(path.debias, "Met_Raw_DOY.csv"))
ann.cor <- read.csv(file.path(path.debias, "Met_Corrected_Annual.csv"))
doy.cor <- read.csv(file.path(path.debias, "Met_Corrected_DOY.csv"))

ann.raw[ann.raw$met.var %in% c("tair.max", "tair.min"),"raw"] <- ann.raw[ann.raw$met.var %in% c("tair.max", "tair.min"),"raw"] - 273.15
doy.raw[doy.raw$met.var %in% c("tair.max", "tair.min"),"raw"] <- doy.raw[doy.raw$met.var %in% c("tair.max", "tair.min"),"raw"] - 273.15
ann.cor[ann.cor$met.var %in% c("tair.max", "tair.min"),c("mean", "lwr", "upr")] <- ann.cor[ann.cor$met.var %in% c("tair.max", "tair.min"),c("mean", "lwr", "upr")] - 273.15
doy.cor[doy.cor$met.var %in% c("tair.max", "tair.min"),c("mean", "lwr", "upr")] <- doy.cor[doy.cor$met.var %in% c("tair.max", "tair.min"),c("mean", "lwr", "upr")] - 273.15


fact.smooth = 10
for(v in unique(ann.raw$met.var)){
  for(mod in unique(ann.raw$dataset2) ){
    ann.raw[ann.raw$met.var==v & ann.raw$dataset2==mod, "raw.smooth"] <- zoo::rollapply(ann.raw[ann.raw$met.var==v & ann.raw$dataset2==mod, "raw"], fact.smooth, mean, fill=NA)
    doy.raw[doy.raw$met.var==v & doy.raw$dataset2==mod, "raw.smooth"] <- zoo::rollapply(doy.raw[doy.raw$met.var==v & doy.raw$dataset2==mod, "raw"], fact.smooth, mean, fill=NA) 
  }
    
  for(mod in unique(ann.cor$dataset) ){
    ann.cor[ann.cor$met.var==v & ann.cor$dataset==mod, "mean.smooth"] <- zoo::rollapply(ann.cor[ann.cor$met.var==v & ann.cor$dataset==mod, "mean"], fact.smooth, mean, fill=NA)
    ann.cor[ann.cor$met.var==v & ann.cor$dataset==mod, "lwr.smooth" ] <- zoo::rollapply(ann.cor[ann.cor$met.var==v & ann.cor$dataset==mod, "lwr" ], fact.smooth, mean, fill=NA)
    ann.cor[ann.cor$met.var==v & ann.cor$dataset==mod, "upr.smooth" ] <- zoo::rollapply(ann.cor[ann.cor$met.var==v & ann.cor$dataset==mod, "upr" ], fact.smooth, mean, fill=NA)
    
    doy.cor[doy.cor$met.var==v & doy.cor$dataset==mod, "mean.smooth"] <- zoo::rollapply(doy.cor[doy.cor$met.var==v & doy.cor$dataset==mod, "mean"], fact.smooth, mean, fill=NA)
    doy.cor[doy.cor$met.var==v & doy.cor$dataset==mod, "lwr.smooth" ] <- zoo::rollapply(doy.cor[doy.cor$met.var==v & doy.cor$dataset==mod, "lwr" ], fact.smooth, mean, fill=NA)
    doy.cor[doy.cor$met.var==v & doy.cor$dataset==mod, "upr.smooth" ] <- zoo::rollapply(doy.cor[doy.cor$met.var==v & doy.cor$dataset==mod, "upr" ], fact.smooth, mean, fill=NA)
  }
  
}


summary(ann.raw)

ann.raw$dataset <- factor(ann.raw$dataset, levels=c("NLDAS", "CRUNCEP", "bcc-csm1-1", "CCSM4", "MIROC-ESM", "MPI-ESM-P"))

png(file.path(path.google, "figures", "TimeSeries_Met_Raw_1800-2015_raw.png"), height=6, width=6, unit="in", res=220)
print(
  ggplot(data=ann.raw[ann.raw$met.var %in% c("tair.min", "swdown"),]) +
    facet_grid(met.var~., scales="free_y") +
    geom_line(aes(x=Year, y=raw, color=dataset, size=dataset, group=dataset2, alpha=dataset)) +
    geom_vline(xintercept=splice.points, linetype="dashed", size=1) +
    scale_color_manual(values=c("black", "orange2", "deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    scale_size_manual(values=c(2.5, rep(1.5, length(unique(ann.raw$dataset))-1))) +
    scale_alpha_manual(values=c(1, 1, rep(0.75, length(unique(ann.raw$dataset))-2))) +
    coord_cartesian(xlim=c(1800, 2015), expand=0) +
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

png(file.path(path.google, "figures", "TimeSeries_Met_Raw_0850-2015_smooth.png"), height=6, width=6, unit="in", res=220)
print(
  ggplot(data=ann.raw[ann.raw$met.var %in% c("tair.min", "swdown"),]) +
    facet_grid(met.var~., scales="free_y") +
    geom_line(aes(x=Year, y=raw.smooth, color=dataset, size=dataset, group=dataset2, alpha=dataset)) +
    geom_vline(xintercept=splice.points, linetype="dashed", size=1) +
    scale_color_manual(values=c("black", "orange2", "deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    scale_size_manual(values=c(4, rep(2, length(unique(ann.raw$dataset))-1))) +
    scale_alpha_manual(values=c(1, 1, rep(0.75, length(unique(ann.raw$dataset))-2))) +
    coord_cartesian(expand=0) +
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

doy.raw$dataset <- factor(doy.raw$dataset, levels=c("bcc-csm1-1", "CCSM4", "MIROC-ESM", "MPI-ESM-P", "CRUNCEP", "NLDAS"))

png(file.path(path.google, "figures", "TimeSeries_Met_Raw_DOY.png"), height=6, width=6, unit="in", res=220)
print(
  ggplot(data=doy.raw[doy.raw$met.var %in% c("tair.min", "swdown"),]) +
    facet_grid(met.var~., scales="free_y") +
    geom_line(aes(x=DOY, y=raw, color=dataset, size=dataset, group=dataset2)) +
    # geom_vline(xintercept=splice.points, linetype="dashed", size=1) +
    scale_color_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4", "orange2", "black")) +
    scale_size_manual(values=c(rep(2, length(unique(ann.raw$dataset))-1), 1.5)) +
    # scale_alpha_manual(values=c(rep(0.75, length(unique(ann.raw$dataset))-2), 1, 1)) +
    coord_cartesian(expand=0) +
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


# ------------
ann.cor$dataset <- factor(ann.cor$dataset, levels=c("bcc-csm1-1", "CCSM4", "MIROC-ESM", "MPI-ESM-P"))

png(file.path(path.google, "figures", "TimeSeries_Met_Cor_1800-2015_raw.png"), height=6, width=6, unit="in", res=220)
print(
  ggplot(data=ann.cor[ann.cor$met.var %in% c("tair.min", "swdown") & ann.cor$Year>=1800,]) +
    facet_grid(met.var~., scales="free_y") +
    geom_ribbon(aes(x=Year, ymin=lwr, ymax=upr, fill=dataset), alpha=0.5) +
    geom_line(aes(x=Year, y=mean, color=dataset)) +
    geom_vline(xintercept=splice.points, linetype="dashed", size=1) +
    scale_fill_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    scale_color_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    scale_size_manual(values=c(2.5, rep(1.5, length(unique(ann.cor$dataset))-1))) +
    scale_alpha_manual(values=c(1, 1, rep(0.75, length(unique(ann.cor$dataset))-2))) +
    # coord_cartesian(xlim=c(1800, 2015), expand=0) +
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

png(file.path(path.google, "figures", "TimeSeries_Met_Cor_1800-2015_smooth.png"), height=6, width=6, unit="in", res=220)
print(
  ggplot(data=ann.cor[ann.cor$met.var %in% c("tair.min", "swdown") & ann.cor$Year>=1800,]) +
    facet_grid(met.var~., scales="free_y") +
    geom_ribbon(aes(x=Year, ymin=lwr.smooth, ymax=upr.smooth, fill=dataset), alpha=0.5) +
    geom_line(aes(x=Year, y=mean.smooth, color=dataset), size=2) +
    geom_vline(xintercept=splice.points, linetype="dashed", size=1) +
    scale_fill_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    scale_color_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    # scale_size_manual(values=c(2.5, rep(1.5, length(unique(ann.cor$dataset))-1))) +
    # scale_alpha_manual(values=c(1, 1, rep(0.75, length(unique(ann.cor$dataset))-2))) +
    coord_cartesian(xlim=c(1800, 2015), expand=0) +
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

png(file.path(path.google, "figures", "TimeSeries_Met_Cor_0850-2015_smooth.png"), height=8, width=15, unit="in", res=220)
print(
  ggplot(data=ann.cor[ann.cor$met.var %in% c("tair.min", "swdown"),]) +
    facet_grid(met.var~., scales="free_y") +
    geom_ribbon(aes(x=Year, ymin=lwr.smooth, ymax=upr.smooth, fill=dataset), alpha=0.5) +
    geom_line(aes(x=Year, y=mean.smooth, color=dataset), size=2) +
    geom_vline(xintercept=splice.points, linetype="dashed", size=1) +
    scale_fill_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    scale_color_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    # scale_size_manual(values=c(2.5, rep(1.5, length(unique(ann.cor$dataset))-1))) +
    # scale_alpha_manual(values=c(1, 1, rep(0.75, length(unique(ann.cor$dataset))-2))) +
    coord_cartesian(expand=0) +
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

png(file.path(path.google, "figures", "TimeSeries_Met_Cor_0850-2015_smooth2.png"), height=8, width=15, unit="in", res=220)
print(
  ggplot(data=ann.cor[ann.cor$met.var %in% c("tair.min", "precip"),]) +
    facet_grid(met.var~., scales="free_y") +
    geom_ribbon(aes(x=Year, ymin=lwr.smooth, ymax=upr.smooth, fill=dataset), alpha=0.5) +
    geom_line(aes(x=Year, y=mean.smooth, color=dataset), size=2) +
    geom_vline(xintercept=splice.points, linetype="dashed", size=1) +
    scale_fill_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    scale_color_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    # scale_size_manual(values=c(2.5, rep(1.5, length(unique(ann.cor$dataset))-1))) +
    # scale_alpha_manual(values=c(1, 1, rep(0.75, length(unique(ann.cor$dataset))-2))) +
    coord_cartesian(expand=0) +
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


doy.cor$dataset <- factor(doy.cor$dataset, levels=c("bcc-csm1-1", "CCSM4", "MIROC-ESM", "MPI-ESM-P"))
# data=ann.raw[ann.raw$met.var %in% c("tair.min", "swdown"),]
png(file.path(path.google, "figures", "TimeSeries_Met_Cor_DOY.png"), height=6, width=6, unit="in", res=220)
print(
  ggplot(data=doy.cor[doy.cor$met.var %in% c("tair.min", "swdown"),]) +
    facet_grid(met.var~., scales="free_y") +
    geom_line(data=doy.raw[doy.raw$dataset=="NLDAS" & doy.raw$met.var %in% c("tair.min", "swdown"),], aes(x=DOY, y=raw), color="black") +
    geom_ribbon(aes(x=DOY, ymin=lwr, ymax=upr, fill=dataset), alpha=0.5) +
    geom_line(aes(x=DOY, y=mean, color=dataset)) +
    # geom_vline(xintercept=splice.points, linetype="dashed", size=1) +
    scale_fill_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    scale_color_manual(values=c("deepskyblue", "seagreen4", "gold3", "dodgerblue4")) +
    # scale_size_manual(values=c(rep(2, length(unique(ann.cor$dataset))-1), 1.5)) +
    # scale_alpha_manual(values=c(rep(0.75, length(unique(ann.cor$dataset))-2), 1, 1)) +
    coord_cartesian(expand=0) +
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
