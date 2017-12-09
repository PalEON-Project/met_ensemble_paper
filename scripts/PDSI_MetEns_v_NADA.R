# A figure showing the output of our met downscaling compared to a NADA reconstruction

library(ggplot2)

path.met <- "~/Desktop/Research/met_ensembles/data/met_ensembles/HARVARD.v5/"
path.google <- "~/Google Drive/PalEON_Met_Ensembles/"

splice.points=c(1850, 1901, 2010)

# ---------------
# Because it's easy, let start with empirical vs. ensemble PDSI for an illustrative figure
# ---------------
pdsi.nada <- read.csv(file.path(path.google, "data", "pdsi-reconstruction_Harvard.csv"))
pdsi.nada <- pdsi.nada[,2:ncol(pdsi.nada)]
names(pdsi.nada)[1] <- "year"
pdsi.nada$dataset <- as.factor("Empirical")
summary(pdsi.nada)

pdsi.ens <- read.csv(file.path(path.google, "data/HARVARD.v5/month", "PDSI_AllMembers.csv"))
yr.ind <- data.frame(year=rep(850:2015, each=12))
pdsi.ens.ann <- aggregate(pdsi.ens[,2:ncol(pdsi.ens)], by=list(yr.ind$year), mean)
summary(pdsi.ens.ann)

pdsi.met <- data.frame(year=850:2015)
pdsi.met$pdsi <- apply(pdsi.ens.ann[,2:ncol(pdsi.ens.ann)], 1, mean)
pdsi.met$pdsi_lower <- apply(pdsi.ens.ann[,2:ncol(pdsi.ens.ann)], 1, quantile, 0.025)
pdsi.met$pdsi_upper <- apply(pdsi.ens.ann[,2:ncol(pdsi.ens.ann)], 1, quantile, 0.975)
pdsi.met$dataset <- as.factor("Modeled")

pdsi <- rbind(pdsi.met, pdsi.nada)
summary(pdsi)

fact.smooth = 10
for(dat in unique(pdsi$dataset)){
  pdsi[pdsi$dataset==dat,"pdsi.smooth"] <- zoo::rollapply(pdsi[pdsi$dataset==dat,"pdsi"], width=fact.smooth, FUN=mean, fill=NA)
  pdsi[pdsi$dataset==dat,"lower.smooth"] <- zoo::rollapply(pdsi[pdsi$dataset==dat,"pdsi_lower"], width=fact.smooth, FUN=mean, fill=NA)
  pdsi[pdsi$dataset==dat,"upper.smooth"] <- zoo::rollapply(pdsi[pdsi$dataset==dat,"pdsi_upper"], width=fact.smooth, FUN=mean, fill=NA)
}

png(file.path(path.google, "figures", "PDSI_MetEns_v_NADA_smooth.png"), height=8, width=15, unit="in", res=220)
print(
  ggplot(data=pdsi) +
    geom_ribbon(aes(x=year, ymin=lower.smooth, ymax=upper.smooth, fill=dataset), alpha=0.5) +
    geom_line(aes(x=year, y=pdsi.smooth, color=dataset), size=1.5) +
    geom_vline(xintercept = splice.points, linetype="dashed", size=1, color="gray30") +
    scale_fill_manual(name="", values=c("red3", "black")) +
    scale_color_manual(name="", values=c("red3", "black")) +
    scale_x_continuous(expand=c(0,0)) +
    labs(x="Year", y="PDSI") +
    theme_bw() +
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

png(file.path(path.base, "figures", "PDSI_MetEns_v_NADA.png"), height=6, width=15, unit="in", res=220)
print(
  ggplot(data=pdsi) +
    geom_ribbon(aes(x=year, ymin=pdsi_lower, ymax=pdsi_upper, fill=dataset), alpha=0.5) +
    geom_line(aes(x=year, y=pdsi, color=dataset), size=1.5) +
    scale_fill_manual(name="", values=c("red3", "black")) +
    scale_color_manual(name="", values=c("red3", "black")) +
    scale_x_continuous(expand=c(0,0)) +
    labs(x="Year", y="PDSI") +
    theme_bw() +
    theme(axis.text = element_text(size=rel(1.5)),
          axis.title = element_text(size=rel(2), face="bold")) +
    theme(legend.position="top",
          legend.title = element_text(size=rel(2)),
          legend.text= element_text(size=rel(1.5)),
          legend.key.size = unit(1.5, "lines"))
)
dev.off()

# ---------------
