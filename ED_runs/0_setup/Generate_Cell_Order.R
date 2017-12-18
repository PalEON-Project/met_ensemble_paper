# ------------------------------------------------------------------
# Reformat Randomly generated order for met iterations for my ED runs
# 
# Christy Rollinson (crollinson@gmail.com)
# 18 December 2017
#
# Order of Operations:
# 1) Read original randomly generated file
# 2) add blank columns for location etc
# 3) SAVE!
# ------------------------------------------------------------------

# -------------------------------
# Libraries, base layers, etc
# -------------------------------
ens.orig <- read.csv("~/Google Drive/PalEON_Met_Ensembles/data/HARVARD.v5/EnsembleOrder.csv")

ens.mat <- matrix(unlist(stringr::str_split(ens.orig$ensemble, "_")), nrow=nrow(ens.orig), byrow = T)
ens.mat[,1] <- gsub("[.]", "-", ens.mat[,1])
summary(ens.mat)

ens.order <- data.frame(Order = 1:nrow(ens.orig), 
                        GCM   = as.character(ens.mat[,1]), 
                        EnsID = as.character(paste(ens.mat[,1], ens.mat[,2], sep="_")), 
                        location = "", spininit = "", SAS = "", spinfinish = "", runs = "")
ens.order$GCM <- as.character(ens.order$GCM)
summary(ens.order)

write.csv(ens.order, "Ensemble_Order.csv", row.names=F, eol="\r\n", quote=F)

# -------------------------------
