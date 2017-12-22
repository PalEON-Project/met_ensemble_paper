# Looking at the Spin init to make sure we're getting sane values
library(ggplot2)
dir.check <- "MetEns_spininit.v1/MIROC-ESM_004.03/MIROC-ESM_004.03_paleon/"
files.check <- dir(dir.check)

df.init <- data.frame(year=1:length(files.check), GPP=NA, NPP=NA, LAI=NA, AGB=NA, SoilMoist=NA)
fcomp.init <- array(dim=c(nrow(df.init), 17))

pb <- txtProgressBar(min=0, max=nrow(df.init), style=3)
for(i in 1:length(files.check)){
  ncT <- ncdf4::nc_open(file.path(dir.check, files.check[i]))
  
  df.init[i,"GPP" ] <- mean(ncdf4::ncvar_get(ncT, "GPP"))
  df.init[i,"NPP" ] <- mean(ncdf4::ncvar_get(ncT, "GPP"))
  df.init[i,"LAI" ] <- mean(ncdf4::ncvar_get(ncT, "LAI"))
  df.init[i,"AGB" ] <- mean(ncdf4::ncvar_get(ncT, "AGB"))
  # df.init[i,"GWBI"] <- mean(ncdf4::ncvar_get(ncT, "GWBI"))
  df.init[i,"SoilMoist"] <- mean(ncdf4::ncvar_get(ncT, "SoilMoist")[9,])
  
  fcomp.init[i,] <- apply(ncdf4::ncvar_get(ncT, "Fcomp"), 1, mean)
  
  ncdf4::nc_close(ncT)
  
  setTxtProgressBar(pb, i)
}

summary(df.init)

fcomp.stack <- stack(data.frame(fcomp.init))
fcomp.stack$year <- 1:nrow(fcomp.init)

fcomp.stack$pft <- fcomp.stack$ind
fcomp.stack$ind <- "fcomp"

init.stack <- stack(df.init[,c("GPP", "NPP", "LAI", "AGB", "SoilMoist")])
init.stack$year <- 1:nrow(df.init)

init.stack <- merge(init.stack, fcomp.stack[fcomp.stack$pft %in% paste0("X", c(5:6, 8:11)),], all=T)

# ggplot(fcomp.stack[fcomp.stack$pft %in% paste0("X", c(5:6, 8:11)),]) +
  geom_line(aes(x=year, y=values, color=pft))

ggplot(init.stack) +
  facet_wrap(~ind, scales="free_y", ncol=1) +
  geom_line(aes(x=year, y=values, color=pft))

