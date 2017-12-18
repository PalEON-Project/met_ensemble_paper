# Use a modified version of the pecan met2model.ED2.R to convert the met we want for this example

# 1. Read in the csv with the ensembles members we want to convert
ens.list <- read.csv(file = "Ensemble_Order.csv", na.strings="")
ens.list

ens.do <- 1:25 # Number of ensemble members to generate met for
yrs.run <- 850:875
cores.use <- min(8, length(yrs.run))
# summary(ens.list)

raw.base <- "/home/crollinson/met_ensemble/data/met_ensembles/HARVARD.v5/1hr/ensembles"
out.base <- "/home/crollinson/met_ensemble_paper/ED_runs/MetEnsemble_ED"
# raw.base <- "~/Desktop/Research/met_ensembles/data/met_ensembles/HARVARD.v5/1hr/ensembles/"
# raw.base <- "/home/crollinson/met_ensemble/data/paleon_sites/HARVARD/NLDAS"
# out.base <- "~/Desktop/Research/met_ensemble_paper/ED_runs/MetEnsemble_ED"

if(!dir.exists(out.base)) dir.create(out.base, recursive = T, showWarnings = F)

ens.list <- ens.list[ens.list$Order %in% ens.do, ]

# Source the pecan function & loop through the ensembles we want
source("met2model.ED2.R")

# Set up Years as a list
year.process <- list()
for(i in 1:length(yrs.run)){
  year.process[[i]] <- yrs.run[i]
}

# Set up a function that will call met2model, but can be easily parallelized
met.parallel <- function(yr, in.base, in.prefix, out.base, path.co2, force.sanity=TRUE, freq.agg=NULL, overwrite=FALSE, ...){
  start.yr <- paste0(yr, "-01-01")
  end.yr   <- paste0(yr, "-12-31")
  
  met2model.ED2(in.path=file.path(in.base, in.prefix), 
                in.prefix=in.prefix, 
                outfolder=file.path(out.base, paste0(in.prefix, "/")), 
                start_date=start.yr, end_date=end.yr, lst = 0, 
                lat = NA, lon = NA, overwrite = TRUE, verbose = FALSE,
                path.co2=path.co2,
                force.sanity=force.sanity, freq.agg=freq.agg)
  
}



# apply met2model in parallel
for(i in 1:nrow(ens.list)){
  out <- parallel::mclapply(year.process, FUN=met.parallel, mc.cores=cores.use,
                            in.base=file.path(raw.base, ens.list[i, "GCM"]), 
                            in.prefix=ens.list[i, "EnsID"],
                            out.base=out.base, 
                            path.co2="/home/crollinson/ED_PalEON/MIP2_Region/phase2_env_drivers_v2/co2/paleon_monthly_co2.nc", 
                            force.sanity=TRUE, freq.agg=NULL, overwrite=FALSE)
  
  print(paste0("Finished Processing: ", ens.list$EnsID[i], " (", min(yrs.run), "-", max(yrs.run), ")"))
}

