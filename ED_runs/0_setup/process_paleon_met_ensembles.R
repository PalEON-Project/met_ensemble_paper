# Use a modified version of the pecan met2model.ED2.R to convert the met we want for this example

# 1. Read in the csv with the ensembles members we want to convert
ens.list <- read.csv(file = "Ensemble_Order.csv", na.strings="")
ens.list
# summary(ens.list)

raw.base <- "~/met_ensemble/data/met_ensembles/HARVARD/1hr/ensembles"
out.base <- "~/met_ensemble_paper/ED_runs/MetEnsemble_ED"

if(!dir.exists(out.base)) dir.create(out.base, recursive = T, showWarnings = F)

# Source the pecan function & loop through the ensembles we want
source("met2model.ED2.R")
for(i in 1:nrow(ens.list)){
  met2model.ED2(in.path=file.path(raw.base, ens.list[i,"GCM"], ens.list[i,"EnsID"]), 
                in.prefix=ens.list[i, "EnsID"], 
                outfolder=file.path(out.base, ens.list[i, "EnsID"]), 
                start_date="0850-01-01", end_date="2015-12-31", lst = 0, 
                lat = NA, lon = NA, overwrite = FALSE, verbose = FALSE,
                path.co2="/home/crollinson/ED_PalEON/MIP2_Region/phase2_env_drivers_v2/co2/paleon_monthly_co2.nc")
  
}
  