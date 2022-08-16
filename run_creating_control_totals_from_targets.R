# Where is this script
script.dir <- 'J:/Projects/V2050/STC_RGS/Script'
#script.dir <- '~/psrc/R/control-total-vision2050'

# Where do the data tables live
data.dir <- script.dir
#data.dir <- file.path(script.dir, "data")

# Should interpolated numbers be rounded
round.interpolated <- FALSE

# Setting of the time
base.year <- 2020
target.year <- 2050
ref.base.year <- 2018

# INPUTS 
# (if the files are not in data.dir, use a relative path to data.dir)
# name of the file with regional controls with intermediate years
REFCTtable.name <- '2018_PSRC_Macroeconomic_Forecast_rev.xlsx' # used for scaling
#REFCTtable.name <- NULL # use this if no scaling is desired
# name of the file with all targets
target.file <- "ConTot080122.xlsx"
# if the above file is Excel file, which sheet contains the city-level targets
target.sheet <- "control_id_working"

# OUTPUTS
# name of the output file
output.file.suffix <- if(is.null(REFCTtable.name)) "NoScale" else ""
output.file.name <- paste0("TargetsRebasedOutput", output.file.suffix, "-", Sys.Date(), ".xlsx")
# name of the output file with the interpolated control totals
ct.output.file.name <- paste0("Control-Totals-LUV3RebasedTrg", output.file.suffix, "-", Sys.Date(), ".xlsx")

# for running on Hana's Mac; normally comment out
source("hanas_paths.R") 

setwd(script.dir)
source("create_control_totals_luv3_rebased_targets.R")
