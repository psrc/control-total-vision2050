# Which scenario is this
scenario <- "H2O2"

# Where is this script
script.dir <- 'J:/Projects/V2050/STC_RGS/Script'

# Where do the data tables live
data.dir <- script.dir

# Where does the Juris_Reporting file live
juris.dir <- "J:/Projects/V2050/SEIS/Data_Support/script_input"

# Which RG col to use
RGcols <- list(RGexist = "RGID_Existing", RGprop = "RGID_Proposed")

# name of the input file
REFtable.name <- paste0('TablesFor', scenario, 'RGS-20180929.xlsx')

source("hanas_paths.R") # for running on Hana's Mac; normally comment out
setwd(script.dir)

for(whichRGcol in names(RGcols)) {
  RGcol <- RGcols[[whichRGcol]]
  # name of the output file
  output.file.name <- paste0("RGS2050_", scenario, "_", whichRGcol, "_modInput-", Sys.Date(), ".xlsx") 
  # name of the output file with the interpolated control totals
  ct.output.file.name <- paste0("Control-Totals-", scenario, "-", whichRGcol, "-", Sys.Date(), ".xlsx")
  source("Create_2050_4model.R")
}
