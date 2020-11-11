# Which scenario is this
scenario <- "PA-Final"

# Where is this script
script.dir <- 'J:/Projects/V2050/STC_RGS/Script'
#script.dir <- '~/psrc/R/control-total-vision2050'

# Where do the data tables live
data.dir <- script.dir
#data.dir <- file.path(script.dir, "review")

# Where does the Juris_Reporting file live
juris.dir <- "J:/Projects/V2050/SEIS/Data_Support/script_input"
#juris.dir <- data.dir

# Which RG col to use
RGcol <- "RGID_Proposed" #"RGID_Existing" 

# Should interpolated numbers be rounded
round.interpolated <- FALSE

# name of the input file
REFtable.name <- paste0('TablesFor', scenario, '-20201110.xlsx')
# name of the file with regional controls with intermediate years
REFCTtable.name <- 'REF-Regional-Controls.csv'

# name of the output file
output.file.name <- paste0("RGS2050_", scenario, "_modInput-", Sys.Date(), ".xlsx") 
# name of the output file with the interpolated control totals
ct.output.file.name <- paste0("Control-Totals-", scenario, "-", Sys.Date(), ".xlsx")

#source("hanas_paths.R") # for running on Hana's Mac; normally comment out

setwd(script.dir)
source("create_control_totals.R")
