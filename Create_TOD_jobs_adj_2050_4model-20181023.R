# Which scenario is this
scenario <- "TOD-jobs-adj"

# Where is this script
script.dir <- 'J:/Projects/V2050/STC_RGS/Script'

# Where do the data tables live
data.dir <- script.dir

# Where does the Juris_Reporting file live
juris.dir <- "J:/Projects/V2050/SEIS/Data_Support/script_input"

# Which RG col to use
RGcol <- "RGID_Proposed" #"RGID_Existing" 

# Should interpolated numbers be rounded
round.interpolated <- FALSE

# name of the input file
REFtable.name <- paste0('TablesFor', scenario, '-RGS-20181023.xlsx')
# name of the file with regional controls with intermediate years
REFCTtable.name <- 'REF-Regional-Controls.csv'

# name of the output file
output.file.name <- paste0("RGS2050_", scenario, "_modInput-", Sys.Date(), ".xlsx") 
# name of the output file with the interpolated control totals
ct.output.file.name <- paste0("Control-Totals-", scenario, "-", Sys.Date(), ".xlsx")

#source("hanas_paths.R") # for running on Hana's Mac; normally comment out

setwd(script.dir)
source("Create_2050_4model.R")
