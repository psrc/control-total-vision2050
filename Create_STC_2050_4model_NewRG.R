# Where is this script
script.dir <- 'J:/Projects/V2050/STC_RGS/Script'
#script.dir <- '/Volumes/DataTeam/Projects/V2050/STC_RGS/Script' # for Hana
#script.dir <- '/Users/hana/psrc/R/control-total-vision2050' # for Hana

# Where do the data tables live
data.dir <- script.dir
#data.dir <- file.path(script.dir, "data") # for Hana

# Where does the Juris_Reporting file live
juris.dir <- "J:/Projects/V2050/SEIS/Data_Support/script_input"
#juris.dir <- "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support/script_input"
#juris.dir <- data.dir # for Hana

# Which RG col to use
RGcol <- "RGID_Existing" #"RGID_Proposed"

setwd(script.dir)

# name of the input file
#REFtable.name <- 'TablesForSTCRGS.xlsx'
REFtable.name <- 'TablesForSTCRGS-20180928.xlsx'
# name of the output file
output.file.name <- "RGS2050_STC_modInput_2017to2050_hana.xlsx" #"RGS2050_STC_modInput_QC180829_newRG.xlsx"
# name of the output file with the interpolated control totals
ct.output.file.name <- paste0("CTs_hana", Sys.Date(), ".xlsx")

source("Create_2050_4model.R")
