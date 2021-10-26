script.dir <- "/Volumes/DataTeam/Projects/V2050/STC_RGS/Script"
script.dir <- '/Users/hana/psrc/R/control-total-vision2050' # for Hana' psrc mac
#data.dir <- file.path(script.dir, "review")
data.dir <- file.path(script.dir, "data")
juris.dir <- "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support/script_input"

#data.dir <- file.path(script.dir, "data") # for Hana
juris.dir <- data.dir # for Hana

base.year <- 2018
target.year <- 2050

REFtable.name <- paste0('TablesFor', "GenTest", '.xlsx')
REFCTtable.name <- 'REF-Regional-Controls18.csv'
juris.data.emp.name <- paste0('CityDataEmp_', scenario, '.xlsx')
juris.data.pop.name <- paste0('CityDataPop_', scenario, '.xlsx')

output.file.name <- paste0("RGS2050_", scenario, "-", Sys.Date(), ".xlsx") 
#output.file.name <- NULL