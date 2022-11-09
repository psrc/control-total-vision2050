script.dir <- '/Users/hana/psrc/R/control-total-vision2050' # for Hana' psrc mac
#data.dir <- file.path(script.dir, "review")
data.dir <- file.path(script.dir, "data")

#rgs.target.file <- "~/psrc/control_totals/ConTot\ R\ Script\ Inputs\ 071222.xlsx"
#target.file <- "../../../control_totals/ConTot080122.xlsx" # relative path to data.dir
#target.file <- "../../../control_totals/control_id_working_080122.csv" # numeric columns have to be stored without quotes, otherwise it doesn't work
target.file <- "../../../control_totals/control_id_working_110722.xlsx"

#REFtable.name <- paste0('TablesFor', "UtargetsLuv3", '.xlsx')
#REFCTtable.name <- 'REF-Regional-Controls18.csv'
#REFCTtable.name <- '2018_PSRC_Macroeconomic_Forecast_rev.xlsx'
#REFCTtable.name <- NULL # use this if no scaling is desired

#juris.data.emp.name <- paste0('CityDataEmp_', scenario, '.xlsx')
#juris.data.pop.name <- paste0('CityDataPop_', scenario, '.xlsx')
#juris.data.name <- "updated_targets_for_CTinputs.xlsx"

#output.file.suffix <- if(is.null(REFCTtable.name)) "NoScale" else ""
#output.file.name <- paste0("RGS2050_", scenario, "-", Sys.Date(), ".xlsx") 
#output.file.name <- paste0("TargetsVisionOutput", "-", Sys.Date(), ".xlsx")
#output.file.name <- paste0("TargetsRebasedOutput", output.file.suffix, "-", Sys.Date(), ".xlsx")
#output.file.name <- paste0("TargetsRebasedOutput", "-", Sys.Date(), ".xlsx")
#output.file.name <- NULL
#ct.output.file.name <- paste0("Control-Totals-LUV3tol-", scenario, "-", Sys.Date(), ".xlsx")
#ct.output.file.name <- paste0("Control-Totals-LUV3VisTol-", Sys.Date(), ".xlsx")
#ct.output.file.name <- paste0("Control-Totals-LUV3RebasedTrg", output.file.suffix, "-", Sys.Date(), ".xlsx")
#ct.output.file.name <- NULL
