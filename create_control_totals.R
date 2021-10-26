library(data.table)
library(openxlsx)

# Read tables with REF, RGS by county & RG; adopted targets by cities
REF <- data.table(read.xlsx(file.path(data.dir, REFtable.name), sheet = 'REF', 
                 rowNames = FALSE, rows = 1:8)) #might contain sums - exclude by the "rows" argument
RGSrg <- data.table(read.xlsx(file.path(data.dir, REFtable.name), sheet = 'RGSrg'))
RGSCo <- data.table(read.xlsx(file.path(data.dir, REFtable.name), sheet = 'RGScounty'))
setnames(RGSrg, "RG", "RGID")

CityXwalk <- fread(file.path(juris.dir, juris.reporting.name), header = TRUE)
CityData_emp <- data.table(read.xlsx(file.path(data.dir, juris.data.emp.name), sheet = 'CityDataEmp'))
CityData_pop <- data.table(read.xlsx(file.path(data.dir, juris.data.pop.name), sheet = 'CityDataPop'))

# Merge CityData_emp & CityData_pop with the Juris_Reporting dataset,
# => the original RGID column in the pop and emp datasets is removed
CityData_emp[[RGcol]] <- NULL
CityData_pop[[RGcol]] <- NULL
CityData_emp <- merge(CityData_emp, CityXwalk[, c("CityID", RGcol), with = FALSE])
CityData_pop <- merge(CityData_pop, CityXwalk[, c("CityID", RGcol), with = FALSE])
# rename it to just RGID
setnames(CityData_emp, RGcol, "RGID")
setnames(CityData_pop, RGcol, "RGID")

#Step 1: REF delta totals are distributed across counties using the counties' RGS proportions.
dif00.50 <- REF[Year %in% c(2000, 2050), .(Pop = diff(Pop), Emp = diff(Emp), HHPop = diff(HHPop), HH = diff(HH))]
CoGrowth2000_50 <- RGSCo[, .(County, Pop = dif00.50$Pop*RGSPop, Emp = dif00.50$Emp*RGSEmp)]
dif16.50 <- REF[Year %in% c(2016, 2050), .(Pop = diff(Pop), Emp = diff(Emp), HHPop = diff(HHPop), HH = diff(HH))]
CoGrowth2016_50 <- RGSCo[, .(County, Pop = dif16.50$Pop*RGSPop, Emp = dif16.50$Emp*RGSEmp)]

# Step 2: disaggregate the deltas to RGs
RGS2016_50 <- RGSrg[CoGrowth2016_50, .(County, RGID, Pop1650 = i.Pop*RGSPop, Emp1650 = i.Emp*RGSEmp), on = "County"]

# Step 3: For each city, derive the proportion of its target within its RG (for both, pop and employment)
CityData_emp[, RGTargetTots := sum(Target + CR_add), by = .(County, RGID)]
CityData_emp[, RG_Target := (Target + CR_add)/RGTargetTots]
CityData_pop[, RGTargetTots := sum(Target), by = .(County, RGID)]
CityData_pop[, RG_Target := Target/RGTargetTots]

# Step 4: Apply cities proportions from Step 3 to the RG deltas from step 2, yielding cities' deltas
City1650groEmp <- CityData_emp[RGS2016_50, .(County, RGID, CityID, Emp2000 = `2000est`, Emp2016 = `2016est`, 
                                             EmpGro1650 = RG_Target * i.Emp1650), 
                               on = .(County, RGID), nomatch = 0] # inner join
City1650groPop <- CityData_pop[RGS2016_50, .(County, RGID, CityID, Pop2000 = `2000est`, Pop2016 = `2016est`, HHPop00, HHPop16, HH2000, HH2016,
                                             PopGro1650 = RG_Target * i.Pop1650,
                                             HHPopGro1650 = (RG_Target * i.Pop1650) * (1-GQpct),
                                             GQpct2050, PPH2050
                                             ), 
                               on = .(County, RGID), nomatch = 0] # inner join

# Step 5: Add the deltas (county, RGs, cities) to 2017 base year TotPop values to calculate 2050 TotPop values
# City 2050 totals
City1650groEmp[, Emp2050 := EmpGro1650  + Emp2016]
City1650groPop[, Pop2050 := PopGro1650  + Pop2016]
# County totals
CntyTotals <- City1650groEmp[, .(Emp2016 = sum(Emp2016), Emp2050 = sum(Emp2050)), by = County]
CntyTotals <- merge(CntyTotals, City1650groPop[, .(Pop2016 = sum(Pop2016), Pop2050 = sum(Pop2050)), by = County], 
                    by = "County")
# RGs totals
RGTotals <- City1650groEmp[, .(Emp2016 = sum(Emp2016), Emp2050 = sum(Emp2050)), by = .(County, RGID)]
RGTotals <- merge(RGTotals, City1650groPop[, .(Pop2016 = sum(Pop2016), Pop2050 = sum(Pop2050)), by = .(County, RGID)], 
                  by = c("County", "RGID"))

# Institute hierarchical controls to convert TotPop to HHPop
#####
# Step 6: Apply county GQpct2050 values to county TotPop2050 to estimate preliminary county HHPop2050
CntyTotals[RGSCo, HHPop2050p := Pop2050 - Pop2050 * i.GQpct2050, on = "County"]

# Step 7: Control the prelim county HHPop2050 values to regional HHPop2050 (from Macro Forecast) to generate final county HHPop2050
REF.HHPop50 <- REF[Year == 2050, HHPop]
CntyTotals[, HHPopshare := HHPop2050p/sum(CntyTotals$HHPop2050p)]
CntyTotals[, HHPop2050 := REF.HHPop50 * HHPopshare]
CntyTotals[, `:=`(HHPopshare = NULL)]
           
# Step 8: Apply RG GQpct2050 values to RG TotPop2050 to estimate preliminary RG HHPop2050
RGTotals[RGSrg, HHPop2050p := Pop2050 - Pop2050 * i.GQpct2050, on = c("County", "RGID")]
    
# Step 9: Control the prelim RG HHPop2050 values to the associated final county HHPop2050 value to generate final RG HHPop2050
RGTotals[CntyTotals, HHPopTotCT := i.HHPop2050, on = "County"]
RGTotals[, HHPopTot := sum(HHPop2050p), by = County]
RGTotals[, HHPop2050 := HHPop2050p/HHPopTot * HHPopTotCT]
RGTotals[, `:=`(HHPopTotCT = NULL, HHPopTot = NULL)]

# Step 10: Apply city GQpct2050 values to city TotPop2050 to estimate preliminary city HHPop2050
City1650groPop[, HHPop2050p := Pop2050 - Pop2050 * GQpct2050]

# Step 11: Control the prelim city HHPop2050 values to the associated final RG HHPop2050 value to generate final city HHPop2050
City1650groPop[RGTotals, HHPopTotCT := i.HHPop2050, on = c("County", "RGID")]
City1650groPop[, HHPopTot := sum(HHPop2050p), by = .(County, RGID)]
City1650groPop[, HHPop2050 := HHPop2050p/HHPopTot * HHPopTotCT]
City1650groPop[, `:=`(HHPopTotCT = NULL, HHPopTot = NULL)]

# Institute hierarchical controls to convert HHPop to Hholds
#####
# Step 12: Apply county PPH2050 values to county HHPop2050 to estimate preliminary county HH2050
CntyTotals[RGSCo, HH2050p := HHPop2050/i.PPH2050, on = "County"]

# Step 13: Control the prelim county HH2050 values to regional HH2050 (from Macro Forecast) to generate final county HH2050
REF.HH50 <- REF[Year == 2050, HH]
CntyTotals[, HHshare := HH2050p/sum(CntyTotals$HH2050p)]
CntyTotals[, HH2050 := REF.HH50 * HHshare]
CntyTotals[, `:=`(HHshare = NULL)]

# Step 14: Apply RG PPH2050 values to RG HHPop2050 to estimate preliminary RG HH2050
RGTotals[RGSrg, HH2050p := HHPop2050/i.PPH2050, on = c("County", "RGID")]

# Step 15: Control the prelim RG HH2050 values to the associated final county HH2050 value to generate final RG HH2050
RGTotals[CntyTotals, HHTotCT := i.HH2050, on = "County"]
RGTotals[, HHTot := sum(HH2050p), by = County]
RGTotals[, HH2050 := HH2050p/HHTot * HHTotCT]
RGTotals[, `:=`(HHTotCT = NULL, HHTot = NULL)]

# Step 16: Apply city PPH2050 values to city HHPop2050 to estimate preliminary city HH2050
City1650groPop[, HH2050p := HHPop2050/PPH2050]
City1650groPop[is.na(HH2050p), HH2050p := 0]

# Step 17: Control the prelim city HH2050 values to the associated final RG HH2050 value to generate final city HH2050
City1650groPop[RGTotals, HHTotCT := i.HH2050, on = c("County", "RGID")]
City1650groPop[, HHTot := sum(HH2050p), by = .(County, RGID)]
City1650groPop[, HH2050 := HH2050p/HHTot * HHTotCT]
City1650groPop[, `:=`(HHTotCT = NULL, HHTot = NULL)]

# add HH growth column
City1650groPop[,HHGro1650 := HH2050 - HH2016]

#Add juris names
City1650groEmp[CityXwalk, Juris := i.Juris, on = "CityID"]
City1650groPop[CityXwalk, Juris := i.Juris, on = "CityID"]

#Sum RGS
RGS50 <- merge(City1650groPop[, .(Pop0050 = sum(Pop2050 - Pop2000), Pop1650 = sum(Pop2050 - Pop2016), Pop50 = sum(Pop2050), 
                                    HH0050 = sum(HH2050 - HH2000), HH1650 = sum(HH2050 - HH2016), HH50 = sum(HH2050)), by = .(County, RGID)], 
               City1650groEmp[, .(Emp0050 = sum(Emp2050 - Emp2000), Emp1650 = sum(Emp2050 - Emp2016), Emp50 = sum(Emp2050)), by = .(County, RGID)], 
              by = c("County", "RGID"))

# Select the right columns in the right order for outputs
CityRGSEmp <- City1650groEmp[, .(RGID, County, CityID, Juris, Emp2000, Emp2016, EmpGro1650, Emp2050)]
CityRGSPop <- City1650groPop[, .(RGID, County, CityID, Juris, Pop2000, Pop2016, PopGro1650, Pop2050, HHPop2016 = HHPop16, HHPopGro1650, HHPop2050)]
CityRGSHH <- City1650groPop[, .(RGID, County, CityID, Juris, HH2000, HH2016, HHGro1650, HH2050)]

# # put back the name of the original RGID column
setnames(CityRGSEmp, "RGID", RGcol)
setnames(CityRGSPop, "RGID", RGcol)
setnames(CityRGSHH, "RGID", RGcol)
setnames(RGS50, "RGID", RGcol)

#Export 
if(!is.null(output.file.name)) {
    output <- list("RGS2050" = RGS50, "CityPop0050" = CityRGSPop, "CityHH0050" = CityRGSHH,  "CityEmp0050" = CityRGSEmp)
    write.xlsx(output, output.file.name, colNames = TRUE)
}

# Interpolate (this could go into a separate R script)
source("interpolate.R")
if(!is.null(REFCTtable.name)) {
    # read regional totals for adjustments
    regtot <- read.csv(file.path(data.dir, REFCTtable.name), header = TRUE)
    rt.years <- regtot[, "Year"]
    rownames(regtot) <- rt.years
    regtot$Year <- NULL
} else regtot <- NULL

to.interpolate <- list(HHPop = CityRGSPop, HH = CityRGSHH, Emp = CityRGSEmp)
CTs <- list()
unrolled <- NULL
for (indicator in names(to.interpolate)) {
    RCT <- if(is.null(regtot)) NULL else regtot[, indicator]
    names(RCT) <- rownames(regtot)
    CTs[[indicator]] <- interpolate.controls(to.interpolate[[indicator]][order(CityID)], indicator, totals = RCT)
    this.unrolled <- unroll(CTs[[indicator]], indicator, totals = RCT)
    unrolled <- if(is.null(unrolled)) this.unrolled else merge(unrolled, this.unrolled)
}

CTs[["unrolled"]] <- unrolled

if(!is.null(ct.output.file.name)) 
    write.xlsx(CTs, ct.output.file.name, colNames = TRUE)

