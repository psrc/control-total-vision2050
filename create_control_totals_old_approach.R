library(data.table)
library(openxlsx)

# Read tables with REF, RGS by county & RG; adopted targets by cities
REF <- data.table(read.xlsx(file.path(data.dir, REFtable.name), sheet = 'REF', 
                 rowNames = FALSE, rows = 1:8)) #might contain sums - exclude by the "rows" argument
RGSrg <- data.table(read.xlsx(file.path(data.dir, REFtable.name), sheet = 'RGSrg'))
RGSCo <- data.table(read.xlsx(file.path(data.dir, REFtable.name), sheet = 'RGScounty'))

CityXwalk <- fread(file.path(juris.dir, 'Juris_Reporting_PA-v5.csv'), header = TRUE)
CityData_emp <- data.table(read.xlsx(file.path(data.dir, 'CityDataEmp_PA-v5_NoAnnex.xlsx'), sheet = 'CityDataEmp'))
CityData_pop <- data.table(read.xlsx(file.path(data.dir, 'CityDataPop_PA-v5_NoAnnex.xlsx'), sheet = 'CityDataPop'))

# Merge CityData_emp & CityData_pop with the Juris_Reporting dataset,
# => the original RGID column in the pop and emp datasets is removed
CityData_emp[[RGcol]] <- NULL
CityData_pop[[RGcol]] <- NULL
CityData_emp <- merge(CityData_emp, CityXwalk[, c("CityID", RGcol), with = FALSE])
CityData_pop <- merge(CityData_pop, CityXwalk[, c("CityID", RGcol), with = FALSE])
# rename it to just RGID
setnames(CityData_emp, RGcol, "RGID")
setnames(CityData_pop, RGcol, "RGID")

#apportion growth to counties
dif00.50 <- REF[Year %in% c(2000, 2050), .(Pop = diff(Pop), Emp = diff(Emp), HHPop = diff(HHPop), HH = diff(HH))]
CoGrowth2000_50 <- RGSCo[, .(County, Pop = dif00.50$Pop*RGSPop, Emp = dif00.50$Emp*RGSEmp)]
dif16.50 <- REF[Year %in% c(2016, 2050), .(Pop = diff(Pop), Emp = diff(Emp), HHPop = diff(HHPop), HH = diff(HH))]
CoGrowth2016_50 <- RGSCo[, .(County, Pop = dif16.50$Pop*RGSPop, Emp = dif16.50$Emp*RGSEmp)]

# disaggregate the deltas to RGs
RGS2016_50 <- RGSrg[CoGrowth2016_50, .(County, RGID = RG, Pop1650 = i.Pop*RGSPop, Emp1650 = i.Emp*RGSEmp), on = "County"]

#create job target Ratios
CityData_emp[, RGTargetTots := sum(Target + Annex + CR_add), by = .(County, RGID)]
CityData_emp[, RG_Target := (Target + Annex + CR_add)/RGTargetTots]
#create pop target Ratios
CityData_pop[, RGTargetTots := sum(Target + Annex), by = .(County, RGID)]
CityData_pop[, RG_Target := (Target + Annex)/RGTargetTots]

#Apply Target_RG ratios to 2017-50 RG totals
City1650groEmp <- CityData_emp[RGS2016_50, .(County, RGID, CityID, Emp2000 = `2000est`, Emp2016 = `2016est`, 
                                             EmpGro1650 = RG_Target * i.Emp1650), 
                               on = .(County, RGID), nomatch = 0] # inner join
City1650groPop <- CityData_pop[RGS2016_50, .(County, RGID, CityID, Pop2000 = `2000est`, Pop2016 = `2016est`, HHPop00, HHPop16, 
                                             PopGro1650 = RG_Target * i.Pop1650,
                                             HHPopGro1650 = (RG_Target * i.Pop1650) * (1-GQpct)
                                             ), 
                               on = .(County, RGID), nomatch = 0] # inner join
#Sum for City 2050 totals
City1650groEmp[, Emp2050 := EmpGro1650  + Emp2016]
City1650groPop[, Pop2050 := PopGro1650  + Pop2016]
City1650groPop[, HHPop2050 := HHPopGro1650 + HHPop16]

#Add CityGrowth0050 columns
City1650groEmp[, `:=`(Emp0050 = Emp2050 - Emp2000)]
City1650groPop[, `:=`(Pop0050 = Pop2050 - Pop2000, HHPop0050 = HHPop2050 - HHPop00, NoGQ = PopGro1650 - HHPopGro1650)]

#Control HPop to REF total
UnCtrlHHpop <- City1650groPop[NoGQ != 0, sum(HHPopGro1650)] # cities with GQ
NoCtrlHHpop <- City1650groPop[NoGQ == 0, sum(HHPopGro1650)] # cities with no GQ
UnCtrlHHpopRx <- (dif16.50$HHPop - NoCtrlHHpop) / UnCtrlHHpop
HPopCtrlMx <- City1650groPop[, .(CityID, PopGro1650, HHPopGro1650, Ctrl = 1)]
HPopCtrlMx[PopGro1650 - HHPopGro1650 > 0.02, Ctrl := UnCtrlHHpopRx]
HPopCtrlMx[, CtrlHHPop := HHPopGro1650 * Ctrl]
City1650groPop[HPopCtrlMx, HHPopGro1650 := i.CtrlHHPop, on = "CityID"]
City1650groPop[, HHPop2050 := HHPopGro1650 + HHPop16]
City1650groPop[, HHPop0050 := HHPop2050 - HHPop00]

#Create HH
City1650groHH <- CityData_pop[City1650groPop, .(RGID, County, CityID, HH2000, HH2016, PPH, HH2050 = i.HHPop2050 / PPH), on = "CityID"]
City1650groHH[, HHGro1650 := HH2050 - HH2016]

#Control HH to REF total
CtrlHHRx <- dif16.50$HH/ City1650groHH[HHGro1650 > 0, sum(HHGro1650)]
City1650groHH[, Ctrl := 1]
City1650groHH[HHGro1650 > 0, Ctrl := CtrlHHRx]
City1650groHH[, HHGro1650 := HHGro1650 * Ctrl]
City1650groHH[, HH2050 := HHGro1650 + HH2016]
City1650groHH[, HH0050 := HH2050 - HH2000]
City1650groHH[, Ctrl := NULL]

#Add juris names
City1650groEmp[CityXwalk, Juris := i.Juris, on = "CityID"]
City1650groPop[CityXwalk, Juris := i.Juris, on = "CityID"]
City1650groHH[CityXwalk, Juris := i.Juris, on = "CityID"]

#Sum RGS
RGS50 <- merge(merge(City1650groPop[, .(Pop0050 = sum(Pop2050 - Pop2000), Pop1650 = sum(Pop2050 - Pop2016), Pop50 = sum(Pop2050)), by = .(County, RGID)], 
                    City1650groHH[, .(HH0050 = sum(HH2050 - HH2000), HH1650 = sum(HH2050 - HH2016), HH50 = sum(HH2050)), by = .(County, RGID)], 
                    by = c("County", "RGID")),
               City1650groEmp[, .(Emp0050 = sum(Emp2050 - Emp2000), Emp1650 = sum(Emp2050 - Emp2016), Emp50 = sum(Emp2050)), by = .(County, RGID)], 
              by = c("County", "RGID"))

# Select the right columns in teh right order for outputs
CityRGSEmp <- City1650groEmp[, .(RGID, County, CityID, Juris, Emp2000, Emp2016, EmpGro1650, Emp2050)]
CityRGSPop <- City1650groPop[, .(RGID, County, CityID, Juris, Pop2000, Pop2016, PopGro1650, Pop2050, HHPop2016 = HHPop16, HHPopGro1650, HHPop2050)]
CityRGSHH <- City1650groHH[, .(RGID, County, CityID, Juris, HH2000, HH2016, HHGro1650, HH2050)]

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

