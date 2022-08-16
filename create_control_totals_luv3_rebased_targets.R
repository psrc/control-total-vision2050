# Creates a dataset with jurisdictional control totals
# Base year and target year can be set.
# The script should be called from run_creating_control_totals_from_targets.R
# Hana Sevcikova, PSRC
# 08/16/2022

library(data.table)
library(openxlsx)
library(tools)

# set various column names
base.year.short <- substr(as.character(base.year), 3,4)
target.year.short <- substr(as.character(target.year), 3,4)
basecol <- paste0(base.year, "est")
basecol.ref <- paste0(ref.base.year, "BY")
basecol.hhpop <- paste0("HHPop", base.year.short)
basecol.hhpop.ref <- paste0(basecol.ref, "hhpop")
basecol.hh <- paste0("HH", base.year.short)
basecol.hh.ref <- paste0(basecol.ref, "hh")
targetcol.pph <- paste0("PPH", target.year.short)
targetcol.gqpct <- paste0("GQpct", target.year.short)
targetcol <- paste0(target.year, "trg")

# read city-level target file
CityData <- if(file_ext(target.file) == "csv") fread(file.path(data.dir, target.file)) else data.table(read.xlsx(file.path(data.dir, target.file), 
                                                                                            sheet = target.sheet, startRow = 1))

# select columns needed
CityData_pop <- CityData[, .(control_id, county_id, RGID, BaseEst = TotPop20, Target = TotPopTrg,
                             EndTrg = TotPop50, HHPopBase = HHpop20, HHBase = HH20, 
                             FirstPop = Pop18, FirstHH = HH18, FirstHHpop = HHpop18, 
                             GQpct = GQpct20, EndGQpct = GQpct50, EndPPH = PPH50)]

CityData_emp <- CityData[, .(control_id, county_id, RGID, BaseEst = TotEmp20_wCRnoMil, Target = TotEmpTrg_wCRnoMil,
                             EndTrg = TotEmp50_wCRnoMil, FirstEmp = Emp18)]

# renaming again to be consistent with previous implementations
CityGroEmp <- CityData_emp[, .(county_id, RGID, control_id, EmpBY = FirstEmp, EmpBase = BaseEst,
                             EmpGro = Target, EmpTarget = EndTrg)]
CityGroPop <- CityData_pop[, .(county_id, RGID, control_id, 
                                      PopBY = FirstPop, PopBase = BaseEst,
                                      HHPopBY = FirstHHpop, HHPopBase,
                                      HHBY = FirstHH, HHBase,
                                      PopGro = Target, PopTarget = EndTrg, 
                                      GQpctTarget = EndGQpct, PPHTarget = EndPPH
                                    )] 

CityGroPop[, HHPopTarget := PopTarget - PopTarget * GQpctTarget/100]
CityGroPop[, HHTarget := HHPopTarget/PPHTarget]
CityGroPop[is.na(HHTarget), HHTarget := 0]

# add HH growth column
CityGroPop[, HHGro := HHTarget - HHBase]
CityGroPop[, HHPopGro := HHPopTarget - HHPopBase]

# add Juris name
CityGroEmp[CityData, Juris := i.name, on = "control_id"]
CityGroPop[CityData, Juris := i.name, on = "control_id"]


#Sum RGS
RGSTarget <- merge(CityGroPop[, .(PopDelta = sum(PopTarget - PopBase), PopTarget = sum(PopTarget), 
                                HHDelta = sum(HHTarget - HHBase), HHTarget = sum(HHTarget)), by = .(county_id, RGID)], 
               CityGroEmp[, .(EmpDelta = sum(EmpTarget - EmpBase), EmpTarget = sum(EmpTarget)), by = .(county_id, RGID)], 
              by = c("county_id", "RGID"))

# Select the right columns in the right order for outputs
CityRGSEmp <- CityGroEmp[, .(RGID, county_id, control_id, Juris, EmpBY, EmpBase, EmpGro, EmpTarget)]
CityRGSPop <- CityGroPop[, .(RGID, county_id, control_id, Juris, PopBY, PopBase, PopGro, PopTarget, HHPopBY, HHPopBase, HHPopGro, HHPopTarget)]
CityRGSHH <- CityGroPop[, .(RGID, county_id, control_id, Juris, HHBY, HHBase, HHGro, HHTarget)]

# Rename time columns
growth.suffix <- paste0(substr(as.character(base.year), 3,4), target.year.short)

setnames(CityRGSPop, c("PopBY", "PopBase", "PopGro", "PopTarget", "HHPopBY", "HHPopBase", "HHPopGro", "HHPopTarget"),
         c(paste0("Pop", ref.base.year), paste0("Pop", base.year), paste0("PopGro", growth.suffix), paste0("Pop", target.year), 
           paste0("HHPop", ref.base.year), paste0("HHPop", base.year), paste0("HHPopGro", growth.suffix), paste0("HHPop", target.year)))
setnames(CityRGSEmp, c("EmpBY", "EmpBase", "EmpGro", "EmpTarget"),
         c(paste0("Emp", ref.base.year), paste0("Emp", base.year), paste0("EmpGro", growth.suffix), paste0("Emp", target.year)))
setnames(CityRGSHH, c("HHBY", "HHBase", "HHGro", "HHTarget"),
         c(paste0("HH", ref.base.year), paste0("HH", base.year), paste0("HHGro", growth.suffix), paste0("HH", target.year)))
setnames(RGSTarget, c("PopDelta", "PopTarget", "HHDelta", "HHTarget", "EmpDelta", "EmpTarget"),
         c(paste0("Pop", growth.suffix), paste0("Pop", target.year.short),
           paste0("HH", growth.suffix), paste0("HH", target.year.short),
           paste0("Emp", growth.suffix), paste0("Emp", target.year.short)))

#Export 
if(!is.null(output.file.name)) {
    output <- list("RGs" = RGSTarget, "CityPop" = CityRGSPop, "CityHH" = CityRGSHH,  "CityEmp" = CityRGSEmp)
    write.xlsx(output, output.file.name, colNames = TRUE)
}

# Interpolate (this could go into a separate R script)
source("interpolate.R")

ankers <- c(ref.base.year, base.year, target.year)
years.to.fit <- c(ankers[1], seq(base.year, 2040, by = 5), 2044, 2050) 

if(!is.null(REFCTtable.name)) {
    # read regional totals for adjustments
    if(file_ext(REFCTtable.name) == "xlsx"){ # assuming reading directly from the REF final product sheet, thus needs some pre-processing
        refall <- data.table(read.xlsx(file.path(data.dir, REFCTtable.name), sheet = "Forecast"))
        cols <- paste0(years.to.fit[years.to.fit > base.year], "Q2")
        regtot <- data.frame(Pop = t(refall[X1 == "Population", cols, with = FALSE]),
                            HHPop = t(refall[X2 == "Household Population", cols, with = FALSE]),
                            HH = t(refall[X1 == "Households", cols, with = FALSE]),
                            Emp = t(refall[X1 == "Estimated Total Employment", cols, with = FALSE]))
        rownames(regtot) <- substr(cols, 1, 4)
    } else {
        regtot <- read.csv(file.path(data.dir, REFCTtable.name), header = TRUE)
        rt.years <- regtot[, "Year"]
        rownames(regtot) <- rt.years
        regtot <- subset(regtot, Year > base.year)
        regtot$Year <- NULL
    }
} else regtot <- NULL

to.interpolate <- list(HHPop = CityRGSPop, HH = CityRGSHH, Emp = CityRGSEmp, Pop = CityRGSPop)
CTs <- list()
unrolled <- NULL

for (indicator in names(to.interpolate)) {
    RCT <- if(is.null(regtot)) NULL else regtot[, indicator]
    names(RCT) <- rownames(regtot)
    CTs[[indicator]] <- interpolate.controls.with.ankers(to.interpolate[[indicator]][order(control_id)], indicator, 
                                             anker.years = ankers, years.to.fit = years.to.fit,
                                             totals = RCT)
    this.unrolled <- unroll(CTs[[indicator]], indicator, totals = RCT, new.id.col = "subreg_id")
    unrolled <- if(is.null(unrolled)) this.unrolled else merge(unrolled, this.unrolled, all = TRUE)
}

CTs[["unrolled"]] <- unrolled

if(!is.null(ct.output.file.name)) 
    write.xlsx(CTs, ct.output.file.name, colNames = TRUE)

