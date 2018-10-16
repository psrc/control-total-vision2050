# This script is to be called from another script that makes the various settings:
# data.dir, juris.dir, REFtable.name etc.

library(openxlsx)
library(sqldf)

#create table with adopted targets/annexed bits county, cityID, RG 
REF <- read.xlsx(file.path(data.dir, REFtable.name), sheet = 'REF', 
                 rowNames = TRUE, rows = 1:8)
RGSrg <- read.xlsx(file.path(data.dir, REFtable.name), sheet = 'RGSrg', cols = 1:4)
RGSCo <- read.xlsx(file.path(data.dir, REFtable.name), sheet = 'RGScounty', rows = 1:5)
CityXwalk <- read.csv(file.path(juris.dir, 'Juris_Reporting.csv'), header = TRUE)
CityData_emp <- read.xlsx(file.path(data.dir, 'CityDataEmp.xlsx'), sheet = 'CityDataEmp',
                          rows = 1:158)
CityData_pop <- read.xlsx(file.path(data.dir, 'CityDataPop.xlsx'), sheet = 'CityDataPop',
                          rows = 1:158)

# Merge CityData_emp & CityData_pop with the Juris_Reporting dataset,
# => the original RGID column in the pop and emp datasets is removed
CityData_emp[[RGcol]] <- NULL
CityData_pop[[RGcol]] <- NULL
CityData_emp <- merge(CityData_emp, CityXwalk[, c("CityID", RGcol)])
CityData_pop <- merge(CityData_pop, CityXwalk[, c("CityID", RGcol)])
# rename it to just RGID
colnames(CityData_emp)[which(colnames(CityData_emp) == RGcol)] <- "RGID"
colnames(CityData_pop)[which(colnames(CityData_pop) == RGcol)] <- "RGID"

#create + populate table to hold growth 2000-50, etc.
#Be aware that 2016 values are 2017 for pop and households, etc
REFdelta <- REF
REFdelta[] <- NA
rownames(REFdelta) <- c('2000-16', '2000-50', '2040-50', '2016-50', '2000-30', '2000-31', '2000-35')
#REFdelta <- REFdelta[-c(4:7),]
REFdelta['2000-16',] <- REF['2016',] - REF['2000',]
REFdelta['2000-50',] <- REF['2050',] - REF['2000',]
REFdelta['2040-50',] <- REF['2050',] - REF['2040',]
REFdelta['2016-50',] <- REF['2050',] - REF['2016',]
REFdelta['2000-30',] <- REF['2030',] - REF['2000',]
REFdelta['2000-31',] <- REF['2031',] - REF['2000',]
REFdelta['2000-35',] <- REF['2035',] - REF['2000',]

#apportion growth to counties
CoGrowth2000_50 <- RGSCo
colnames(CoGrowth2000_50) <- c('County', 'Pop', 'Emp')
CoGrowth2000_50$Pop <- REFdelta["2000-50", "Pop"]*RGSCo$RGSPop
CoGrowth2000_50$Emp <- REFdelta["2000-50", "Emp"]*RGSCo$RGSEmp

CoGrowth2016_50 <- RGSCo
colnames(CoGrowth2016_50) <- c('County', 'Pop', 'Emp')
CoGrowth2016_50$Pop <- REFdelta["2016-50", "Pop"]*RGSCo$RGSPop
CoGrowth2016_50$Emp <- REFdelta["2016-50", "Emp"]*RGSCo$RGSEmp


#setup 00-50 growth (RG x Co) to apply RG ratios to

 RGS2016_50 <- sqldf(' select County, RG, RGSPop * Pop as Pop1650, RGSEmp * Emp as Emp1650 
                     from RGSrg 
                     join CoGrowth2016_50 using (County)')
 
# #check that RG growth 2000-16 is less than 2000-50, and flag if not
# RGS2000_16 <- sqldf(' select CityData_pop.County, CityData_pop.RGID as RG, sum(CityData_pop.`2016est`- CityData_pop.`2000est`) as Pop0016, sum(CityData_emp.`2016est`- CityData_emp.`2000est`) as Emp0016
#                     from CityData_pop join CityData_emp using (CityID)
#                     group by CityData_pop.County, CityData_pop.RGID')
# 
# CheckGrowth <- merge(RGS2000_16, RGS2000_50, by = c("County", "RG"))
# CheckGrowth$Pop1650 <- CheckGrowth$PopGro - CheckGrowth$Pop0016
# CheckGrowth$Emp1650 <- CheckGrowth$EmpGro - CheckGrowth$Emp0016
# CheckGrowth$ChkPop <- ifelse(CheckGrowth$PopGro < CheckGrowth$Pop0016, "NO", "ok")
# CheckGrowth$ChkEmp <- ifelse(CheckGrowth$EmpGro < CheckGrowth$Emp0016, "NO", "ok")
# 
# #Adjust 2000-50 growth where 2000-16/17 growth is higher
# for(ind in c("Pop", "Emp")) {
#   AdjDF <- NULL
#   chkcol <- paste0("Chk", ind)
#   grocol <- paste0(ind, "Gro")
#   delcol <- paste0(ind, "1650")
#   for(cnty in unique(CheckGrowth[CheckGrowth[[chkcol]] == "NO", "County"])) {
#     checkdf <- subset(CheckGrowth, County == cnty)
#     irow <- which(checkdf[[chkcol]] == "NO")
#     Adj2000_50 <- subset(RGS2000_50, County == cnty)
#     Adj2000_50 <- merge(Adj2000_50, checkdf[, c("County", "RG", delcol)])
#     adjrow <- which(Adj2000_50[[delcol]] < 0) 
#     Adj2000_50$Share <- Adj2000_50[[grocol]] / sum(Adj2000_50[[grocol]])
#     Adj2000_50$Share[adjrow] <- 0
#     Adj2000_50$Share <- Adj2000_50$Share/sum(Adj2000_50$Share) # rescale to 1
#     Adj2000_50$AdjGro <- - sum(checkdf[[delcol]][irow]) * Adj2000_50$Share
#     Adj2000_50$AdjGro[adjrow] <- Adj2000_50[[delcol]][adjrow]
#     AdjDF <- rbind(AdjDF, Adj2000_50)
#   }
#   if(!is.null(AdjDF)) {
#     RGS2000_50 <- merge(RGS2000_50, AdjDF[, c("County", "RG", "AdjGro")], all = TRUE)
#     RGS2000_50[is.na(RGS2000_50$AdjGro), "AdjGro"] <- 0
#     RGS2000_50[[grocol]] <- RGS2000_50[[grocol]] -  RGS2000_50$AdjGro
#     RGS2000_50$AdjGro <- NULL
#   }
# }

#Subtract out 2000-16/17 growth 
#RGS2016_50 <- sqldf(' select RGS2000_50.County, RGS2000_50.RG, (PopGro - Pop0016) as Pop1650, (EmpGro - Emp0016) as Emp1650 from RGS2000_50
#                    join RGS2000_16 using (County, RG)')

#create job target Ratios
CityData_emp$CoRGid <- paste(CityData_emp$County, CityData_emp$RGID, sep = '_')
# removed Annex
#RGTargetTots <- sqldf(' select County, RGID, sum(Target + Annex + CR_add) as RGTarget 
#                      from CityData_emp group by County, RGID')
RGTargetTots <- sqldf(' select County, RGID, sum(Target + CR_add) as RGTarget 
                      from CityData_emp group by County, RGID')
RGTargetTots$CoRGid <- paste(RGTargetTots$County, RGTargetTots$RGID, sep = '_')
#CityData_emp$RG_Target <- (CityData_emp$Target + CityData_emp$Annex + CityData_emp$CR_add)/sqldf(' select RGTarget from CityData_emp join RGTargetTots using (CoRGid)')
CityData_emp$RG_Target <- (CityData_emp$Target + CityData_emp$CR_add)/sqldf(' select RGTarget from CityData_emp join RGTargetTots using (CoRGid)')
colnames(CityData_emp[,11]) <- "RG_Target"

#create pop target Ratios
CityData_pop$CoRGid <- paste(CityData_pop$County, CityData_pop$RGID, sep = '_')
# removed Annex
#RGTargetTotsPOP <- sqldf(paste(' select County, RGID, sum(Target + Annex) as RGTarget 
#                      from CityData_pop group by County, RGID'))
RGTargetTotsPOP <- sqldf(paste(' select County, RGID, sum(Target) as RGTarget 
                      from CityData_pop group by County, RGID'))
RGTargetTotsPOP$CoRGid <- paste(RGTargetTotsPOP$County, RGTargetTotsPOP$RGID, sep = '_')
#CityData_pop$RG_Target <- (CityData_pop$Target + CityData_pop$Annex)/sqldf(' select RGTarget from CityData_pop join RGTargetTotsPOP using (CoRGid)')
CityData_pop$RG_Target <- CityData_pop$Target/sqldf(' select RGTarget from CityData_pop join RGTargetTotsPOP using (CoRGid)')
colnames(CityData_pop[,10]) <- "RG_Target"

#Apply Target_RG ratios to 2017-50 RG totals
City1650groEmp <- sqldf(' select CityData_emp.RGID, CityData_emp.County, CityData_emp.CityID,`2000est`, `2016est`as Emp2016, RG_Target * Emp1650 as EmpGro1650 
                        from CityData_emp 
                        join RGS2016_50 
                        on CityData_emp.County = RGS2016_50.County
                        and CityData_emp.RGID = RGS2016_50.RG ')
City1650groPop <- sqldf(' select CityData_pop.RGID, CityData_pop.County, CityData_pop.CityID, `2000est`as Pop2000, HHpop00, `2016est`as Pop2016, HHPop16, RG_Target * Pop1650 as PopGro1650, (RG_Target * Pop1650) * (1-GQpct) as HHPopGro1650 
                        from CityData_pop 
                        join RGS2016_50
                        on CityData_pop.County = RGS2016_50.County
                        and CityData_pop.RGID = RGS2016_50.RG ')

#Sum for City 2050 totals
City1650groEmp$Emp2050 <- sqldf(' select EmpGro1650  + "2016est" as Emp2050 from City1650groEmp join CityData_emp using (CityID)')
colnames(City1650groEmp[,7]) <- "Emp2050"
City1650groPop$Pop2050 <- sqldf(' select PopGro1650 + "2016est" as Pop2050 from City1650groPop join CityData_pop using (CityID)')
colnames(City1650groPop[,10]) <- "Pop2050"
City1650groPop$HHPop2050 <- sqldf(' select HHPopGro1650  + City1650groPop.HHPop16 as HHPop2050 from City1650groPop join CityData_pop using (CityID)')
colnames(City1650groPop[,11]) <- "HHPop2050"

#Add CityGrowth0050 columns
City1650groEmp$Emp0050 <- City1650groEmp$Emp2050 - City1650groEmp$`2000est`
City1650groPop$Pop0050 <- City1650groPop$Pop2050 - City1650groPop$Pop2000
City1650groPop$HHPop0050 <- City1650groPop$HHPop2050 - City1650groPop$HHPop00
City1650groPop$NoGQ <- City1650groPop$PopGro1650 - City1650groPop$HHPopGro1650
#City1650groPop$HHPopGro1650 <- City1650groPop$HHPop2050 - City1650groPop$HHPop16

#Control HPop to REF total
UnCtrlHHpop <- sqldf('select sum(HHPopGro1650) as SumHHPop from City1650groPop where NoGQ <>0')
NoCtrlHHpop <- sqldf('select sum(HHPopGro1650) as SumHHPop from City1650groPop where NoGQ = 0')
UnCtrlHHpopRx <- (REFdelta[4,2] - NoCtrlHHpop) / UnCtrlHHpop
HPopCtrlMx <- sqldf('select CityID, PopGro1650, HHPopGro1650 from City1650groPop')
HPopCtrlMx$Ctrl <- ifelse(HPopCtrlMx$PopGro1650 - HPopCtrlMx$HHPopGro1650 > 0.02, UnCtrlHHpopRx$SumHHPop, 1)
HPopCtrlMx$CtrlHHPop <- HPopCtrlMx$HHPopGro1650 * HPopCtrlMx$Ctrl

City1650groPop$HHPopGro1650 <- HPopCtrlMx$CtrlHHPop
City1650groPop$HHPop2050 <- City1650groPop$HHPopGro1650 + City1650groPop$HHPop16
City1650groPop$HHPop0050 <- City1650groPop$HHPop2050 - City1650groPop$HHPop00

# UnCtrlHHpop <- sqldf('select sum(HHPop2050) as SumHHPop from City1650groPop where Pop2050 > HHPop2050')
# NoCtrlHHpop <- sqldf('select sum(HHPop2050) as SumHHPop from City1650groPop where Pop2050 = HHPop2050')
# UnCtrlHHpopRx <- (REF[7,2]- NoCtrlHHpop) / UnCtrlHHpop
# HPopCtrlMx <- sqldf('select CityID, Pop2050, HHPop2050 from City1650groPop')
# HPopCtrlMx$Ctrl <-ifelse(HPopCtrlMx$Pop2050 > HPopCtrlMx$HHPop2050, UnCtrlHHpopRx$SumHHPop, 1)
# HPopCtrlMx$CtrlHHPop <- HPopCtrlMx$HHPop2050 * HPopCtrlMx$Ctrl
# City1650groPop$HHPop2050 <- HPopCtrlMx$CtrlHHPop

#Create HH
City1650groHH <- sqldf(' select CityData_pop.RGID, CityData_pop.County, CityData_pop.CityID, HH2000, HH2016, PPH, HHPopGro1650 / PPH as HHGro1650 
                        from CityData_pop 
                       join City1650groPop using (CityID) ')

#Sum for City 2050 totals
City1650groHH$HH2050 <- sqldf(' select HHGro1650 + City1650groHH.HH2016 as HH2050 from City1650groHH join CityData_pop using (CityID)')
colnames(City1650groHH[,8]) <- "HH2050"

#Control HH to REF total
UnCtrlHH <- sqldf('select sum(HHGro1650) as SumHH from City1650groHH where HHGro1650 >0 ')
#NotCtrlHH <- sqldf('select sum(HHGro1650) as SumHH from City1650groHH where HHGro1650 =0 ')
CtrlHHRx <- REFdelta[4,3] / UnCtrlHH$SumHH
HHCtrlMx <- sqldf('select CityID, HH2050, HHGro1650 from City1650groHH')
HHCtrlMx$Ctrl <- ifelse(HHCtrlMx$HHGro1650 > 0, CtrlHHRx, 1)
HHCtrlMx$CtrldHH <- HHCtrlMx$HHGro1650 * HHCtrlMx$Ctrl

City1650groHH$HHGro1650 <- HHCtrlMx$CtrldHH
City1650groHH$HH2050 <- City1650groHH$HHGro1650 + City1650groHH$HH2016
City1650groHH$HH0050 <- City1650groHH$HH2050 - City1650groHH$HH2000

#Add juris names
CityRGSEmp <- sqldf('select City1650groEmp.RGID, City1650groEmp.County, City1650groEmp.CityID, Juris, `2000est` as Emp2000, Emp2016 ,Empgro1650, Emp2050 
                    from City1650groEmp 
                    join CityXwalk using (CityID)')
CityRGSPop <- sqldf('select City1650groPop.RGID, City1650groPop.County, City1650groPop.CityID, Juris, Pop2000, Pop2016, Popgro1650, Pop2050, HHPop00, HHPop16 as HHPop2016, HHPopGro1650, HHPop2050 
                    from City1650groPop 
                    join CityXwalk using (CityID)')
CityRGSHH <- sqldf('select City1650groHH.RGID, City1650groHH.County, City1650groHH.CityID, Juris, HH2000, HH2016, HHgro1650, HH2050 
                    from City1650groHH 
                    join CityXwalk using (CityID)')


#Sum RGS
RGS50 <- sqldf(' select CityRGSPop.County, CityRGSPop.RGID, sum(Pop2050 - Pop2000) as Pop0050, sum(Pop2050 - Pop2016) as Pop1650, sum("Pop2050") as Pop50, sum(HH2050 - HH2000) as HH0050, sum(HH2050 - HH2016) as HH1650, sum(HH2050) as HH50, sum(Emp2050 - Emp2000) as Emp0050, sum(Emp2050 - Emp2016) as Emp1650, sum("Emp2050") as Emp50 
               from CityRGSPop 
               join CityRGSEmp using (CityID) join CityRGSHH using (CityID) 
               group by CityRGSPop.County, CityRGSPop.RGID ')

# put back the name of the original RGID column
colnames(CityRGSPop)[which(colnames(CityRGSPop) == "RGID")] <- RGcol
colnames(CityRGSHH)[which(colnames(CityRGSHH) == "RGID")] <- RGcol
colnames(CityRGSEmp)[which(colnames(CityRGSEmp) == "RGID")] <- RGcol
colnames(RGS50)[which(colnames(RGS50) == "RGID")] <- RGcol

#Export outta here
if(!is.null(output.file.name)) {
  output <- list("RGS2050" = RGS50, "CityPop0050" = CityRGSPop, "CityHH0050" = CityRGSHH,  "CityEmp0050" = CityRGSEmp)
  write.xlsx(output, output.file.name, colNames = TRUE)
  # write.xlsx(output, "Y:/VISION 2050/Data/2050_RGS/STC_final/RGS2050_STC_modInput.xlsx", colNames = TRUE)
}

# Interpolate (this could go into a separate R script)
source("interpolate.R")
if(!is.null(REFCTtable.name)) {
  # read regional totals for adjustments
  regtot <- read.csv(file.path(data.dir, REFCTtable.name), header = TRUE)
  #regtot <- data.frame(Year = c(2000, 2017, seq(2020, 2050, by = 5)), 
  #                     HHPop = seq(3200000, 5720000, length = 9), HH = seq(1282774, 2419949, length = 9))
  rt.years <- regtot[, "Year"]
  rownames(regtot) <- rt.years
  regtot$Year <- NULL
} else regtot <- NULL

to.interpolate <- list(HHPop = CityRGSPop, HH = CityRGSHH, Emp = CityRGSEmp)
CTs <- list()
round.to.int <- if(exists("round.interpolated")) round.interpolated else TRUE
for (indicator in names(to.interpolate)) {
    RCT <- if(is.null(regtot)) NULL else regtot[, indicator]
    names(RCT) <- rownames(regtot)
    CTs[[indicator]] <- interpolate.controls(to.interpolate[[indicator]], indicator, totals = RCT, 
                                             round.to.int = round.to.int)
}

if(!is.null(ct.output.file.name)) 
  write.xlsx(CTs, ct.output.file.name, colNames = TRUE)

  