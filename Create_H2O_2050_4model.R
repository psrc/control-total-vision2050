dir <- 'J:/Projects/V2050/STC_RGS/Script'
setwd(dir)
#create table with adopted targets/annexed bits county, cityID, RG 
library(openxlsx)
REF <- read.xlsx('TablesForSTCRGS.xlsx', sheet = 'REF', rowNames = TRUE)
RGSrg <- read.xlsx('TablesForSTCRGS.xlsx', sheet = 'RGSrg')
RGSCo <- read.xlsx('TablesForSTCRGS.xlsx', sheet = 'RGScounty')
CityXwalk <- read.csv('Juris_Reporting.csv', header = TRUE)
CityData_emp <- read.xlsx('CityData_Emp.xlsx', sheet = 'CityDataEmp')
CityData_pop <- read.xlsx('CityData_Pop.xlsx', sheet = 'CityDataPop')

#create + populate table to hold growth 2000-50, etc.
#Be aware that "2016" values are 2017 for pop and households, etc
REFdelta <- REF
REFdelta[]<- NA
rownames(REFdelta) <- c('2000-16', '2000-50', '2040-50', '2016-50', '2000-30', '2000-31', '2000-35')
#REFdelta <- REFdelta[-c(4:7),]
REFdelta[1,] <-REF[2,] - REF[1,]
REFdelta[2,] <-REF[7,] - REF[1,]
REFdelta[3,] <-REF[7,] - REF[6,]
REFdelta[4,] <-REF[7,] - REF[2,]
REFdelta[5,] <-REF[3,] - REF[1,]
REFdelta[6,] <-REF[4,] - REF[1,]
REFdelta[7,] <-REF[5,] - REF[1,]

#apportion growth to counties
CoGrowth2016_50 <- RGSCo
colnames(CoGrowth2016_50) <- c('County', 'Pop', 'Emp')
CoGrowth2016_50$Pop <- REFdelta[4,1]*RGSCo$RGSPop
CoGrowth2016_50$Emp <- REFdelta[4,4]*RGSCo$RGSEmp

#setup 16-50 growth (RG x Co) to apply RG ratios to
library(sqldf)
RGS2016_50 <- sqldf(' select County, RG, RGSPop * Pop as PopGro, RGSEmp * Emp as EmpGro 
                    from RGSrg 
                    join CoGrowth2016_50 using (County)')
###
# #check that RG growth 2000-16 is less than 2000-50, and flag if not
# RGS2000_16 <- sqldf(' select CityData_pop.County, CityData_pop.RG, sum(CityData_pop.`2016est`- CityData_pop.`2000est`) as Pop0016, sum(CityData_emp.`2016est`- CityData_emp.`2000est`) as Emp0016
#                     from CityData_pop join CityData_emp using (CityID)
#                     group by CityData_pop.County, CityData_pop.RG')
# 
# CheckGrowth <- merge(RGS2000_16, RGS2000_50, by = c("County", "RG"))
# CheckGrowth$Pop1650 <- CheckGrowth$PopGro - CheckGrowth$Pop0016
# CheckGrowth$Emp1650 <- CheckGrowth$EmpGro - CheckGrowth$Emp0016
# CheckGrowth$ChkPop <- ifelse(CheckGrowth$PopGro < CheckGrowth$Pop0016, "NO", "ok")
# CheckGrowth$ChkEmp <- ifelse(CheckGrowth$EmpGro < CheckGrowth$Emp0016, "NO", "ok")

# #Adjust 2000-50 growth where 2000-16/17 growth is higher (this is hard coded for King Co UU Pop only)
# Adj2000_50 <- sqldf(' select RGS2000_50.County, RGS2000_50.RG, RGS2000_50.PopGro, Pop1650, ChkPop from RGS2000_50 
#             join CheckGrowth using (County) where ChkPop = "NO" ')
# Adj2000_50$CoSum <- (sum(Adj2000_50$PopGro)) - Adj2000_50[5,3]
# Adj2000_50$Share <- Adj2000_50$PopGro / Adj2000_50$CoSum
# Adj2000_50[5,7] <- -1
# Adj2000_50$AdjGro <- (Adj2000_50$Pop1650 * Adj2000_50$Share) *-1
# RGS2000_50[1:6,3] <- sqldf('select (RGS2000_50.PopGro - AdjGro) from RGS2000_50 join Adj2000_50 using (County, RG) ')
# 
# #Subtract out 2000-16/17 growth 
# RGS2016_50 <- sqldf(' select RGS2000_50.County, RGS2000_50.RG, (PopGro - Pop0016) as Pop1650, (EmpGro - Emp0016) as Emp1650 from RGS2000_50
#                     join RGS2000_16 using (County, RG)')
###

#create job target Ratios
CityData_emp$CoRGid <- paste(CityData_emp$County, CityData_emp$RG, sep = '_')
RGTargetTots <- sqldf(' select County, RG, sum(Target + Annex + CR_add) as RGTarget 
                      from CityData_emp group by County, RG')
RGTargetTots$CoRGid <- paste(RGTargetTots$County, RGTargetTots$RG, sep = '_')
CityData_emp$RG_Target <- (CityData_emp$Target + CityData_emp$Annex + CityData_emp$CR_add)/sqldf(' select RGTarget from RGTargetTots join CityData_emp using (CoRGid)')
colnames(CityData_emp[,11]) <- "RG_Target"

#create pop target Ratios
CityData_pop$CoRGid <- paste(CityData_pop$County, CityData_pop$RG, sep = '_')
RGTargetTotsPOP <- sqldf(' select County, RG, sum(Target + Annex) as RGTarget 
                      from CityData_pop group by County, RG')
RGTargetTotsPOP$CoRGid <- paste(RGTargetTotsPOP$County, RGTargetTotsPOP$RG, sep = '_')
CityData_pop$RG_Target <- (CityData_pop$Target + CityData_pop$Annex)/sqldf(' select RGTarget from RGTargetTotsPOP join CityData_pop using (CoRGid)')
colnames(CityData_pop[,10]) <- "RG_Target"

#Apply Target_RG ratios to 2017-50 RG totals
City1650groEmp <- sqldf(' select CityData_emp.RG, CityData_emp.County, CityData_emp.CityID,`2000est`, `2016est`as Emp2016, RG_Target * EmpGro as EmpGro1650 
                        from CityData_emp 
                        join RGS2016_50 using (County, RG) ')
City1650groPop <- sqldf(' select CityData_pop.RG, CityData_pop.County, CityData_pop.CityID, `2000est`as Pop2000, HHpop00, `2016est`as Pop2016, HHPop16, RG_Target * PopGro as PopGro1650, RG_Target * PopGro * (1-GQpct) as HHPopGro1650 
                        from CityData_pop 
                        join RGS2016_50 using (County, RG) ')

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
City1650groPop$HHPopGro1650 <- City1650groPop$HHPop2050 - City1650groPop$HHPop16

#Control HPop to REF total
UnCtrlHHpop <- sqldf('select sum(HHPopGro1650) as SumHHPop from City1650groPop where Pop2016 > HHPop16')
NoCtrlHHpop <- sqldf('select sum(HHPopGro1650) as SumHHPop from City1650groPop where Pop2016 = HHPop16')
UnCtrlHHpopRx <- (REFdelta[4,2]- NoCtrlHHpop) / UnCtrlHHpop
HPopCtrlMx <- sqldf('select CityID, PopGro1650, HHPopGro1650 from City1650groPop')
HPopCtrlMx$Ctrl <-ifelse(HPopCtrlMx$PopGro1650 - HPopCtrlMx$HHPopGro1650>0.02, UnCtrlHHpopRx$SumHHPop, 1)
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
######
#Create HH
City1650groHH <- sqldf(' select CityData_pop.RG, CityData_pop.County, CityData_pop.CityID, HH2000, HH2016, PPH, HHPopGro1650 / PPH as HHGro1650 
                        from CityData_pop 
                       join City1650groPop using (CityID) ')

#Sum for City 2050 totals
City1650groHH$HH2050 <- sqldf(' select HHGro1650 + City1650groHH.HH2016 as HH2050 from City1650groHH join CityData_pop using (CityID)')
colnames(City1650groHH[,8]) <- "HH2050"

#Control HH to REF total
UnCtrlHH <- sqldf('select sum(HHGro1650) as SumHH from City1650groHH where HHGro1650 >0 ')
#NotCtrlHH <- sqldf('select sum(HHGro1650) as SumHH from City1650groHH where HHGro1650 =0 ')
CtrlHHRx <- REFdelta[4,3]/ UnCtrlHH$SumHH
HHCtrlMx <- sqldf('select CityID, HH2050, HHGro1650 from City1650groHH')
HHCtrlMx$Ctrl <-ifelse(HHCtrlMx$HHGro1650 >0, CtrlHHRx, 1)
HHCtrlMx$CtrldHH <- HHCtrlMx$HHGro1650 * HHCtrlMx$Ctrl

City1650groHH$HHGro1650 <- HHCtrlMx$CtrldHH
City1650groHH$HH2050 <- City1650groHH$HHGro1650 + City1650groHH$HH2016
City1650groHH$HH0050 <- City1650groHH$HH2050 - City1650groHH$HH2000

#Add juris names
CityRGSEmp <- sqldf('select City1650groEmp.RG, City1650groEmp.County, City1650groEmp.CityID, Juris, `2000est` as Emp2000, Emp2016 ,Empgro1650, Emp2050 
                    from City1650groEmp 
                    join CityXwalk using (CityID)')
CityRGSPop <- sqldf('select City1650groPop.RG, City1650groPop.County, City1650groPop.CityID, Juris, Pop2000, Pop2016, Popgro1650, Pop2050, HHPop00, HHPop16, HHPopGro1650, HHPop2050 
                    from City1650groPop 
                    join CityXwalk using (CityID)')
CityRGSHH <- sqldf('select City1650groHH.RG, City1650groHH.County, City1650groHH.CityID, Juris, HH2000, HH2016, HHgro1650, HH2050 
                    from City1650groHH 
                    join CityXwalk using (CityID)')


#Sum RGS
RGS50 <- sqldf(' select CityRGSPop.County, CityRGSPop.RG, sum(Pop2050 - Pop2000) as Pop0050, sum("Pop2050") as Pop50, sum(HH2050 - HH2000) as HH0050, sum(HH2050) as HH50, sum(Emp2050 - Emp2000) as Emp0050, sum("Emp2050") as Emp50 
               from CityRGSPop 
               join CityRGSEmp using (CityID) join CityRGSHH using (CityID) 
               group by CityRGSPop.County, CityRGSPop.RG ')

#Export outta here
output <- list("RGS2050" = RGS50, "CityPop0050" = CityRGSPop, "CityHH0050" = CityRGSHH,  "CityEmp0050" = CityRGSEmp)
  write.xlsx(output, "RGS2050_H2O_modInput.xlsx", colNames = TRUE)
  write.xlsx(output, "Y:/VISION 2050/Data/2050_RGS/STC_final/RGS2050_H2O_modInput.xlsx", colNames = TRUE)
  