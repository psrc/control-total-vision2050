library(openxlsx)
library(sqldf)
library(tidyverse)
# library(data.table)

# dir <- 'J:/Projects/V2050/STC_RGS/Script'
dir <- "C:/Users/CLam/Desktop/control-total-vision2050"
setwd(dir)

# set base year
byear <- "2017"
byr <- substr(byear, 3, 4)
byrest <- paste0(byear, "est")
estbyr <- paste0("est", byear)

#create table with adopted targets/annexed bits county, cityID, RG 
REF <- read.xlsx('TablesForSTCRGS.xlsx', sheet = 'REF', rowNames = TRUE)
RGSrg <- read.xlsx('TablesForSTCRGS.xlsx', sheet = 'RGSrg') # shares for regional geogs (24)
RGSCo <- read.xlsx('TablesForSTCRGS.xlsx', sheet = 'RGScounty') # shares for counties (4)
CityXwalk <- read.csv('Juris_Reporting.csv', header = TRUE)
CityData_emp <- read.xlsx('CityData_Emp.xlsx', sheet = 'CityDataEmp')
CityData_pop <- read.xlsx('CityData_Pop.xlsx', sheet = 'CityDataPop')

selcols <- grep("est", colnames(CityData_emp), value = TRUE)
newcolname <- lapply(selcols, function(x) str_extract(x, "\\d+")) %>% unlist() %>% paste0("est", .)
colnames(CityData_emp)[grep("est", colnames(CityData_emp))] <- newcolname
colnames(CityData_pop)[grep("est", colnames(CityData_pop))] <- newcolname

#create + populate table to hold growth 2000-50, etc.
#Be aware that 2016 values are 2017 for pop and households, etc
REFdelta <- REF
REFdelta[]<- NA
rownames(REFdelta) <- c(paste0("2000-", byr), '2000-50', '2040-50', paste0(byear, "-50"), '2000-30', '2000-31', '2000-35')
#REFdelta <- REFdelta[-c(4:7),]
REFdelta[1,] <-REF[2,] - REF[1,]
REFdelta[2,] <-REF[7,] - REF[1,]
REFdelta[3,] <-REF[7,] - REF[6,]
REFdelta[4,] <-REF[7,] - REF[2,]
REFdelta[5,] <-REF[3,] - REF[1,]
REFdelta[6,] <-REF[4,] - REF[1,]
REFdelta[7,] <-REF[5,] - REF[1,]

#apportion growth to counties
CoGrowth2000_50 <- RGSCo
colnames(CoGrowth2000_50) <- c('County', 'Pop', 'Emp')
CoGrowth2000_50$Pop <- REFdelta[2,1]*RGSCo$RGSPop
CoGrowth2000_50$Emp <- REFdelta[2,4]*RGSCo$RGSEmp

#setup 00-50 growth (RG x Co) to apply RG ratios to
RGS2000_50 <- sqldf(' select County, RG, RGSPop * Pop as PopGro, RGSEmp * Emp as EmpGro 
                    from RGSrg 
                    join CoGrowth2000_50 using (County)')

#check that RG growth 2000-17 is less than 2000-50, and flag if not
## Calc 2000-201X for pop and emp
rgscol <- c("County", "RG", "CityID", "est2000", estbyr)
newcol.pop <- paste0("Pop00", byr)
newcol.emp <- paste0("Emp00", byr)

RGS2000_1X <- CityData_pop %>%
  select_(.dots = rgscol) %>%
  mutate_(.dots = setNames(paste(estbyr, "-est2000"), newcol.pop)) %>%
  select(-one_of(c("est2000", paste0("est", byear)))) %>%
  left_join(CityData_emp, by = c("County", "RG", "CityID")) %>%
  select_(.dots = c(rgscol, newcol.pop)) %>%
  mutate_(.dots = setNames(paste(estbyr, "-est2000"), newcol.emp)) %>%
  select(-one_of(c("est2000", paste0("est", byear)), "CityID")) %>%
  group_by(County, RG) %>%
  summarise_all(sum)

## Compare 
CheckGrowth <- merge(RGS2000_1X, RGS2000_50, by = c("County", "RG"))
CheckGrowth <- CheckGrowth %>%
  mutate_(.dots = setNames(paste("PopGro -", newcol.pop), paste0("Pop", byr, "50"))) %>%
  mutate_(.dots = setNames(paste("EmpGro -", newcol.emp), paste0("Emp", byr, "50"))) %>%
  mutate_(.dots = setNames(paste("ifelse(PopGro <", newcol.pop, ", 'NO', 'ok')"), "ChkPop")) %>%
  mutate_(.dots = setNames(paste("ifelse(EmpGro <", newcol.emp, ", 'NO', 'ok')"), "ChkEmp")) 

## ??????????
#Adjust 2000-50 growth where 2000-16/17 growth is higher (this is hard coded for King Co UU Pop only)
Adj2000_50.a <- RGS2000_50 %>% select_(.dots = c("County", "RG", "PopGro"))
CheckGrowth.no <- CheckGrowth %>% filter(ChkPop == "NO") %>% select_(.dots = c("County", paste0("Pop", byr, "50"), "ChkPop"))
Adj2000_50 <- Adj2000_50.a %>%
  left_join(CheckGrowth.no, by = "County") %>%
  filter_(.dots = paste("!is.na(", paste0("Pop", byr, "50"),")") )
Adj2000_50$CoSum <- (sum(Adj2000_50$PopGro)) - Adj2000_50[5,3]
Adj2000_50$Share <- Adj2000_50$PopGro / Adj2000_50$CoSum
Adj2000_50[5,7] <- -1
Adj2000_50$AdjGro <- (Adj2000_50[, grep(paste0("Pop", byr, "50"), colnames(Adj2000_50))] * Adj2000_50$Share) *-1
RGS2000_50[1:6,3] <- sqldf('select (RGS2000_50.PopGro - AdjGro) from RGS2000_50 join Adj2000_50 using (County, RG) ')

#Subtract out 2000-16/17 growth 
## Subtract 2000-17 growth from 2000-50
RGS201X_50 <- RGS2000_50 %>%
  left_join(RGS2000_1X, by = c("County", "RG")) %>%
  mutate_(.dots = c(setNames(paste0("PopGro - ", newcol.pop), paste0("Pop", byr, "50")),
                    setNames(paste0("EmpGro - ", newcol.emp), paste0("Emp", byr, "50")))) %>%
  select_(.dots = c("County", "RG", paste0("Pop", byr, "50"), paste0("Emp", byr, "50")))
  
#create job target Ratios
CityData_emp$CoRGid <- paste(CityData_emp$County, CityData_emp$RG, sep = '_')
RGTargetTots <- CityData_emp %>%
  mutate(RGTarget = Target + Annex + CR_add) %>%
  group_by(County, RG) %>%
  summarise(RGTarget = sum(RGTarget))

RGTargetTots$CoRGid <- paste(RGTargetTots$County, RGTargetTots$RG, sep = '_')
CityData_emp$RG_Target <- (CityData_emp$Target + CityData_emp$Annex + CityData_emp$CR_add)/sqldf(' select RGTarget from RGTargetTots join CityData_emp using (CoRGid)')
colnames(CityData_emp[,11]) <- "RG_Target"

#create pop target Ratios
CityData_pop$CoRGid <- paste(CityData_pop$County, CityData_pop$RG, sep = '_')
RGTargetTotsPOP <- CityData_pop %>%
  mutate(RGTarget = Target + Annex) %>%
  select(County, RG, RGTarget) %>%
  group_by(County, RG) %>%
  summarise_all(sum)
  
RGTargetTotsPOP$CoRGid <- paste(RGTargetTotsPOP$County, RGTargetTotsPOP$RG, sep = '_')
CityData_pop$RG_Target <- (CityData_pop$Target + CityData_pop$Annex)/sqldf(' select RGTarget from RGTargetTotsPOP join CityData_pop using (CoRGid)')
colnames(CityData_pop[,10]) <- "RG_Target"

#Apply Target_RG ratios to 201X-50 RG totals
##Sum for City 2050 totals
##Add CityGrowth0050 columns
City1X50groEmp <- CityData_emp %>%
  left_join(RGS201X_50, by = c("County", "RG")) %>%
  select_(.dots = c("RG", "County", "CityID", "est2000", setNames(estbyr, paste0("Emp", byear)), "RG_Target", paste0("Emp", byr, "50"))) %>%
  mutate_(.dots = c(setNames(paste0("RG_Target *", paste0("Emp", byr, "50")), paste0("EmpGro", byr, "50")))) %>%
  select_(.dots = paste0("-", c(paste0("Emp", byr, "50"), "RG_Target"))) %>%
  mutate_(.dots = setNames(paste0(paste0("EmpGro", byr, "50"), "+", paste0("Emp", byear)), "Emp2050")) %>%
  mutate_(.dots = setNames(paste0("Emp2050 - est2000"), "Emp0050"))
  
City1X50groPop <- CityData_pop %>%
  left_join(RGS201X_50, by = c("County", "RG")) %>%
  select_(.dots = c("RG", "County", "CityID", setNames("est2000", "Pop2000"), setNames(estbyr, paste0("Pop", byear)), "HHPop00", paste0("HHPop", byr), "RG_Target", paste0("Pop", byr, "50"), "GQpct")) %>%
  mutate_(.dots = c(setNames(paste0("RG_Target *", paste0("Pop", byr, "50")), paste0("PopGro", byr, "50")),
                    setNames(paste0("(RG_Target * ", paste0("Pop", byr, "50) * (1-GQpct)")), paste0("HHPopGro", byr, "50"))
                    )) %>%
  select_(.dots = c("RG", "County", "CityID", "Pop2000", "HHPop00", paste0("Pop", byear), paste0("HHPop", byr), paste0("PopGro", byr, "50"), paste0("HHPopGro", byr, "50"))) %>%
  mutate_(.dots = c(setNames(paste0(paste0("PopGro", byr, "50"), "+", paste0("Pop", byear)), "Pop2050"),
                    setNames(paste0(paste0("HHPopGro", byr, "50"), "+", paste0("HHPop", byr)), "HHPop2050"))) %>%
  mutate_(.dots = c(setNames(paste0("Pop2050 - Pop2000"), "Pop0050"),
                    setNames(paste0("HHPop2050 - HHPop00"), "HHPop0050"),
                    setNames(paste0(paste0("PopGro", byr, "50"), "-", paste0("HHPopGro", byr, "50")), "NoGQ")
                    ))

#Control HPop to REF total
UnCtrlHHpop <- City1X50groPop %>%
  filter(NoGQ != 0) %>%
  summarise_(.dots = setNames(paste0("sum(HHPopGro", byr, "50)"), "SumHHPop"))

NoCtrlHHpop <- City1X50groPop %>%
  filter(NoGQ == 0) %>%
  summarise_(.dots = setNames(paste0("sum(HHPopGro", byr, "50)"), "SumHHPop"))

UnCtrlHHpopRx <- (REFdelta[paste0(byear, "-50"), "HHPop"]- NoCtrlHHpop$SumHHPop) / UnCtrlHHpop$SumHHPop

HPopCtrlMx <- City1X50groPop %>%
  select_(.dots = c("CityID", paste0("PopGro", byr, "50"), paste0("HHPopGro", byr, "50"))) %>%
  mutate_(.dots = setNames(paste0("ifelse(", paste0("PopGro", byr, "50"), "-", paste0("HHPopGro", byr, "50"), "> 0.02, UnCtrlHHpopRx, 1)"), "Ctrl")) %>%
  mutate_(.dots = setNames(paste(paste0("HHPopGro", byr, "50"), "* Ctrl" ), "CtrlHHPop"))

HPopCtrlMx.a <- HPopCtrlMx %>%
  select(CityID, CtrlHHPop)

City1X50groPop <- City1X50groPop %>%
  left_join(HPopCtrlMx.a, by = "CityID") %>%
  mutate_(.dots = c(setNames("CtrlHHPop", paste0("HHPopGro", byr, "50")),
                    setNames(paste0(paste0("HHPopGro", byr, "50"), "+", paste0("HHPop", byr)), "HHPop2050"),
                    setNames(paste0("HHPop2050", "+", "HHPop00"), "HHPop0050"))) %>%
  select_(.dots = "-CtrlHHPop")


#Create HH
#Sum for City 2050 totals
City1X50groPop.a <- City1X50groPop %>%
  select_(.dots = c("CityID", paste0("HHPopGro", byr, "50")))

City1X50groHH <- CityData_pop %>%
  select_(.dots = c("RG", "County", "CityID", paste0("HH", c("2000", byear)), "PPH")) %>%
  left_join(City1X50groPop.a, by = "CityID") %>%
  mutate_(.dots = setNames(paste0(paste0("HHPopGro", byr, "50"), "/", "PPH"), paste0("HHGro", byr, "50"))) %>%
  mutate_(.dots = setNames(paste0(paste0("HHGro", byr, "50"), "+", paste0("HH", byear)), "HH2050")) %>%
  select_(.dots = paste0("-HHPopGro", byr, "50"))

#Control HH to REF total
UnCtrlHH <- City1X50groHH %>%
  filter_(.dots = paste0(paste0("HHGro", byr, "50"), "> 0")) %>%
  summarise_(.dots = setNames(paste0("sum(HHGro", byr, "50)"), "SumHH"))
  
CtrlHHRx <- REFdelta[paste0(byear, "-50"), "HH"]/ UnCtrlHH$SumHH

HHCtrlMx <- City1X50groHH %>%
  select_(.dots = c("CityID", "HH2050", paste0("HHGro", byr, "50"))) %>%
  mutate_(.dots = setNames(paste0("ifelse(", paste0("HHGro", byr, "50"), "> 0, CtrlHHRx, 1)"), "Ctrl")) %>%
  mutate_(.dots = setNames(paste0(paste0("HHGro", byr, "50"), "* Ctrl"), "CtrldHH"))

HHCtrlMx.a <- HHCtrlMx %>%
  select_(.dots = c("CityID", "CtrldHH"))

City1X50groHH <- City1X50groHH %>%
  left_join(HHCtrlMx.a, by = "CityID") %>%
  mutate_(.dots = c(setNames("CtrldHH", paste0("HHGro", byr, "50")),
                    setNames(paste0(paste0("HHGro", byr, "50"), "+", paste0("HH", byear)), "HH2050"),
                    setNames(paste0("HH2050", "+", "HH2000"), "HH0050"))) %>%
  select_(.dots = "-CtrldHH")

#Add juris names
CityRGSEmp <- City1X50groEmp %>%
  left_join(CityXwalk, by = c("RG", "CityID")) %>%
  select_(.dots = c("RG", "County", "CityID", "Juris", setNames("est2000", "Emp2000"), paste0("Emp", byear), paste0("EmpGro", byr, "50"), "Emp2050"))

CityRGSPop <- City1X50groPop %>%
  left_join(CityXwalk, by = c("RG", "CityID")) %>%
  select_(.dots = c("RG", "County", "CityID", "Juris", "Pop2000", paste0("Pop", byear), paste0("PopGro", byr, "50"), "Pop2050", "HHPop00", paste0("HHPop", byr), paste0("HHPopGro", byr, "50"), "HHPop2050"))

CityRGSHH <- City1X50groHH %>%
  left_join(CityXwalk, by = c("RG", "CityID")) %>%
  select_(.dots = c("RG", "County", "CityID", "Juris", "HH2000", paste0("HH", byear), paste0("HHGro", byr, "50"), "HH2050"))

#Sum RGS
RGS50 <- CityRGSPop %>%
  left_join(CityRGSEmp, by = c("County", "RG", "CityID", "Juris")) %>%
  left_join(CityRGSHH,  by = c("County", "RG", "CityID", "Juris")) %>%
  select(County, RG, contains("2000"), contains("2050")) %>%
  mutate_(.dots = c(setNames("Pop2050 - Pop2000", "Pop0050"),
                    setNames("HH2050 - HH2000", "HH0050"),
                    setNames("Emp2050 - Emp2000", "Emp0050"))) %>%
  group_by(County, RG) %>%
  summarise(Pop0050 = sum(Pop0050), Pop50 = sum(Pop2050), HH0050 = sum(HH0050), HH50 = sum(HH2050), Emp0050 = sum(Emp0050), Emp50 = sum(Emp2050))

#Export outta here
output <- list("RGS2050" = RGS50, "CityPop0050" = CityRGSPop, "CityHH0050" = CityRGSHH,  "CityEmp0050" = CityRGSEmp)
  write.xlsx(output, "RGS2050_STC_modInput.xlsx", colNames = TRUE)
  # write.xlsx(output, "Y:/VISION 2050/Data/2050_RGS/STC_final/RGS2050_STC_modInput.xlsx", colNames = TRUE)
  