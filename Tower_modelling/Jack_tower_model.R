
# Set up workspace and import the data ----

setwd("C:\\Users\\scworlan\\Documents\\Thermoelectric\\R_code\\Thermoelectric\\Tower_modelling")

## Read input from excel
library(xlsx)

### Plant characteristics
PlantChar = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                       sheetIndex="Input_SCW",
                       colIndex = 1:3,
                       rowIndex = 2:722,
                       header=TRUE,
                       stringsAsFactors = FALSE)

### Design characteristics
DesignChar = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                      sheetIndex="Input_SCW",
                      colIndex = c(1,64:66),
                      rowIndex = 2:722,
                      header=TRUE,
                      stringsAsFactors = FALSE)

### Added heat load MMBtu
HeatLoad = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                     sheetIndex="Input_SCW",
                     colIndex = c(1,4:15),
                     rowIndex = 2:722,
                     header=TRUE,
                     stringsAsFactors = FALSE)

### Dry bulb air temperature Ta (oC)  										
DryBulb = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                     sheetIndex="Input_SCW",
                     colIndex = c(1,16:27),
                     rowIndex = 2:722,
                     header=TRUE,
                     stringsAsFactors = FALSE)

### Wet bulb air temperature Twb (oC)    									
WetBulb = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                    sheetIndex="Input_SCW",
                    colIndex = c(1,28:39),
                    rowIndex = 2:722,
                    header=TRUE,
                    stringsAsFactors = FALSE)

### Natural water temperature  T (oC)  										
NaturalWater = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                    sheetIndex="Input_SCW",
                    colIndex = c(1,40:51),
                    rowIndex = 2:722,
                    header=TRUE,
                    stringsAsFactors = FALSE)

### Wind speed at 2m W (mph)  										    									
WindSpeed = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                    sheetIndex="Input_SCW",
                    colIndex = c(1,51:62),
                    rowIndex = 2:722,
                    header=TRUE,
                    stringsAsFactors = FALSE)

# Modelling ----

## convert elevation to mb to psia for all plants (F3,F4,K2)
PlantChar$mb = ((44331.514-(PlantChar$Elevation*0.3048))/11880.516)^(1/0.1902632) 
PlantChar$psia = PlantChar$mb/68.94757293 

## Calculate saturation vapor pressure at inlet air wet bulb temperature (L)
Ew = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(WetBulb[,2:13]+273)))+
               (-0.545/0.11)*log((WetBulb[,2:13]+273)/273)) 

Ew = cbind(PlantChar$Plant_ID,Ew); #include plant ID
colnames(Ew)[1] = "Plant_ID"

## saturated vapor pressure from dry bulb temperature (N,M)
esat_psia = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(DryBulb[,2:13]+273)))+
              (-0.545/0.11)*log((DryBulb[,2:13]+273)/273))/68.94757293

esat_mb = esat_psia*68.94757293
  
esat_psia = cbind(PlantChar$Plant_ID,esat_psia); #include plant ID
colnames(esat_psia)[1] = "Plant_ID"

esat_mb = cbind(PlantChar$Plant_ID,esat_mb); #include plant ID
colnames(esat_mb)[1] = "Plant_ID"


## Actual vapor pressure in inlet air (O)
x1 = Ew[,2:13]-PlantChar$mb
x2 = (DryBulb[,2:13]-WetBulb[,2:13]) 
x3 = 1+(0.00115*WetBulb[,2:13])*0.00066
      
vap_mb = x1*x2*x3





