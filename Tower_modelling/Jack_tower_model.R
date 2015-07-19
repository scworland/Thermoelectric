
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
                      colIndex = 64:66,
                      rowIndex = 2:722,
                      header=TRUE,
                      stringsAsFactors = FALSE)

### Added heat load MMBtu
HeatLoad = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                     sheetIndex="Input_SCW",
                     colIndex = 4:15,
                     rowIndex = 2:722,
                     header=TRUE,
                     stringsAsFactors = FALSE)

### Dry bulb air temperature Ta (oC)  										
DryBulb = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                     sheetIndex="Input_SCW",
                     colIndex = 16:27,
                     rowIndex = 2:722,
                     header=TRUE,
                     stringsAsFactors = FALSE)

### Wet bulb air temperature Twb (oC)    									
WetBulb = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                    sheetIndex="Input_SCW",
                    colIndex = 28:39,
                    rowIndex = 2:722,
                    header=TRUE,
                    stringsAsFactors = FALSE)

### Natural water temperature  T (oC)  										
NaturalWater = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                    sheetIndex="Input_SCW",
                    colIndex = 40:51,
                    rowIndex = 2:722,
                    header=TRUE,
                    stringsAsFactors = FALSE)

### Wind speed at 2m W (mph)  										    									
WindSpeed = read.xlsx("Towers_test_input_one_plant_7_17_2015.xlsx",
                    sheetIndex="Input_SCW",
                    colIndex = 51:62,
                    rowIndex = 2:722,
                    header=TRUE,
                    stringsAsFactors = FALSE)

# Modelling ----

## Create unique vector for plant ID
PlantID = data.frame(PlantChar[,1])
colnames(PlantID) = "ID"

## convert elevation to mb to psia for all plants (F3,F4,K2)
PlantChar$mb = ((44331.514-(PlantChar$Elevation*0.3048))/11880.516)^(1/0.1902632) 
PlantChar$psia = PlantChar$mb/68.94757293 

## Calculate saturation vapor pressure at inlet air wet bulb temperature (L)
Ew = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(WetBulb+273)))+
               (-0.545/0.11)*log((WetBulb+273)/273)) 

## saturated vapor pressure from dry bulb temperature (N,M)
esat_psia = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(DryBulb+273)))+
              (-0.545/0.11)*log((DryBulb+273)/273))/68.94757293

esat_mb = esat_psia*68.94757293

## Actual vapor pressure in inlet air (O)
x1 = PlantChar$mb*0.00066
x2 = DryBulb-WetBulb 
x3 = 1+(0.00115*WetBulb)

vap_mb = Ew - (x1*x2*x3)

## relative humidity of inlet air (K)
phi = vap_mb/Ew

## Pounds of water vapor per pound of dry air in inlet air, calculated per L&M '71 eqn 3 (Q)
LbLb = (0.622*phi*esat_psia)/(PlantChar$psia-(phi*esat_psia))

## enthalpy of inlet air calculated per L&M '71 eqn 4 (J and AI)
inlet_H=0.24*(DryBulb*(9/5)+32)+LbLb*(1061.8+0.44*(DryBulb*(9/5)+32))











