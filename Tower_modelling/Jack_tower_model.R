
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

## convert elevation to mb to psia for all plants
PlantChar$mb = ((44331.514-(PlantChar$Elevation*0.3048))/11880.516)^(1/0.1902632) # F3 & F4
PlantChar$psia = PlantChar$mb/68.94757293 # K2

## convert Twb and Tdb to SI units !!! not needed
#DryBulb[,2:13] = (DryBulb[,2:13]-32)/1.8
#WetBulb[,2:13] = (WetBulb[,2:13]-32)/1.8

## Calculate saturation vapor pressure at inlet air wet bulb temperature
Ew = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(WetBulb[,2:13]+273)))+
               (-0.545/0.11)*log((WetBulb[,2:13]+273)/273)) # L

Ew = cbind(PlantChar$Plant_ID,Ew); 
colnames(Ew)[1] = "Plant_ID"








