# Set up workspace and import the data ----

setwd("C:\\Users\\scworlan\\Documents\\Thermoelectric\\R_code\\Thermoelectric\\Tower_modelling")

## Read input from excel
library(xlsx)

filename = "Towers_test_input_one_plant_7_17_2015.xlsx"
sheetname = "Input_SCW"

### Plant characteristics
PlantChar = read.xlsx(file = filename,
                      sheetIndex = sheetname,
                      colIndex = 1:3,
                      startRow = 2,
                      header = TRUE,
                      stringsAsFactors = FALSE)

### Design characteristics
DesignChar = read.xlsx(file = filename,
                       sheetIndex = sheetname,
                       colIndex = 64:66,
                       startRow = 2,
                       header = TRUE,
                       stringsAsFactors = FALSE)

### Added heat load MMBtu
HeatLoad = read.xlsx(file = filename,
                     sheetIndex = sheetname,
                     colIndex = 4:15,
                     startRow = 2,
                     header = TRUE,
                     stringsAsFactors = FALSE)

### Dry bulb air temperature Ta (oC)        							
DryBulb = read.xlsx(file = filename,
                    sheetIndex = sheetname,
                    colIndex = 16:27,
                    startRow = 2,
                    header = TRUE,
                    stringsAsFactors = FALSE)

### Wet bulb air temperature Twb (oC)    									
WetBulb = read.xlsx(file = filename,
                    sheetIndex = sheetname,
                    colIndex = 28:39,
                    startRow = 2,
                    header = TRUE,
                    stringsAsFactors = FALSE)

### Natural water temperature  T (oC)  										
NaturalWater = read.xlsx(file = filename,
                         sheetIndex = sheetname,
                         colIndex = 40:51,
                         startRow = 2,
                         header = TRUE,
                         stringsAsFactors = FALSE)

### Wind speed at 2m W (mph)  										    									
WindSpeed = read.xlsx(file = filename,
                      sheetIndex = sheetname,
                      colIndex = 51:62,
                      startRow = 2,
                      header = TRUE,
                      stringsAsFactors = FALSE)

### locations and names of plants
location = read.xlsx(file = filename,
                     sheetIndex = "locations_SCW",
                     colIndex = 1:4,
                     startRow = 1,
                     header = TRUE,
                     stringsAsFactors = FALSE)

# Modelling ----

## Create unique vector for plant ID
PlantID = data.frame(PlantChar[,1])
colnames(PlantID) = "Plant_ID"

## convert elevation to mb to psia for all plants (F3,F4,K2)
PlantChar$atm_mb = ((44331.514-(PlantChar$Elevation*0.3048))/11880.516)^(1/0.1902632) 
PlantChar$atm_psia = PlantChar$atm_mb/68.94757293 

## Calculate saturation vapor pressure at inlet air wet bulb temperature (L)
Pw_mb = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(WetBulb+273)))+
                  (-0.545/0.11)*log((WetBulb+273)/273)) 

Pw_psia = Pw_mb/68.94757293 

## saturated vapor pressure from dry bulb temperature (N,M)
Ps_mb = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(DryBulb+273)))+
                         (-0.545/0.11)*log((DryBulb+273)/273))

Ps_psia = Ps_mb/68.94757293

## Actual vapor pressure in inlet air (O)
vap_mb = Pw_mb - (PlantChar$atm_mb*0.00066*(DryBulb-WetBulb)*(1+(0.00115*WetBulb)))

## relative humidity of inlet air (K)
phi = vap_mb/Ps_mb

## Pounds of water vapor per pound of dry air in inlet air, calculated per L&M '71 eqn 3 (Q)
w1 = (0.622*phi*Ps_psia)/(PlantChar$atm_psia-(phi*Ps_psia))

## enthalpy of inlet air calculated per L&M '71 eqn 4 (J and AI)
Ha1=0.24*(DryBulb*(9/5)+32)+w1*(1061.8+0.44*(DryBulb*(9/5)+32))

## inlet air specific volume in cubic feet per pound - pertains to vapor/gas mixture
sv = ((1+w1*1.585918)*286.9*((273.15+DryBulb)/(PlantChar$atm_psia*6894.757))/0.3048^3)/2.20462262


# Mapping ----
library(RColorBrewer)
library(ggmap)

## subset the plants used in the model
plants = merge(location,PlantChar,by="Plant_ID")
plants = subset(plants, lon > -130)

## load the state data
state = map_data('state')

m1 = ggplot() + ggtitle("U.S. Thermoelectric Plants")
m1 = m1 + geom_polygon(data=state,aes(long,lat, group=group), color = "white", fill= "black") 
m1 = m1 + coord_fixed(1.3) + theme_bw(base_size = 20)
m1 = m1 + geom_point(data=plants, aes(lon,lat,color=Elevation), size=2)
m1 = m1 + scale_color_gradientn(colours = rev(brewer.pal(n=11,name = 'RdYlBu')))
m1



