
# Set up workspace and import the data ----

setwd("C:\\Users\\scworlan\\Documents\\Thermoelectric\\R_code\\Thermoelectric\\Tower_modelling")

## Read input from excel ----
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

# Modelling Phase one ----
-------------------------------------------------------------------------------------
## plant characteristics pre-calculations
  
## Create unique vector for plant ID
PlantID = data.frame(PlantChar[,1])
colnames(PlantID) = "Plant_ID"

## convert elevation to mb to psia for all plants (F3,F4,K2)
PlantChar$atm_mb = ((44331.514-(PlantChar$Elevation*0.3048))/11880.516)^(1/0.1902632) 
PlantChar$atm_psia = PlantChar$atm_mb/68.94757293 

-------------------------------------------------------------------------------------
## Month (and design characteristics) x plant calculations (13xn)
  
## Add design Twb, Tdb and nwT to WetBulb, DryBulb and NaturalWater matrices
DryBulb$design =  (DesignChar$Tdb-32)*5/9
WetBulb$design =  (DesignChar$Twb-32)*5/9
NaturalWater$design = DesignChar$nwT
  
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

## inlet air specific volume in cubic feet per pound - pertains to vapor/gas mixture (R)
sv = ((1+w1*1.585918)*286.9*((273.15+DryBulb)/(PlantChar$atm_psia*6894.757))
      /0.3048^3)/2.20462262

## specific volume of dry air ft3/lb (AD)
svdry = sv*(1+w1)

-------------------------------------------------------------------------------------
## mixture of dimensions. This next session has more details then the final, although
## they do many of the same calculations
  
# L&M '71 equation 1 ----

## Load CITI inputs (this will eventually be calculated here)
CITI = read.xlsx(file = filename,
                   sheetIndex = "CITI",
                   colIndex = 1:6,
                   startRow = 1,
                   header = TRUE,
                   stringsAsFactors = FALSE)

## plant x month x parameter
# library(reshape2)
# x1 = cbind(PlantID,DryBulb)
# x1 = melt(cbind(PlantID,DryBulb),id="Plant_ID")

## condenser range in degrees F (X and AB)
cRange = CITI[,4]

## L/G mass ratio (Y)
LG = CITI[,5]

## condenser heat load in Btu/hr (S)
cHL = 1000000

## Condenser discharge in gpm (T)
cQ = cHL/(60*8.3*cRange)

## Tower air flow mass lbair/hr (AC)
Ma = cQ*8.3*60/LG

## specific volume of dry air ft3/lb for design
vdDC = svdry$design[54]

## Tower airflow volume ft3/hr (AE)
Va = Ma * vdDC

## Air flow vs median air flow (AF)
Vamed = Va/median(Va)

## Max monthly assumed makeup water T in degrees F (AG)
MupWT = (NaturalWater$design[54]*(9/5)+32)

## 1% assumed makeup gpm (AA)
gpm1 = 2.00803212851406

## Assumed makeup ratio (AH)
gpm1R = gpm1/cQ

# L&M '71 equations 5-10 ----

### Create lookup table
Tc = seq(0.00, 80, 0.01) #check
Tf = (Tc*(9/5))+32 #check
mb = 6.1078*10^((Tc*7.5)/(Tc+237.3)) #check
psia = mb/68.94757293 #check
W = (0.622*mb)/(PlantChar$atm_mb[54]-(0.378*mb))
H = (0.24*Tf)+(W*(1061+0.444*Tf))
SatH = data.frame(Tc,mb,psia,H,Tf,W)

### function to find index of closest value per
### http://stackoverflow.com/questions/10160400/r-find-nearest-index
nearest.vec <- function(x, vec)
{
  smallCandidate <- findInterval(x, vec, all.inside=TRUE)
  largeCandidate <- smallCandidate + 1
  #nudge is TRUE if large candidate is nearer, FALSE otherwise
  nudge <- 2 * x > vec[smallCandidate] + vec[largeCandidate]
  return(smallCandidate + nudge)
}

## equation 5: Delta enthalpy BTU/lbair (AJ)
DH = ((MupWT-32)*gpm1*60*8.3+cHL)/Ma

# excel spread sheet has
#=+(($AG9-32)*AA9*60*8.3+$S9)/$AC9
# which I think should be
#=+($AG9-32)*(5/9)*AA9*60*8.3+$S9)/$AC9

## equation 6: outlet enthalpy BTU/lbair (AK)
Ha1DC = Ha1$design[54]
Ha2 = Ha1DC + DH
#Ha2 = Ha1 + DH

## equation 7: outlet temperature in degrees F (AL)

### find the temperature that corresponds to the outlet enthalpy
index = nearest.vec(Ha2,SatH$H)

### find outlet temperature (AL)
To = SatH$Tf[index]

## equation 9: outlet moisture content lbwater/lbair (AM)
W2 = SatH$W[index]

## equation 10: amount of water evaporated (AN)
w1DC = w1$design[54]
gpm2 = Ma*(W2-w1DC)/(8.3*60)
gpm2R = gpm2/cQ

## Iterative loops of equations 5-10 ----

### initialize (outside loop)
gpm = gpm1
Ha1DC = Ha1$design[54]
w1DC = w1$design[54]
Rold = rep(0.5, length(DH))
RR = 0.5
times = 0

### while loop with makup ratio/makeup ratio
while(RR < 1) {
  DH = ((MupWT-32)*gpm*60*8.3+cHL)/Ma
  Ha2 = Ha1DC + DH
  index = nearest.vec(Ha2,SatH$H)
  To = SatH$Tf[index]
  W2 = SatH$W[index]
  gpm = Ma*(W2-w1DC)/(8.3*60)
  R = gpm/cQ
  RR = min(R/Rold)
  Rold = R
  times = times + 1
}

----
  
### initialize (outside loop)  
gpm = gpm1
Ha1DC = Ha1$design[54]
w1DC = w1$design[54]
gpm_old = rep(0, length(DH))
dgpm = 1
times = 0
thold = 4e-6 # ml/hr or 100s gal/year

### while loop with change in gpm
while(dgpm > 0) {
  DH = ((MupWT-32)*gpm*60*8.3+cHL)/Ma
  Ha2 = Ha1DC + DH
  index = nearest.vec(Ha2,SatH$H)
  To = SatH$Tf[index]
  W2 = SatH$W[index]
  gpm = Ma*(W2-w1DC)/(8.3*60)
  dgpm = max(abs(gpm-gpm_old))
  gpm_old = gpm
  times = times + 1
}

## While loop test ----

### initialize
gpm = gpm1
Ha1DC = Ha1$design[54]
w1DC = w1$design[54]
gpm_old = rep(0, length(DH))
dgpm = 1
times = 0


### pass one
DH = ((MupWT-32)*gpm*60*8.3+cHL)/Ma
Ha2 = Ha1DC + DH
index = nearest.vec(Ha2,SatH$H)
To = SatH$Tf[index]
W2 = SatH$W[index]
gpm_1 = Ma*(W2-w1DC)/(8.3*60)
dgpm_1 = max(abs(gpm_1-gpm1))

### pass two
DH = ((MupWT-32)*gpm_1*60*8.3+cHL)/Ma
Ha2 = Ha1DC + DH
index = nearest.vec(Ha2,SatH$H)
To = SatH$Tf[index]
W2 = SatH$W[index]
gpm_2 = Ma*(W2-w1DC)/(8.3*60)
dgpm_2 = max(abs(gpm_2-gpm_1))

### pass three
DH = ((MupWT-32)*gpm_2*60*8.3+cHL)/Ma
Ha2 = Ha1DC + DH
index = nearest.vec(Ha2,SatH$H)
To = SatH$Tf[index]
W2 = SatH$W[index]
gpm_3 = Ma*(W2-w1DC)/(8.3*60)
dgpm_3 = max(abs(gpm_3-gpm_2))

### pass four
DH = ((MupWT-32)*gpm_3*60*8.3+cHL)/Ma
Ha2 = Ha1DC + DH
index = nearest.vec(Ha2,SatH$H)
To = SatH$Tf[index]
W2 = SatH$W[index]
gpm_4 = Ma*(W2-w1DC)/(8.3*60)
dgpm_4 = max(abs(gpm_4-gpm_3))

# Min, med, and max estimations----

## Condenser duty MMBtu/hr (BK)
cD = cHL/1000000

## Water flow to evaporate entire duty (BL), is the amount of water 
# that would have to be evaporated to use up all of the condenser duty, 
# assuming no sensible heat is transferred to the air. 
# This number varies a little with makeup water temperature.
Y = NaturalWater$design[54]
X = cD*(1000000*7.48051945564918/(60*(1000*(1-((Y)+
  288.9414)/(508929.2*((Y)+68.12963))*
  ((Y)-3.9863)^2))*0.0624*((((-0.0000614342)*
  (Y)^3 + (0.00158927)*(Y)^2 - (2.36418)*
  (Y) + 2500.79)*0.947817/(2.2046)))))

## “final evaporation ratio” (BM), is the ratio of the amount 
#of water estimated to be evaporated in the tower divided by 
# the amount of water that would evaporated if all the condenser 
# duty were used to evaporate water. This ratio can exceed 1 when 
# the tower cools the inlet air, as on line 9. Ratios smaller than 
# 1 are typical.

Evap = gpm/X

-------------------------------------------------------------------------------------
## mixture of dimensions, uses for loops and nested while loops


CITI = read.xlsx(file = filename, sheetIndex = "CITI", header = TRUE)
cHL = 1000000
cRange = CITI[,4]
cQ = cHL/(60*8.3*cRange)
VaDC = matrix(ncol=nrow(CITI),
          nrow=nrow(PlantChar))

# first calculate the volume air flow for the design conditions !CHECK!
for (i in 1:nrow(PlantChar)){
LGDC = CITI[,5]
MaDC = cQ*8.3*60/LGDC
vdDC = svdry$design[i]
VaDC[i,] = MaDC * vdDC
}

# then use inputs + VaDC for remaining calculations !CHECK!
#preallocate
emin=matrix(ncol=ncol(DryBulb),
          nrow=nrow(PlantChar))

emed=matrix(ncol=ncol(DryBulb),
          nrow=nrow(PlantChar))

emax=matrix(ncol=ncol(DryBulb),
          nrow=nrow(PlantChar))

e25=matrix(ncol=ncol(DryBulb),
            nrow=nrow(PlantChar))

e75=matrix(ncol=ncol(DryBulb),
            nrow=nrow(PlantChar))

for (j in 1:ncol(DryBulb)){
for (i in 1:nrow(PlantChar)){
Ma = VaDC[i,]/svdry[i,j] #check
LG = (cQ*8.33*60)/Ma
vd = VaDC[i]
MupWT = (NaturalWater[i,j]*(9/5)+32)
gpm1 = 2.00803212851406
gpm = gpm1
gpm_old = rep(0, length(DH))
dgpm = 1
times = 0
thold = 4e-6 

while(dgpm > thold) {
  DH = ((MupWT-32)*gpm*60*8.3+cHL)/Ma
  Ha2 = Ha1[i,j] + DH
  index = nearest.vec(Ha2,SatH$H)
  To = SatH$Tf[index]
  W2 = SatH$W[index]
  gpm = Ma*(W2-w1[i,j])/(8.3*60)
  dgpm = max(abs(gpm-gpm_old))
  gpm_old = gpm
  times = times + 1
}

cD = cHL/1000000
Y = NaturalWater[i,j]
X = cD*(1000000*7.48051945564918/(60*(1000*(1-((Y)+
    288.9414)/(508929.2*((Y)+68.12963))*
    ((Y)-3.9863)^2))*0.0624*((((-0.0000614342)*
    (Y)^3 + (0.00158927)*(Y)^2 - (2.36418)*
    (Y) + 2500.79)*0.947817/(2.2046)))))
Evap = gpm/X

emin[i,j] = min(Evap)
emed[i,j] = median(Evap)
emax[i,j] = max(Evap)
e25[i,j] = quantile(Evap,0.25)
e75[i,j] = quantile(Evap,0.75)
}
}

# Mapping ----
library(RColorBrewer)
library(ggmap)

## subset the plants used in the model
PlantChar$med = emed$design
plants = merge(location,PlantChar,by="Plant_ID")
plants = subset(plants, lon > -130)

## load the state data
state = map_data('state')

m1 = ggplot() + ggtitle("U.S. Thermoelectric Plants")
m1 = m1 + geom_polygon(data=state,aes(long,lat, group=group), color = "white", fill= "black") 
m1 = m1 + coord_fixed(1.3) + theme_bw(base_size = 20)
m1 = m1 + geom_point(data=plants, aes(lon,lat,color=med), size=2)
m1 = m1 + scale_color_gradientn(colours = rev(brewer.pal(n=11,name = 'RdYlBu')))
m1



