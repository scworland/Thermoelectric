
## Set up workspace and import the data ----

setwd("C:\\Users\\scworlan\\Documents\\Thermoelectric\\R_code\\Thermoelectric\\Tower_modelling")

## Read input from excel ----
library(xlsx)

filename = "Towers_test_input_one_plant_7_17_2015.xlsx"
sheetname = "Input_SCW"

options(java.parameters = "-Xmx4g")

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

# Modelling Phase one 

## plant characteristics pre-calculations ----
  
## Create unique vector for plant ID
PlantID = data.frame(PlantChar[,1])
colnames(PlantID) = "Plant_ID"

## convert elevation to mb to psia for all plants
PlantChar$atm_mb = ((44331.514-(PlantChar$Elevation*0.3048))/11880.516)^(1/0.1902632) 
PlantChar$atm_psia = PlantChar$atm_mb/68.94757293 

## Month (and design characteristics) x plant calculations ----

## this section of codes creates matrices with columns 1-12 as Jan-Dec and
## column 13 is the design condition, and the rows are the plants
  
## Add design Twb, Tdb and nwT to WetBulb, DryBulb and NaturalWater matrices
DryBulb$design = (DesignChar$Tdb-32)*5/9
WetBulb$design = (DesignChar$Twb-32)*5/9
NaturalWater$design = DesignChar$nwT

## Calculate saturation vapor pressure at inlet air wet bulb temperature
Pw_mb = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(WetBulb+273)))+
                     (-0.545/0.11)*log((WetBulb+273)/273)) 

Pw_psia = Pw_mb/68.94757293 

## saturated vapor pressure from dry bulb temperature
Ps_mb = 6.1078*exp(((595.9-273*-0.545)/0.11)*((1/273)-(1/(DryBulb+273)))+
                     (-0.545/0.11)*log((DryBulb+273)/273))

Ps_psia = Ps_mb/68.94757293

## Actual vapor pressure in inlet air
vap_mb = Pw_mb - (PlantChar$atm_mb*0.00066*(DryBulb-WetBulb)*(1+(0.00115*WetBulb)))

## relative humidity of inlet air
phi = vap_mb/Ps_mb

## Pounds of water vapor per pound of dry air in inlet air, calculated per L&M '71 eqn 3
w1 = (0.622*phi*Ps_psia)/(PlantChar$atm_psia-(phi*Ps_psia))

## enthalpy of inlet air calculated per L&M '71 eqn 4
Ha1=0.24*(DryBulb*(9/5)+32)+w1*(1061.8+0.44*(DryBulb*(9/5)+32))

## inlet air specific volume in cubic feet per pound - pertains to vapor/gas mixture
sv = ((1+w1*1.585918)*286.9*((273.15+DryBulb)/(PlantChar$atm_psia*6894.757))
      /0.3048^3)/2.20462262

## specific volume of dry air ft3/lb
svdry = sv*(1+w1)

## custom function for lookup table ----
### http://stackoverflow.com/questions/10160400/r-find-nearest-index
nearest.vec <- function(x, vec)
{
  smallCandidate <- findInterval(x, vec, all.inside=TRUE)
  largeCandidate <- smallCandidate + 1
  #nudge is TRUE if large candidate is nearer, FALSE otherwise
  nudge <- 2 * x > vec[smallCandidate] + vec[largeCandidate]
  return(smallCandidate + nudge)
}

## Actual model ----
CITI = read.xlsx(file = filename,
                 sheetIndex = "CITI",
                 colIndex = 1:6,
                 startRow = 1,
                 header = TRUE,
                 stringsAsFactors = FALSE)


cHL = 1000000
cRange = CITI[,4]
cQ = cHL/(60*8.3*cRange)

### preallocate the tower airflow volume matrix
VaDC = matrix(ncol=nrow(CITI), nrow=nrow(PlantChar))

### first calculate the volume air flow for the design conditions
for (i in 1:nrow(PlantChar)){
  LGDC = CITI[,5]
  MaDC = cQ*8.3*60/LGDC
  vdDC = svdry$design[i]
  VaDC[i,] = MaDC * vdDC
}

### Use inputs + VaDC for remaining calculations
### preallocate matrices
emin=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
emed=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
emax=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
e25=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
e75=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))

for (j in 1:ncol(DryBulb)){
  for (i in 1:nrow(PlantChar)){
    Ma = VaDC[i,]/svdry[i,j] 
    LG = (cQ*8.33*60)/Ma
    MupWT = (NaturalWater[i,j]*(9/5)+32)
    gpm1 = 2.00803212851406
    gpm = gpm1
    gpm_old = rep(0,nrow(CITI))
    dgpm = 1
    times = 0
    thold = 4e-6 

    ## Create lookup table 
    Tc = seq(0.00, 80, 0.01) 
    Tf = (Tc*(9/5))+32 
    mb = 6.1078*10^((Tc*7.5)/(Tc+237.3)) 
    psia = mb/68.94757293 
    W = (0.622*mb)/(PlantChar$atm_mb[i]-(0.378*mb))
    H = (0.24*Tf)+(W*(1061+0.444*Tf))
    SatH = data.frame(Tc,mb,psia,H,Tf,W)

    
    ### Start the timer
    ptm <- proc.time()
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
    ### stop the timer
    proc.time() - ptm
    
    cD = cHL/1000000

    Dutygpm = cD*(1000000*7.48051945564918/(60*(1000*(1-((NaturalWater[i,j])+
        288.9414)/(508929.2*((NaturalWater[i,j])+68.12963))*
        ((NaturalWater[i,j])-3.9863)^2))*0.0624*((((-0.0000614342)*
        (NaturalWater[i,j])^3 + (0.00158927)*(NaturalWater[i,j])^2 - (2.36418)*
        (NaturalWater[i,j]) + 2500.79)*0.947817/(2.2046)))))
    
    Evap = gpm/Dutygpm
    
    emin[i,j] = min(Evap)
    emed[i,j] = median(Evap)
    emax[i,j] = max(Evap)
    e25[i,j] = quantile(Evap,0.25)
    e75[i,j] = quantile(Evap,0.75)
  }
}


## Export to excel ----
output = data.frame(cbind(PlantID,emin,emed,emax,e25,e75))
cols = rep(colnames(DryBulb),5)
colnames(output)[2:ncol(output)] = cols

write.xlsx(output,"Tower_model_output.xlsx",row.names=F)