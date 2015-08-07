
## clear workspace environment
rm(list=ls(all=T)) 

## Set up workspace and import the data ----
setwd("~/Thermoelectric/R_code/Thermoelectric/Tower_modelling")

### Start the timer
ptm <- proc.time()

## read from CSV file
filename = "Winterized_input.csv"

data = read.csv(filename, header=T, skip=4)
param = read.csv(filename, header=T, skip=1, nrows=1,
                 colClasses = c(rep(NA, 3), rep("NULL", 63)))

min_T = param[1,1]
min_approach = param[1,2]
max_approach = param[1,3]

### Plant characteristics
PlantChar = data[1:3]

### Design characteristics
DesignChar = data[64:66]

### Added heat load MMBtu
HeatLoad = data[4:15]
col_names = colnames(HeatLoad)

### Dry bulb air temperature Ta           					
DryBulb = data[16:27]
colnames(DryBulb) = col_names

### Wet bulb air temperature Twb    									
WetBulb = data[28:39]
colnames(WetBulb) = col_names

### Natural water temperature 										
NaturalWater = data[40:51]
colnames(NaturalWater) = col_names

### Wind speed at 2m W (mph)  										    									
WindSpeed = data[51:62]
colnames(WindSpeed) = col_names

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

## set minimum T for wet and dry bulbs
DryBulb[DryBulb<min_T] = min_T
WetBulb[WetBulb<min_T] = min_T

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

### Read in CTI file
CTI = read.csv(file = "CTI_input.csv", header=T, skip=3)
CTI_param = read.csv("CTI_input.csv", header=T, skip=1, nrows=1,
                 colClasses = c(rep(NA, 5), rep("NULL", 1)))

### name parameters created by user
min_app = CTI_param[1,1]
max_app = CTI_param[1,2]
cond_app = CTI_param[1,3]
min_steam = CTI_param[1,4]
steam_cushion = CTI_param[1,5]

### Create new LG values (1,1.33,1.667,2)
CTI$LG2 = rep(c(1,1+(1/3),1+(2/3),2))

### preallocate matrices
emin=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
emed=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
emax=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
e25=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))
e75=matrix(ncol=ncol(DryBulb),nrow=nrow(PlantChar))

plant = 55177
plantindex = which(PlantChar$Plant_ID==plant)

for (i in 1:nrow(PlantChar)){
#for (i in plantindex){

### typical steam
typSteam = 92 + (DesignChar$Twb[i]-55)/40*28.5
max_steam = typSteam + steam_cushion
  
CTI$Approach_elev = (CTI[,5]-CTI[,4])/(3500-600)*(PlantChar$Elevation[i]-600) + CTI[,4]

### Create vectors needed for approach interpolation
LGR = (CTI$LG2-1.2)/(1.8-1.2)
diffA68 = rep(diff(CTI$Approach_elev)[seq(1, nrow(CTI), 4)], each=4)
diffA78 = rep(diff(CTI$Approach_elev)[seq(3, nrow(CTI), 4)], each=4)
APP68 = LGR*diffA68 + rep(CTI$Approach_elev[seq(1, nrow(CTI), 4)], each=4)
APP78 = LGR*diffA78 + rep(CTI$Approach_elev[seq(3, nrow(CTI), 4)], each=4)

### Interpolate new approach for LG, elevation and design Twb
CTI$Approach2 = ((DesignChar$Twb[i]-68)/10)*(APP78-APP68)+APP68

### add steam T
CTI$SteamT = DesignChar$Twb[i] + CTI$Range + CTI$Approach2 + cond_app

### censor towers
CTI2 = subset(CTI, Approach2 > min_app & Approach2 < max_app)
CTI2 = subset(CTI, SteamT > min_steam & SteamT < max_steam)

### some other parameters
cHL = 1000000
cRange = CTI2[,4]
cQ = cHL/(60*8.3*cRange)

### first calculate the volume air flow for the design conditions
LGDC = CTI2[,8]
MaDC = cQ*8.3*60/LGDC
VaDC = MaDC * svdry$design[i]


### Use inputs + VaDC for monthly calculations
for (j in 1:ncol(DryBulb)){
    Ma = VaDC/svdry[i,j] 
    LG = (cQ*8.33*60)/Ma
    MupWT = (NaturalWater[i,j]*(9/5)+32)
    gpm1 = 2.00803212851406
    gpm = gpm1
    gpm_old = rep(0,nrow(CTI2))
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

### stop the timer
proc.time() - ptm

## Export to excel ----
output = data.frame(cbind(PlantID,emin,emed,emax,e25,e75))
cols = rep(colnames(DryBulb),5)
colnames(output)[2:ncol(output)] = cols

write.csv(output,"Tower_model_output.csv",row.names=F)

## Plots ----
library(ggplot2)
library(reshape2)
library(RColorBrewer)

### melt wide frames into long format and combine
Twbm = melt(WetBulb[,1:12])
Tdbm = melt(DryBulb[,1:12])
emedm = melt(emed[,1:12])
CC = cbind(Twbm,Tdbm[,2],emedm[,3])
colnames(CC) = c("month","Twb","Tdb","medEvap")
CC$Plant_ID = rep(PlantChar$Plant_ID,each=12)
CC2 = subset(CC, medEvap > 1.1)

# all points
p = ggplot(data=CC2)
p = p + geom_point(aes(Twb,medEvap,color = month))
p = p + scale_color_manual(values=c("royalblue4", "royalblue3",
                                    "cornflowerblue", "darkgoldenrod",
                                    "darkgoldenrod1", "firebrick3",
                                    "darkred", "darkred",
                                    "darkgoldenrod1", "darkgoldenrod",
                                    "cornflowerblue", "royalblue3"))
p = p + theme_grey(base_size=20)
p

# subset of points
p2 = ggplot(data=CC2,aes(Twb,medEvap,color = month))
#p2 = p2 + geom_point(size=5)
p2 = p2 + theme_grey(base_size=20)
p2 = p2 
p2 = p2 + geom_text(aes(label=Plant_ID),size=5)
p2
