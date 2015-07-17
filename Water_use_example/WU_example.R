

setwd("C:\\Users\\scworlan\\Documents\\Thermoelectric\\R_code\\Thermoelectric\\Water_use_example")

# Read in the national water use data
d = read.delim("USGS_national_WU_data.txt", header=T)

# Input the MS river Q
Q1 = 593000 #ft3/s
gal = 7.48 #gallons in ft3
Q2 = (Q1*gal*60*60*24)/1e9 #bgal/day

# Calculate the difference matrix 
library(plyr)
d2 = as.matrix(t(d[,2:7])) #convert data.frame to matrix
d3 = t(diff(d2,lag=1)) # take difference of matrix

# Calculate as percentage of MS Q
d4 = round(d3/Q2*100, 1)

# Reformat and export to excel
library(xlsx)
out = data.frame(d[,1],d4)
colnames(out) = c("Type","1985-1990","1990-1995","1995-2000","2000-2005","2005-2010")
write.xlsx(out,file = "WU_output_R.xlsx",row.names = FALSE)

