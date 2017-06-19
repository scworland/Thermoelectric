
# set working directory (where data is located)
setwd("~/Documents/Thermoelectric/may2017")

library(dpylr)
library(igraph)

# load data
data <- read.csv("data/Bo_gen_Y2015_terse_shuffled.csv", stringsAsFactors = F)

plants <- unique(data$plant)
d.out <- data.frame()

for(i in 1:length(plants)){
  plant_i <- plants[i]
  dsub <- filter(data, plant==plant_i)
  edges <- cbind(dsub$boiler,dsub$generator)
  g <- graph_from_edgelist(edges)
  groups <- clusters(g)$membership
  d <- data.frame(names=names(groups), 
                  result2=paste0(plant_i,"^",groups), 
                  plant=plant_i,
                  row.names=NULL,
                  stringsAsFactors = F)
  
  d.out <- rbind(d.out,d)

  # jpeg(file = paste("figures/","plant",plant_i, '.jpeg', sep = ''))
  #      
  # plot(g, vertex.size=5,
  #      vertex.label.dist=0.5, 
  #      vertex.color="red", 
  #      edge.arrow.size=0.7,
  #      main=paste0("plant ", plant_i))
  # dev.off()
}

result <- data %>%
  left_join(d.out, by=c("plant","boiler" = "names"))

write.csv(result,"bogen2015_shuffled_scw.csv", row.names = F)

