
# set working directory (where data is located)
setwd("~/Documents/Thermoelectric/R_code/Thermoelectric/may2017")

library(dplyr)
library(igraph)

# load data from CSV file
data <- read.csv("data/Bo_coo_Y2015_terse_input.csv", stringsAsFactors = F)

# build custom function to do associations. The functions only required input is a data frame
# with column names 'Plant_ID', 'Vertex_1', and 'Vertex_2'. All other columns will be ignored

associate <- function(data,save_image=FALSE){
  
  # test: Check that data provided by user is correct
  if(!is.data.frame(data) || any(names(data) != c('Plant_ID', 'Vertex_1','Vertex_2'))) {
    stop("data must be a data.frame with columns 'Plant_ID', 'Vertex_1', and 'Vertex_2' for this function to continue")
  }
  
  # extract vector of unique plant IDs
  plants <- unique(data$Plant_ID)
  
  # preallocated dataframe for output
  d_out <- data.frame()
  
  # for-loop that does the work
  for(i in 1:length(plants)){
    
    # grab plant[i]
    plant_i <- plants[i]
    
    # subset data by plant_i
    dsub <- dplyr::filter(data, Plant_ID==plant_i)
    
    # prepare 'edges' to pass to graph function
    edges <- cbind(dsub$Vertex_1,dsub$Vertex_2)
    
    # build graph from edges
    g <- graph_from_edgelist(edges)
    
    # extract 'groups' from graph
    groups <- clusters(g)$membership
    
    # put everything together for plant_i
    d <- data.frame(Vertex_2=names(groups), 
                    result=paste0("bogen^",plant_i,"^",groups), 
                    Plant_ID=plant_i,
                    row.names=NULL,
                    stringsAsFactors = F)
    
    # interatively append rows
    d_out <- rbind(d_out,d)
    
    # if save_image==TRUE, then save images
    if(save_image) { 
      
      jpeg(file = paste("figures/","plant",plant_i, '.jpeg', sep = ''))
      
      plot(g, vertex.size=5,
           vertex.label.dist=0.5,
           vertex.color="red",
           edge.arrow.size=0.7,
           main=paste0("plant ", plant_i))
      dev.off()
      
    }
    
  } 
  
  # return output data
  return(d_out)
}

# run 'associate' function
d_out <- associate(data, save_image = FALSE)

# join function output to main data file
result <- dplyr::left_join(data,d_out, by=c("Plant_ID","Vertex_2"))

# write output to a CSV
write.csv(result,"Plant_associations_output.csv", row.names = F)

