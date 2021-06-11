#######################################################################
#####       Estimating the primary diet sources in food webs      #####
#####          J. A. Smith Nov 2015 & June 2020                   #####
#######################################################################

## This algorithm works by finding all food chains between a consumer and the base of the food web...
## and sums the product of all these chains for each consumer of interest (fish species).
## There are many chains from the highe rtrophic levels, so these take longer to compute.

## This is coded with 9 prey levels (prey of prey of prey etc), meaning that the longest food chain...
## we consider is 10 organisms long (i.e. a 'fish > invert > algae' chain is 3 organisms long). 
## But these longer chains are usually uncommon and not influential.
## A good check: if all energy is accounted for, this number of levels is fine (see notes at end).

## NOTE: this algorithm is not very smart, and takes exponentially longer with every additional level. 
## A smarter way may be to work from the bottom and calculate using direct prey only when possible.

## NOTE: if needed, put the 'unidentfied' prey type as final column (and column must be all zeros)
## NOTE: probably avoid cannibalism within diet matrix



###########################################
###    Set up and Load data             ###
###########################################

## Set directory (where the function and data are saved)
setwd("H:/Honours Computer Documents/Running James Code")


## Load diet data (a diet matrix, with diet compositions as columns [columns sum to 1])
diet_matrix_sim <- read.csv("Truong_diets_noCann.csv")

## Load relative biomass data (columns sum to one; must be in same order as species in diet matrix)
fish_biomass <- read.csv("Goddard_RelativeBiomass.csv")  #this example has four sites


###########################################
###    Calculate diet sources           ###
###########################################

## Specify the column numbers of diet matrix corresponding to the consumers 
## The number of columns must be equal to the number of rows in 'fish_biomass'
fish_cols <- 1:19  #in Truong, we had 19 fish and invert consumers in the first 19 columns/rows
# we include inverts here, but they have zero weight in 'fish_biomass', so contribute nothing to final result

## Specify the column/row numbers for the sources (zoopl., phytopl., macroalgae, and detritus)
row_zoo <- 20
row_phy <- 21
row_mac <- 22
row_det <- 23

## Create objects to store results
results <- array(0,c(5,length(fish_cols)))
colnames(results) <- names(diet_matrix_sim)[fish_cols]
skip <- character()
loop_links <- 0

## Run the diet sources algorithm
source("DIETSOURCES_Jun2020_function.R")


## View results
results <- t(results)  #transpose for later
colnames(results) <- c("Total", "Zoopl", "Phyto", "Macro", "Detritus")
print(results <- as.data.frame(results))
#column 1 is the total energy accounted for (check this = 1 for every species; if not may need more levels)
#column 2 is the proportion of conumed biomass derived from zooplankton
#column 3 is from phytoplankton, then macroalgae, then detritus

## View number of connections
loop_links
#this is the number of chains between any fish consumer and the base of the food web
#this is approximate, so don't trust this too much


###########################################
###    Weight sources by biomass        ###
###########################################

## specify the column in 'fish_biomass' you want to analyse (i.e. which site, if there are multiple)
biomass_col <- 2 #in my data, site 1 is in column 2

## file to store results
wtd_props <- data.frame(Source=c("Zoopl","Phyto","Macro","Detritus","Unident"), Prop=0)

## calculate energy source proportions weighted by fish biomass
wtd_props$Prop[wtd_props$Source=="Zoopl"] <- sum(results$Zoopl * fish_biomass[,biomass_col])
wtd_props$Prop[wtd_props$Source=="Phyto"] <- sum(results$Phyto * fish_biomass[,biomass_col])
wtd_props$Prop[wtd_props$Source=="Macro"] <- sum(results$Macro * fish_biomass[,biomass_col])
wtd_props$Prop[wtd_props$Source=="Detritus"] <- sum(results$Detritus * fish_biomass[,biomass_col])
wtd_props$Prop[wtd_props$Source=="Unident"] <- 1 - sum(wtd_props$Prop[wtd_props$Source %in% c("Phyto","Macro","Detritus")])  #unidentified prey source

sum(wtd_props$Prop[2:5])  #check this sums to 1 (zoopl. excluded because it's not a primary energy source)

## view results (the energy derived from each source for this fish assemblage)
wtd_props

