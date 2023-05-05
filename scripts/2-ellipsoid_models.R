# Project: "Birth, life and death of a lithic technological tradition: exploring 
# the influence of climate on the apparition, generalization and disappearance of 
# the Rayssian during the Middle and Recent Gravettian in France (32‒26.5 ka calBP)"
#
# Oral communication at the INQUA conference 2023, by AVignoles, WEBanks and LKlaric.
# 
#
# ##################
# #ELLIPSOID MODELS#
# ##################
# In this script, we will be creating and comparing ellipsoid models with the package
# ellipsenm. 
# Content:
# 1 - Required packages and functions
# 2 - Loading required data
# 3 - Modeling 
# 
#
# R version: 4.2.2 (2022-10-31 ucrt) 
# Author: Anaïs L. Vignoles. 
# Last modified: 5/05/2023. 


##########################################
# 1 - Required packages and functions ####
if(!require(devtools)) {
  install.packages("devtools")
}
if(!require(ellipsenm)){
  devtools::install_github("marlonecobos/ellipsenm")
}

library(ellipsenm)
#library(terra)
library(raster) ## NB: using raster bc ellipsenm was not updated to terra yet 
library(rgl)



################################
# 2 - Loading required data ####
# Occurrence data
pyr.noail <- read.csv("data-preparation/outputs/occ/pyr_noail_thin_30km.csv")[,2:3]
pyr.noail$Species <- "pyr_noail"

north.MG <- read.csv("data-preparation/outputs/occ/north_MG_thin_30km.csv")[,2:3]
north.MG$Species <- "north_MG"

rays <- read.csv("data-preparation/outputs/occ/rays_thin_30km.csv")[,2:3]
rays$Species <- "rays"

noail <- read.csv("data-preparation/outputs/occ/noail_thin_30km.csv")[,2:3]
noail$Species <- "noail"

GR <- read.csv("data-preparation/outputs/occ/GR_thin_30km.csv")[,2:3]
GR$Species <- "GR"


# Environmental data
## Function to automatize loading of rasters 
stack_PCA <- function(dir) {
  pc1 <- raster(paste0(dir, "/PC1.asc"))
  pc2 <- raster(paste0(dir, "/PC2.asc"))
  pc3 <- raster(paste0(dir, "/PC3.asc"))
  
  s <- stack(pc1, pc2, pc3)
  names(s) <- c("CP1", "CP2", "CP3")
  
  return(s)
}

#
## Loop to retrieve directory names 
dir.name <- paste0("data-preparation/outputs/env/PCAs_scenario") 

ref.env.pyr.noail.trans30 <- stack_PCA(paste0(dir.name, "1/compa1/ref_30ky_pyr_noail_trans30")) 
env.pyr.noail.trans100 <- stack_PCA(paste0(dir.name, "1/compa1/proj_30ky_pyr_noail_trans100"))
env.pyr.noail.trans500 <- stack_PCA(paste0(dir.name, "1/compa1/proj_30ky_pyr_noail_trans500"))

ref.env.north.MG.trans30 <- stack_PCA(paste0(dir.name, "1/compa2/ref_30ky_north_MG_trans30"))
env.north.MG.trans100 <- stack_PCA(paste0(dir.name, "1/compa2/proj_30ky_north_MG_trans100"))
env.north.MG.trans500 <- stack_PCA(paste0(dir.name, "1/compa2/proj_30ky_north_MG_trans500"))

ref.env.GR.trans30 <- stack_PCA(paste0(dir.name, "1/compa3/ref_28.5ky_GR_trans30"))
env.GR.trans100 <- stack_PCA(paste0(dir.name, "1/compa3/proj_28.5ky_GR_trans100"))
env.GR.trans500 <- stack_PCA(paste0(dir.name, "1/compa3/proj_28.5ky_GR_trans500"))

env.pyr.noail.eq <- stack_PCA(paste0(dir.name, "1/compa4/proj_30ky_pyr_noail_eq"))

ref.env.north.MG.trans30 <- stack_PCA(paste0(dir.name, "1/compa5/ref_30ky_north_MG_trans30")) 
env.north.MG.eq <- stack_PCA(paste0(dir.name, "1/compa5/proj_30ky_north_MG_eq"))

env.north.MG.trans30 <- stack_PCA(paste0(dir.name, "1/compa6/proj_30ky_north_MG_trans30"))

env.pyr.noail.30ky <- stack_PCA(paste0(dir.name, "1/compa8/proj_30ky_pyr_noail_trans"))  
env.north.MG.30ky <- stack_PCA(paste0(dir.name, "1/compa9/proj_30ky_north_MG_trans"))



###################
# 3 - Modeling ####
# Creating directories to store results 
dir.create("ellipsoid-models/Scenario1")
dir.create("ellipsoid-models/Scenario2")

for(i in 1:9) {
  dir.create(paste0("ellipsoid-models/Scenario1/compa_", i))
  dir.create(paste0("ellipsoid-models/Scenario2/compa_", i))
}


# Scenario 1 
source("scripts/2.1-scenario1.R")


# Scenario 2 
source("scripts/2.1-scenario2.R")
