# Project: "Birth, life and death of a lithic technological tradition: exploring 
# the influence of climate on the apparition, generalization and disappearance of 
# the Rayssian during the Middle and Recent Gravettian in France (32‒26.5 ka calBP)"
#
# Oral communication at the INQUA conference 2023, by AVignoles, WEBanks and LKlaric.
# 
#
# ##################
# #Data preparation#
# ##################
# In this script, we will be preparing the occurrence and environmental data necessary 
# for the niche analyses contained in this paper. 
# Content: 
# 1 - Required packages and functions
# 2 - Occurrence data
# 3 - Environmental data
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
if(!require(ntbox)){
  devtools::install_github("luismurao/ntbox")
}

library(tidyverse)
library(spThin)
library(maps)
library(terra)
library(ntbox)
library(ggpubr)
library(ggfortify)
library(raster) ## NB: using stack function because ntbox works with raster 

source("scripts/functions/auto_crop.R")
source("scripts/functions/plot_PC.R")
source("scripts/functions/plot_variance.R")


##########################
# 2 - Occurrence data ####
dir.create("data-preparation/outputs/occ")

# Importing sites data 
#
## Noaillian
noaillian.sites <- as_tibble(read.csv("data-preparation/data/occ_noaillian_raw.csv", 
                                      sep = ";", header = TRUE, dec = ",", encoding = "UTF-8"))
noaillian.sites

#
## Rayssian
rayssian.sites <- as_tibble(read.csv("data-preparation/data/occ_rayssian_raw.csv", 
                                     sep = ";", header = TRUE, dec = ",", encoding = "UTF-8"))
rayssian.sites

#
## Recent Gravettian
recent.sites <- as_tibble(read.csv("data-preparation/data/occ_recent_raw.csv", 
                                   sep = ";", header = TRUE, dec = ",", encoding = "UTF-8"))
recent.sites


# Creating occurrence data sets
kept.col <- c("Species", "longitude", "latitude")

#
## Middle Gravettian
### Pyrenees Noaillian
kept.noaillian.sites <- subset(noaillian.sites, valeur == "oui")
sites.pyr.noail <- subset(kept.noaillian.sites, département %in% c("Hautes-Pyrénées",
                                                                    "Haute-Garonne",
                                                                    "Pyrénées-Atlantiques",
                                                                    "Landes",
                                                                    "Ariège") |
                            pays == "Espagne")
sites.pyr.noail$Species <- "pyr_noail"

map("france")
points(x = sites.pyr.noail$longitude, y = sites.pyr.noail$latitude, pch = 19)

save.pyr.noail <- subset(sites.pyr.noail, select = kept.col)
write_csv(save.pyr.noail, "data-preparation/outputs/occ/pyr_noail_full.csv")

##
### Middle Gravettian north of the Garonne river valley
sites.north.noail <- kept.noaillian.sites %>% dplyr::anti_join(sites.pyr.noail, by ="département")
sites.north.noail <- subset(sites.north.noail, pays == "France")
sites.north.noail <- subset(sites.north.noail, !département %in% c("Var",
                                                                 "Ardèche"))
kept.rayssian.sites <- subset(rayssian.sites, valeur == "oui")

sites.north.MG <- sites.north.noail %>% dplyr::full_join(kept.rayssian.sites)
sites.north.MG$Species <- "north_MG"

map("france")
points(x = sites.north.MG$longitude, y = sites.north.MG$latitude, pch = 19)

save.north.MG <- subset(sites.north.MG, select = kept.col)
write.csv(save.north.MG, "data-preparation/outputs/occ/north_MG_full.csv")

##
### Rayssien
sites.rays <- kept.rayssian.sites
sites.rays$Species <- "Rays"

map("france")
points(x = sites.rays$longitude, y = sites.rays$latitude, pch = 19)

save.rays <- subset(sites.rays, select = kept.col)
write.csv(save.rays, "data-preparation/outputs/occ/Rays_full.csv")

##
### Noaillien
sites.noail <- sites.north.noail %>% full_join(sites.pyr.noail)
sites.noail$Species <- "Noail"

map("france")
points(x = sites.noail$longitude, y = sites.noail$latitude, pch = 19)

save.noail <- subset(sites.noail, select = kept.col)
write.csv(save.noail, "data-preparation/outputs/occ/Noail_full.csv")

##
## Recent Gravettian
kept.sites.recent <- subset(recent.sites, valeur == "oui")
kept.sites.recent$Species <- "GR"

map("france")
points(x = kept.sites.recent$longitude, y = kept.sites.recent$latitude, pch = 19)

save.GR <- subset(kept.sites.recent, select = kept.col)
write.csv(save.GR, "data-preparation/outputs/occ/GR_full.csv")


# Spatial thinning
env.res <- 16.6512 

#
## Twice the env. layers resolution (ca. 30 km)
pyr.noail.thin.30km <- spThin::thin(save.pyr.noail, "latitude", "longitude", "Species", thin.par = env.res * 2, reps = 1,
                            locs.thinned.list.return = TRUE, write.files = FALSE, out.dir = "outputs/occ")

north.MG.thin.30km <- spThin::thin(save.north.MG, "latitude", "longitude", "Species", thin.par = env.res * 2, reps = 1,
                           locs.thinned.list.return = TRUE, write.files = FALSE, out.dir = "outputs/occ")

rays.thin.30km <- spThin::thin(save.rays, "latitude", "longitude", "Species", thin.par = env.res * 2, reps = 1,
                           locs.thinned.list.return = TRUE, write.files = FALSE, out.dir = "outputs/occ")

noail.thin.30km <- spThin::thin(save.noail, "latitude", "longitude", "Species", thin.par = env.res * 2, reps = 1,
                            locs.thinned.list.return = TRUE, write.files = FALSE, out.dir = "outputs/occ")

GR.thin.30km <- spThin::thin(save.GR, "latitude", "longitude", "Species", thin.par = env.res * 2, reps = 1,
                     locs.thinned.list.return = TRUE, write.files = FALSE, out.dir = "outputs/occ")

#
## Saving outputs
write.csv(pyr.noail.thin.30km, "data-preparation/outputs/occ/pyr_noail_thin_30km.csv")
write.csv(north.MG.thin.30km, "data-preparation/outputs/occ/north_MG_thin_30km.csv")
write.csv(rays.thin.30km, "data-preparation/outputs/occ/rays_thin_30km.csv")
write.csv(noail.thin.30km, "data-preparation/outputs/occ/noail_thin_30km.csv")
write.csv(GR.thin.30km, "data-preparation/outputs/occ/GR_thin_30km.csv")



#############################
# 3 - Environmental data ####
dir.create("data-preparation/outputs/env")


# Extracting and downscaling climatic data 
## Equilibrium simulations from HadCM3 model (Beyer et al. 2020) 
###lien vers le script###

#
## Pseudo-transient simulations from HadCM3b model (Armstrong et al. 2019)
### lien vers le script 30 years###
### lien vers le script 100 years###
### lien vers le script 500 years###


# M calibration area
## Defining M calibration area 
### Construction of M mask area
mask.r <- terra::rast(vals = 0,
                         xmin = -10, 
                         xmax = 20, 
                         ymin = 35, 
                         ymax = 55,
                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                         resolution = c(0.166512, 0.166512))

##
### Loading coastlines (-90m) and LGM ice 
coast <- terra::vect("data-preparation/data/contour_poly.shp")
coast <- terra::crop(coast, mask.r)
terra::crs(coast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
coast.r <- terra::rasterize(coast, mask.r)
coast.r[coast.r == 1] <- 0
coast.r[is.na(coast.r)] <- 1
coast.r[coast.r == 0] <- NA

ice <- terra::vect("data-preparation/data/CalottesLGM.shp")
ice <- terra::crop(ice, mask.r)
terra::crs(ice) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
ice.r <- terra::rasterize(ice, mask.r)
ice.r[ice.r == 1] <- 0
ice.r[is.na(ice.r)] <- 1
ice.r[ice.r == 0] <- NA

##
### Calculating circles around occurrences -> max raw material circulation distance
#### Pyrenees Noaillian = 220 km
circle.pyr.noail <- terra::buffer(vect(cbind(save.pyr.noail$longitude, save.pyr.noail$latitude), 
                                  crs = "+proj=longlat"), 220000)
circle.pyr.noail.r <- terra::rasterize(circle.pyr.noail, mask.r)

###
#### Northern Middle Gravettian = 220 km
circle.north.MG <- terra::buffer(vect(cbind(save.north.MG$longitude, save.north.MG$latitude), 
                                      crs = "+proj=longlat"), 220000)
circle.north.MG.r <- terra::rasterize(circle.north.MG, mask.r)

###
#### Noaillian = 220 km
circle.noail <- terra::buffer(vect(cbind(save.noail$longitude, save.noail$latitude), 
                                   crs = "+proj=longlat"), 220000)
circle.noail.r <- terra::rasterize(circle.noail, mask.r)

###
#### Rayssian = 220 km
circle.rays <- terra::buffer(vect(cbind(save.rays$longitude, save.rays$latitude), 
                                  crs = "+proj=longlat"), 220000)
circle.rays.r <- terra::rasterize(circle.rays, mask.r)

###
#### Recent Gravettian = 270 km
circle.GR <- terra::buffer(vect(cbind(save.GR$longitude, save.GR$latitude), 
                                crs = "+proj=longlat"), 220000)
circle.GR.r <- terra::rasterize(circle.GR, mask.r)

##
### Combining masks
mask.geom <-  terra::mask(coast.r, ice.r)

mask.pyr.noail <- terra::mask(mask.geom, circle.pyr.noail.r)
mask.north.MG <- terra::mask(mask.geom, circle.north.MG.r)
mask.noail <- terra::mask(mask.geom, circle.noail.r)
mask.rays <- terra::mask(mask.geom, circle.rays.r)
mask.GR <- terra::mask(mask.geom, circle.GR.r)

all.masks <- c(mask.pyr.noail, mask.north.MG, mask.noail, mask.rays, mask.GR)

#
## Cropping climatic variables with M 
### Getting directory names
env.dir.names <- c("HadCM3b-trans/sensi_30ans", "HadCM3b-trans/sensi_100ans", "HadCM3b-trans/sensi_500ans") 
tp.names <- c("28.5ka", "30ka")
dir.name2 <- NULL

for(i in 1:3){
  dir.name1 <- paste0("data-preparation/outputs/env/", env.dir.names[i])
  for(j in 1:2){
    dir.name2 <- c(dir.name2, (paste0(dir.name1, "/", tp.names[j], "/Eu_clipped/GAM_ds")))
  }
}

dir.name3 <- c("data-preparation/outputs/env/HadCM3-eq/30ka/Eu_clipped/GAM_ds", dir.name2)
 
##
### writing masked layers 
mask.names <- c("M_pyr_noail", "M_north_MG", "M_noail", "M_rays", "M_GR")

for(i in 1:5) {
  for(j in 1:length(dir.name3)) {
    auto_crop(mask = all.masks[[i]], mask.name = mask.names[i], raster.dir = dir.name3[j])
  }
}


# Environmental PCAs 
## loading env files
names.env <- c("ctemp", "dprec", "mprec", "mtemp", "wprec", "wtemp")

##
### Equilibre
env.pyr.noail.eq.30k <- stack(list.files(paste0(dir.name3[1], "/M_pyr_noail_clipped"), full.names = TRUE))
names(env.pyr.noail.eq.30k) <- names.env 

env.north.MG.eq.30k <- stack(list.files((paste0(dir.name3[1], "/M_north_MG_clipped")), full.names = TRUE))
names(env.north.MG.eq.30k) <- names.env

env.noail.eq.30k <- stack(list.files(paste0(dir.name3[1], "/M_noail_clipped"), full.names = TRUE))
names(env.noail.eq.30k) <- names.env

env.rays.eq.30k <- stack(list.files((paste0(dir.name3[1], "/M_rays_clipped")), full.names = TRUE))
names(env.rays.eq.30k) <- names.env

##
### Transient
#### 30 ans
env.pyr.noail.trans.30a.30k <- stack(list.files((paste0(dir.name3[3], "/M_pyr_noail_clipped")), full.names = TRUE))
names(env.pyr.noail.trans.30a.30k) <- names.env

env.north.MG.trans.30a.30k <- stack(list.files((paste0(dir.name3[3], "/M_north_MG_clipped")), full.names = TRUE))
names(env.north.MG.trans.30a.30k) <- names.env

env.noail.trans.30a.30k <- stack(list.files((paste0(dir.name3[3], "/M_noail_clipped")), full.names = TRUE))
names(env.noail.trans.30a.30k) <- names.env

env.rays.trans.30a.30k <- stack(list.files((paste0(dir.name3[3], "/M_rays_clipped")), full.names = TRUE))
names(env.rays.trans.30a.30k) <- names.env

env.GR.trans.30a.28.5k <- stack(list.files((paste0(dir.name3[2], "/M_GR_clipped")), full.names = TRUE))
names(env.GR.trans.30a.28.5k) <- names.env

###
#### 100 ans
env.pyr.noail.trans.100a.30k <- stack(list.files((paste0(dir.name3[5], "/M_pyr_noail_clipped")), full.names = TRUE))
names(env.pyr.noail.trans.100a.30k) <- names.env

env.north.MG.trans.100a.30k <- stack(list.files((paste0(dir.name3[5], "/M_north_MG_clipped")), full.names = TRUE))
names(env.north.MG.trans.100a.30k) <- names.env

env.noail.trans.100a.30k <- stack(list.files((paste0(dir.name3[5], "/M_noail_clipped")), full.names = TRUE))
names(env.noail.trans.100a.30k) <- names.env

env.rays.trans.100a.30k <- stack(list.files((paste0(dir.name3[5], "/M_rays_clipped")), full.names = TRUE))
names(env.rays.trans.100a.30k) <- names.env

env.GR.trans.100a.28.5k <- stack(list.files((paste0(dir.name3[4], "/M_GR_clipped")), full.names = TRUE))
names(env.GR.trans.100a.28.5k) <- names.env

###
#### 500 ans
env.pyr.noail.trans.500a.30k <- stack(list.files((paste0(dir.name3[7], "/M_pyr_noail_clipped")), full.names = TRUE))
names(env.pyr.noail.trans.500a.30k) <- names.env

env.north.MG.trans.500a.30k <- stack(list.files((paste0(dir.name3[7], "/M_north_MG_clipped")),  full.names = TRUE))
names(env.north.MG.trans.500a.30k) <- names.env

env.noail.trans.500a.30k <- stack(list.files((paste0(dir.name3[7], "/M_noail_clipped")), full.names = TRUE))
names(env.noail.trans.500a.30k) <- names.env

env.rays.trans.500a.30k <- stack(list.files((paste0(dir.name3[7], "/M_rays_clipped")),  full.names = TRUE))
names(env.rays.trans.500a.30k) <- names.env

env.GR.trans.500a.28.5k <- stack(list.files((paste0(dir.name3[6], "/M_GR_clipped")), full.names = TRUE))
names(env.GR.trans.500a.28.5k) <- names.env

#
## PCA
### Scenario 1: north MG vs pyr noail vs GR
dir.create("data-preparation/outputs/env/PCAs_scenario1")

for(i in 1:9) {
  dir.create(paste0("data-preparation/outputs/env/PCAs_scenario1/compa", i))
}

###
#### compa 1 : pyr noail transient 30 ans vs 100/500 ans
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa1/ref_30ky_pyr_noail_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa1/proj_30ky_pyr_noail_trans100")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa1/proj_30ky_pyr_noail_trans500")

compa1.1 <- spca(layers_stack = env.pyr.noail.trans.30a.30k, layers_to_proj = env.pyr.noail.trans.100a.30k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa1/ref_30ky_pyr_noail_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa1/proj_30ky_pyr_noail_trans100")
c1.1 <- readRDS("data-preparation/outputs/env/PCAs_scenario1/compa1/ref_30ky_pyr_noail_trans30/pca_object23_05_05_15_23.rds")

tiff("data-preparation/outputs/env/PCAs_scenario1/ACP-compa1-4-6_variance.tiff")
plot_variance(c1.1, "ACP -- pyr noail - HadCM3b transitoires moyennées sur 30 ans")
dev.off()

c1.1_PC <- plot_PC(c1.1, "ACP -- pyr noail - HadCM3b transitoires moyennées sur 30 ans")
ggsave("data-preparation/outputs/env/PCAs_scenario1/ACP-compa1-4-6_plot.tiff", c1.1_PC, width = 10, height = 5)

compa1.2 <- spca(layers_stack = env.pyr.noail.trans.30a.30k, layers_to_proj = env.pyr.noail.trans.500a.30k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa1/ref_30ky_pyr_noail_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa1/proj_30ky_pyr_noail_trans500")

###
#### compa 2 : north MG transient 30 ans vs 100/500 ans
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa2/ref_30ky_north_MG_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa2/proj_30ky_north_MG_trans100")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa2/proj_30ky_north_MG_trans500")

compa2.1 <- spca(layers_stack = env.north.MG.trans.30a.30k, layers_to_proj = env.north.MG.trans.100a.30k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa2/ref_30ky_north_MG_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa2/proj_30ky_north_MG_trans100")
c2.1 <- readRDS("data-preparation/outputs/env/PCAs_scenario1/compa2/ref_30ky_north_MG_trans30/pca_object23_05_05_15_27.rds")

tiff("data-preparation/outputs/env/PCAs_scenario1/ACP-compa2-5_variance.tiff")
plot_variance(c2.1, "ACP -- north MG - HadCM3b transitoires moyennées sur 30 ans")
dev.off()

c2.1_PC <- plot_PC(c2.1, "ACP -- north MG - HadCM3b transitoires moyennées sur 30 ans")
ggsave("data-preparation/outputs/env/PCAs_scenario1/ACP-compa2-5_plot.tiff", c2.1_PC, width = 10, height = 5)

compa2.2 <- spca(layers_stack = env.north.MG.trans.30a.30k, layers_to_proj = env.north.MG.trans.500a.30k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa2/ref_30ky_north_MG_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa2/proj_30ky_north_MG_trans500")

###
#### compa 3 : pyr noail transient 30 ans vs equilibrium
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa4/ref_30ky_pyr_noail_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa4/proj_30ky_pyr_noail_eq")

compa4 <- spca(layers_stack = env.pyr.noail.trans.30a.30k, layers_to_proj = env.pyr.noail.eq.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa4/ref_30ky_pyr_noail_trans30", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa4/proj_30ky_pyr_noail_eq")

###
#### compa 4 : north_MG transient 30 ans vs equilibrium
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa5/ref_30ky_north_MG_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa5/proj_30ky_north_MG_eq")

compa5 <- spca(layers_stack = env.north.MG.trans.30a.30k, layers_to_proj = env.north.MG.eq.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa5/ref_30ky_north_MG_trans30", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa5/proj_30ky_north_MG_eq")

###
#### compa 5 : pyr noail transient 30 ans vs north_MG transient 30 ans
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa6/ref_30ky_pyr_noail_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa6/proj_30ky_north_MG_trans30")

compa6 <- spca(layers_stack = env.pyr.noail.trans.30a.30k, layers_to_proj = env.north.MG.trans.30a.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa6/ref_30ky_pyr_noail_trans30", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa6/proj_30ky_north_MG_trans30")

###
#### compa 7 : pyr noail equilibrium vs north_MG equilibrium
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa7/ref_30ky_pyr_noail_eq")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa7/proj_30ky_north_MG_eq")

compa7 <- spca(layers_stack = env.pyr.noail.eq.30k, layers_to_proj = env.north.MG.eq.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa7/ref_30ky_pyr_noail_eq", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa7/proj_30ky_north_MG_eq")
c7 <- readRDS("data-preparation/outputs/env/PCAs_scenario1/compa7/ref_30ky_pyr_noail_eq/pca_object23_05_05_15_29.rds")

tiff("data-preparation/outputs/env/PCAs_scenario1/ACP-compa7_variance.tiff")
plot_variance(c7, "ACP -- pyr_noail - HadCM3 à l'équilibre")
dev.off()

c7_PC <- plot_PC(c7, "ACP -- pyr_noail - HadCM3 à l'équilibre")
ggsave("data-preparation/outputs/env/PCAs_scenario1/ACP-compa7_plot.tiff", c7_PC, width = 10, height = 5)

###
#### compa 3 : GR transient 30 ans vs 100/500 ans
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa3/ref_28.5ky_GR_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa3/proj_28.5ky_GR_trans100")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa3/proj_28.5ky_GR_trans500")

compa3.1 <- spca(layers_stack = env.GR.trans.30a.28.5k, layers_to_proj = env.GR.trans.100a.28.5k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa3/ref_28.5ky_GR_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa3/proj_28.5ky_GR_trans100")
c3.1 <- readRDS("data-preparation/outputs/env/PCAs_scenario1/compa3/ref_28.5ky_GR_trans30/pca_object23_05_05_15_29.rds")

tiff("data-preparation/outputs/env/PCAs_scenario1/ACP-compa3-8-9_variance.tiff")
plot_variance(c3.1, "ACP -- GR - HadCM3b transitoires moyennées sur 30 ans")
dev.off()

c3.1_PC <- plot_PC(c3.1, "ACP -- GR - HadCM3b transitoires moyennées sur 30 ans")
ggsave("data-preparation/outputs/env/PCAs_scenario1/ACP-compa3-8-9_plot.tiff", c3.1_PC, width = 10, height = 5)

compa3.2 <- spca(layers_stack = env.GR.trans.30a.28.5k, layers_to_proj = env.GR.trans.500a.28.5k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa3/ref_28.5ky_GR_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa3/proj_28.5ky_GR_trans500")

### 
#### compa 8 : GR transient 30 ans vs pyr noail transient 30 ans
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa8/ref_28.5ky_GR_trans")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa8/proj_30ky_pyr_noail_trans")

compa8 <- spca(layers_stack = env.GR.trans.30a.28.5k, layers_to_proj = env.pyr.noail.trans.30a.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa8/ref_28.5ky_GR_trans", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa8/proj_30ky_pyr_noail_trans")

### 
#### compa 9 : GR transient 30 ans vs north MG transient 30 ans
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa9/ref_28.5ky_GR_trans")
dir.create("data-preparation/outputs/env/PCAs_scenario1/compa9/proj_30ky_north_MG_trans")

compa9 <- spca(layers_stack = env.GR.trans.30a.28.5k, layers_to_proj = env.north.MG.trans.30a.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario1/compa9/ref_28.5ky_GR_trans", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario1/compa9/proj_30ky_north_MG_trans")

##
### Scenario 2 : Noail vs Rays vs GR 
dir.create("data-preparation/outputs/env/PCAs_scenario2")
for(i in 1:9) {
  dir.create(paste0("data-preparation/outputs/env/PCAs_scenario2/compa", i))
}

###
#### compa 1 : Noail transient 30 ans vs 100/500 ans
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa1/ref_30ky_noail_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa1/proj_30ky_noail_trans100")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa1/proj_30ky_noail_trans500")

compa1.1 <- spca(layers_stack = env.noail.trans.30a.30k, layers_to_proj = env.noail.trans.100a.30k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa1/ref_30ky_Noail_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa1/proj_30ky_Noail_trans100")
c1.1 <- readRDS("data-preparation/outputs/env/PCAs_scenario2/compa1/ref_30ky_Noail_trans30/pca_object23_05_05_15_31.rds")

tiff("data-preparation/outputs/env/PCAs_scenario2/ACP-compa1-4-6_variance.tiff")
plot_variance(c1.1, "ACP -- Noail - HadCM3b transitoires moyennées sur 30 ans")
dev.off()

c1.1_PC <- plot_PC(c1.1, "ACP -- Noail - HadCM3b transitoires moyennées sur 30 ans")
ggsave("data-preparation/outputs/env/PCAs_scenario2/ACP-compa1-4-6_plot.tiff", c1.1_PC, width = 10, height = 5)

compa1.2 <- spca(layers_stack = env.noail.trans.30a.30k, layers_to_proj = env.noail.trans.500a.30k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa1/ref_30ky_Noail_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa1/proj_30ky_Noail_trans500")

###
#### compa 2 : Rays transient 30 ans vs 100/500 ans
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa2/ref_30ky_Rays_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa2/proj_30ky_Rays_trans100")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa2/proj_30ky_Rays_trans500")

compa2.1 <- spca(layers_stack = env.rays.trans.30a.30k, layers_to_proj = env.rays.trans.100a.30k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa2/ref_30ky_Rays_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa2/proj_30ky_Rays_trans100")
c2.1 <- readRDS("data-preparation/outputs/env/PCAs_scenario2/compa2/ref_30ky_Rays_trans30/pca_object23_05_05_15_32.rds")

tiff("data-preparation/outputs/env/PCAs_scenario2/ACP-compa2-5_variance.tiff")
plot_variance(c2.1, "ACP -- Rays - HadCM3b transitoires moyennées sur 30 ans")
dev.off()

c2.1_PC <- plot_PC(c2.1, "ACP -- Rays - HadCM3b transitoires moyennées sur 30 ans")
ggsave("data-preparation/outputs/env/PCAs_scenario2/ACP-compa2-5_plot.tiff", c2.1_PC, width = 10, height = 5)

compa2.2 <- spca(layers_stack = env.rays.trans.30a.30k, layers_to_proj = env.rays.trans.500a.30k,
                 sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa2/ref_30ky_Rays_trans30", layers_format = ".asc",
                 sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa2/proj_30ky_Rays_trans500")

###
#### compa 4 : Noail transient 30 ans vs equilibrium
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa4/ref_30ky_Noail_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa4/proj_30ky_Noail_eq")

compa4 <- spca(layers_stack = env.noail.trans.30a.30k, layers_to_proj = env.noail.eq.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa4/ref_30ky_Noail_trans30", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa4/proj_30ky_Noail_eq")

###
#### compa 5 : Rays transient 30 ans vs equilibrium
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa5/ref_30ky_Rays_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa5/proj_30ky_Rays_eq")

compa5 <- spca(layers_stack = env.rays.trans.30a.30k, layers_to_proj = env.rays.eq.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa5/ref_30ky_Rays_trans30", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa5/proj_30ky_Rays_eq")

###
#### compa 6 : Noail transient 30 ans vs Rays transient 30 ans
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa6/ref_30ky_Noail_trans30")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa6/proj_30ky_Rays_trans30")

compa6 <- spca(layers_stack = env.noail.trans.30a.30k, layers_to_proj = env.rays.trans.30a.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa6/ref_30ky_Noail_trans30", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa6/proj_30ky_Rays_trans30")

###
#### compa 7 : Noail equilibrium vs Rays equilibrium
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa7/ref_30ky_Noail_eq")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa7/proj_30ky_Rays_eq")

compa7 <- spca(layers_stack = env.noail.eq.30k, layers_to_proj = env.rays.eq.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa7/ref_30ky_Noail_eq", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa7/proj_30ky_Rays_eq")
c7 <- readRDS("data-preparation/outputs/env/PCAs_scenario2/compa7/ref_30ky_Noail_eq/pca_object23_05_05_15_38.rds")

tiff("data-preparation/outputs/env/PCAs_scenario2/ACP-compa7_variance.tiff")
plot_variance(c7, "ACP -- Noail - HadCM3 à l'équilibre")
dev.off()

c7_PC <- plot_PC(c7, "ACP -- Noail - HadCM3 à l'équilibre")
ggsave("data-preparation/outputs/env/PCAs_scenario2/ACP-compa7_plot.tiff", c7_PC, width = 10, height = 5)

###
####compa 8 : GR transient 30 ans vs Noail transient 30 ans
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa8/ref_28.5ky_GR_trans")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa8/proj_30ky_Noail_trans")

compa8 <- spca(layers_stack = env.GR.trans.30a.28.5k, layers_to_proj = env.noail.trans.30a.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa8/ref_28.5ky_GR_trans", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa8/proj_30ky_Noail_trans")

###
#### compa 9 : GR transient 30 ans vs Rays transient 30 ans
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa9/ref_28.5ky_GR_trans")
dir.create("data-preparation/outputs/env/PCAs_scenario2/compa9/proj_30ky_Rays_trans")

compa9 <- spca(layers_stack = env.GR.trans.30a.28.5k, layers_to_proj = env.rays.trans.30a.30k,
               sv_dir = "data-preparation/outputs/env/PCAs_scenario2/compa9/ref_28.5ky_GR_trans", layers_format = ".asc",
               sv_proj_dir = "data-preparation/outputs/env/PCAs_scenario2/compa9/proj_30ky_Rays_trans")

