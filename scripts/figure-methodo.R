# figure méthodo 

library(terra)

# get data 
occ.gr <- read.csv("data-preparation/outputs/occ/GR_full.csv")[,3:4]
occ.r <- read.csv("data-preparation/outputs/occ/rays_full.csv")[,3:4]

mask.gr <- terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/28.5ka/Eu_clipped/GAM_ds/M_GR_clipped/ctemp_M_GR_clipped.tiff")
mask.r <- terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/30ka/Eu_clipped/GAM_ds/M_rays_clipped/ctemp_M_rays_clipped.tiff")

all.gr <- c(mask.gr,
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/28.5ka/Eu_clipped/GAM_ds/M_GR_clipped/wtemp_M_GR_clipped.tiff"),
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/28.5ka/Eu_clipped/GAM_ds/M_GR_clipped/mtemp_M_GR_clipped.tiff"),
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/28.5ka/Eu_clipped/GAM_ds/M_GR_clipped/dprec_M_GR_clipped.tiff"),
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/28.5ka/Eu_clipped/GAM_ds/M_GR_clipped/wprec_M_GR_clipped.tiff"),
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/28.5ka/Eu_clipped/GAM_ds/M_GR_clipped/mprec_M_GR_clipped.tiff"))

all.r <- c(mask.r,
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/30ka/Eu_clipped/GAM_ds/M_rays_clipped/wtemp_M_rays_clipped.tiff"),
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/30ka/Eu_clipped/GAM_ds/M_rays_clipped/mtemp_M_rays_clipped.tiff"),
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/30ka/Eu_clipped/GAM_ds/M_rays_clipped/dprec_M_rays_clipped.tiff"),
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/30ka/Eu_clipped/GAM_ds/M_rays_clipped/wprec_M_rays_clipped.tiff"),
            terra::rast("data-preparation/outputs/env/HadCM3b-trans/sensi_500ans/30ka/Eu_clipped/GAM_ds/M_rays_clipped/mprec_M_rays_clipped.tiff"))

names(all.gr) <- c("ctemp", "wtemp", "mtemp", "dprec", "wprec", "mprec")
names(all.r) <- c("ctemp", "wtemp", "mtemp", "dprec", "wprec", "mprec")

PC.gr <- c(terra::rast("data-preparation/outputs/env/PCAs_scenario2/compa9/ref_28.5ky_GR_trans/PC1.asc"),
          terra::rast("data-preparation/outputs/env/PCAs_scenario2/compa9/ref_28.5ky_GR_trans/PC2.asc"),
          terra::rast("data-preparation/outputs/env/PCAs_scenario2/compa9/ref_28.5ky_GR_trans/PC3.asc"))

PC.r <- c(terra::rast("data-preparation/outputs/env/PCAs_scenario2/compa9/proj_30ky_Rays_trans/PC1.asc"),
           terra::rast("data-preparation/outputs/env/PCAs_scenario2/compa9/proj_30ky_Rays_trans/PC2.asc"),
           terra::rast("data-preparation/outputs/env/PCAs_scenario2/compa9/proj_30ky_Rays_trans/PC3.asc"))
           

topo <- crop(terra::rast("data-preparation/data/ETOPO1_Bed_c_geotiff.tif"), 
             terra::rast(vals = 0,
                      xmin = -10, 
                      xmax = 20, 
                      ymin = 35, 
                      ymax = 55,
                      crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                      resolution = c(0.166512, 0.166512)))

ocean.pal <- colorRampPalette(c("#000728","#002650","#00426E",
                                "#005E8C","#0096C8","#22A9C2",
                                "#45BCBB","#67CFB5",
                                "#BCF8B9","#DBFBDC"))
land.pal <- colorRampPalette(c("#EBFDED","#336600","#F3CA89","#D9A627","#A49019",
                               "#9F7B0D","#996600","#B27676","#C2B0B0",
                               "#E5E5E5","#FFFFFF"))
waterBreaks <- seq(-5234, 0, by=100)
landBreaks <- seq(0, 4239, by=100)
colours <- c(ocean.pal(length(waterBreaks)), land.pal(length(landBreaks)))

# occurrences GR + mask
plot(topo, col = colours, xlab = "longitude (°E)", ylab ="latitude (°N)")
points(occ.gr, pch = 21, bg = "black")

plot(mask.gr, col = "lightpink", add=T)
points(occ.gr, pch = 21, bg = "black")

plot(all.gr)
plot(PC.gr)

# occurrences rayssien + mask
plot(topo, col = colours, xlab = "longitude (°E)", ylab ="latitude (°N)")
points(occ.r, pch = 21, bg = "black")

plot(mask.r, col = "lightgreen", add=T)
points(occ.r, pch = 21, bg = "black")

plot(all.r)
plot(PC.r)

