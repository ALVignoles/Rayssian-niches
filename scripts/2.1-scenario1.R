# ellipsenm process
# from thesis scripts, unmodified (except for paths)
# Scenario 1


########################################
# Comparing mean transient climatologies

##########
## compa 1: trans30y vs trans100y vs 500y for pyr.noail
### overlap objects
pyr.noail.trans30.mve1 <- ellipsenm::overlap_object(pyr.noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                level = 95, variables = ref.env.pyr.noail.trans30)

pyr.noail.trans100.mve1 <- ellipsenm::overlap_object(pyr.noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                 level = 95, variables = env.pyr.noail.trans100)

pyr.noail.trans500.mve1 <- ellipsenm::overlap_object(pyr.noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                 level = 95, variables = env.pyr.noail.trans500)

### niche overlap analysis 
overlap.mve.pyr.noail.trans.1 <- ellipsenm::ellipsoid_overlap(pyr.noail.trans30.mve1, pyr.noail.trans100.mve1, overlap_type = "all",
                                                          significance_test = TRUE, replicates = 1000)

summary(overlap.mve.pyr.noail.trans.1) # saving data manually

overlap.mve.pyr.noail.trans.2 <- ellipsenm::ellipsoid_overlap(pyr.noail.trans30.mve1, pyr.noail.trans500.mve1, overlap_type = "all",
                                                          significance_test = TRUE, replicates = 1000)

summary(overlap.mve.pyr.noail.trans.2) # saving data manually

###
### plots 1
val.pyr.noail.trans30 <- na.omit(values(env.pyr.noail.trans30))
val.pyr.noail.trans100 <- na.omit(values(env.pyr.noail.trans100))
val.pyr.noail.trans500 <- na.omit(values(env.pyr.noail.trans500))

lims <- apply(rbind(val.pyr.noail.trans30, val.pyr.noail.trans100), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]


# *overlap 3d
cols <- c("#99FFFF", "#0066CC")
ellipsenm::plot_overlap(overlap.mve.pyr.noail.trans.1, niche_col = cols, data_col = cols, legend = F)
plot3d(val.pyr.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.trans100, col = "#006699", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(-3, 0, 3)
yat <- c(-4, 0, 4)
zat <- c(-1.5, 0, 1.5)

# *move the plot around and then save the view
## *View 1
# view3d1 <- par3d()$userMatrix
# saveRDS(view3d1, "ellipsoid-models/Scenario1/view3dov1.rds")
view3d1 <- readRDS("ellipsoid-models/Scenario1/view3dov1.rds")

## *View 2
# view3d2 <- par3d()$userMatrix
# saveRDS(view3d2, "ellipsoid-models/Scenario1/view3dov2.rds")
view3d2 <- readRDS("ellipsoid-models/Scenario1/view3dov2.rds")

## *View 3
# view3d3 <- par3d()$userMatrix
# saveRDS(view3d3, "ellipsoid-models/Scenario1/view3dov3.rds")
view3d3 <- readRDS("ellipsoid-models/Scenario1/view3dov3.rds")


# *save views
dir.create("ellipsoid-models/Scenario1/compa_1/1.1")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.pyr.noail.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.trans100, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_1/1.1/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_1/1.1/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.pyr.noail.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.trans100, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_1/1.1/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_1/1.1/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.pyr.noail.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.trans100, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_1/1.1/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_1/1.1/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}


# *significance test
png("ellipsoid-models/Scenario1/compa_1/1.1/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.pyr.noail.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 40), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.pyr.noail.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.pyr.noail.trans.1@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_1/1.1/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.pyr.noail.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 40), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.pyr.noail.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.pyr.noail.trans.1@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

###
### plots 2
lims <- apply(rbind(val.pyr.noail.trans30, val.pyr.noail.trans500), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

# *overlap 3d
cols <- c("#99FFFF", "#0066CC")
ellipsenm::plot_overlap(overlap.mve.pyr.noail.trans.2, niche_col = cols, data_col = cols, legend = F)
plot3d(val.pyr.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.trans500, col = "#006699", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

# *save the views
dir.create("ellipsoid-models/Scenario1/compa_1/1.2")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.pyr.noail.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.trans500, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_1/1.2/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_1/1.2/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.pyr.noail.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.trans500, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_1/1.2/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_1/1.2/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.pyr.noail.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.trans500, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_1/1.2/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_1/1.2/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_1/1.2/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.pyr.noail.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 40), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.pyr.noail.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.pyr.noail.trans.2@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_1/1.2/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.pyr.noail.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 40), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.pyr.noail.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.pyr.noail.trans.2@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()



###########
## compa 2: trans30y vs trans100y vs trans500y for north.MG
### overlap objects
north.MG.trans30.mve1 <- ellipsenm::overlap_object(north.MG, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                 level = 95, variables = env.north.MG.trans30)

north.MG.trans100.mve1 <- ellipsenm::overlap_object(north.MG, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                  level = 95, variables = env.north.MG.trans100)

north.MG.trans500.mve1 <- ellipsenm::overlap_object(north.MG, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                  level = 95, variables = env.north.MG.trans500)

### niche overlap analysis 
overlap.mve.north.MG.trans.1 <- ellipsenm::ellipsoid_overlap(north.MG.trans30.mve1, north.MG.trans100.mve1, overlap_type = "all",
                                                           significance_test = TRUE, replicates = 1000)

summary(overlap.mve.north.MG.trans.1) # saving data manually

overlap.mve.north.MG.trans.2 <- ellipsenm::ellipsoid_overlap(north.MG.trans30.mve1, north.MG.trans500.mve1, overlap_type = "all",
                                                           significance_test = TRUE, replicates = 1000)

summary(overlap.mve.north.MG.trans.2) # saving data manually

###
### plots 1
val.north.MG.trans30 <- na.omit(values(env.north.MG.trans30))
val.north.MG.trans100 <- na.omit(values(env.north.MG.trans100))
val.north.MG.trans500 <- na.omit(values(env.north.MG.trans500))

lims <- apply(rbind(val.north.MG.trans30, val.north.MG.trans100), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

# *overlap 3d
cols <- c("#99FF66", "#009E73")
ellipsenm::plot_overlap(overlap.mve.north.MG.trans.1, niche_col = cols, data_col = cols, legend = F)
plot3d(val.north.MG.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.trans100, col = "#006600", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c( 0, 4, 8)
yat <- c( -4, 0, 4)
zat <- c(-5, 0, 5)

# *save views
dir.create("ellipsoid-models/Scenario1/compa_2/2.1")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.north.MG.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.north.MG.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.trans100, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_2/2.1/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_2/2.1/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.north.MG.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.north.MG.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.trans100, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_2/2.1/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_2/2.1/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.north.MG.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.north.MG.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.trans100, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_2/2.1/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_2/2.1/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_2/2.1/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.north.MG.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 80), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.north.MG.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.north.MG.trans.1@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_2/2.1/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.north.MG.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 80), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.north.MG.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.north.MG.trans.1@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


###
### plots 2
lims <- apply(rbind(val.north.MG.trans30, val.north.MG.trans500), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]


# *overlap 3d
cols <- c("#99FF66", "#009E73")
ellipsenm::plot_overlap(overlap.mve.north.MG.trans.2, niche_col = cols, data_col = cols, legend = F)
plot3d(val.north.MG.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.trans500, col = "#006600", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))


# *save the views
dir.create("ellipsoid-models/Scenario1/compa_2/2.2")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.north.MG.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.north.MG.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.trans500, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_2/2.2/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_2/2.2/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.north.MG.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.north.MG.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.trans500, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_2/2.2/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_2/2.2/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.north.MG.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.north.MG.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.trans500, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_2/2.2/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_2/2.2/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_2/2.2/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.north.MG.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 80), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.north.MG.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.north.MG.trans.2@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_2/2.2/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.north.MG.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 80), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.north.MG.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.north.MG.trans.2@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


###########
## compa 3: trans30y vs trans100y vs trans500y for GR
### overlap objects
GR.trans30.mve1 <- ellipsenm::overlap_object(GR, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                             level = 95, variables = env.GR.trans30)

GR.trans100.mve1 <- ellipsenm::overlap_object(GR, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                              level = 95, variables = env.GR.trans100)

GR.trans500.mve1 <- ellipsenm::overlap_object(GR, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                              level = 95, variables = env.GR.trans500)

### niche overlap analysis 
overlap.mve.GR.trans.1 <- ellipsenm::ellipsoid_overlap(GR.trans30.mve1, GR.trans100.mve1, overlap_type = "all",
                                                       significance_test = TRUE, replicates = 1000)

summary(overlap.mve.GR.trans.1) # saving data manually

overlap.mve.GR.trans.2 <- ellipsenm::ellipsoid_overlap(GR.trans30.mve1, GR.trans500.mve1, overlap_type = "all",
                                                       significance_test = TRUE, replicates = 1000)

summary(overlap.mve.GR.trans.2) # saving data manually

###
### plots 1
val.GR.trans30 <- na.omit(values(env.GR.trans30))
val.GR.trans100 <- na.omit(values(env.GR.trans100))
val.GR.trans500 <- na.omit(values(env.GR.trans500))

lims <- apply(rbind(val.GR.trans30, val.GR.trans100), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

# *overlap 3d
cols <- c("#D55E00", "#CC0033")
ellipsenm::plot_overlap(overlap.mve.GR.trans.1, niche_col = cols, data_col = cols, legend = F)
plot3d(val.GR.trans30, col = "#FF9999", add = TRUE)
plot3d(val.GR.trans100, col = "#993333", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(8, 4, 0)
yat <- c( -4, 0, 4)
zat <- c(-4, 0, 4)

# *save views
dir.create("ellipsoid-models/Scenario1/compa_3/3.1")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.GR.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.trans30, col = "#FF9999", add = TRUE)
plot3d(val.GR.trans100, col = "#993333", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_3/3.1/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_3/3.1/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.GR.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.trans30, col = "#FF9999", add = TRUE)
plot3d(val.GR.trans100, col = "#993333", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_3/3.1/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_3/3.1/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.GR.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.trans30, col = "#FF9999", add = TRUE)
plot3d(val.GR.trans100, col = "#993333", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_3/3.1/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_3/3.1/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_3/3.1/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.trans.1@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_3/3.1/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.trans.1@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


###
### plots 2
lims <- apply(rbind(val.GR.trans30, val.GR.trans500), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]


# *overlap 3d
cols <- c("#D55E00", "#CC0033")
ellipsenm::plot_overlap(overlap.mve.GR.trans.2, niche_col = cols, data_col = cols, legend = F)
plot3d(val.GR.trans30, col = "#FF9999", add = TRUE)
plot3d(val.GR.trans500, col = "#993333", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))


# *save the views
dir.create("ellipsoid-models/Scenario1/compa_3/3.2")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.GR.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.trans30, col = "#FF9999", add = TRUE)
plot3d(val.GR.trans500, col = "#993333", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_3/3.2/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_3/3.2/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.GR.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.trans30, col = "#FF9999", add = TRUE)
plot3d(val.GR.trans500, col = "#993333", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_3/3.2/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_3/3.2/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.GR.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.trans30, col = "#FF9999", add = TRUE)
plot3d(val.GR.trans500, col = "#993333", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_3/3.2/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_3/3.2/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_3/3.2/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0,50), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.trans.2@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_3/3.2/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0,50), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.trans.2@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()



######################################################################
# Comparing climatic models (trans30y vs eq for GMnorth and Noail pyr)

###########
## compa 4 : noaillian pyr 
### overlap objects
pyr.noail.trans30.mve1 <- ellipsenm::overlap_object(pyr.noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                level = 95, variables = env.pyr.noail.trans30)

pyr.noail.eq.mve1 <- ellipsenm::overlap_object(pyr.noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                           level = 95, variables = env.pyr.noail.eq)

### niche overlap analysis 
overlap.mve.pyr.noail <- ellipsenm::ellipsoid_overlap(pyr.noail.trans30.mve1, pyr.noail.eq.mve1, overlap_type = "all",
                                                  significance_test = TRUE, replicates = 1000)

summary(overlap.mve.pyr.noail) # saving data manually

###
### plots
dir.create("ellipsoid-models/Scenario1/compa_4")

val.pyr.noail.trans <- na.omit(values(env.pyr.noail.trans30))
val.pyr.noail.eq <- na.omit(values(env.pyr.noail.eq))

lims <- apply(rbind(val.pyr.noail.trans, val.pyr.noail.eq), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

# *overlap 3d
cols <- c("#99FFFF", "#0066CC")
ellipsenm::plot_overlap(overlap.mve.pyr.noail, niche_col = cols, data_col = cols, legend = F)
plot3d(val.pyr.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.eq, col = "#006699", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(-5, 0, 5)
yat <- c(-5, 0, 5)
zat <- c(-10, -5, 0)


# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.pyr.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.eq, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_4/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_4/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.pyr.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.eq, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_4/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_4/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.pyr.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.pyr.noail.eq, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_4/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_4/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_4/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.pyr.noail@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.pyr.noail@significance_results$full_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.pyr.noail@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_4/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.pyr.noail@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 1000), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.pyr.noail@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.pyr.noail@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


###########
## compa 5 : north.MG trans30 vs eq
### overlap objects
north.MG.trans30.mve1 <- ellipsenm::overlap_object(north.MG, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                 level = 95, variables = env.north.MG.trans30)

north.MG.eq.mve1 <- ellipsenm::overlap_object(north.MG, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                            level = 95, variables = env.north.MG.eq)

### niche overlap analysis 
overlap.mve.north.MG <- ellipsenm::ellipsoid_overlap(north.MG.trans30.mve1, north.MG.eq.mve1, overlap_type = "all",
                                                   significance_test = TRUE, replicates = 1000)

summary(overlap.mve.north.MG) # saving data manually

###
### plots
dir.create("ellipsoid-models/Scenario1/compa_5")
val.north.MG.trans <- na.omit(values(env.north.MG.trans30))
val.north.MG.eq <- na.omit(values(env.north.MG.eq))

lims <- apply(rbind(val.north.MG.trans, val.north.MG.eq), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

#### overlap 3d
cols <- c("#99FF66", "#009E73")
ellipsenm::plot_overlap(overlap.mve.north.MG, niche_col = cols, data_col = cols, legend = F)
plot3d(val.north.MG.trans, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.eq, col = "#006600", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(0, 7, 14)
yat <- c(-5, 0, 5)
zat <- c(-5, 0, 5)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.north.MG, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.north.MG.trans, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_5/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_5/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.north.MG, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.north.MG.trans, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_5/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_5/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.north.MG, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.north.MG.trans, col = "#CCFF99", add = TRUE)
plot3d(val.north.MG.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_5/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_5/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_5/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.north.MG@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.north.MG@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.north.MG@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_5/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.north.MG@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 1000), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.north.MG@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.north.MG@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()



####################################################
# Comparing MG north with Noail pyr for eq and trans

##########
## compa 6: trans models 
### overlap objects
pyr.noail.trans30.mve1 <- ellipsenm::overlap_object(pyr.noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                level = 95, variables = env.pyr.noail.trans30)

north.MG.trans30.mve1 <- ellipsenm::overlap_object(north.MG, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                 level = 95, variables = env.north.MG.trans30)

### niche overlap analysis 
overlap.mve.trans30 <- ellipsenm::ellipsoid_overlap(pyr.noail.trans30.mve1, north.MG.trans30.mve1, overlap_type = "all",
                                                    significance_test = TRUE, replicates = 1000)

summary(overlap.mve.trans30) # saving data manually

###
### plots
dir.create("ellipsoid-models/Scenario1/compa_6")

val.pyr.noail.trans <- na.omit(values(env.pyr.noail.trans30))
val.north.MG.trans <- na.omit(values(env.north.MG.trans30))

lims <- apply(rbind(val.pyr.noail.trans, val.north.MG.trans), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

# *overlap 3d
cols <- c("#99FFFF", "#99FF66")
ellipsenm::plot_overlap(overlap.mve.trans30, niche_col = cols, data_col = cols, legend = F)
plot3d(val.pyr.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.north.MG.trans, col = "#CCFF99", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(-5, 0, 5)
yat <- c(-4, 0, 4)
zat <- c(-2, 0, 2)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.trans30, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.north.MG.trans, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_6/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_6/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.trans30, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.north.MG.trans, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_6/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_6/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.trans30, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.north.MG.trans, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_6/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_6/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_6/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.trans30@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 50), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.trans30@significance_results$full_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.trans30@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_6/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.trans30@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 50), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.trans30@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.trans30@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


##########
## compa 7 : eq models
### overlap objects
pyr.noail.eq.mve1 <- ellipsenm::overlap_object(pyr.noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                           level = 95, variables = env.pyr.noail.eq)

north.MG.eq.mve1 <- ellipsenm::overlap_object(north.MG, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                            level = 95, variables = env.north.MG.eq)

### niche overlap analysis 
overlap.mve.eq <- ellipsenm::ellipsoid_overlap(pyr.noail.eq.mve1, north.MG.eq.mve1, overlap_type = "all",
                                               significance_test = TRUE, replicates = 1000)
summary(overlap.mve.eq) # saving data manually


###
### plots
dir.create("ellipsoid-models/Scenario1/compa_7")

val.pyr.noail.eq <- na.omit(values(env.pyr.noail.eq))
val.north.MG.eq <- na.omit(values(env.north.MG.eq))

lims <- apply(rbind(val.pyr.noail.eq, val.north.MG.eq), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

#### overlap 3d
cols <- c("#0066CC", "#009E73")
ellipsenm::plot_overlap(overlap.mve.eq, niche_col = cols, data_col = cols, legend = F)
plot3d(val.pyr.noail.eq, col = "#006699", add = TRUE)
plot3d(val.north.MG.eq, col = "#006600", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(-5, 0, 5)
yat <- c(-2, 0, 2)
zat <- c(-2, 0, 2)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.eq, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.eq, col = "#006699", add = TRUE)
plot3d(val.north.MG.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_7/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_7/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.eq, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.eq, col = "#006699", add = TRUE)
plot3d(val.north.MG.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_7/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_7/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.eq, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.pyr.noail.eq, col = "#006699", add = TRUE)
plot3d(val.north.MG.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_7/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_7/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_7/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.eq@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 30), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.eq@significance_results$full_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.eq@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_7/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.eq@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 30), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.eq@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.eq@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


#################################################################
# Comparing GR with GMnorth and noail pyr (with trans simulations)

###########
## compa 8 : GR vs pyr.noail 
### overlap objects
GR.28.5ky.mve1 <- ellipsenm::overlap_object(GR, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                            level = 95, variables = env.GR.28.5ky)

pyr.noail.30ky.mve1 <- ellipsenm::overlap_object(pyr.noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                             level = 95, variables = env.pyr.noail.30ky)

### niche overlap analysis 
overlap.mve.GR.vs.pyr.noail <- ellipsenm::ellipsoid_overlap(GR.28.5ky.mve1, pyr.noail.30ky.mve1, overlap_type = "all",
                                                        significance_test = TRUE, replicates = 1000)

summary(overlap.mve.GR.vs.pyr.noail) # saving data manually

###
### plots
dir.create("ellipsoid-models/Scenario1/compa_8")

val.GR.28.5 <- na.omit(values(env.GR.28.5ky))
val.pyr.noail.30 <- na.omit(values(env.pyr.noail.30ky))

lims <- apply(rbind(val.GR.28.5, val.pyr.noail.30), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

#### overlap 3d 
cols <- c("#D55E00", "#99FFFF")
ellipsenm::plot_overlap(overlap.mve.GR.vs.pyr.noail, niche_col = cols, data_col = cols, legend = F)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.pyr.noail.30, col = "#CCFFFF", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(0, 5, 10)
yat <- c(-5, 0, 5)
zat <- c(-4, 0, 4)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.GR.vs.pyr.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.pyr.noail.30, col = "#CCFFFF", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_8/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_8/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.GR.vs.pyr.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.pyr.noail.30, col = "#CCFFFF", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_8/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_8/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.GR.vs.pyr.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.pyr.noail.30, col = "#CCFFFF", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_8/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_8/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_8/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.vs.pyr.noail@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.vs.pyr.noail@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.vs.pyr.noail@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_8/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.vs.pyr.noail@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.vs.pyr.noail@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.vs.pyr.noail@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


##########
## compa 9 : GR vs GMnorth 
### env data
### overlap objects
GR.28.5ky.mve1 <- ellipsenm::overlap_object(GR, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                            level = 95, variables = env.GR.28.5ky)

north.MG.30ky.mve1 <- ellipsenm::overlap_object(north.MG, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                              level = 95, variables = env.north.MG.30ky)

### niche overlap analysis 
overlap.mve.GR.vs.north.MG <- ellipsenm::ellipsoid_overlap(GR.28.5ky.mve1, north.MG.30ky.mve1, overlap_type = "all",
                                                         significance_test = TRUE, replicates = 1000)

summary(overlap.mve.GR.vs.north.MG) # saving data manually

#
### plots
dir.create("ellipsoid-models/Scenario1/compa_9")

val.GR.28.5 <- na.omit(values(env.GR.28.5ky))
val.north.MG.30 <- na.omit(values(env.north.MG.30ky))

lims <- apply(rbind(val.GR.28.5, val.north.MG.30), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

#### overlap 3d
cols <- c("#D55E00", "#99FF66")
ellipsenm::plot_overlap(overlap.mve.GR.vs.north.MG, niche_col = cols, data_col = cols, legend = F)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.north.MG.30, col = "#CCFF99", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(0, 4, 8)
yat <- c(-4, 0, 4)
zat <- c(-4, 0, 4)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.GR.vs.north.MG, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.north.MG.30, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_9/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_9/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.GR.vs.north.MG, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.north.MG.30, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_9/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_9/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.GR.vs.north.MG, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.north.MG.30, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario1/compa_9/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario1/compa_9/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario1/compa_9/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.vs.north.MG@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.vs.north.MG@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.vs.north.MG@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario1/compa_9/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.vs.north.MG@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.vs.north.MG@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.vs.north.MG@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()