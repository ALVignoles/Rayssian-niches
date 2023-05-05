# ellipsenm process
# from thesis scripts, unmodified (except for paths)
# Scenario 2


########################################
# Comparing mean transient climatologies

##########
## compa 1: trans30y vs trans100y vs 500y for noail
### overlap objects
noail.trans30.mve1 <- ellipsenm::overlap_object(noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                    level = 95, variables = ref.env.noail.trans30)

noail.trans100.mve1 <- ellipsenm::overlap_object(noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                     level = 95, variables = env.noail.trans100)

noail.trans500.mve1 <- ellipsenm::overlap_object(noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                     level = 95, variables = env.noail.trans500)

### niche overlap analysis 
overlap.mve.noail.trans.1 <- ellipsenm::ellipsoid_overlap(noail.trans30.mve1, noail.trans100.mve1, overlap_type = "all",
                                                              significance_test = TRUE, replicates = 1000)

summary(overlap.mve.noail.trans.1) # saving data manually

overlap.mve.noail.trans.2 <- ellipsenm::ellipsoid_overlap(noail.trans30.mve1, noail.trans500.mve1, overlap_type = "all",
                                                              significance_test = TRUE, replicates = 1000)

summary(overlap.mve.noail.trans.2) # saving data manually

###
### plots 1
val.noail.trans30 <- na.omit(values(env.noail.trans30))
val.noail.trans100 <- na.omit(values(env.noail.trans100))
val.noail.trans500 <- na.omit(values(env.noail.trans500))

lims <- apply(rbind(val.noail.trans30, val.noail.trans100), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]


# *overlap 3d
cols <- c("#99FFFF", "#0066CC")
ellipsenm::plot_overlap(overlap.mve.noail.trans.1, niche_col = cols, data_col = cols, legend = F)
plot3d(val.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.trans100, col = "#006699", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(-3, 0, 3)
yat <- c(-4, 0, 4)
zat <- c(-1.5, 0, 1.5)

# *move the plot around and then save the view
## *View 1
# view3d1 <- par3d()$userMatrix
# saveRDS(view3d1, "ellipsoid-models/Scenario2/view3dov1.rds")
view3d1 <- readRDS("ellipsoid-models/Scenario2/view3dov1.rds")

## *View 2
# view3d2 <- par3d()$userMatrix
# saveRDS(view3d2, "ellipsoid-models/Scenario2/view3dov2.rds")
view3d2 <- readRDS("ellipsoid-models/Scenario2/view3dov2.rds")

## *View 3
# view3d3 <- par3d()$userMatrix
# saveRDS(view3d3, "ellipsoid-models/Scenario2/view3dov3.rds")
view3d3 <- readRDS("ellipsoid-models/Scenario2/view3dov3.rds")


# *save views
dir.create("ellipsoid-models/Scenario2/compa_1/1.1")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.noail.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.trans100, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_1/1.1/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_1/1.1/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.noail.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.trans100, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_1/1.1/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_1/1.1/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.noail.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.trans100, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_1/1.1/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_1/1.1/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}


# *significance test
png("ellipsoid-models/Scenario2/compa_1/1.1/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.noail.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 40), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.noail.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.noail.trans.1@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario2/compa_1/1.1/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.noail.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 40), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.noail.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.noail.trans.1@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

###
### plots 2
lims <- apply(rbind(val.noail.trans30, val.noail.trans500), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

# *overlap 3d
cols <- c("#99FFFF", "#0066CC")
ellipsenm::plot_overlap(overlap.mve.noail.trans.2, niche_col = cols, data_col = cols, legend = F)
plot3d(val.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.trans500, col = "#006699", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

# *save the views
dir.create("ellipsoid-models/Scenario2/compa_1/1.2")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.noail.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.trans500, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_1/1.2/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_1/1.2/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.noail.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.trans500, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_1/1.2/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_1/1.2/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.noail.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans30, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.trans500, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_1/1.2/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_1/1.2/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario2/compa_1/1.2/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.noail.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 40), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.noail.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.noail.trans.2@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario2/compa_1/1.2/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.noail.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 40), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.noail.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.noail.trans.2@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()



###########
## compa 2: trans30y vs trans100y vs trans500y for rays
### overlap objects
rays.trans30.mve1 <- ellipsenm::overlap_object(rays, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                   level = 95, variables = env.rays.trans30)

rays.trans100.mve1 <- ellipsenm::overlap_object(rays, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                    level = 95, variables = env.rays.trans100)

rays.trans500.mve1 <- ellipsenm::overlap_object(rays, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                    level = 95, variables = env.rays.trans500)

### niche overlap analysis 
overlap.mve.rays.trans.1 <- ellipsenm::ellipsoid_overlap(rays.trans30.mve1, rays.trans100.mve1, overlap_type = "all",
                                                             significance_test = TRUE, replicates = 1000)

summary(overlap.mve.rays.trans.1) # saving data manually

overlap.mve.rays.trans.2 <- ellipsenm::ellipsoid_overlap(rays.trans30.mve1, rays.trans500.mve1, overlap_type = "all",
                                                             significance_test = TRUE, replicates = 1000)

summary(overlap.mve.rays.trans.2) # saving data manually

###
### plots 1
val.rays.trans30 <- na.omit(values(env.rays.trans30))
val.rays.trans100 <- na.omit(values(env.rays.trans100))
val.rays.trans500 <- na.omit(values(env.rays.trans500))

lims <- apply(rbind(val.rays.trans30, val.rays.trans100), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

# *overlap 3d
cols <- c("#99FF66", "#009E73")
ellipsenm::plot_overlap(overlap.mve.rays.trans.1, niche_col = cols, data_col = cols, legend = F)
plot3d(val.rays.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.rays.trans100, col = "#006600", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c( 0, 4, 8)
yat <- c( -4, 0, 4)
zat <- c(-5, 0, 5)

# *save views
dir.create("ellipsoid-models/Scenario2/compa_2/2.1")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.rays.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.rays.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.rays.trans100, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_2/2.1/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_2/2.1/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.rays.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.rays.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.rays.trans100, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_2/2.1/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_2/2.1/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.rays.trans.1, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.rays.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.rays.trans100, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_2/2.1/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_2/2.1/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario2/compa_2/2.1/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.rays.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 80), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.rays.trans.1@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.rays.trans.1@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario2/compa_2/2.1/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.rays.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 80), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.rays.trans.1@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.rays.trans.1@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


###
### plots 2
lims <- apply(rbind(val.rays.trans30, val.rays.trans500), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]


# *overlap 3d
cols <- c("#99FF66", "#009E73")
ellipsenm::plot_overlap(overlap.mve.rays.trans.2, niche_col = cols, data_col = cols, legend = F)
plot3d(val.rays.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.rays.trans500, col = "#006600", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))


# *save the views
dir.create("ellipsoid-models/Scenario2/compa_2/2.2")
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.rays.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.rays.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.rays.trans500, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_2/2.2/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_2/2.2/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.rays.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.rays.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.rays.trans500, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_2/2.2/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_2/2.2/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.rays.trans.2, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.rays.trans30, col = "#CCFF99", add = TRUE)
plot3d(val.rays.trans500, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_2/2.2/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_2/2.2/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario2/compa_2/2.2/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.rays.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 80), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.rays.trans.2@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.rays.trans.2@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario2/compa_2/2.2/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.rays.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 80), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.rays.trans.2@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.rays.trans.2@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()



######################################################################
# Comparing climatic models (trans30y vs eq for Rays and Noail)

###########
## compa 4 : noaillian 
### overlap objects
noail.trans30.mve1 <- ellipsenm::overlap_object(noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                    level = 95, variables = env.noail.trans30)

noail.eq.mve1 <- ellipsenm::overlap_object(noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                               level = 95, variables = env.noail.eq)

### niche overlap analysis 
overlap.mve.noail <- ellipsenm::ellipsoid_overlap(noail.trans30.mve1, noail.eq.mve1, overlap_type = "all",
                                                      significance_test = TRUE, replicates = 1000)

summary(overlap.mve.noail) # saving data manually

###
### plots
dir.create("ellipsoid-models/Scenario2/compa_4")

val.noail.trans <- na.omit(values(env.noail.trans30))
val.noail.eq <- na.omit(values(env.noail.eq))

lims <- apply(rbind(val.noail.trans, val.noail.eq), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

# *overlap 3d
cols <- c("#99FFFF", "#0066CC")
ellipsenm::plot_overlap(overlap.mve.noail, niche_col = cols, data_col = cols, legend = F)
plot3d(val.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.eq, col = "#006699", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(-5, 0, 5)
yat <- c(-5, 0, 5)
zat <- c(-10, -5, 0)


# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.eq, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_4/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_4/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.eq, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_4/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_4/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.noail.eq, col = "#006699", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_4/View3")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_4/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario2/compa_4/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.noail@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.noail@significance_results$full_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.noail@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario2/compa_4/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.noail@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 1000), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.noail@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.noail@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


###########
## compa 5 : rays trans30 vs eq
### overlap objects
rays.trans30.mve1 <- ellipsenm::overlap_object(rays, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                   level = 95, variables = env.rays.trans30)

rays.eq.mve1 <- ellipsenm::overlap_object(rays, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                              level = 95, variables = env.rays.eq)

### niche overlap analysis 
overlap.mve.rays <- ellipsenm::ellipsoid_overlap(rays.trans30.mve1, rays.eq.mve1, overlap_type = "all",
                                                     significance_test = TRUE, replicates = 1000)

summary(overlap.mve.rays) # saving data manually

###
### plots
dir.create("ellipsoid-models/Scenario2/compa_5")
val.rays.trans <- na.omit(values(env.rays.trans30))
val.rays.eq <- na.omit(values(env.rays.eq))

lims <- apply(rbind(val.rays.trans, val.rays.eq), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

#### overlap 3d
cols <- c("#99FF66", "#009E73")
ellipsenm::plot_overlap(overlap.mve.rays, niche_col = cols, data_col = cols, legend = F)
plot3d(val.rays.trans, col = "#CCFF99", add = TRUE)
plot3d(val.rays.eq, col = "#006600", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(0, 7, 14)
yat <- c(-5, 0, 5)
zat <- c(-5, 0, 5)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.rays, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.rays.trans, col = "#CCFF99", add = TRUE)
plot3d(val.rays.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_5/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_5/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.rays, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.rays.trans, col = "#CCFF99", add = TRUE)
plot3d(val.rays.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_5/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_5/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.rays, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.rays.trans, col = "#CCFF99", add = TRUE)
plot3d(val.rays.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_5/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_5/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario2/compa_5/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.rays@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.rays@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.rays@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario2/compa_5/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.rays@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 1000), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.rays@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.rays@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()



####################################################
# Comparing MG north with Noail for eq and trans

##########
## compa 6: trans models 
### overlap objects
noail.trans30.mve1 <- ellipsenm::overlap_object(noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                    level = 95, variables = env.noail.trans30)

rays.trans30.mve1 <- ellipsenm::overlap_object(rays, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                   level = 95, variables = env.rays.trans30)

### niche overlap analysis 
overlap.mve.trans30 <- ellipsenm::ellipsoid_overlap(noail.trans30.mve1, rays.trans30.mve1, overlap_type = "all",
                                                    significance_test = TRUE, replicates = 1000)

summary(overlap.mve.trans30) # saving data manually

###
### plots
dir.create("ellipsoid-models/Scenario2/compa_6")

val.noail.trans <- na.omit(values(env.noail.trans30))
val.rays.trans <- na.omit(values(env.rays.trans30))

lims <- apply(rbind(val.noail.trans, val.rays.trans), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

# *overlap 3d
cols <- c("#99FFFF", "#99FF66")
ellipsenm::plot_overlap(overlap.mve.trans30, niche_col = cols, data_col = cols, legend = F)
plot3d(val.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.rays.trans, col = "#CCFF99", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(-5, 0, 5)
yat <- c(-4, 0, 4)
zat <- c(-2, 0, 2)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.trans30, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.rays.trans, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_6/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_6/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.trans30, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.rays.trans, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_6/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_6/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.trans30, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.trans, col = "#CCFFFF", add = TRUE)
plot3d(val.rays.trans, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_6/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_6/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario2/compa_6/significance_test_full.png",
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

png("ellipsoid-models/Scenario2/compa_6/significance_test_union.png",
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
noail.eq.mve1 <- ellipsenm::overlap_object(noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                               level = 95, variables = env.noail.eq)

rays.eq.mve1 <- ellipsenm::overlap_object(rays, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                              level = 95, variables = env.rays.eq)

### niche overlap analysis 
overlap.mve.eq <- ellipsenm::ellipsoid_overlap(noail.eq.mve1, rays.eq.mve1, overlap_type = "all",
                                               significance_test = TRUE, replicates = 1000)
summary(overlap.mve.eq) # saving data manually


###
### plots
dir.create("ellipsoid-models/Scenario2/compa_7")

val.noail.eq <- na.omit(values(env.noail.eq))
val.rays.eq <- na.omit(values(env.rays.eq))

lims <- apply(rbind(val.noail.eq, val.rays.eq), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

#### overlap 3d
cols <- c("#0066CC", "#009E73")
ellipsenm::plot_overlap(overlap.mve.eq, niche_col = cols, data_col = cols, legend = F)
plot3d(val.noail.eq, col = "#006699", add = TRUE)
plot3d(val.rays.eq, col = "#006600", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(-5, 0, 5)
yat <- c(-2, 0, 2)
zat <- c(-2, 0, 2)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.eq, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.eq, col = "#006699", add = TRUE)
plot3d(val.rays.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_7/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_7/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.eq, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.eq, col = "#006699", add = TRUE)
plot3d(val.rays.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_7/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_7/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.eq, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.noail.eq, col = "#006699", add = TRUE)
plot3d(val.rays.eq, col = "#006600", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_7/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_7/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario2/compa_7/significance_test_full.png",
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

png("ellipsoid-models/Scenario2/compa_7/significance_test_union.png",
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
# Comparing GR with Rays and Noail (with trans simulations)

###########
## compa 8 : GR vs noail 
### overlap objects
GR.28.5ky.mve1 <- ellipsenm::overlap_object(GR, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                            level = 95, variables = env.GR.28.5ky)

noail.30ky.mve1 <- ellipsenm::overlap_object(noail, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                 level = 95, variables = env.noail.30ky)

### niche overlap analysis 
overlap.mve.GR.vs.noail <- ellipsenm::ellipsoid_overlap(GR.28.5ky.mve1, noail.30ky.mve1, overlap_type = "all",
                                                            significance_test = TRUE, replicates = 1000)

summary(overlap.mve.GR.vs.noail) # saving data manually

###
### plots
dir.create("ellipsoid-models/Scenario2/compa_8")

val.GR.28.5 <- na.omit(values(env.GR.28.5ky))
val.noail.30 <- na.omit(values(env.noail.30ky))

lims <- apply(rbind(val.GR.28.5, val.noail.30), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

#### overlap 3d 
cols <- c("#D55E00", "#99FFFF")
ellipsenm::plot_overlap(overlap.mve.GR.vs.noail, niche_col = cols, data_col = cols, legend = F)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.noail.30, col = "#CCFFFF", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(0, 5, 10)
yat <- c(-5, 0, 5)
zat <- c(-4, 0, 4)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.GR.vs.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.noail.30, col = "#CCFFFF", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_8/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_8/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.GR.vs.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.noail.30, col = "#CCFFFF", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_8/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_8/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.GR.vs.noail, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.noail.30, col = "#CCFFFF", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_8/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_8/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario2/compa_8/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.vs.noail@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.vs.noail@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.vs.noail@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario2/compa_8/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.vs.noail@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.vs.noail@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.vs.noail@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()


##########
## compa 9 : GR vs Rays 
### env data
### overlap objects
GR.28.5ky.mve1 <- ellipsenm::overlap_object(GR, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                            level = 95, variables = env.GR.28.5ky)

rays.30ky.mve1 <- ellipsenm::overlap_object(rays, species = "Species", longitude = "Longitude", latitude = "Latitude", method = "mve1",
                                                level = 95, variables = env.rays.30ky)

### niche overlap analysis 
overlap.mve.GR.vs.rays <- ellipsenm::ellipsoid_overlap(GR.28.5ky.mve1, rays.30ky.mve1, overlap_type = "all",
                                                           significance_test = TRUE, replicates = 1000)

summary(overlap.mve.GR.vs.rays) # saving data manually

#
### plots
dir.create("ellipsoid-models/Scenario2/compa_9")

val.GR.28.5 <- na.omit(values(env.GR.28.5ky))
val.rays.30 <- na.omit(values(env.rays.30ky))

lims <- apply(rbind(val.GR.28.5, val.rays.30), 2, range)
xlim <- lims[, 1]; ylim <- lims[, 2]; zlim <- lims[, 3]

#### overlap 3d
cols <- c("#D55E00", "#99FF66")
ellipsenm::plot_overlap(overlap.mve.GR.vs.rays, niche_col = cols, data_col = cols, legend = F)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.rays.30, col = "#CCFF99", add = TRUE)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1))

xat <- c(0, 4, 8)
yat <- c(-4, 0, 4)
zat <- c(-4, 0, 4)

# *save the views
angle <- rep(5 * pi / 180, 360/5)

## *View 1 
par3d(params = list(userMatrix = view3d1))
plot_overlap(overlap.mve.GR.vs.rays, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.rays.30, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_9/View1")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 0, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_9/View1/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 2
par3d(params = list(userMatrix = view3d2))
plot_overlap(overlap.mve.GR.vs.rays, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.rays.30, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_9/View2")

for (i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 0, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_9/View2/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

## *View 3 
par3d(params = list(userMatrix = view3d3))
plot_overlap(overlap.mve.GR.vs.rays, legend = F, change_labels = T, niche_col = cols, data_col = cols)
plot3d(val.GR.28.5, col = "#FF9999", add = TRUE)
plot3d(val.rays.30, col = "#CCFF99", add = TRUE)
axes3d(xat = xat, yat = yat, zat = zat)
decorate3d(xlim = xlim, ylim = ylim, zlim = zlim, aspect = c(1, 1, 1),
           axes = FALSE, xlab = "", ylab = "", zlab = "")

dir.create("ellipsoid-models/Scenario2/compa_9/View3")

for(i in seq(angle)) {
  view3d(userMatrix = rotate3d(par3d("userMatrix"), angle[i], 1, 1, 1))
  rgl.snapshot(filename=paste(paste("ellipsoid-models/Scenario2/compa_9/View3/", "frame-", sep=""), sprintf("%03d", i), ".png", sep = ""))
}

# *significance test
png("ellipsoid-models/Scenario2/compa_9/significance_test_full.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.vs.rays@significance_results$full_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.vs.rays@significance_results$full_random$Niche_1_vs_2$overlap, 0.05, na.rm = T),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.vs.rays@full_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()

png("ellipsoid-models/Scenario2/compa_9/significance_test_union.png",
    width = 750, height = 550)
par(mar = c(4.5, 4.5, 0.5, 0.5))
par(cex = 0.85)
hist(overlap.mve.GR.vs.rays@significance_results$union_random$Niche_1_vs_2$overlap, breaks = seq(0, 1, 0.01),
     main = "", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 60), cex.lab = 1.8, cex.axis = 1.2)
abline(v = quantile(overlap.mve.GR.vs.rays@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "darkgreen", lwd = 2, lty = 2)
abline(v = overlap.mve.GR.vs.rays@union_overlap$overlap, col = "green", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("green", "darkgreen"), lty = c(1, 2), lwd = 2, cex = 1.5)
box(bty = "l")
dev.off()