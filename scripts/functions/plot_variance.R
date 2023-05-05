#'
#'
#'plot_variance
#'
#'
#'A DECRIRE
#'
#'

plot_variance <- function(x, title){
  f2 <- summary(x)
  
  plot(f2$importance[3,1:5]*100, main = title, xlab = "Composante principale", 
       ylab = "Pourcentage de la variance expliquée", ylim=c(0,100),
       type = "b",frame.plot = T, cex = 1.5)
  points(f2$importance[2,1:5]*100, pch = 17, cex = 1.5)
  lines(f2$importance[2,1:5]*100, lty = 2, lwd = 1.5)
  
  legend(x = 3.5, y = 60, legend = c("Cumulative", "Non cumulative"),
         lty = c(1,2),pch = c(21,17),bty = "n",cex=0.85,pt.bg = 'white')
}
