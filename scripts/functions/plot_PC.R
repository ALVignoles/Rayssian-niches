#'
#'plot_PC 
#'
#'A DECRIRE


plot_PC <- function(x, title){
  PC1.2 <- autoplot(x, loadings = TRUE, loadings.label = TRUE) +
    theme_classic() 
  
  PC2.3 <- autoplot(x, x = 2, y = 3, loadings = TRUE, loadings.label = TRUE) + 
    theme_classic()
  
  plot <- ggarrange(PC1.2, PC2.3, nrow = 1)
  plot <- annotate_figure(plot, top = text_grob(title, size = 14))
  
  return(plot)
}