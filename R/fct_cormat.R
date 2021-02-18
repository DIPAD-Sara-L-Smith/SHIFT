#' Generate the Collinearity Matrix plot for best_subset
#'
#' @param data dataframe containing the data (r$data)
#' @param ind_vars character vector of the names of indepenant variables
#'
#' @return ggplot object for the collinearity matrix
#'
#' @export

plot_cormat <- function(data, ind_vars) {
  dataplot <- select(data, ind_vars)
  cormat <- round(cor(dataplot),2)
  melted_cormat <- reshape2::melt(cormat)
  lower_tri <- cormat
  lower_tri[lower.tri(lower_tri)] <- NA #OR upper.tri function
  #Finished correlation matrix heatmap
  melted_cormat <- reshape2::melt(lower_tri, na.rm = TRUE)
  # Heatmap
  plot <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue",
                         high = "red",
                         mid = "white",
                         midpoint = 0,
                         limit = c(-1,1),
                         space = "Lab",
                         name="Pearson\nCorrelation") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45,
                                     vjust = 1,
                                     size = 12,
                                     hjust = 1))+
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7,
                                 barheight = 1,
                                 title.position = "top",
                                 title.hjust = 0.5))
  return(plot)

}
