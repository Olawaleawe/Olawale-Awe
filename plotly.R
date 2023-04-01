#############################
#install.packages(plotly)
library(plotly)
############### Testing plotly
m <- highlight_key(mpg)
p <- ggplot(m, aes(displ, hwy)) + geom_point()
gg <- highlight(ggplotly(p), "plotly_selected")
crosstalk::bscols(gg, DT::datatable(m))
###################Starting the real work##################################################################
########################################################################
set.seed(123)                                                     # Set seed for reproducibility
data <- matrix(rnorm(100, 0, 10), nrow = 10, ncol = 10)           # Create example data
colnames(data) <- paste0("col", 1:10)                             # Column names
rownames(data) <- paste0("row", 1:10)                             # Row names
heatmap(data)                                                     # Apply heatmap function
heatmap(data, Rowv = NA, Colv = NA)                               # Remove dendrogram
my_colors <- colorRampPalette(c("cyan", "deeppink3"))             # Manual color range
heatmap(data, col = my_colors(100))                               # Heatmap with manual colors
################################################
#install.packages("reshape")                                       # Install reshape package
library("reshape")                                                # Load reshape package                                                   # First six rows of data
#install.packages("ggplot2")                                       # Install ggplot2 package
library("ggplot2")                                                # Load ggplot2 package
######################################################
data_melt <- melt(data)                                           # Reorder data
head(data_melt)
ggp <- ggplot(data_melt, aes(X1, X2)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value))
ggp                                                               # Print heatmap
ggp + scale_fill_gradient(low = "green", high = "black")          # Manual colors of heatmap
##################################################################################
##Start with plotly
#install.packages("plotly")                                        # Install plotly package
library("plotly")                                                 # Load plotly package
plot_ly(z = data, type = "heatmap")                               # Apply plot_ly function
plot_ly(z = data, colorscale = "Greys", type = "heatmap")         # Manual colors
