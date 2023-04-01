library(ggplot2)
library(ggiraph)
data <- mtcars
data$carname <- row.names(data)
gg_point = ggplot(data = data) +
  geom_point_interactive(aes(x = wt, y = qsec, color = disp,
                             tooltip = carname, data_id = carname)) + 
  theme_minimal()

girafe(ggobj = gg_point)
