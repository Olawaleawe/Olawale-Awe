##############################################################################
# Learning ggplot2
###########################################################################
#install.packages('ggplot2')
library(ggplot2)
#install.packages("tidyverse")
library (tidyverse)


########################Warming up##########################
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()

##################Getting Started####################################################################
###################################################################################
#We are using the iris dataset
data(iris)
dim(iris)
head(iris)
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, col=Species))+geom_point()
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, col=Species, shape=Species))+geom_point()
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, col=Species))+geom_point() +geom_smooth()
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, col=Species))+geom_point(color = "blue") + geom_smooth(color = "red")
# color aesthetic defined for each geom point
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, col=Species))+geom_point() +geom_smooth(se = FALSE)
# color aesthetic defined only for a particular geom_point layer
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length)) +geom_point(aes(col = Species)) +geom_smooth(se = FALSE)
##########################################################
data(mtcars)
glimpse (mtcars)
################# Bar chart##############################################
ggplot(mtcars, aes(x = gear)) +geom_bar()
ggplot(mtcars, aes(x = gear)) +geom_bar()+coord_flip()
ggplot(mtcars, aes(hp, mpg)) + geom_point(color = "blue") + stat_summary(fun.y = "mean", geom = "line", linetype = "dashed")
ggplot(mtcars, aes(hp, mpg)) + geom_point(color = "blue") + geom_rug(show.legend = FALSE) +stat_summary(fun.y = "mean",geom = "line", linetype = "dashed")
###################Histogram###########################################
ggplot(mtcars,aes(x=mpg)) + geom_histogram()
#######################Boxplot#####################################################
ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + geom_boxplot()
mtcars$cyl <- as.factor(mtcars$cyl)
ggplot(mtcars, aes(x=(cyl), y=mpg,color = cyl)) + geom_boxplot()+scale_color_manual(values = c("#3a0ca3", "#c9184a", "#3a5a40"))
###########################Violin Plot###################
ggplot(mtcars, aes(factor(cyl), mpg))+ geom_violin(aes(fill = cyl))
###########################Pie chart##########################
ggplot(mtcars, aes(x="", y=mpg, fill=cyl)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)
############################Polar plot#####################################
mtcars %>%
  dplyr::group_by(cyl) %>%
  dplyr::summarize(mpg = median(mpg)) %>%
  ggplot(aes(x = cyl, y = mpg)) + geom_col(aes(fill =cyl), color = NA) + labs(x = "", y = "Median mpg") + coord_polar()
######################### Bump Chart##################################################
ggplot(mtcars, aes(x = hp, y = mpg, group = cyl)) + geom_line(aes(color = cyl), size = 2) + geom_point(aes(color = cyl), size = 4) + scale_y_reverse(breaks = 1:nrow(mtcars))
############################# Contour Plot#####################################################
ggplot(mtcars, aes(mpg, hp)) + geom_density_2d_filled(show.legend = FALSE) + coord_cartesian(expand = FALSE) + labs(x = "mpg")
###################################################################################################################
##################################################################################################################################