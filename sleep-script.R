if(!require(mlbench)){install.packages("mlbench"); require(mlbench)} # common datasets to use
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)} 
if(!require(modelr)){install.packages("modelr"); library(modelr)} 

# some dependencies for caret that aren't automatically installed
if(!require(ModelMetrics)){install.packages("ModelMetrics"); require(ModelMetrics)}
if(!require(recipes)){install.packages("recipes"); require(recipes)}
if(!require(DEoptimR)){install.packages("DEoptimR"); require(DEoptimR)}

if(!require(caret)){install.packages("caret"); require(caret)} # ML package WITHOUT its dependencies. Should not take as long
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
set.seed(370)

if(!require(caret)){install.packages("caret", dependencies = c("Depends", "Suggests")); require(caret)}

# load data
sleep_data <- read.csv("./Sleep Drinking - Sheet1.csv")

# Scatter plot

p <- ggplot(sleep_data, aes(x = Drinks, y = GPA))  +
  geom_point()


# Feature selection
control <- trainControl(method="repeatedcv", number = 10, repeats = 3)

model <- train(GPA ~., data=sleep_data, method = "knn", trControl = control)

importance <- varImp(model)

ggplot(importance)
