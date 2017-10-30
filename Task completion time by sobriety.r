
library(dplyr)
library(ggplot2)
library(reshape2)

sober <- c(885, 1250)

intoxicated <- c(1058, 805)

participants <- c("A", "B")

experiment <- melt(data.frame(participants, sober, intoxicated), variable = "sobriety", id = "participants")



ggplot(experiment, aes(x=participants, y = value, fill=factor(sobriety))) + geom_bar(stat="identity", position="dodge") + 
  scale_fill_discrete(name = "sobriety", breaks=c("sober","intoxicated"), labels=c("Sober","Intoxicated")) + ggtitle("Task Completion Time by Sobriety") + xlab("Participants") + ylab("Task Completion Time (seconds)")
