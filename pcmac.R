computers <- read.csv("./PilotStudy2Responses.csv")
library(ggplot2)
library(dplyr)
ggplot(data=computers) + geom_bar(mapping=aes(x=What.operating.system.do.you.primarily.use.)) + ggtitle("What Operating System Do You Use?")
pc.data <- computers %>% filter(What.operating.system.do.you.primarily.use.=="PC")
pc.gpa. <- ggplot(data=pc.data) + geom_bar(mapping=aes(x=What.is.your.GPA.)) + ggtitle("What is your average GPA if you have a PC?")
pc.gpa.
mac.data <- computers %>% filter(What.operating.system.do.you.primarily.use.=="MAC")
mac.gpa. <- ggplot(data=mac.data) + geom_bar(mapping=aes(x=What.is.your.GPA.)) + ggtitle("What is your average GPA if you have a PC?")
mac.gpa.

