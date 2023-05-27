install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

getwd()
setwd("~/Documents/nonparametric/tarea2")
df <- read.csv("./tarea2.csv")

resume1 <- df %>% group_by(Origen) %>%
	summarise(media = mean(Tiempo), dest = sd(Tiempo),
	min = min(Tiempo), Q1 = quantile(Tiempo, 0.25),
	Q2 = quantile(Tiempo, 0.5), Q3 = quantile(Tiempo, 0.75), max = max(Tiempo))

resume2 <- df %>% group_by(Tipo) %>%
	summarise(media = mean(Capitalizacion), dest = sd(Capitalizacion),
	min = min(Capitalizacion), Q1 = quantile(Capitalizacion, 0.25),
	Q2 = quantile(Capitalizacion, 0.5), Q3 = quantile(Capitalizacion, 0.75), max = max(Capitalizacion))

wilcox.test(Tiempo ~ Origen, df)
kruskal.test(Capitalizacion ~ Tipo, df)

# BoxPlots
png("~/Documents/nonparametric/tarea2/imgs/img1.png")
ggplot(df, aes(x = Origen, y = Tiempo, fill = Origen)) +
	geom_boxplot()
dev.off()

png("~/Documents/nonparametric/tarea2/imgs/img2.png")
ggplot(df, aes(x = Tipo, y = Capitalizacion, fill = as.factor(Tipo))) +
	geom_boxplot()
dev.off()