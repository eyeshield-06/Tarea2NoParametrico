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

wilcox.test(Tiempo ~ Origen, df, paired = FALSE)
df_1 <- df %>% arrange(Tiempo) %>% mutate(rangos = rank(Tiempo))
R_1 <- sum(df_1$rangos[df_1$Origen == "nacional"]) # suma rangos grupo 1
R_2 <- sum(df_1$rangos[df_1$Origen == "extranjera"]) # suma rangos grupo 2
n1 <- 41
n2 <- 59
n <- n1 + n2
U1 <- n1*n2 + (n1*(n1+1))/2 - R_1
U2 <- n1*n2 + (n2*(n2+1))/2 - R_2
U <- min(U1, U2)
U2 <- 
# sum t_k^3 - t_k
temp <- 0
v_unicos <- unique(df_1$rangos)
for (i in v_unicos){
	aux <- sum(df_1$rangos == i)
	temp <- temp + aux^3 - aux

}
temp
mu_u <- (n1*n2)/2
sigma_u <- sqrt((n1*n2*(n+1))/12 - n1*n2*temp/(12*n*(n-1)))
z <- (U-mu_u)/sigma_u
p_value <- 2*pnorm(z)

kruskal.test(Capitalizacion ~ Tipo, df)

# BoxPlots
png("~/Documents/nonparametric/tarea2/imgs/img1.png", width = 1200, height = 700)
ggplot(df, aes(x = Origen, y = Tiempo, fill = Origen)) +
	geom_boxplot() +
	coord_flip()
dev.off()

png("~/Documents/nonparametric/tarea2/imgs/img2.png", width = 1200, height = 700)
ggplot(df, aes(x = Tipo, y = Capitalizacion, fill = as.factor(Tipo))) +
	geom_boxplot() +
	coord_flip()
dev.off()