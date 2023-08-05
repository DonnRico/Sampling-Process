knitr::opts_chunk$set(echo = FALSE)
library(kableExtra)
library(sampling)
library(stratbr)
library(ggplot2)
library(dplyr)
library(survey)
library(rstatix)
library(tidyverse)
library(ggpubr)
library(vcd)
library(tinytex)
library(pander)
options(tinytex.verbose = TRUE)

data <- read.csv("./graines.csv", stringsAsFactors = TRUE)
data$Couleur <- factor(data$Couleur)
data <- data[-1]
colnames(data) <- c("Obs","Poids","Couleur","Intensité", "Régularité")

knitr::include_graphics("./logo.jpg")

knitr::include_graphics("./roses.jpg")

summary(data[-1]) |>  kbl() |> kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

hist(data$Poids, nclass = 100, col = "pink", border = "white",
     main = paste("Poids des", nrow(data), "graines"), xlab = "Poids [carat]", ylab = "Densité",
     proba = TRUE, xlim = c(0,3), ylim = c(0, 5/2))
lines(density(data$Poids, na.rm = TRUE), lwd = 2, col = "midnightblue" )

data$Intensité <- as.factor(data$Intensité)
data$Régularité <- as.factor(data$Régularité)
tab <- xtabs(~ Régularité + Intensité + Couleur, data = data)
vcd::doubledecker(Intensité ~ Couleur + Régularité, data = tab,
                  main = "Intensité ind. Régularité | Couleur")
vcd::doubledecker(Régularité ~ Intensité + Couleur, data = tab,
                  main = "Régularité ind. Couleur | Intensité")
vcd::doubledecker(Couleur ~ Régularité + Intensité, data = tab,
                  main = "Couleur ind. Intensité | Régularité")


knitr::include_graphics("./sondage en grappes.png")
knitr::include_graphics("./stratification.png")

knitr::include_graphics("./Decomposition.png")

set.seed(2022)
n <- 1050
N <- dim(data)[1]
datastrat <- data.frame(xtabs(~Couleur + Intensité + Régularité, data = data))
datastrat <- datastrat[order(datastrat$Couleur, datastrat$Intensité, datastrat$Régularité),]
nh <- data.frame(round(n / N * datastrat$Freq))
nh$Select<- nh$round.n.N...datastrat.Freq.
nh <- nh[-1]

data <- data[order(data$Couleur, data$Intensité, data$Régularité),]
6 * sum(nh[1:15,]) + 7 * sum(nh[16:30,]) + 8 * sum(nh[31:45,])

nH <- as.vector(nh$Select)
Sample <- sampling::strata(data, stratanames = c("Couleur", "Intensité", "Régularité"),
                           size = nH, method = "srswor")
Echant <- getdata(data, Sample)
Echantillon_1 <- Echant[-(1:8)]
write.csv(Echantillon_1,"Echantillon_1.csv")

Echant1 <- read.csv("./E1_Aymeric_Warnauts.csv", stringsAsFactors = TRUE)
Echant1 <- Echant1[-1]
colnames(Echant1) <- c("Obs","Prix","Poids","Couleur","Intensité", "Régularité")
Echant1$Intensité <- factor(Echant1$Intensité)
Echant1$Régularité <- factor(Echant1$Régularité)

cout <- function(x){
  if(x == "Bleu") y <- 6
if(x == "Jaune") y <- 7
if(x == "Rouge") y <- 8
return(y)
}

data$Coût <- sapply(data$Couleur, cout)
Echant1$Coût <- sapply(Echant1$Couleur, cout)

Echant1 %>% shapiro_test(Prix) %>% kbl() %>% kable_minimal() %>%
  kable_styling(latex_options = c("hold_position"))
Echant1 %>% group_by(Couleur) %>% shapiro_test(Prix)  %>% kbl()  %>%
  kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

hist(Echant1$Prix, nclass = 40, col = "yellow", border = "white",
     main = paste("Prix des", nrow(Echant), "graines échantillonnées"),
     xlab = "Prix [Euros]", ylab = "Densité", proba = TRUE)
lines(density(Echant1$Prix, na.rm = TRUE), lwd = 2, col = "midnightblue" )

hist(Echant$Poids, nclass = 40, col = "pink", border = "white",
     main = paste("Poids des", nrow(Echant), "graines échantillonnées"),
     xlab = "Poids [carat]", ylab = "Densité",
     proba = TRUE, xlim = c(0,3), ylim = c(0, 5/2))
lines(density(Echant$Poids, na.rm = TRUE), lwd = 2, col = "midnightblue" )

library(patchwork)
theme_set(theme_bw())
p1 <- ggplot(Echant1, aes(x=Poids, y=Prix, color = Couleur)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)+ 
  scale_colour_manual(values = c("lightblue", "yellow", "Red")) 
p3 <- ggplot(Echant1, aes(x=Intensité, y=Prix, color = Couleur))+
  geom_boxplot() + scale_colour_manual(values = c("lightblue", "yellow", "Red"))+
  stat_summary(fun = mean, geom = "point", shape = 15) + theme(legend.position = "none")
p2 <- ggplot(Echant1, aes(x=Régularité, y=Prix, color = Couleur))+
  geom_boxplot() + scale_colour_manual(values = c("lightblue", "yellow", "Red"))+
  stat_summary(fun = mean, geom = "point", shape = 15) + theme(legend.position = "none")
(p1 + p2) / p3 + plot_layout(guides = "collect") 

outliers1 <- data.frame(Echant1 %>% group_by(Couleur) %>% identify_outliers(Prix))  
outliers1[which(outliers1$is.extreme == TRUE), ] %>% kbl() %>%
  kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

model1 <- lm(Prix~Couleur, data = Echant1)
ggqqplot(residuals(model1))
ggqqplot(Echant1, "Prix", facet.by = "Couleur")
plot(model1,1)

Echant1 %>% kruskal_test(Prix~Couleur) %>% kbl() %>% kable_minimal() %>% kable_styling(latex_options = c("hold_position"))
Echant1 %>% kruskal_effsize(Prix~Couleur) %>% kbl() %>% kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

Echant1 %>% dunn_test(Prix~Couleur, p.adjust.method = "bonferroni") %>%
  kbl() %>% kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

outliers2 <- data.frame(Echant1 %>% group_by(Intensité)
                        %>% identify_outliers(Prix))  
outliers2[which(outliers2$is.extreme == TRUE), ] %>% kbl()%>%
  kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

Echant1 %>% group_by(Intensité) %>% shapiro_test(Prix)  %>% kbl()  %>% kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

model2 <- lm(Prix~Intensité, data = Echant1)
ggqqplot(residuals(model2))
ggqqplot(Echant1, "Prix", facet.by = "Intensité")
plot(model2,1)

Echant1 %>% kruskal_test(Prix~Intensité) %>% kbl() %>% 
  kable_minimal() %>% kable_styling(latex_options = c("hold_position"))
Echant1 %>% kruskal_effsize(Prix~Intensité) %>% kbl() %>% kable_minimal() %>%
  kable_styling(latex_options = c("hold_position"))

Echant1 %>% dunn_test(Prix~Intensité, p.adjust.method = "bonferroni") %>%
  kbl() %>% kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

outliers3 <- data.frame(Echant1 %>% group_by(Régularité) %>%
                          identify_outliers(Prix))  
outliers3[which(outliers3$is.extreme == TRUE), ] %>% kbl()  %>%
  kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

model3 <- lm(Prix~Régularité, data = Echant1)
ggqqplot(residuals(model3))
ggqqplot(Echant1, "Prix", facet.by = "Régularité")
plot(model3,1)

Echant1 %>% group_by(Régularité) %>% shapiro_test(Prix)  %>% kbl() %>%
  kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

Echant1 %>% kruskal_test(Prix~Régularité) %>% kbl() %>% kable_minimal() %>%
  kable_styling(latex_options = c("hold_position"))
Echant1 %>% kruskal_effsize(Prix~Régularité) %>% kbl() %>% kable_minimal() %>%
  kable_styling(latex_options = c("hold_position"))

summary(lm(Echant1$Prix ~ Echant1$Poids))$coefficients %>% kbl() %>%
  kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

ggplot(Echant1, aes(x=Poids, y=Prix, color = Régularité)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(Echant1, aes(x=Poids, y=Prix, color = Intensité, shape = Couleur, linetype = Couleur)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

formula(lm(Prix~Poids*Intensité, data = Echant1))
summary(lm(Prix~Poids*Intensité, data = Echant1))$coefficients |> kbl() |>
  kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

p1 <- ggplot(Echant1, aes(x = Intensité, y = Poids, color = Intensité, fill = Intensité)) +
  geom_boxplot(alpha = 0.5) + labs(title = "Boxplots de l'échantillon") + geom_violin(mapping = aes(alpha = 0.1))
p2 <- ggplot(data, aes(x = Intensité, y = Poids, color = Intensité, fill = Intensité)) +
  geom_boxplot(alpha = 0.5) + labs(title = "Boxplots de la population") + geom_violin(mapping = aes(alpha = 0.1)) +
  theme(legend.position = "NONE")
p1 + p2 + plot_layout(guides = "collect")

sum(Echant1$Prix)
sup <- as.vector(Echant1$Obs)
data <- data[order(data$Obs),]
data1 <- data[-sup,]
outliersf <- data.frame(data1 %>% group_by(Intensité) %>% identify_outliers(Poids))  
outliersf[which(outliersf$is.extreme == TRUE), ]  

reg <- lm(Prix~Poids*Intensité*Couleur*Régularité, data = Echant1)
coefficients(reg) |> kbl(col.names = "Coef", vline = "|") |>
  kable_minimal() %>% kable_styling(latex_options = c("hold_position"))
prix.data <- predict.lm(reg, data)
sum(prix.data)

datastrat2 <- data1[which(as.numeric(data1$Intensité) > 1 & data1$Poids > 2), ]
data1[which(data1$Intensité == 5  & data1$Poids >= 2), ]

Nh <- as.numeric(table(datastrat2$Couleur))
Sh <-  sapply(unique(Echant1$Couleur), function(x) {
sd(Echant1$Prix[Echant1$Couleur == x])
})
t(Sh) |> kbl() |> kable_minimal() %>% kable_styling(latex_options = c("hold_position"))

data$select <- 0
data <- data[order(data$Obs),]
for (i in Echant1$Obs) {
  data[i,]$select <- 1
}
data$select <- as.factor(data$select)

Echant2O <- data.frame(data[which(data$select !=1),] |> identify_outliers(Poids))
Echant2O
sum(Echant2O[which(Echant2O$Intensité !=1 & Echant2O$Poids > 2.18),]$Coût)
7719 - 7333
Echant2a <- Echant2O[which(Echant2O$Intensité !=1 & Echant2O$Poids > 2.18),]

for (i in Echant2a$Obs) {
  data[i,]$select <- 1
}
data$select <- as.factor(data$select)

Echant2O <- data.frame(data[which(data$select !=1),] |> identify_outliers(Poids))
sum(Echant2O[which(Echant2O$is.extreme),]$Coût)
Echant2b <- Echant2O[which(Echant2O$is.extreme),]

for (i in Echant2b$Obs) {
  data[i,]$select <- 1
}
data$select <- as.factor(data$select)

Echant2O <- data.frame(data[which(data$select !=1),] |> identify_outliers(Poids))
sum(Echant2O[which(Echant2O$is.outlier & Echant2O$Intensité == 5 &
                     Echant2O$Poids > 2.1 & Echant2O$Couleur == "Jaune"),]$Coût)
Echant2c <- Echant2O[which(Echant2O$is.outlier &
                             Echant2O$Intensité == 5 &
                             Echant2O$Poids > 2.1 & Echant2O$Couleur == "Jaune"),]

nrow(data[which(data$select == 1),])
for (i in Echant2c$Obs) {
  data[i,]$select <- 1
}
data$select <- as.factor(data$select)

Echant2 <- rbind(Echant2a,Echant2b,Echant2c)

Echant2[-802,]
Echant2[which(Echant2$Couleur == "Rouge" &
                Echant2$Poids < 2.2 &Echant2$Intensité == 2), ]$select <- 1
Echant2 <- Echant2[which(Echant2$select !=1),]
sum(Echant2$Coût)

Echantillon_2 <- as.vector(Echant2$Obs)
write.csv(Echantillon_2,"./Echantillon_2.csv", row.names = FALSE)

Echantfinal <- read.csv("./E2_Aymeric_Warnauts.csv", stringsAsFactors = TRUE)
Echantfinal <- Echantfinal[-1]
colnames(Echantfinal) <- c("Obs","Prix","Poids","Couleur","Intensité", "Régularité")
Echantfinal$Intensité <- factor(Echantfinal$Intensité)
Echantfinal$Régularité <- factor(Echantfinal$Régularité)
Echantfinal$Coût <- sapply(Echantfinal$Couleur, cout)

hist(Echantfinal$Prix, nclass = 40, col = "yellow", border = "white",
     main = paste("Prix des", nrow(Echantfinal), "graines échantillonnées"),
     xlab = "Prix [Euros]", ylab = "Densité", proba = TRUE)
lines(density(Echant1$Prix, na.rm = TRUE), lwd = 2, col = "midnightblue" )
lines(density(Echantfinal$Prix, na.rm = TRUE), lwd = 2, col = "blue" )

hist(Echantfinal$Poids, nclass = 40, col = "pink", border = "white",
     main = paste("Poids des", nrow(Echantfinal), "graines échantillonnées"),
     xlab = "Poids [carat]", ylab = "Densité", proba = TRUE)
lines(density(Echant1$Poids, na.rm = TRUE), lwd = 2, col = "midnightblue" )
lines(density(Echantfinal$Poids, na.rm = TRUE), lwd = 2, col = "blue" )

p1 <- ggplot(Echantfinal, aes(x=Poids, y=Prix, color = Couleur)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  scale_colour_manual(values = c("lightblue", "yellow", "Red")) 
p3 <- ggplot(Echantfinal, aes(x=Intensité, y=Prix, color = Couleur)) +
  geom_boxplot() + scale_colour_manual(values = c("lightblue", "yellow", "Red")) +
  stat_summary(fun = mean, geom = "point", shape = 15) + theme(legend.position = "none")
p2 <- ggplot(Echantfinal, aes(x=Régularité, y=Prix, color = Couleur)) + geom_boxplot() +
  scale_colour_manual(values = c("lightblue", "yellow", "Red")) +
  stat_summary(fun = mean, geom = "point", shape = 15) + theme(legend.position = "none")

Echantanalyse <- rbind.data.frame(Echant1, Echantfinal)
p4 <- ggplot(Echantanalyse, aes(x=Poids, y=Prix, color = Couleur)) + geom_point() +
  geom_smooth(method = "glm", se = FALSE) + scale_colour_manual(values = c("lightblue", "yellow", "Red")) 
p5 <- ggplot(Echantanalyse, aes(x=Intensité, y=Prix, color = Couleur)) + geom_boxplot() +
  scale_colour_manual(values = c("lightblue", "yellow", "Red")) + 
  stat_summary(fun = mean, geom = "point", shape = 15) + theme(legend.position = "none")
p6 <- ggplot(Echantanalyse, aes(x=Régularité, y=Prix, color = Couleur)) + geom_boxplot() +
  scale_colour_manual(values = c("lightblue", "yellow", "Red")) +
  stat_summary(fun = mean, geom = "point", shape = 15) + theme(legend.position = "none")
(p1 + p4 + p2 + p6) / (p3 + p5) + plot_layout(guides = "collect")

## NA

cbind(datastrat, nh) %>%  kbl() %>% kable_minimal(full_width = FALSE, position = "center") %>%  
  row_spec(1:15, color = "blue") %>%  row_spec(16:30, color = "orange") %>%
  row_spec(31:45, color = "red") %>% kable_styling(latex_options = c("hold_position"))
