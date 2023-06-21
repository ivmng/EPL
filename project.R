install.packages("BSDA")
library(BSDA)


data <- read.csv("EPL_20_21.csv")
data$Perc_Passes_Completed <- data$Perc_Passes_Completed/100

# Q1 

mancity <- subset(data,subset=(Club=="Manchester City"))
mean_goal <- mean(data$Goals)
mean_city <- mean(mancity$Goals)
sd_city <- sd(mancity$Goals)
n_city <-nrow(mancity)
n_city
t_testcity <-t.test(mancity$Goals,mu=mean_goal,alternative = "greater") # REJECT H0
t_testcity

# Q2


dfdata <- data[data$Position==c("DF","DF,FW","DF,MF","MF,DF","FW,DF"),]
mean_ylw_df <- mean(dfdata$Yellow_Cards)
mean_ylw_df
mfdata <- data[data$Position==c("MF","MF,FW","FW,MF","DF,MF","MF,DF"),]
mean_ylw_mf <- mean(mfdata$Yellow_Cards)
mean_ylw_mf
sd_df_ylw <- sd(dfdata$Yellow_Cards)
sd_df_ylw
n_df <- nrow(dfdata)
n_df
sd_mf_ylw <- sd(mfdata$Yellow_Cards)
sd_mf_ylw
n_mf <- nrow(mfdata)
n_mf
ftest <- var.test(dfdata$Yellow_Cards,mfdata$Yellow_Cards,alternative="two.sided")#VARYANSLAR EÅÄ°T DEÄÄ°L
ftest
z.test(dfdata$Yellow_Cards, mfdata$Yellow_Cards, alternative="greater",sigma.x=sd_df_ylw,sigma.y = sd_mf_ylw,conf.level=.95) #FAILED TO REJECT H0


# Q3

harry_kane_z_test <- function(gol_sayisi, toplam_mac_sayisi, ornek_olasilik) {
  # Harry Kane'in gol atma oranÄ±nÄ± hesapla
  gol_orani <- gol_sayisi / toplam_mac_sayisi
  
  # Standart hata hesapla
  standart_hata <- sqrt(ornek_olasilik * (1 - ornek_olasilik) / toplam_mac_sayisi)
  
  # Z istatistiÄŸini hesapla
  z_istatistigi <- (gol_orani - ornek_olasilik) / standart_hata
  
  # Kritik deÄŸeri belirle (Ã¶rneÄŸin, %95 gÃ¼ven aralÄ±ÄŸÄ± iÃ§in)
  kritik_deger <- qnorm(0.95)
  
  # Hipotez testini yap
  if (z_istatistigi > kritik_deger) {
    return("Harry Kane'in gol atma olasılığı %50'nin üstündedir.")
  } else {
    return("Harry Kane'in gol atma olasılığı %50'nin üstündedir.")
  }
}

# Ã–rnek kullanÄ±m
gol_sayisi <- 23  
toplam_mac_sayisi <- 35  
ornek_olasilik <- 0.5  
sonuc <- harry_kane_z_test(gol_sayisi, toplam_mac_sayisi, ornek_olasilik)
print(sonuc)
gol_orani <- gol_sayisi / toplam_mac_sayisi
standart_hata <- sqrt(ornek_olasilik * (1 - ornek_olasilik) / toplam_mac_sayisi)
z_istatistigi <- (gol_orani - ornek_olasilik) / standart_hata
z_istatistigi
z_istatistigi #REJECT H0




# Q4

library(dplyr)
#################
less <- subset(data,subset=(data$Age<28))
greater <- subset(data,subset=(Age>=28))
mean_less <- mean(less$Perc_Passes_Completed)
mean_greater <- mean(greater$Perc_Passes_Completed)
sd_less <- sd(less$Perc_Passes_Completed)
sd_greater <- sd(greater$Perc_Passes_Completed)
n_less<-nrow(less)
n_greater<-nrow(greater)
z_value<-(mean_less-mean_greater)/sqrt((mean_less*(1-mean_less)/n_less)+(mean_greater*(1-mean_greater)/n_greater))

#####################

# Q5
library(ggplot2)
model <-lm(Goals~xG,data = data)

summary(model)
cor <- cor(data$xG,data$Goals)
cor
modeltext <- paste("Goals=",round(model$coefficients[2],2),"xG +",round(model$coefficients[1],2))
modeltext
ggplot(data, aes(x = Goals, y = xG))+ geom_point()+geom_smooth(method = "lm")
RSE <- sigma(model)*100/mean(data)
r2<-0.6819591
#normality
hist(data$Goals, xlab="Goals",ylab="Frequency",main="Histogram of Goals")
#linearity
plot(data$Goals~data$xG,xlab="xG",ylab="Goals")
#homoscedacity
par(mfrow=c(2,2))
plot(model)
shapiro <- shapiro.test(residuals(model))
format(shapiro$p.value,scientific = F) # <0.05 highly significant, normality assumption is ok
#xG is a good statistic for making inference about goals




# Q6
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(corrplot)

model <- lm(data$Assists~data$xA+data$Passes_Attempted+data$Perc_Passes_Completed)
model

summary(model)
summary(model)$coefficient
confint(model)
#Compute the correlation matrix of your data
cor_matrix <- cor(select(data,Assists,xA,Passes_Attempted))
cor_matrix
# Create a correlation plot
corrplot(cor_matrix, type = "upper", tl.cex = 0.8)
dataa <- data[,c(10,11,12,16)]
chart.Correlation(dataa, histogram=TRUE, pch=19)

RSE <- sigma(model)/mean(data$Assists)
RSE

#Q7 
anovaa <- aov(Starts~Yellow_Cards*Age,data=data)
summary(anovaa)

































