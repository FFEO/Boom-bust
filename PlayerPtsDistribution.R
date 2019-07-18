#Fantasy football analysis
##approximating players weekly score distribution
##Evan R.DeLancey


library(ggplot2)
library(dplyr)
library(stats)
library(MASS)
setwd("~/Desktop/FFA")
d <- read.csv("FF_WeeklyPoints.csv")


setwd("~/Desktop/FFA/Figures")
#------------------------------------------------------------------------------------------------------------
#Edelman - consistent
#------------------------------------------------------------------------------------------------------------
player <- d %>% filter(Player == "Edelman")

#plot actual distribution
png("EdelmanDist.png", height = 700, width = 1200)
ggplot(player, aes(FantasyPoints)) + geom_histogram(binwidth = 2, colour = "#B0B7BC" , fill = "#002244") +
  xlab("Weekly fantasy points (HPPR)") + ggtitle("Edelman weekly point distribution") +
  theme_minimal(base_size = 30, base_family = "Avenir") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#plot modeled player distribution
playerModel <- rbind(player, player[1:4,])
playerModel[,2] <- rnorm(16, mean = mean(player[,2]), sd = sd(player[,2]))
png("EdelmanModelDist.png", height = 700, width = 1200)
ggplot(playerModel, aes(FantasyPoints)) + geom_histogram(binwidth = 2, colour = "#B0B7BC" , fill = "#002244") +
  xlab("Weekly fantasy points (HPPR)") + ggtitle("Edelman MODELED weekly point distribution") +
  theme_minimal(base_size = 30, base_family = "Avenir") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#------------------------------------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------
#Cooper - inconsistent
#------------------------------------------------------------------------------------------------------------
player <- d %>% filter(Player == "Cooper")

C1 <- "#003594"
#plot actual distribution
png("CooperDist.png", height = 700, width = 1200)
ggplot(player, aes(FantasyPoints)) + geom_histogram(binwidth = 2, colour = "#B0B7BC" , fill = C1) +
  xlab("Weekly fantasy points (HPPR)") + ggtitle("Cooper weekly point distribution") +
  theme_minimal(base_size = 30, base_family = "Avenir") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#plot modeled player distribution
playerModel <- rbind(player, player[1,])
playerModel[,2] <- rexp(16, rate = (1/mean(player[,2])))
png("CooperModelDist.png", height = 700, width = 1200)
ggplot(playerModel, aes(FantasyPoints)) + geom_histogram(binwidth = 2, colour = "#B0B7BC" , fill = C1) +
  xlab("Weekly fantasy points (HPPR)") + ggtitle("Cooper MODELED weekly point distribution") +
  theme_minimal(base_size = 30, base_family = "Avenir") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#------------------------------------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------------------------------------







#------------------------------------------------------------------------------------------------------------
#Henry - inconsistent
#------------------------------------------------------------------------------------------------------------
player <- d %>% filter(Player == "Henry")

C1 <- "#418FDE"
#plot actual distribution
png("HenryDist.png", height = 700, width = 1200)
ggplot(player, aes(FantasyPoints)) + geom_histogram(binwidth = 2, colour = "#B0B7BC" , fill = C1) +
  xlab("Weekly fantasy points (HPPR)") + ggtitle("Henry weekly point distribution") +
  theme_minimal(base_size = 30, base_family = "Avenir") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#plot modeled player distribution
playerModel <- rbind(player, player[1,])
playerModel[,2] <- rexp(16, rate = (1/mean(player[,2])))
png("HenryModelDist.png", height = 700, width = 1200)
ggplot(playerModel, aes(FantasyPoints)) + geom_histogram(binwidth = 2, colour = "#B0B7BC" , fill = C1) +
  xlab("Weekly fantasy points (HPPR)") + ggtitle("Henry MODELED weekly point distribution") +
  theme_minimal(base_size = 30, base_family = "Avenir") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#------------------------------------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------------
#Carson - consistent
#------------------------------------------------------------------------------------------------------------
player <- d %>% filter(Player == "Carson")

C1 <- "#69BE28"
#plot actual distribution
png("CarsonDist.png", height = 700, width = 1200)
ggplot(player, aes(FantasyPoints)) + geom_histogram(binwidth = 3, colour = "#B0B7BC" , fill = C1) +
  xlab("Weekly fantasy points (HPPR)") + ggtitle("Carson weekly point distribution") +
  theme_minimal(base_size = 30, base_family = "Avenir") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#plot modeled player distribution
playerModel <- rbind(player, player[1:3,])
playerModel[,2] <- rnorm(16, mean = mean(player[,2]), sd = sd(player[,2]))
png("CarsonModelDist.png", height = 700, width = 1200)
ggplot(playerModel, aes(FantasyPoints)) + geom_histogram(binwidth = 3, colour = "#B0B7BC" , fill = C1) +
  xlab("Weekly fantasy points (HPPR)") + ggtitle("Carson MODELED weekly point distribution") +
  theme_minimal(base_size = 30, base_family = "Avenir") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#------------------------------------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------------------------------------



