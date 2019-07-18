#Fantasy football analysis
##Effect of "boom or bust" players on season win totals
##Evan R.DeLancey


#----------------------------------------------------------------------------------
#Load libraries
#----------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(stats)
library(MASS)
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
#Load data and extract info from data
#----------------------------------------------------------------------------------
setwd("~/Desktop/FFA")
d <- read.csv("FF_WeeklyPoints.csv")
LeagueScores <- read.csv("WeeklyScores2018.csv")
#Get mean and sd of weekly league scores
WeeklyMeanScore <- mean(LeagueScores[,1])
WeeklySDScore <- sd(LeagueScores[,1])
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#PART 1
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
#Build the teams
#----------------------------------------------------------------------------------
#Consisten team
position.C <- c("QB", "RB1", "RB2", "WR1", "WR2", "TE", "FLEX", "K", "DST")
mean.C <-     c(17.8,  17.5,  14.4,  18.7,  14.2,  6.2,    7.1, 8.2,   7.5)
sd.C <-       c(7.2,   5.2,   6.6,    4.3,   3.4,  2.5,    4.5, 2.0,   5.4)
distribution <- rep("normal", 9)
ConsistenTeam <- data.frame(position.C, mean.C, sd.C, distribution)
colnames(ConsistenTeam) <- c("position", "mean", "sd", "distribution")

#Inconsisten team
position.C <- c("QB", "RB1", "RB2", "WR1", "WR2", "TE", "FLEX", "K", "DST")
mean.C <-     c(21.2,  18.6,  12.2,  18.1,   11.9,  6.1,    7.8, 9.2,   6.5)
sd.C <-       c(10.2,   7.2,  12.2,   9.3,   13.0,  5.5,    5.5, 2.3,   5.4)
distribution <- c("normal", "normal", "exponential", "exponential", 
                  "exponential", "normal", "exponential", "normal", 
                  "exponential")
InconsistenTeam <- data.frame(position.C, mean.C, sd.C, distribution)
colnames(InconsistenTeam) <- c("position", "mean", "sd", "distribution")
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
#simulate week fucntion
#----------------------------------------------------------------------------------
simulateWeek <- function(df, oppMean, oppSD) {
  scores <- vector()
  for (i in 1:length(df[,1])) {
    row <- df[i,]
    scores[i] <- if(row[,4] == "normal") {
        rnorm(1, row[,2], row[,3])
      }else{
        rexp(1, (1/row[,2]))
      }
  }
  yourScore <- sum(scores)
  oppScore <- rnorm(1, oppMean, oppSD)
  result <- ifelse(yourScore > oppScore, "W", "L")
  return(data.frame(score=yourScore, opponentScore=oppScore, weekResult=result))
}
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
#Win count for consistent team over 1,000 seasons
#----------------------------------------------------------------------------------
winCount.Consistent <- vector()
ptsFor.Consistent <- vector()
for (i in 1:1000) {
  season <- vector()
  scr <- vector()
  for (j in 1:16) {
    week <- simulateWeek(ConsistenTeam, WeeklyMeanScore, WeeklySDScore)
    season[j] <- as.character(week[,3])
    scr[j] <- week[,1]
  }
  winCount.Consistent[i] <- length(season[season=="W"])
  ptsFor.Consistent[i] <- sum(scr)
}
hist(winCount.Consistent)
mean(winCount.Consistent)
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
#Win count for boom/bust team over 1,000 seasons
#----------------------------------------------------------------------------------
winCount.Inconsistent <- vector()
ptsFor.Inconsistent <- vector()
for (i in 1:1000) {
  season <- vector()
  for (j in 1:16) {
    week <- simulateWeek(InconsistenTeam, WeeklyMeanScore, WeeklySDScore)
    season[j] <- as.character(week[,3])
    scr[j] <- week[,1]
  }
  winCount.Inconsistent[i] <- length(season[season=="W"])
  ptsFor.Inconsistent[i] <- sum(scr)
}
hist(winCount.Inconsistent)
mean(winCount.Inconsistent)
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
#Charting results
#----------------------------------------------------------------------------------
tm1 <- rep("Consistent", 1000)
tm2 <- rep("Boom/bust", 1000)
tm <- c(tm1, tm2)
wns <- c(winCount.Consistent, winCount.Inconsistent)
pf <- c(ptsFor.Consistent, ptsFor.Inconsistent)

charting <- data.frame(
  Team = tm,
  Wins = wns,
  PointsFor = pf
)

setwd("~/Desktop/FFA/Figures")

png("YearlyWins.png", height = 700, width = 1000)
ggplot(charting, aes(x=Team, y=Wins)) + geom_violin(bw=0.5, fill = "#969696") +
  theme_minimal(base_size = 35, base_family = "Avenir") +
  ylab("Wins") + ggtitle("Yearly win distribution - 1,000 seasons") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png("YearlyPoints.png", height = 700, width = 1000)
ggplot(charting, aes(x=Team, y=PointsFor)) + geom_violin(fill = "#969696") +
  theme_minimal(base_size = 35, base_family = "Avenir") +
  ylab("Points for") + ggtitle("Yearly points for distribution  - 1,000 seasons") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#END PART 1
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------








#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#PART 2
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
#Test win difference with different average weekly points
#----------------------------------------------------------------------------------
weeklyPoints <- seq(80,143, by = 2)

totalWinC <- vector()
totalWinIC <- vector()
winDif <- vector()
for (i in 1:length(weeklyPoints)){
  teamAvg <- weeklyPoints[i]
  baseRatio <- teamAvg/WeeklyMeanScore
  
  #adjust team scoring
  newCteam <- ConsistenTeam
  newCteam[,2] <- newCteam[,2] * baseRatio
  newCteam[,3] <- newCteam[,3] * baseRatio
  
  newICteam <- InconsistenTeam
  newICteam[,2] <- newICteam[,2] * baseRatio
  newICteam[,3] <- newICteam[,3] * baseRatio
  
  #new consistent team wins
  winC <- vector()
  for (k in 1:1000) {
    season <- vector()
    for (j in 1:16) {
      week <- simulateWeek(newCteam, WeeklyMeanScore, WeeklySDScore)
      season[j] <- as.character(week[,3])
    }
    winC[k] <- length(season[season=="W"])
  }
  
  #new inconsistent team wins
  winIC <- vector()
  for (k in 1:1000) {
    season <- vector()
    for (j in 1:16) {
      week <- simulateWeek(newICteam, WeeklyMeanScore, WeeklySDScore)
      season[j] <- as.character(week[,3])
    }
    winIC[k] <- length(season[season=="W"])
  }
  totalWinC[i] <- mean(winC)
  totalWinIC[i] <- mean(winIC)
  winDif[i] <- (mean(winC)) - (mean(winIC))
  print(i)
}
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
#Plot change in weekly points versus expected wins
#----------------------------------------------------------------------------------
diffFromMean <- weeklyPoints - WeeklyMeanScore
sWins <- c(totalWinC, totalWinIC)
Team <- c(rep("Consistent",32), rep("Boom/bust",32))
curve.df <- data.frame(
  weeklyPoints = c(weeklyPoints, weeklyPoints),
  diffMean = c(diffFromMean, diffFromMean),
  Wins = sWins,
  Team = Team
)
setwd("~/Desktop/FFA/Figures")
png("WinCurve.png", height = 900, width = 1400)
ggplot(curve.df, aes(x = diffMean, y = Wins, color = Team)) + geom_line(size = 3) +
  xlab("Mean weekly team points \n (difference from league mean)") + ylab("Season win total") +
  theme_minimal(base_size = 45, base_family = "Avenir") +
  scale_y_continuous(breaks=seq(2, 14, 1)) + scale_x_continuous(breaks=seq(-30, 30, 5)) +
  theme(panel.grid.major = element_line(colour = "#F4F4F4"), panel.grid.minor = element_line(colour = "#F4F4F4")) +
  scale_color_manual(name = "", breaks = c("Consistent", "Boom/bust"), values = c("#2171b5", "#41ab5d")) +
  geom_vline(xintercept = 0, color = "#BDBDBD", size = 2)
dev.off()

wDiff <- totalWinC - totalWinIC
diff.df <- data.frame(
  weeklyPoints = weeklyPoints,
  diffMean = diffFromMean,
  diff = wDiff
)

png("WinDifference.png", height = 900, width = 1400)
ggplot(diff.df, aes(x = diffMean, y = diff)) + 
  geom_hline(yintercept = 0, color = "#BDBDBD", size = 2) +
  geom_vline(xintercept = 0, color = "#BDBDBD", size = 2) +
  geom_xspline(color = "#ef3b2c", size = 2.5) +
  xlab("Mean weekly team points \n (difference from league mean)") + ylab("Difference in win total  \n (consistent - boom/bust)") +
  theme_minimal(base_size = 45, base_family = "Avenir") +
  scale_y_continuous(breaks=seq(-1, 2, 0.5)) + scale_x_continuous(breaks=seq(-30, 30, 5)) +
  theme(panel.grid.major = element_line(colour = "#F4F4F4"), panel.grid.minor = element_line(colour = "#F4F4F4"))
dev.off()
  
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
#Estimating chance of winning the championship
#----------------------------------------------------------------------------------
playoffWinCount.C <- vector()
for (i in 1:1000) {
  season <- vector()
  for (j in 1:3) {
    week <- simulateWeek(InconsistenTeam, WeeklyMeanScore, WeeklySDScore)
    season[j] <- as.character(week[,3])
  }
  playoffWinCount.C[i] <- length(season[season=="W"])
}

playoffWinCount.IC <- vector()
for (i in 1:1000) {
  season <- vector()
  for (j in 1:3) {
    week <- simulateWeek(InconsistenTeam, WeeklyMeanScore, WeeklySDScore)
    season[j] <- as.character(week[,3])
  }
  playoffWinCount.IC[i] <- length(season[season=="W"])
}

championRate.C <- (length(playoffWinCount.C[playoffWinCount.C==3]))/1000
championRate.IC <- (length(playoffWinCount.IC[playoffWinCount.IC==3]))/1000
#----------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------





