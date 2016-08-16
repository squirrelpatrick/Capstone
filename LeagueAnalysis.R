# Initialisation
library("dplyr")
setwd("~patrickmcguinness/downloads")

# PHASE 1
# Read in data and format
# saving result as premiership.csv

league98 <- read.csv("E0 (6).csv")
league99 <- read.csv("E0 (7).csv")
league00 <- read.csv("E0 (8).csv")
league01 <- read.csv("E0 (9).csv")
league02 <- read.csv("E0 (10).csv")
league03 <- read.csv("E0 (11).csv")
league04 <- read.csv("E0 (12).csv")
league05 <- read.csv("E0 (13).csv")
league06 <- read.csv("E0 (14).csv")
league07 <- read.csv("E0 (15).csv")
league08 <- read.csv("E0 (16).csv")
league09 <- read.csv("E0 (17).csv")
league10 <- read.csv("E0 (18).csv")
league11 <- read.csv("E0 (19).csv")
league12 <- read.csv("E0 (20).csv")
league13 <- read.csv("E0 (21).csv")
league14 <- read.csv("E0 (22).csv")
league15 <- read.csv("E0 (23).csv")

# keep first 7columns

league98 <- select(league98, 1:7)
league99 <- select(league99, 1:7)
league00 <- select(league00, 1:7)
league01 <- select(league01, 1:7)
league02 <- select(league02, 1:7)
league03 <- select(league03, 1:7)
league04 <- select(league04, 1:7)
league05 <- select(league05, 1:7)
league06 <- select(league06, 1:7)
league07 <- select(league07, 1:7)
league08 <- select(league08, 1:7)
league09 <- select(league09, 1:7)
league10 <- select(league10, 1:7)
league11 <- select(league11, 1:7)
league12 <- select(league12, 1:7)
league13 <- select(league13, 1:7)
league14 <- select(league14, 1:7)
league15 <- select(league15, 1:7)

# add season column

league98$Season <- 1
league99$Season <- 2
league00$Season <- 3
league01$Season <- 4
league02$Season <- 5
league03$Season <- 6
league04$Season <- 7
league05$Season <- 8
league06$Season <- 9
league07$Season <- 10
league08$Season <- 11
league09$Season <- 12
league10$Season <- 13
league11$Season <- 14
league12$Season <- 15
league13$Season <- 16
league14$Season <- 17
league15$Season <- 18

# join data with rbind

premiership <- rbind(league98, league99, league00, league01, league02,
                     league03, league04, league05, league06, league07,
                     league08, league09, league10, league11, league12,
                     league13, league14, league15)

# remove blank rows
premiership <- premiership %>%
                filter(Div == "E0")

# put season column as column2

premiership <- premiership[,c(8,1:7)]

# write to file
write.csv(premiership, file = "premiership.csv")

# PHASE2
# output file -> premiership2.csv

# We are going to recreate the league table as prior to each match
# As a snapshot for each team
# labelling home team as Team1
# away team as Team2
# to avoid confusion as we will have home and away 
# records for each of the teams

# looping through each of the seasons, pulling the data
# then adding the converted data to new file

premiership2 <- 0
for (s in 1:18) {
    data <- filter(premiership, Season == s)
    print(s)
  
# create index

Index <- as.integer(rownames(data))
data2 <- data.frame(Index, data)


# create
# Team1 Home W, D, L, GF, GA,
# Team2 Away W, D, L, GF, GA  
# with cumulative sums / dplyr

data2 <- data %>%
  group_by(HomeTeam) %>%
   mutate(Team1P = 0,
          Team1HomeW  = cumsum(FTR == "H") - (FTR == "H"),
          Team1HomeD  = cumsum(FTR == "D") - (FTR == "D"),
          Team1HomeL  = cumsum(FTR == "A") - (FTR == "A"),
          Team1HomeGF = cumsum(FTHG) - FTHG, 
          Team1HomeGA = cumsum(FTAG) - FTAG,
          Team1AwayW = 0,
          Team1AwayD = 0,
          Team1AwayL = 0,
          Team1AwayGF = 0,
          Team1AwayGA = 0,
          Team1GD = 0,
          Team1Pts = 0) %>%
  group_by(AwayTeam) %>%
  mutate( Team2P = 0,
          Team2HomeW = 0,
          Team2HomeD = 0,
          Team2HomeL = 0,
          Team2HomeGF = 0,
          Team2HomeGA = 0,
          Team2AwayW  = cumsum(FTR == "A") - (FTR == "A"),
          Team2AwayD  = cumsum(FTR == "D") - (FTR == "D"),
          Team2AwayL  = cumsum(FTR == "H") - (FTR == "H"),
          Team2AwayGF = cumsum(FTAG) - FTAG,
          Team2AwayGA = cumsum(FTHG) - FTHG,
          Team2GD = 0,
          Team2Pts = 0
          )

# Loop through the match index to pull the other half of the table
# ie away records for Team1, home records for the Team2



for(i in 2:nrow(data2)) {
  for (j in (i-1):1) {
      if (data2$HomeTeam[i] == data2$AwayTeam[j]) {
        data2$Team1AwayW[i]  <- data2$Team2AwayW[j]  + (data2$FTR[j] == "A")
        data2$Team1AwayD[i]  <- data2$Team2AwayD[j]  + (data2$FTR[j] == "D")
        data2$Team1AwayL[i]  <- data2$Team2AwayL[j]  + (data2$FTR[j] == "H")
        data2$Team1AwayGF[i] <- data2$Team2AwayGF[j] + data2$FTAG[j]
        data2$Team1AwayGA[i] <- data2$Team2AwayGA[j] + data2$FTHG[j]
        break()
      }
  }
  for (j in (i-1):1) {
    if (data2$AwayTeam[i] == data2$HomeTeam[j]) {
      data2$Team2HomeW[i]  <- data2$Team1HomeW[j]  + (data2$FTR[j] == "H")
      data2$Team2HomeD[i]  <- data2$Team1HomeD[j]  + (data2$FTR[j] == "D")
      data2$Team2HomeL[i]  <- data2$Team1HomeL[j]  + (data2$FTR[j] == "A")
      data2$Team2HomeGF[i] <- data2$Team1HomeGF[j] + data2$FTHG[j]
      data2$Team2HomeGA[i] <- data2$Team1HomeGA[j] + data2$FTAG[j]
      break()
    }
  }
}

# complete Played, Goal Difference and Points Variables

data2$Team1P <- data2$Team1HomeW + 
                data2$Team1HomeD + 
                data2$Team1HomeL +
                data2$Team1AwayW +
                data2$Team1AwayD +
                data2$Team1AwayL

data2$Team1GD <- data2$Team1HomeGF +
                 data2$Team1AwayGF -
                 data2$Team1HomeGA -
                 data2$Team1AwayGA

data2$Team1Pts <- data2$Team1HomeW * 3 + data2$Team1HomeD +
                  data2$Team1AwayW * 3 + data2$Team1AwayD

data2$Team2P <- data2$Team2HomeW + 
                data2$Team2HomeD + 
                data2$Team2HomeL +
                data2$Team2AwayW +
                data2$Team2AwayD +
                data2$Team2AwayL

data2$Team2GD <-  data2$Team2HomeGF +
                  data2$Team2AwayGF -
                  data2$Team2HomeGA -
                  data2$Team2AwayGA


data2$Team2Pts <- data2$Team2HomeW * 3 + data2$Team2HomeD +
                  data2$Team2AwayW * 3 + data2$Team2AwayD
# add augmented data to premiership2

if (s == 1) {premiership2 <- data2} else {
  premiership2 <- rbind(premiership2, data2)
}

# end for loop for seasons
}

# write to file
write.csv(premiership2, file = "premiership2.csv")

# PHASE 3
# Create End of Season Tables
# Output File -> final_tables.csv

final_tables_home <-  premiership2 %>%
                      group_by(Season, HomeTeam) %>%
                      summarise(Div = "E0", HomePlayed = n(), HomeW = sum(FTR == "H"), 
                      HomeD = sum(FTR == "D"), HomeL = sum(FTR == "A"), HomeGF = sum(FTHG), 
                      HomeGA = sum(FTAG)
                      )

final_tables_away <-  premiership2 %>%
                      group_by(Season, AwayTeam) %>%
                      summarise(Div = "E0", AwayPlayed = n(), AwayW = sum(FTR == "A"), 
                      AwayD = sum(FTR == "D"), AwayL = sum(FTR == "H"), AwayGF = sum(FTAG), 
                      AwayGA = sum(FTHG)
                      )
final_tables_home <-  final_tables_home %>%
                      rename(Team = HomeTeam)

final_tables_away <-  final_tables_away %>%
                      rename(Team = AwayTeam)

# merge tables and format

final_tables <- inner_join(final_tables_home, final_tables_away)
final_tables <- final_tables %>%
                mutate(Played = HomePlayed + AwayPlayed, 
                       GF = HomeGF + AwayGF, 
                       GA = HomeGA + AwayGA, 
                       GD = GF - GA,
                       Pts = 3 * (HomeW + AwayW) + HomeD + AwayD) %>%
                arrange(Season, desc(Pts)) %>%
                group_by(Season) %>%
                mutate(Rank = min_rank(-Pts))

final_tables <- final_tables[,c(3,1,21,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]

# write to file

write.csv(final_tables, file = "final_tables.csv")


# create features for analysis

# simple to start with
# ratios of H/D/L games won for home team and away team in current season

premiership2 <- premiership2 %>%
  mutate(Team1HomeP = Team1HomeW + Team1HomeD + Team1HomeL,
          Team1AwayP = Team1AwayW + Team1AwayD + Team1AwayL,
          Team2HomeP = Team2HomeW + Team2HomeD + Team2HomeL,                 
          Team2AwayP = Team2AwayW + Team2AwayD + Team2AwayL,
          T1HWr = Team1HomeW / Team1HomeP, # 'Team1HomeWinRatio'
          T1HDr = Team1HomeD / Team1HomeP, # 'Team1HomeDrawRatio'
          T1HLr = Team1HomeL / Team1HomeP, # 'Team1HomeLossRatio'
          T1AWr = Team1AwayW / Team1AwayP, # 'Team1AwayWinRatio'
          T1ADr = Team1AwayD / Team1AwayP, # 'Team1AwayDrawRatio'
         
          T1ALr = Team1AwayL / Team1AwayP, # 'Team1AwayLossRatio'
          T2HWr = Team2HomeW / Team2HomeP, # 'Team2HomeWinRatio'
          T2HDr = Team2HomeD / Team2HomeP, # 'Team2HomeDrawRatio'
          T2HLr = Team2HomeL / Team2HomeP, # 'Team2HomeLossRatio'
          T2AWr = Team2AwayW / Team2AwayP, # 'Team2AwayWinRatio'
          T2ADr = Team2AwayD / Team2AwayP, # 'Team2AwayDrawTaio'
          T2ALr = Team2AwayL / Team2AwayP  # 'Team2AwayLossRatio'
  )

# create dummy variables
premiership2 <- premiership2 %>%
        mutate(ResH = as.integer(FTR == "H"), # 'Home Win'
               ResD = as.integer(FTR == "D"), # 'Draw'
               ResA = as.integer(FTR == "A")  # 'Away Win'
        )

# attempt first logistic regression!
model0 <- glm(ResH ~ 0, data = premiership2, family = "binomial")
model1 <- glm(ResH ~ T1HWr, data = premiership2, family = "binomial")
model2 <- glm(ResH ~ T1HWr + T2ALr, data = premiership2, family = "binomial")
model3 <- glm(ResH ~ T1HWr + T1HDr + T1AWr + T1ADr + T2HLr + T2HDr + T2ALr + T2ADr, data = premiership2, family = "binomial")
premiershipF5 <- filter(premiership2, Team1P >= 5, Team2P >= 5)
model4 <- glm(ResH ~ T1HWr + T1HDr + T1AWr + T1ADr + T2HLr + T2HDr + T2ALr + T2ADr, data = premiershipF5, family = "binomial")
premiershipF10 <- filter(premiership2, Team1P >= 10, Team2P >= 10)
model5 <- glm(ResH ~ T1HWr + T1HDr + T1AWr + T1ADr + T2HLr + T2HDr + T2ALr + T2ADr, data = premiershipF10, family = "binomial")
premiershipF15 <- filter(premiership2, Team1P >= 15, Team2P >= 15)
model6 <- glm(ResH ~ T1HWr + T1HDr + T1AWr + T1ADr + T2HLr + T2HDr + T2ALr + T2ADr, data = premiershipF15, family = "binomial")
premiershipF20 <- filter(premiership2, Team1P >= 15, Team2P >= 15)
model7 <- glm(ResH ~ T1HWr + T1HDr + T1AWr + T1ADr + T2HLr + T2HDr + T2ALr + T2ADr, data = premiershipF20, family = "binomial")

# model 6 & 7 look pretty similar
predict(model7, premiership2[6827,], type="response")

#interesting result that the away form of the away team
# appears more significant than the other weightings

# attempting to incorporate goal differences
# create ratio columns

premiership2 <- premiership2 %>%
                mutate(T1GDr = Team1GD/Team1P,
                       T2GDr = Team2GD/Team1P
                       )

#run filter again
premiershipF15 <- filter(premiership2, Team1P >= 15, Team2P >= 15)

model8 <- glm(ResH ~ T1HWr + T1HDr + T1AWr + T1ADr + 
              T2HLr + T2HDr + T2ALr + T2ADr + 
                T1GDr + T2GDr,
              data = premiershipF15, family = "binomial")

# interesting in that GD appears to be more important to measure Team1 form than win/draw %
# see what happens when removing win/draw % for team1

model9 <- glm(ResH ~ T2HLr + T2HDr + T2ALr + T2ADr + T1GDr + T2GDr,
              data = premiershipF15, family = "binomial")


# lower AIC!

# try relying on goal difference for Team2
model10 <- glm(ResH ~ T1GDr + T2GDr,
              data = premiershipF15, family = "binomial")

seems best!

# let's plot the results
library(ggplot2)
ggplot(premiership2, aes(x = T1GDr, y = T2GDr, col = ResH)) +
  geom_point()

# nice but plot not too clear. Let's do a 2D probability density plot using the hexbin package.
install.packages("hexbin")
library(hexbin)
my.colors <- function (n) {
  rev(heat.colors(n))
}
hexbinplot(T1GDr ~ T2GDr, data = premiership2, colramp = my.colors, 
           colorcut = seq(0, 1, length = 10))

# that didn't work :D! We don't want a count of the points, 
# want the aggregated probability of success in the region


