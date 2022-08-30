install.packages("sqldf")
library('sqldf')


# Analyzes advanced NBA statistics of all teams and players from the 2022 playoffs
playerstats <- read_csv("Downloads/playerstats.csv")
teamstats <- read_csv("Downloads/teamstats.csv")


# Creates a lineup of five players, one at each position, who each have the highest PER at their position
PERteam <- sqldf("SELECT Player, Pos, Tm, MAX(PER) AS Player_Efficiency_Rating 
                 FROM playerstats
                 GROUP BY Pos
                 ORDER BY PER DESC
                 ")


# Creates a lineup of five players, one at each position, who each have the most winshares at their position
WSteam <- sqldf("SELECT Player, Pos, Tm, MAX(WS) AS Winshares 
                 FROM playerstats
                 GROUP BY Pos
                 ORDER BY WS DESC
                 ")


# Displays each of the players on the team with the most wins and orders them by their winshares descending
Max_contributions_to_most_wins <- sqldf("SELECT Player, teamstats.Tm as Team, MAX(teamstats.W) As Wins, playerstats.WS AS Winshares 
                 FROM playerstats
                 JOIN teamstats
                 ON playerstats.Tm = teamstats.Tm AND teamstats.TM = 'GSW'
                 GROUP BY playerstats.Player
                 ORDER BY playerstats.WS DESC
                 ")


# Selects all players with a negative BPM score 
Below_avg_bpm <- sqldf("SELECT PLAYER, Pos, Tm, BPM
                       FROM playerstats
                       WHERE BPM LIKE '%-%'
                       ")


# Graphs Usage Rate vs Player Efficiency for all players and shows where majority of
# the points are concentrated
library(hexbin)
x <- playerstats$`USG%`
y <- playerstats$PER
bin <- hexbin(x, y, xbins = 50)
plot(bin, main = "Usage Vs PER", xlab = "Usage Rate", ylab = "PER")


# Plots Age vs Pace along with the regression line to show correlation
attach(teamstats)
plot(Age, Pace, main="Age vs Pace",
     xlab = "Age", ylab = "Pace",
     abline(lm(Pace ~ Age), col="red"))


# Joins teamstats and playerstats based off team and compares PER, Pace, TS%, and W
# along with trend lines and correlation
Combined_graph <- sqldf("SELECT playerstats.PER, teamstats.Pace, Playerstats.'TS%', teamstats.W as Wins 
                 FROM playerstats
                 JOIN teamstats
                 ON playerstats.Tm = teamstats.Tm 
                 ")


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  tx <- format(c(r, 0.123456789), digits = digits)[1]
  tx <- paste(prefix, tx, sep = "")
  if (missing(cex.cor)) cex.cor <- 0.8/strwidth(tx)
  text(0.5, 0.5, tx, cex =  cex.cor * (1 + r) / 2)
}


pairs(
  Combined_graph[,1:4],
  upper.panel = panel.cor,
  lower.panel = panel.smooth
)











