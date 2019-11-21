Exploratory Data Analysis
================

## Paul Nguyen & David Herrero-Quevedo

``` r
#load necessary libraries
library(nbastatR)
library(tidyverse)
library(BBmisc)
```

## Data Description

### Provenance

We obtained our datasets from basketball-reference.com and
stats.nba.com. We used the R package `nbastatR` to obtain the data from
those
websites.

``` r
players <- bref_players_stats(seasons = 1984:2020, tables = c("advanced", "per_game"), include_all_nba = TRUE)
```

    ## parsed http://www.basketball-reference.com/leagues/NBA_1984_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1985_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1986_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1987_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1988_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1989_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1990_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1991_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1992_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1993_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1994_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1995_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1996_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1997_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1998_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1999_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2000_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2001_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2002_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2003_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2004_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2005_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2006_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2007_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2008_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2009_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2010_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2011_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2012_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2013_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2014_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2015_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2016_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2017_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2018_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2019_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2020_advanced.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1984_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1985_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1986_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1987_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1988_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1989_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1990_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1991_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1992_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1993_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1994_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1995_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1996_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1997_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1998_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_1999_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2000_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2001_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2002_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2003_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2004_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2005_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2006_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2007_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2008_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2009_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2010_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2011_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2012_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2013_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2014_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2015_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2016_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2017_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2018_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2019_per_game.html
    ## parsed http://www.basketball-reference.com/leagues/NBA_2020_per_game.html
    ## Advanced
    ## Assigning NBA player dictionary to df_dict_nba_players to your environment
    ## PerGame

``` r
players_post1984 <- filter(players, yearSeason >= 1984) %>% filter(countGames > 29)
dropping <-c("slugSeason", "idPlayerNBA", "namePlayerBREF", "urlPlayerBREF", "slugPlayerBREF", "urlPlayerThumbnail",
             "urlPlayerHeadshot", "slugPosition","urlPlayerPhoto", "urlPlayerStats", "urlPlayerActionPhoto",
             "yearSeasonFirst","countTeamsPlayerSeason","countGamesStarted","isSeasonCurrent","slugPlayerSeason", "agePlayer",
             "slugTeamBREF", "isHOFPlayer", "slugTeamsBREF")
players_post1984 <- players_post1984[,!(names(players_post1984) %in% dropping)]

for(year in 1984:2020){
  eval(parse(text = paste( "players_",year," <- filter(players_post1984, yearSeason == ",year,")", sep = "" )))
  for(i in 6:48){
    eval(parse(text = paste( "players_",year,"[,",i,"] <- normalize(players_",year,"[,",i,"],method = 'range', range = c(0,1))", sep = "")))
  }
}

players_post1984_normalized <- players_1984
for(year in 1985:2020){
  eval(parse(text = paste( "players_post1984_normalized <- rbind(players_post1984_normalized, players_",year,")", sep = "")))
}
```

### Unit of observation and Variables

Our unit of observation is a player during one season. Our original
dataset consists of 21235 observations and 75 variables, but we’ve
reduced this number to 11695 observations after selecting seasons after
1983 and players that have played in at least 30 games in the season.
We’ve also taken out some unnecessary variables to make a 11695
observation, 55 variable dataset. We have included descriptions of the
variables at the bottom of the page.

## Data Exploration

### Missingness

The reasoning behind choosing seasons after 1983 is in part that we are
not missing any predictor for any observation.

### Univariate analysis of the response

Numerical, graphical (\#TODO: need to complete)

### Bi-, trivariate analysis of the response

graphical (\#TODO: need to complete)

### Variables used:

#### Shooting:

  - pctFTRate: The percentage of total field goal attempts that are Free
    Throws.
  - pct3PRate: The percentage of total field goal attempts that are 3-pt
    shots.
  - pctFG: Field Goal Percentage.
  - pctFG3: Field Goal 3-pt Percentage.
  - pctFG2: Field Goal 2-pt Percentage.
  - pctEFG: Effective Field Goal Percentage. (FGM + (0.5 \* 3PM)/FGA).
    This statistic adjusts for the fact that a 3-pt field goal is worth
    one more point than a regular 2-pt field goal.
  - pctFT: Free Throw Percentage.
  - fgmPerGame: Field Goal Made per Game.
  - fgaPerGame: Field Goal Attempts per game
  - fg3mPerGame: Field Goal Made (3-pt) per game.
  - fg3aPerGame: Field Goal Attempts (3-pt) per game.
  - fg2mPerGame: Field Goal Made (2-pt) per game.
  - fg2aPerGame: Field Goal Attempts (2-pt) per game.
  - ftmPerGame: Free Throws Made per game.
  - ftaPerGame: Free Throw Attempts per game.
  - ptsPerGame: Points per game.

#### Rebounds

  - pctORB: Offensive Rebound Percentage (Rebound Rate).
  - pctTRB: Total Rebound Percentage (Rebound Rate).
  - pctDRB: Deffensive Rebound Percentage (Rebound Rate).
  - orbPerGame: Offensive Rebounds per game.
  - drbPerGame: Deffensive Rebounds per game.
  - trbPerGame: Total Rebounds per game.

#### Passing

  - pctAST: Assists Percentage.
  - pctTOV: Turnover Percentage.
  - astPerGame: Assists per game.
  - tovPerGame: Turnovers per game.

#### Defense

  - pctSTL: Steals percentage.
  - pctBLK: Blocks percentage.
  - stlPerGame: Steals per game.
  - blkPerGame: Blocks per game.
  - pfPerGame: Personal Fouls per game.

#### Efficiency stats

  - ratioOWS: Offensive Win Shares.
  - ratioDWS: Deffensive Win Shares.
  - ratioWS: Win Shares.
  - ratioWSPer48: Win Shares per 48 minutes.
  - ratioOBPM: Offensive Box Plus/Minus.
  - ratioDBPM: Deffensive Box Plus/Minus.
  - ratioBPM: Box Plus/Minus.
  - ratioVORP: Value Over Replacement Player.
  - minutesPerGame: Minutes Per Game
  - pctUSG: Usage Percentage. (100 \* ((FGA + 0.44 \* FTA + TOV) \* (Tm
    MP / 5)) / (MP \* (Tm FGA + 0.44 \* Tm FTA + Tm TOV))
