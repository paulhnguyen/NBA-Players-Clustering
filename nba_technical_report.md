NBA Modern Positions
================

``` r
#load necessary libraries
library(nbastatR)
library(tidyverse)
library(BBmisc)
library(corrplot)
library(gganimate)
library(plotly)
library(ggrepel)
```

## Abstract

In this project, my group will describe the changes in playstyle types
throughout the NBA’s history. We will perform PCA analysis to determine
the essential traits for different players, and then use important
principal components and unsupervised learning techinques to cluster
players into different “types” and see the movement in the proportions
of types over time.

## Introduction

The NBA, initially created in 1946, has seen much change throughout its
history, for example, the introduction of the 3 point line in the 79/80
season and the elimination of hand checking in 1990. These changes have
altered the playstyles of many teams; some players, especially former
players, have critisized these changes, such as the reliance of many
teams on jump shooting. If you look at the Houston Rocket’s shotchart,
you may notice huge regions of white space away from the basket and the
3 point line in order to maximize the efficiency of their shots. We
expect to see a steady increase in 3 point shooting from when the 3
point line was introduced in the NBA. We also expect to see a decrease
in big, slow players that may have trouble defending these shots, and a
decrease in players that specialize in shooting the midrange.

### The data

We obtained our datasets from basketball-reference.com and
stats.nba.com. We used the R package `nbastatR` to obtain the data from
those websites.

#### Unit of observation and Variables

Our unit of observation is a player during one season, for example,
Stephen Curry in 2018. Our original dataset consists of 21235
observations and 75 variables, but we’ve reduced this number to 10223
observations after selecting seasonsafter 1983 and players that have
played in at least 600 minutes in the season. We’ve also taken out some
unnecessary variables to make a 11695 observation, 49 variable dataset.

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

  - ratioPER: Player Efficiency Rating
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

#### Other

  - namePlayer: Name of player
  - groupPosition: Player Position. Either Guard, Forward, or Center
  - yearSeason: Season year
  - countGames: Games played during season
  - minutes: Minutes played during season
  - isAllNBA: Did player make season’s all NBA team?

<!-- end list -->

``` r
# Gets player stats from 1984 to 2020.
players_post1984 <- bref_players_stats(seasons = 1984:2020, tables = c("advanced", "per_game"), include_all_nba = TRUE, return_message = FALSE)
```

    ## Advanced
    ## Assigning NBA player dictionary to df_dict_nba_players to your environment
    ## PerGame

``` r
# Filters by amount of time they played.
ggplot(players_post1984, mapping = aes(x = minutes)) + 
   geom_histogram() +
   geom_vline(xintercept = 600, color = "tomato")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
 players_post1984 <- filter(players_post1984, minutes > 600)

 # Variables to drop
dropping <-c("slugSeason", "idPlayerNBA", "namePlayerBREF", "urlPlayerBREF",
             "slugPlayerBREF", "urlPlayerThumbnail", "urlPlayerHeadshot",
             "slugPosition","urlPlayerPhoto", "urlPlayerStats",
             "urlPlayerActionPhoto","yearSeasonFirst",
             "countTeamsPlayerSeason","countGamesStarted",
             "isSeasonCurrent","slugPlayerSeason", "agePlayer",
             "slugTeamBREF", "isHOFPlayer", "slugTeamsBREF", 
             "countTeamsPlayerSeasonPerGame", "groupAllNBA",
             "numberAllNBATeam", "isAllNBA2", "isAllNBA1",
             "isAllNBA3" )
# Dropping variables
players_post1984 <- players_post1984[,!(names(players_post1984) %in% dropping)]

# Groups players by season, and normalizes by season.
for(year in 1984:2020){
  eval(parse(text = paste( "players_",year," <- filter(players_post1984, yearSeason == ",year,")", sep = "" )))
  for(i in 6:48){
    eval(parse(text = paste( "players_",year,"[,",i,"] <- normalize(players_",year,"[,",i,"],method = 'range', range = c(0,1))", sep = "")))
  }
}

# Ungroups players by season.
players_post1984_normalized <- players_1984
for(year in 1985:2020){
  eval(parse(text = paste( "players_post1984_normalized <- rbind(players_post1984_normalized, players_",year,")", sep = "")))
}
players_post1984_all_nba_normalized <- players_post1984_normalized %>% filter(isAllNBA == TRUE)
```

## Exploratory Data Analysis

``` r
#some summary statistics
summarystatistics1984 <- players_post1984 %>%
  group_by(yearSeason, groupPosition) %>%
  summarize(meanpctFTRate = mean(pctFTRate),
            meanpct3PRate = mean(pct3PRate),
            meanpctFG = mean(pctFG),
            meanpctFG3 = mean(pctFG3),
            meanpctFG2 = mean(pctFG2),
            meanpctEFG = mean(pctEFG),
            meanpctFT = mean(pctFT),
            meanfgmPerGame = mean(fgmPerGame),
            meanfgaPerGame = mean(fgaPerGame),
            meanfg3mPerGame = mean(fg3mPerGame),
            meanfg3aPerGame = mean(fg3aPerGame),
            meanfg2mPerGame = mean(fg2mPerGame),
            meanfg2aPerGame = mean(fg2aPerGame),
            meanftmPerGame = mean(ftmPerGame),
            meanftaPerGame = mean(ftaPerGame),
            meanptsPerGame = mean(ptsPerGame),
            meanpctORB = mean(pctORB),
            meanpctTRB = mean(pctTRB),
            meanpctDRB = mean(pctDRB),
            meanorbPerGame = mean(orbPerGame),
            meandrbPerGame = mean (drbPerGame),
            meantrbPerGame = mean(trbPerGame),
            meanpctAST = mean(pctAST),
            meanpctTOV = mean(pctTOV),
            meanastPerGame = mean(astPerGame),
            meantovPerGame = mean(tovPerGame),
            meanpctSTL = mean(pctSTL),
            meanpctBLK = mean(pctBLK),
            meanstlPerGame = mean(stlPerGame),
            meanblkPerGame = mean(blkPerGame),
            meanpfPerGame = mean(pfPerGame),
            meanratioOWS = mean(ratioOWS),
            meanratioDWS = mean(ratioDWS),
            meanratioWS = mean(ratioWS),
            meanratioWSPer48 = mean(ratioWSPer48),
            meanratioOBPM = mean(ratioOBPM),
            meanratioDBPM = mean(ratioDBPM),
            meanratioBPM = mean(ratioBPM),
            meanratioVORP = mean(ratioVORP),
            sdpctFTRate = sd(pctFTRate),
            sdpct3PRate = sd(pct3PRate),
            sdpctFG = sd(pctFG),
            sdpctFG3 = sd(pctFG3),
            sdpctFG2 = sd(pctFG2),
            sdpctEFG = sd(pctEFG),
            sdpctFT = sd(pctFT),
            sdfgmPerGame = sd(fgmPerGame),
            sdfgaPerGame = sd(fgaPerGame),
            sdfg3mPerGame = sd(fg3mPerGame),
            sdfg3aPerGame = sd(fg3aPerGame),
            sdfg2mPerGame = sd(fg2mPerGame),
            sdfg2aPerGame = sd(fg2aPerGame),
            sdftmPerGame = sd(ftmPerGame),
            sdftaPerGame = sd(ftaPerGame),
            sdptsPerGame = sd(ptsPerGame),
            sdpctORB = sd(pctORB),
            sdpctTRB = sd(pctTRB),
            sdpctDRB = sd(pctDRB),
            sdorbPerGame = sd(orbPerGame),
            sddrbPerGame = sd (drbPerGame),
            sdtrbPerGame = sd(trbPerGame),
            sdpctSTL = sd(pctSTL),
            sdpctBLK = sd(pctBLK),
            sdstlPerGame = sd(stlPerGame),
            sdblkPerGame = sd(blkPerGame),
            sdpfPerGame = sd(pfPerGame),
            sdratioOWS = sd(ratioOWS),
            sdratioDWS = sd(ratioDWS),
            sdratioWS = sd(ratioWS),
            sdratioWSPer48 = sd(ratioWSPer48),
            sdratioOBPM = sd(ratioOBPM),
            sdratioDBPM = sd(ratioDBPM),
            sdratioBPM = sd(ratioBPM),
            sdratioVORP = sd(ratioVORP)
            )
head(summarystatistics1984)
```

    ## # A tibble: 6 x 76
    ## # Groups:   yearSeason [2]
    ##   yearSeason groupPosition meanpctFTRate meanpct3PRate meanpctFG meanpctFG3
    ##        <dbl> <chr>                 <dbl>         <dbl>     <dbl>      <dbl>
    ## 1       1984 C                     0.394       0.00437     0.512     0.0709
    ## 2       1984 F                     0.366       0.0154      0.494     0.128 
    ## 3       1984 G                     0.292       0.0497      0.470     0.215 
    ## 4       1985 C                     0.406       0.00487     0.505     0.0232
    ## 5       1985 F                     0.348       0.0202      0.490     0.129 
    ## 6       1985 G                     0.274       0.0665      0.478     0.239 
    ## # … with 70 more variables: meanpctFG2 <dbl>, meanpctEFG <dbl>,
    ## #   meanpctFT <dbl>, meanfgmPerGame <dbl>, meanfgaPerGame <dbl>,
    ## #   meanfg3mPerGame <dbl>, meanfg3aPerGame <dbl>, meanfg2mPerGame <dbl>,
    ## #   meanfg2aPerGame <dbl>, meanftmPerGame <dbl>, meanftaPerGame <dbl>,
    ## #   meanptsPerGame <dbl>, meanpctORB <dbl>, meanpctTRB <dbl>, meanpctDRB <dbl>,
    ## #   meanorbPerGame <dbl>, meandrbPerGame <dbl>, meantrbPerGame <dbl>,
    ## #   meanpctAST <dbl>, meanpctTOV <dbl>, meanastPerGame <dbl>,
    ## #   meantovPerGame <dbl>, meanpctSTL <dbl>, meanpctBLK <dbl>,
    ## #   meanstlPerGame <dbl>, meanblkPerGame <dbl>, meanpfPerGame <dbl>,
    ## #   meanratioOWS <dbl>, meanratioDWS <dbl>, meanratioWS <dbl>,
    ## #   meanratioWSPer48 <dbl>, meanratioOBPM <dbl>, meanratioDBPM <dbl>,
    ## #   meanratioBPM <dbl>, meanratioVORP <dbl>, sdpctFTRate <dbl>,
    ## #   sdpct3PRate <dbl>, sdpctFG <dbl>, sdpctFG3 <dbl>, sdpctFG2 <dbl>,
    ## #   sdpctEFG <dbl>, sdpctFT <dbl>, sdfgmPerGame <dbl>, sdfgaPerGame <dbl>,
    ## #   sdfg3mPerGame <dbl>, sdfg3aPerGame <dbl>, sdfg2mPerGame <dbl>,
    ## #   sdfg2aPerGame <dbl>, sdftmPerGame <dbl>, sdftaPerGame <dbl>,
    ## #   sdptsPerGame <dbl>, sdpctORB <dbl>, sdpctTRB <dbl>, sdpctDRB <dbl>,
    ## #   sdorbPerGame <dbl>, sddrbPerGame <dbl>, sdtrbPerGame <dbl>, sdpctSTL <dbl>,
    ## #   sdpctBLK <dbl>, sdstlPerGame <dbl>, sdblkPerGame <dbl>, sdpfPerGame <dbl>,
    ## #   sdratioOWS <dbl>, sdratioDWS <dbl>, sdratioWS <dbl>, sdratioWSPer48 <dbl>,
    ## #   sdratioOBPM <dbl>, sdratioDBPM <dbl>, sdratioBPM <dbl>, sdratioVORP <dbl>

``` r
ggplot(data = summarystatistics1984, mapping = aes(x = yearSeason, y = meanpctFG, color = groupPosition)) + 
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#taking a look at some stats over time, grouped by year and position
#fg pct
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanpctFG, color = groupPosition)) +  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
#3FGPct, see the introduction of 3 point line
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanpctFG3,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
#meanFGM
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanfgmPerGame,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
#meanFGA
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanfgaPerGame,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

``` r
#mean3FGM 
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanfg3mPerGame,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-6.png)<!-- -->

``` r
#mean3FGA
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanfg3aPerGame, color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-7.png)<!-- -->

``` r
#meanFTA
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanftaPerGame, 
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-8.png)<!-- -->

``` r
#meanFTM
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanftmPerGame,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-9.png)<!-- -->

``` r
#meanORB
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanorbPerGame,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-10.png)<!-- -->

``` r
#meanDRB
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meandrbPerGame,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-11.png)<!-- -->

``` r
#meanAST
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanastPerGame, 
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-12.png)<!-- -->

``` r
#meanSTL
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanstlPerGame,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-13.png)<!-- -->

``` r
#meanBLK
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanblkPerGame,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-14.png)<!-- -->

``` r
#meanTOV
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meantovPerGame, 
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-15.png)<!-- -->

``` r
#meanPTS
ggplot(data = summarystatistics1984, 
       mapping = aes(x = yearSeason, y = meanptsPerGame,
                     color = groupPosition)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-16.png)<!-- -->

``` r
#also a correlation matrix plot
rcorr <- round(cor(players_post1984_normalized[,c(5:48)]),2)
corrplot(rcorr, method="color")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-3-17.png)<!-- -->

## Modeling

``` r
pca <- prcomp(players_post1984_normalized[,-c(1:5, 49)])
d <- as.data.frame(pca$x)
pc1 <- pca$rotation[, 1]
pc1
```

    ##        ratioPER pctTrueShooting       pct3PRate       pctFTRate          pctORB 
    ##   -0.1984170030   -0.1098251655    0.0457894087   -0.0653791751    0.0008296429 
    ##          pctDRB          pctTRB          pctAST          pctSTL          pctBLK 
    ##   -0.0708471347   -0.0617375414   -0.0725301436    0.0238124679    0.0219014529 
    ##          pctTOV          pctUSG        ratioOWS        ratioDWS         ratioWS 
    ##    0.0469486995   -0.1559844693   -0.1735141039   -0.1599903992   -0.1981977758 
    ##    ratioWSPer48       ratioOBPM       ratioDBPM        ratioBPM       ratioVORP 
    ##   -0.1546449814   -0.1714358310   -0.0542357833   -0.1724266162   -0.1729094230 
    ##           pctFG          pctFG3          pctFG2          pctEFG           pctFT 
    ##   -0.0892980629   -0.0296739705   -0.0870225890   -0.0810719726   -0.0436442204 
    ##  minutesPerGame      fgmPerGame      fgaPerGame     fg3mPerGame     fg3aPerGame 
    ##   -0.2996746855   -0.2520668299   -0.2392597403   -0.0736958096   -0.0760735708 
    ##     fg2mPerGame     fg2aPerGame      ftmPerGame      ftaPerGame      orbPerGame 
    ##   -0.2396351958   -0.2383603309   -0.2097406616   -0.2065354032   -0.1245996424 
    ##      drbPerGame      trbPerGame      astPerGame      stlPerGame      blkPerGame 
    ##   -0.1747619007   -0.1673664685   -0.1195779000   -0.1460398184   -0.0817928380 
    ##      tovPerGame       pfPerGame      ptsPerGame 
    ##   -0.2129638577   -0.1379736128   -0.2475416157

``` r
pc2 <- pca$rotation[, 2]
pc2
```

    ##        ratioPER pctTrueShooting       pct3PRate       pctFTRate          pctORB 
    ##     0.008191871     0.024748503    -0.302846306     0.132760494     0.083987925 
    ##          pctDRB          pctTRB          pctAST          pctSTL          pctBLK 
    ##     0.273996670     0.316031526    -0.176986740     0.124383514    -0.238601597 
    ##          pctTOV          pctUSG        ratioOWS        ratioDWS         ratioWS 
    ##     0.046919899    -0.072367036    -0.032279075     0.069300408     0.000978460 
    ##    ratioWSPer48       ratioOBPM       ratioDBPM        ratioBPM       ratioVORP 
    ##     0.036979806    -0.108603095     0.166393788     0.003523136    -0.008902242 
    ##           pctFG          pctFG3          pctFG2          pctEFG           pctFT 
    ##     0.160328184    -0.339221194     0.098310649     0.041026647    -0.148400102 
    ##  minutesPerGame      fgmPerGame      fgaPerGame     fg3mPerGame     fg3aPerGame 
    ##    -0.076619320    -0.060939806    -0.101065469    -0.279414508    -0.290015636 
    ##     fg2mPerGame     fg2aPerGame      ftmPerGame      ftaPerGame      orbPerGame 
    ##     0.016448699    -0.005909921    -0.021397230     0.007780478     0.224691897 
    ##      drbPerGame      trbPerGame      astPerGame      stlPerGame      blkPerGame 
    ##     0.153827914     0.187210404    -0.149347458    -0.109547155     0.152157009 
    ##      tovPerGame       pfPerGame      ptsPerGame 
    ##    -0.054004976     0.133903897    -0.074649155

``` r
pca3 <- pca$rotation[, 3]
d$namePlayer <- players_post1984_normalized$namePlayer
d$isAllNBA <- players_post1984_normalized$isAllNBA
d$yearSeason <- players_post1984_normalized$yearSeason

pcaplot <- ggplot(d, aes(x = PC1, y = PC2)) +
  geom_point(size = .5, alpha = .7) +
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage")
pcaplot
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
d1 <- tibble(PC = 1:43,
                PVE = pca$sdev^2 /
                  sum(pca$sdev^2))

ggplot(d1, aes(x = PC, y = PVE)) +
  geom_line() + 
  geom_point()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
#some PCA exploration
pca_rotations <- data.frame(pca$rotation)
pca_rotations$variables <- rownames(pca_rotations)
PC1graph <- 
  ggplot(data = pca_rotations, mapping = aes(x = variables, y = PC1, 
                                          fill = variables)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Usage (-) vs Non-Usage (+)")
PC1graph
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
#note important characteristics: tov, pts, minutes, fgm, fga
PC2graph <- 
  ggplot(data = pca_rotations, mapping = aes(x = variables, y = PC2, 
                                          fill = variables)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")+
  labs(title = "Shooters (-) vs Baseline(+)")
PC2graph
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
#note important characteristics: (+) rebounding stats, (-) 3pt shooting, asts
PC3graph <- 
  ggplot(data = pca_rotations, mapping = aes(x = variables, y = PC3, 
                                          fill = variables)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Outside the Arc (-) vs Inside the Arc (+)")
PC3graph
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

``` r
#important characteristics: (+) turnovers, usage, field goals worth 2
#                            (-) True shooting, EFG, 3 pointers

#lets now create a plot with some players that we will recognize
currentplayersplot <- ggplot(d, aes(x = PC1, y = PC2)) +
  geom_point(size = .5, alpha = .1) +
  geom_text_repel(data = subset(d, isAllNBA == TRUE & 
                            yearSeason %in% 2017:2019 ),
            aes(label = namePlayer)) +
  geom_point(data = subset(d, isAllNBA == TRUE & 
                            yearSeason %in% 2017:2019), 
             mapping = aes(x = PC1, y = PC2),
             size = .5, alpha = .7, color = "purple2") +
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage")
currentplayersplot
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->

``` r
famousplayersplot <-  ggplot(d, aes(x = PC1, y = PC2)) +
  geom_point(size = .5, alpha = .1) +
  geom_text_repel(data = subset(d, namePlayer %in% c("Lebron James", 
                                               "Michael Jordan",
                                               "Stephen Curry",
                                               "Kobe Bryant",
                                               "Tim Duncan",
                                               "Magic Johnson",
                                               "Kevin Durant")),
            aes(label = namePlayer)) +
  geom_point(color = "purple",
             data = subset(d, namePlayer %in% c("Lebron James", 
                                               "Michael Jordan",
                                               "Stephen Curry",
                                               "Kobe Bryant",
                                               "Tim Duncan",
                                               "Magic Johnson",
                                               "Kevin Durant"),
             size = .5, alpha = .6)) +
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage")
famousplayersplot
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->

``` r
#animated plot, allstars throughout the seasons
allnbaplayerspca <- d %>%
  filter(isAllNBA == TRUE)
#staticplot
staticplot <- ggplot(data = allnbaplayerspca, 
                     mapping = aes(x = PC1, y = PC2)) +
  geom_point(alpha = .8, color = "purple") + 
  geom_text_repel(data = allnbaplayerspca, 
                  mapping = aes(label = namePlayer)) +
  geom_point(data = d, mapping = aes(x = PC1, y = PC2), alpha = .1) +
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage")
staticplot
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->

``` r
#byseason
byseasonplot <- staticplot + transition_time(yearSeason) +
  labs(title = "Season: {frame_time}")

animate(byseasonplot, renderer = gifski_renderer(), nframes = 37, fps = 2)
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-4-1.gif)<!-- -->

``` r
set.seed(13)
n_pcas <- as_tibble(pca$x[,1:3])
#choosing k for kmeans
n_clusters_kmeans1<-kmeans(n_pcas, centers = 1, nstart = 20)
n_clusters_kmeans2<-kmeans(n_pcas, centers = 2, nstart = 20)
n_clusters_kmeans3<-kmeans(n_pcas, centers = 3, nstart = 20)
n_clusters_kmeans4<-kmeans(n_pcas, centers = 4, nstart = 20)
n_clusters_kmeans5<-kmeans(n_pcas, centers = 5, nstart = 20)
n_clusters_kmeans6<-kmeans(n_pcas, centers = 6, nstart = 20)
```

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 516050)
    
    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 516050)

``` r
kmeansvariation <- tibble("K" = 1:6,
                              "SS" = c(n_clusters_kmeans1$tot.withinss,
                                       n_clusters_kmeans2$tot.withinss,
                                       n_clusters_kmeans3$tot.withinss,
                                       n_clusters_kmeans4$tot.withinss,
                                       n_clusters_kmeans5$tot.withinss,
                                       n_clusters_kmeans6$tot.withinss))
kmeansvariationplot <- 
  ggplot(data = kmeansvariation, mapping = aes(x = K, y = SS)) +
  geom_line() +
  geom_point() +
  labs(title = "K means scree plot")
kmeansvariationplot
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#does not seem to be a dinstinct elbow. We will decide on K=3, as it is
#near the middle of complete aggregation and no aggregation, and because
#generally, there seem to be three types of players


n_pcasdist <- dist(n_pcas)
distmatrix <- as.matrix(n_pcasdist)


n_clusters_hier<-hclust(dist(n_pcas))
n_clusters_hier_plot<-plot(n_clusters_hier,
                           xlab = "", ylab = "", sub = "", 
                           main = "Complete Linkage")
n_clusters_hier_plot
```

    ## NULL

``` r
abline(h = 3.7, col = "tomato")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
n_clusters_hier_cut <- cutree(n_clusters_hier, 3)

n_pcas_kmeans <- n_pcas %>% mutate(cluster = n_clusters_kmeans3$cluster)
n_pcas_hier <- n_pcas %>% mutate(cluster = n_clusters_hier_cut)

ggplot(n_pcas_kmeans, aes(x = PC1, y = PC2,
                          color = as.factor(cluster))) + 
  geom_point(alpha = .6) +
  labs(title = "kmeans clustering")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
ggplot(n_pcas_hier, aes(x = PC1, y = PC2, 
                        color = as.factor(cluster))) +
  geom_point(alpha = .6) +
  labs(title = "hierarchical clustering")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

``` r
#plot_ly(n_pcas_kmeans, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster)

players_post1984_pca_clusters <- players_post1984 %>% 
  mutate(PC1 = pca$x[,1], PC2 = pca$x[,2], PC3 = pca$x[,3],
         hier_cluster = n_clusters_hier_cut, kmeans_cluster = n_clusters_kmeans3$cluster)




players_post1984_pca_clusters_year <- players_post1984_pca_clusters %>% 
  group_by(yearSeason, kmeans_cluster) %>% count

players_post1984_pca_year <- players_post1984_pca_clusters %>% 
  group_by(yearSeason) %>% count
players_post1984_pca_clusters_year_1 <- players_post1984_pca_clusters_year %>%
  filter(kmeans_cluster == 1)
players_post1984_pca_clusters_year_2 <- players_post1984_pca_clusters_year %>%
  filter(kmeans_cluster == 2)
players_post1984_pca_clusters_year_3 <- players_post1984_pca_clusters_year %>%
  filter(kmeans_cluster == 3)

cluster_proportions_1 = players_post1984_pca_clusters_year_1$n/players_post1984_pca_year$n
cluster_proportions_2 = players_post1984_pca_clusters_year_2$n/players_post1984_pca_year$n
cluster_proportions_3 = players_post1984_pca_clusters_year_3$n/players_post1984_pca_year$n
cluster_proportions <- tibble(year = c(1984:2020), one = cluster_proportions_1, two = cluster_proportions_2, three = cluster_proportions_3)

clusterdataframe <- data.frame(cluster1 = cluster_proportions_1,
                               cluster2 = cluster_proportions_2,
                               cluster3 = cluster_proportions_3,
                               year = c(1984:2020))
clusterdf <- data.frame(year = rep(c(1984:2020), 3),
                        proportion = c(cluster_proportions_1,
                                       cluster_proportions_2,
                                       cluster_proportions_3),
                        cluster = c(rep("Cluster 1", 37),
                                    rep("Cluster 2", 37),
                                    rep("Cluster 3", 37)))


ggplot(clusterdf, mapping = aes(x = year, y = proportion, color = cluster)) +
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

``` r
#players_post1984_pca_clusters_year
```

``` r
#what clusters do all-nba players end up in?
allnbaclusterssummary <- players_post1984_pca_clusters%>%
  filter(isAllNBA == TRUE) %>%
  group_by(hier_cluster) %>%
  summarize(n = n())
allnbahierchdata <- players_post1984_pca_clusters%>%
  filter(isAllNBA == TRUE)


allnbaclusterstatic <- ggplot(data = allnbahierchdata, 
       mapping = aes(x = PC1, y = PC2, color = as.factor(hier_cluster))) +
  geom_point() +
  geom_point(data = n_pcas_hier, 
             mapping = aes(x = PC1, y = PC2, 
                        color = as.factor(cluster)),
             alpha = .1)
allnbabyseasonplot <- allnbaclusterstatic+
  transition_time(yearSeason) +
  labs(title = "Season: {frame_time}")
animate(allnbabyseasonplot,
        renderer = gifski_renderer(), nframes = 36, fps = 2)
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-6-1.gif)<!-- -->

``` r
#now for all-nba pca analysis

pca_all_nba <- prcomp(players_post1984_all_nba_normalized[,-c(1:5, 49)])
d_all_nba <- as.data.frame(pca_all_nba$x)

ggplot(d_all_nba, aes(x = PC1, y = PC2)) +
  geom_point(size = .5, alpha = .7)
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
d2 <- tibble(PC = 1:43,
                PVE = pca$sdev^2 /
                  sum(pca$sdev^2))

d3 <- tibble(PC = 1:43,
             PVE = pca_all_nba$sdev^2 / sum(pca_all_nba$sdev^2))

ggplot(d2, aes(x = PC, y = PVE)) +
  geom_line() + 
  geom_point() +
  labs(title = "Skree Plot for NBA")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
ggplot(d3, aes(x = PC, y = PVE)) +
  geom_line() + 
  geom_point() +
  labs(title = "Skree Plot for All-NBA")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
pca_all_nba_rotations <- data.frame(pca_all_nba$rotation)
pca_all_nba_rotations$variables <- rownames(pca_all_nba_rotations)

PC1_all_nba_graph <- 
  ggplot(data = pca_all_nba_rotations, mapping = aes(x = variables, y = PC1, 
                                          fill = variables)) +
  geom_col()+
  coord_flip()+
  theme(legend.position = "none") +
  labs(title = "Rebounders(-) vs Shooters(+)")
PC1_all_nba_graph
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->

``` r
PC2_all_nba_graph <- 
  ggplot(data = pca_all_nba_rotations, mapping = aes(x = variables, y = PC2, 
                                          fill = variables)) +
  geom_col()+
  coord_flip()+
  theme(legend.position = "none") +
  labs(title = "Efficient (-) vs Not Efficient (+) ")
PC2_all_nba_graph
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->

``` r
PC3_all_nba_graph <- 
  ggplot(data = pca_all_nba_rotations, mapping = aes(x = variables, y = PC3, 
                                          fill = variables)) +
  geom_col()+
  coord_flip()+
  theme(legend.position = "none") +
  labs(title = "Team (-) vs Scoring (+)")
PC3_all_nba_graph
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->

``` r
PC4_all_nba_graph <- 
  ggplot(data = pca_all_nba_rotations, mapping = aes(x = variables, y = PC4, 
                                          fill = variables)) +
  geom_col()+
  coord_flip()+
  theme(legend.position = "none")
PC4_all_nba_graph
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->

``` r
n_pcas_all_nba <- as_tibble(pca_all_nba$x[,1:3])
n_clusters_kmeans_all_nba <-kmeans(n_pcas_all_nba, centers = 3)
n_clusters_hier_all_nba <-hclust(dist(n_pcas_all_nba))
n_clusters_hier_plot_all_nba<-plot(n_clusters_hier_all_nba)
abline(h = 2.7, col = "tomato")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
n_clusters_hier_cut_all_nba <- cutree(n_clusters_hier_all_nba, 3)

n_pcas_kmeans_all_nba <- n_pcas_all_nba %>% mutate(cluster = n_clusters_kmeans_all_nba$cluster)
n_pcas_hier_all_nba <- n_pcas_all_nba %>% mutate(cluster = n_clusters_hier_cut_all_nba)

ggplot(n_pcas_kmeans_all_nba, aes(x = PC1, y = PC2,
                                  color = as.factor(cluster))) +
  geom_point() +
  labs(title = "all-nba kmeans clustering")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
ggplot(n_pcas_hier_all_nba, aes(x = PC1, y = PC2, 
                                color = as.factor(cluster))) + 
  geom_point() +
  labs(title = "all-nba hierarchical clustering")
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
#plot_ly(n_pcas_hier_all_nba, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster)


players_post1984_pca_clusters_all_nba <- players_post1984_all_nba_normalized %>% 
  mutate(PC1 = pca_all_nba$x[,1], PC2 = pca_all_nba$x[,2], PC3 = pca_all_nba$x[,3],
         hier_cluster = n_clusters_hier_cut_all_nba, kmeans_cluster = n_clusters_kmeans_all_nba$cluster)
players_post1984_pca_clusters_year_all_nba <- players_post1984_pca_clusters_all_nba %>% 
  group_by(yearSeason, hier_cluster) %>% count

players_post1984_pca_year_all_nba <- players_post1984_pca_clusters_all_nba %>% 
  group_by(yearSeason) %>% count
players_post1984_pca_clusters_year_1_all_nba <- players_post1984_pca_clusters_year_all_nba %>%
  filter(hier_cluster == 1)
players_post1984_pca_clusters_year_2_all_nba <- players_post1984_pca_clusters_year_all_nba %>%
  filter(hier_cluster == 2)
players_post1984_pca_clusters_year_3_all_nba <- players_post1984_pca_clusters_year_all_nba %>%
  filter(hier_cluster == 3)

cluster_proportions_1_all_nba = players_post1984_pca_clusters_year_1_all_nba$n/players_post1984_pca_year_all_nba$n
cluster_proportions_2_all_nba = players_post1984_pca_clusters_year_2_all_nba$n/players_post1984_pca_year_all_nba$n
cluster_proportions_3_all_nba = players_post1984_pca_clusters_year_3_all_nba$n/players_post1984_pca_year_all_nba$n

cluster_proportions_all_nba <- tibble(year = c(1984:2019), one = cluster_proportions_1_all_nba, two = cluster_proportions_2_all_nba, three = cluster_proportions_3_all_nba)

#new graph:
allnbaclusterdf <- data.frame(year = rep(c(1984:2019), 3),
                        proportion = c(cluster_proportions_1_all_nba,
                                       cluster_proportions_2_all_nba,
                                       cluster_proportions_3_all_nba),
                        cluster = c(rep("Cluster 1", 36),
                                    rep("Cluster 2", 36),
                                    rep("Cluster 3", 36)))
ggplot(allnbaclusterdf, mapping = aes(x = year, y = proportion, 
                                      color = cluster)) + 
  geom_line()
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
#some more animated graphs
#counts
nbaclusters <- tibble(n_pcas_kmeans_all_nba$cluster, 
                          n_pcas_hier_all_nba$cluster,
                          allnbaplayerspca$yearSeason) %>%
  mutate(kmeanscluster=n_pcas_kmeans_all_nba$cluster ,
         hierarchicalcluster = n_pcas_hier_all_nba$cluster,
         yearSeason= allnbaplayerspca$yearSeason)
nbaclusters <- nbaclusters[,-c(1:3)]


clustercountkmeansallnba <- nbaclusters %>%
  group_by(yearSeason, kmeanscluster) %>%
  summarize(count = n())
clustercounthierchallnba <- nbaclusters %>%
  group_by(yearSeason, hierarchicalcluster) %>%
  summarize(count = n())


#allnba kmean and hierarchical clusters over time
kmeansclustergraphallnba <-
  ggplot(data = clustercountkmeansallnba, 
         mapping = aes(x = yearSeason, y = count, color = 
                         as.factor(kmeanscluster))) +
  geom_line() +
  ylim(-.5,12)
kmeansclustergraphallnba
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
hierarchicalgraphallnba <-
  ggplot(data = clustercounthierchallnba, 
         mapping = aes(x = yearSeason, y = count,
                       color = as.factor(hierarchicalcluster))) +
  geom_line() +
  ylim(-.5,12)
hierarchicalgraphallnba
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
#all-nba player's clusters over time
n_pcas_kmeans_all_nba <- n_pcas_kmeans_all_nba %>%
  mutate(yearSeason = allnbaplayerspca$yearSeason) 
n_pcas_hier_all_nba <- n_pcas_hier_all_nba %>%
    mutate(yearSeason = allnbaplayerspca$yearSeason) 
allnbakmeans <- 
  ggplot(n_pcas_kmeans_all_nba, aes(x = PC1, y = PC2, 
                                    color = as.factor(cluster)))+
  geom_point()+
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage") +
  labs(title = "All NBA teams over time - kmeans")
allnbakmeans
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
allnbahierch <- 
  ggplot(n_pcas_hier_all_nba, aes(x = PC1, y = PC2, 
                                  color = as.factor(cluster))) + 
  geom_point()+
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage") +
  labs(title = "All NBA teams over time - hierarchical")
allnbahierch  
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
byseasonplotkmeans  <- allnbakmeans + transition_time(yearSeason) +
  labs(title = "Season: {frame_time}") 
animate(byseasonplotkmeans, 
        renderer = gifski_renderer(), nframes = 36, fps = 2)
byseasonplothierch <- allnbahierch + transition_time(yearSeason) +
  labs(title = "Season: {frame_time}")
animate(byseasonplothierch, 
        renderer = gifski_renderer(), nframes = 36, fps = 2)
```

![](nba_technical_report_files/figure-gfm/unnamed-chunk-8-1.gif)<!-- -->

``` r
#nice animated and 3d plots. 3d plots had to be commented when knitted
#animate(byseasonplot, renderer = gifski_renderer(), nframes = 37, fps = 2)
#plot_ly(n_pcas_kmeans, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster)
#plot_ly(n_pcas_hier, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster)
#plot_ly(n_pcas_hier_all_nba, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, type = "scatter3d")
```

## Discussion

### Looking at PCA (Principal Component Analysis)

PC1 seems to be a case of an “Usage” (-) vs “Non-Usage” (+) battle. In
the negative side, we see characteristics such as ptsPerGame,
fgmPerGame, fg2mPerGame, fg2aPerGame, minutesPerGame. The other types of
variables do not tend to take on positive values, but the ones that are
include: pctBLK, pctSTL, pctTOV.  
Looking at PC2, I would describe this as “Shooters” (-) vs “Baseline”
(+). We see positive values for shooting characteristics, such as
pct3PRate, fg3mPerGame, fg3aPerGame, and negative values for
characteristics typical for tall, big players:pctDRB, pctTRB,
blkPerGame. PC3 is the distinction between “Outside the Arc” (-) and
“Inside the Arc” (+) Extreme negative values appear for
pctTrueShooting, pctEFG, pct3PRate. On the positive side, we see
tovPerGame, pctUSG, pctBLK, and fg2aPerGame.

### Clustering

For the entirety of the NBA, we decided on using K-means clustering as
our basis for determining the different “types” of players. This is
because, as you can see from the plot, hierarchichal clustering created
three different groups based on solely their usage. Players were slotted
into high, medium, and low usage. We felt that this was not indicative
of the types of players they were, so we used K-means to look at the
actual characteristics of the players. The three types we created were:

Cluster 1: low usage shooters

Cluster 2: low usage baseline players

Cluster 3: high usage players with a spread for baseline/shooting

We found that both hierarchical and k-means clustering did not seperate
players into clusters based on our third principle component, outside
vs. inside the arc. Additionally, we found that while the proportions of
each cluster changed year by year, cluster 1 remained the highest
proportion of NBA players in each season, and that the proportions of
each cluster, present day, are relatively similar to the proportions we
found in 1984.

### Looking at All-NBA

When we narrow our observations to just the 15 players that were voted
into the all-nba team each season, our first PC is the distinction
between “Rebounders” (-) and “Shooters” (+). Negative variables include
trbPerGame, pctTRB, pctDRB, orbPerGame. I would classify our second PCA
as a battle between “Efficient” (-) and “Not Efficient” (+). We don’t
have many variables for the non-efficient side, mainly turnovers and
blocks, but for the efficient aspect of the principle component, high
magnitude variables include ptsPerGame, VORP, ftmPerGame, and
ftaPerGame. Our third PC, I would classify as “Team” (-) vs “Scoring”
(+) players. “Team” stats include ratioVORP, ratioBPM, pctAST, and
astPerGame, whereas the “Scoring” variables include pstPerGAme, pctUSG,
fgmPerGame, and fg2aPerGame.

### Clustering for All-NBA

It seems that all of the clusters have some spread in PC3, the “Team” vs
“Scoring” principle component. Cluster 2 does have the more scoring
players however. The different types:

Cluster 1: Shooters, mixed efficiency, generally Scorers. Some examples
for these type of players would be Stephen Curry, Damian Lillard, Lebron
James.

Cluster 2: Pretty diverse set of Rebounders and Shooters. Shooters tend
to be scorers while Rebounders are Team players. Players are efficient.
An example for this type of player is Giannis Antetokounmpo

Cluster 3: Team Rebounders, however, not very efficient Remember though,
all of these players are All-NBA players, so their stats are being
compared to the best players each year in the NBA. Some players
representing this cluster are Draymond Green and Rudy Gobert.

We find that the proportion of shooting focused players among All-NBA
teams remains the highest in each season since 1984, and that in recent
years, that this proportion has increased dramatically. Cluster 2 and 3
proportions are pretty similar, although Cluster 3, the Team Rebounders
have taken a dramatic drop in recent years.

### Further Exploration

In future projects, we would like to look at different combinations of
number of principle components, clusters, and cluster methods.
Increasing the number of principle components and clusters would allow
us to create a more detailed collection of increasingly specific types
of players.

Additionally, we would like to perform an in depth dive for one specific
team, preferably one that has some championships under its belt, and how
its general manager responds to challenges, such as players leaving
under free agency, injuries, or how he/she seeks to build a contending
roster. Also, even if the team wins the championship, does he/she feel
the need to add new players that could potentially better the team, but
also potentially mess up team chemistry?

Related to the last inquiry, another question we could ask is how does
team composition affect success? What combination of player types is
optimal in the league? What types of players should managers try to pick
up with a limited salary, limited number of players on a team, and
limited touches for each player? There is only one basketball after all.

### 

## References

To obtain data for this project, we used a package “nbastatR”, developed
by Alex Bresler, which took data from the websites
basketball-reference.com and stats.nba.com.
