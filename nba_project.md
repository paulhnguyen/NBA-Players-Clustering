NBA Modern Positions
================

``` r
#load necessary libraries
library(nbastatR)
library(tidyverse)
library(BBmisc)
library(corrplot)
library(gganimate)
```

``` r
# Gets player stats from 1984 to 2020.
players_post1984 <- bref_players_stats(seasons = 1984:2020, tables = c("advanced", "per_game"), include_all_nba = TRUE)
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
# Filters by amount of time they played.
ggplot(players_post1984, mapping = aes(x = minutes)) + 
   geom_histogram() +
   geom_vline(xintercept = 600)
```

![](nba_project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

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
```

``` r
pca <- prcomp(players_post1984_normalized[,-c(1:5, 49)])
d <- as.data.frame(pca$x)
pc1 <- pca$rotation[, 1]
pc1
```

    ##        ratioPER pctTrueShooting       pct3PRate       pctFTRate          pctORB 
    ##   -0.1983005970   -0.1093611304    0.0446114991   -0.0647713332    0.0009161219 
    ##          pctDRB          pctTRB          pctAST          pctSTL          pctBLK 
    ##   -0.0700374947   -0.0607858529   -0.0727580277    0.0245108654    0.0212159135 
    ##          pctTOV          pctUSG        ratioOWS        ratioDWS         ratioWS 
    ##    0.0472907155   -0.1560062675   -0.1734807205   -0.1600089366   -0.1981641340 
    ##    ratioWSPer48       ratioOBPM       ratioDBPM        ratioBPM       ratioVORP 
    ##   -0.1544842420   -0.1718822222   -0.0538801002   -0.1726861352   -0.1729401844 
    ##           pctFG          pctFG3          pctFG2          pctEFG           pctFT 
    ##   -0.0889145670   -0.0305770644   -0.0863343808   -0.0805329326   -0.0432231125 
    ##  minutesPerGame      fgmPerGame      fgaPerGame     fg3mPerGame     fg3aPerGame 
    ##   -0.2998895356   -0.2521327630   -0.2396053072   -0.0744153019   -0.0769013838 
    ##     fg2mPerGame     fg2aPerGame      ftmPerGame      ftaPerGame      orbPerGame 
    ##   -0.2393336867   -0.2379725493   -0.2102740187   -0.2068602535   -0.1238684669 
    ##      drbPerGame      trbPerGame      astPerGame      stlPerGame      blkPerGame 
    ##   -0.1743708911   -0.1668884971   -0.1198521019   -0.1465998170   -0.0812665818 
    ##      tovPerGame       pfPerGame      ptsPerGame 
    ##   -0.2133118488   -0.1379065883   -0.2479709457

``` r
pc2 <- pca$rotation[, 2]
pc2
```

    ##        ratioPER pctTrueShooting       pct3PRate       pctFTRate          pctORB 
    ##     0.008229544     0.025154303    -0.302072212     0.132369578     0.084388721 
    ##          pctDRB          pctTRB          pctAST          pctSTL          pctBLK 
    ##     0.274858386     0.316735159    -0.177544483     0.124695068    -0.239702234 
    ##          pctTOV          pctUSG        ratioOWS        ratioDWS         ratioWS 
    ##     0.046451946    -0.071807006    -0.031764850     0.070089271     0.001804887 
    ##    ratioWSPer48       ratioOBPM       ratioDBPM        ratioBPM       ratioVORP 
    ##     0.037530614    -0.108632785     0.166371704     0.003848704    -0.008176325 
    ##           pctFG          pctFG3          pctFG2          pctEFG           pctFT 
    ##     0.159966530    -0.338672020     0.098603795     0.041321588    -0.147209737 
    ##  minutesPerGame      fgmPerGame      fgaPerGame     fg3mPerGame     fg3aPerGame 
    ##    -0.075582357    -0.059985441    -0.099951216    -0.278630463    -0.289626927 
    ##     fg2mPerGame     fg2aPerGame      ftmPerGame      ftaPerGame      orbPerGame 
    ##     0.016978015    -0.005219723    -0.020993061     0.008195159     0.224811902 
    ##      drbPerGame      trbPerGame      astPerGame      stlPerGame      blkPerGame 
    ##     0.154785859     0.188029191    -0.149428426    -0.109689812     0.152621746 
    ##      tovPerGame       pfPerGame      ptsPerGame 
    ##    -0.053685910     0.133881667    -0.073864587

``` r
d$namePlayer <- players_post1984_normalized$namePlayer
d$isAllNBA <- players_post1984_normalized$isAllNBA
d$yearSeason <- players_post1984_normalized$yearSeason

pcaplot <- ggplot(d, aes(x = PC1, y = PC2)) +
  geom_point(size = .5, alpha = .7) +
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage")
pcaplot
```

![](nba_project_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
d1 <- data.frame(PC = 1:43,
                PVE = pca$sdev^2 /
                  sum(pca$sdev^2))

ggplot(d1, aes(x = PC, y = PVE)) +
  geom_line() + 
  geom_point()
```

![](nba_project_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
#some PCA exploration
pcarotations <- data.frame(pca$rotation)
pcarotations$variables <- rownames(pcarotations)
PC1graph <- 
  ggplot(data = pcarotations, mapping = aes(x = variables, y = PC1, 
                                          fill = variables)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Usage (-) vs Non-Usage (+)")
PC1graph
```

![](nba_project_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
#note important characteristics: tov, pts, minutes, fgm, fga
PC2graph <- 
  ggplot(data = pcarotations, mapping = aes(x = variables, y = PC2, 
                                          fill = variables)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")+
  labs(title = "Shooters vs Baseline")
PC2graph
```

![](nba_project_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
#note important characteristics: (+) rebounding stats, (-) 3pt shooting, asts
PC3graph <- 
  ggplot(data = pcarotations, mapping = aes(x = variables, y = PC3, 
                                          fill = variables)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")
PC3graph
```

![](nba_project_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

``` r
#idk this one seems pretty hard to interpret lol

#lets now create a plot with some players that we will recognize
currentplayersplot <- ggplot(d, aes(x = PC1, y = PC2)) +
  geom_point(size = .5, alpha = .1) +
  geom_text(data = subset(d, isAllNBA == TRUE & 
                            yearSeason %in% 2017:2019 ),
            aes(label = namePlayer)) +
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage")
currentplayersplot
```

![](nba_project_files/figure-gfm/unnamed-chunk-3-6.png)<!-- -->

``` r
famousplayersplot <-  ggplot(d, aes(x = PC1, y = PC2)) +
  geom_point(size = .5, alpha = .1) +
  geom_text(data = subset(d, namePlayer %in% c("Lebron James", 
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
             size = .5, alpha = .8)) +
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage")
famousplayersplot
```

![](nba_project_files/figure-gfm/unnamed-chunk-3-7.png)<!-- -->

``` r
#animated plot, allstars throughout the seasons
allnbaplayerspca <- d %>%
  filter(isAllNBA == TRUE)
#staticplot
staticplot <- ggplot(data = allnbaplayerspca, 
                     mapping = aes(x = PC1, y = PC2)) +
  geom_point(alpha = .8, color = "purple") + 
  geom_text(data = allnbaplayerspca, mapping = aes(label = namePlayer)) +
  geom_point(data = d, mapping = aes(x = PC1, y = PC2), alpha = .1) +
  ylab("Shooters vs Baseline") +
  xlab("Usage vs Non-Usage")
staticplot
```

![](nba_project_files/figure-gfm/unnamed-chunk-3-8.png)<!-- -->

``` r
#byseason
byseasonplot <- staticplot + transition_time(yearSeason) +
  labs(title = "Season: {frame_time}")
animate(byseasonplot, renderer = gifski_renderer(), nframes = 36, fps = 2)
```

![](nba_project_files/figure-gfm/unnamed-chunk-3-1.gif)<!-- -->

``` r
#to do: k-means clustering, hierchical clustering
```
