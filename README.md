NHL Hockey: ST 558 Project 1
================
Peter Lung
6/12/2021

-   [Required Libraries](#required-libraries)
-   [Functions for pulling data](#functions-for-pulling-data)
    -   [Records Functions](#records-functions)
    -   [Stats API Table Function](#stats-api-table-function)
-   [NHL Tables](#nhl-tables)
    -   [Franchises](#franchises)
    -   [Team Totals](#team-totals)
-   [NHL Data Analysis](#nhl-data-analysis)
    -   [Goalie Records](#goalie-records)
        -   [Histograms](#histograms)
        -   [Scatterplot](#scatterplot)
        -   [Most Goals Scored Against a Goalie in a Single
            Game](#most-goals-scored-against-a-goalie-in-a-single-game)
    -   [Player Names](#player-names)
-   [Bonus: Team Reg. Season Win % by Playoff
    Outcome](#bonus-team-reg-season-win--by-playoff-outcome)

# Required Libraries

``` r
library(httr)
library(stringr)
library(jsonlite)
library(ggplot2)
library(rmarkdown)
library(DT)
library(kableExtra)
library(dplyr)
```

# Functions for pulling data

The hockey data I will be going over is accessible via the following
logic which returns the table of interest, subsetted by the team of
interest (if there is one). There are six records tables to choose from
as well as one stats table to choose from for pulling the data.

-   Franchise table: Contains general information on each historical NHL
    franchise
-   Team totals table: Contains some basic win/loss information for each
    franchise
-   Season records
-   Goalie records
-   Skater records
-   Recent team IDs

We’ll start with the preliminaries: establishing a base url and a
mapping for teams to team IDs.

``` r
#Examples for team-level outputs - we can change the team using these global variables and everything below will update
team <- "Toronto Maple Leafs"
team_id <- 5

#base url
base_url <- "https://records.nhl.com/site/api"

# Mapping team IDs to team names
Teams <- c("Montréal Canadiens", "Montreal Wanderers", "St. Louis Eagles", "Hamilton Tigers", "Toronto Maple Leafs", "Boston Bruins", "Montreal Maroons", "Brooklyn Americans", "Philadelphia Quakers", "New York Rangers", "Chicago Blackhawks", "Detroit Red Wings", "Cleveland Barons", "Los Angeles Kings", "Dallas Stars", "Philadelphia Flyers", "Pittsburgh Penguins", "St. Louis Blues", "Buffalo Sabres", "Vancouver Canucks", "Calgary Flames", "New York Islanders", "New Jersey Devils", "Washington Capitals", "Edmonton Oilers", "Carolina Hurricanes ", "Colorado Avalanche", "Arizona Coyotes", "San Jose Sharks", "Ottawa Senators ", "Tampa Bay Lightning", "Anaheim Ducks", "Florida Panthers", "Nashville Predators", "Winnipeg Jets", "Columbus Blue Jackets", "Minnesota Wild", "Vegas Golden Knights")
ID <- (1:38)
TeamMap <- data.frame(Teams, ID)
```

Next, we’ll start by creating the functions. The first one returns a
table that displays a number of facts about each team from history.

## Records Functions

``` r
#franchise function
franchise <- function() {
  url <- paste0(base_url, "/franchise")
  get <- GET(url)
  txt <- content(get, "text", encoding = "UTF-8")
  json <- fromJSON(txt, flatten = TRUE)
  franch <- as.data.frame(json)
  return(franch)
}
```

The next function displayes win and loss data for each franchise over
its history.

``` r
#team totals function
team_tot <- function() {
  url <- paste0(base_url, "/franchise-team-totals")
  get <- GET(url)
  txt <- content(get, "text", encoding = "UTF-8")
  json <- fromJSON(txt, flatten = TRUE)
  team_tot <- as.data.frame(json)
  return(team_tot)
}
```

Next, the functions that return tables by team. The first one is the
season records table.

``` r
#season records function
season_rec <- function(team = "", team_id) {
  if (team == "all") {
    for (i in 1:38) {
      url <- paste0(base_url, "/franchise-season-records?cayenneExp=franchiseId==",toString(i))
      get <- GET(url)
      txt <- content(get, "text", encoding = "UTF-8")
      json <- fromJSON(txt, flatten = TRUE)
      if (i == 1) {SeasonPull <- as.data.frame(json)} else {SeasonPull <- rbind(SeasonPull, ... = as.data.frame(json))}}}
  else if (team == "") {
    url <- paste0(base_url, "/franchise-season-records?cayenneExp=franchiseId=",toString(team_id))
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    SeasonPull <- as.data.frame(json)}
  else {
    TeamName <- TeamMap %>% filter(Teams == team)
    j <- TeamName[1,2]
    url <- paste0(base_url, "/franchise-season-records?cayenneExp=franchiseId=",toString(j))
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    SeasonPull <- as.data.frame(json)}
  return(SeasonPull)
}
```

The goalie and skater data return the list of players for each team
(goalies in the first table, skaters in the second) and career records
for each player.

``` r
#goalie records totals function
goalie <- function(team = "", team_id) {
  if (team == "all") {
    for (i in 1:38) {
      url <- paste0(base_url, "/franchise-goalie-records?cayenneExp=franchiseId=",toString(i))
      get <- GET(url)
      txt <- content(get, "text", encoding = "UTF-8")
      json <- fromJSON(txt, flatten = TRUE)
      if (i == 1) {GoaliePull <- as.data.frame(json)} else {GoaliePull <- rbind(GoaliePull, ... = as.data.frame(json))}}}
  else if (team == "") {
    url <- paste0(base_url, "/franchise-goalie-records?cayenneExp=franchiseId=",toString(team_id))
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    GoaliePull <- as.data.frame(json)}
  else {
    TeamName <- TeamMap %>% filter(Teams == team)
    j <- TeamName[1,2]
    url <- paste0(base_url, "/franchise-goalie-records?cayenneExp=franchiseId=",toString(j))
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    GoaliePull <- as.data.frame(json)}
  return(GoaliePull)
}

#skater records totals function
skater <- function(team = "", team_id) {
  if (team == "all") {
    for (i in 1:38) {
      url <- paste0(base_url, "/franchise-skater-records?cayenneExp=franchiseId=",toString(i))
      get <- GET(url)
      txt <- content(get, "text", encoding = "UTF-8")
      json <- fromJSON(txt, flatten = TRUE)
      if (i == 1) {SkaterPull <- as.data.frame(json)} else {SkaterPull <- rbind(SkaterPull, ... = as.data.frame(json))}}}
  else if (team == "") {
    url <- paste0(base_url, "/franchise-skater-records?cayenneExp=franchiseId=",toString(team_id))
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    SkaterPull <- as.data.frame(json)}
  else {
    TeamName <- TeamMap %>% filter(Teams == team)
    j <- TeamName[1,2]
    url <- paste0(base_url, "/franchise-skater-records?cayenneExp=franchiseId=",toString(j))
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    SkaterPull <- as.data.frame(json)}
  return(SkaterPull)
}
```

Finally, the last table returns retired numbers and some other
team-specific data.

``` r
#Recent data pull function
recent <- function(team = "", team_id) {
  if (team == "all") {
    for (i in 1:38) {
      url <- paste0(base_url, "/franchise-detail?cayenneExp=mostRecentTeamId=",toString(i))
      get <- GET(url)
      txt <- content(get, "text", encoding = "UTF-8")
      json <- fromJSON(txt, flatten = TRUE)
      if (i == 1) {RecentPull <- as.data.frame(json)} else {RecentPull <- rbind(RecentPull, ... = as.data.frame(json))}}}
  else if (team == "") {
    url <- paste0(base_url, "/franchise-detail?cayenneExp=mostRecentTeamId=",toString(team_id))
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    RecentPull <- as.data.frame(json)}
  else {
    TeamName <- TeamMap %>% filter(Teams == team)
    j <- TeamName[1,2]
    url <- paste0(base_url, "/franchise-detail?cayenneExp=mostRecentTeamId=",toString(j))
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    RecentPull <- as.data.frame(json)}
  return(RecentPull)
}
```

## Stats API Table Function

This function returns a table from the stats API.

``` r
#Stats API data pull function 

# Mapping team IDs to team names (team IDs are different in the Stats API)
TeamsS <- c("Devils", "Islanders", "Rangers", "Flyers", "Penguins", "Bruins", "Sabres", "Canadiens", "Senators", "Maple Leafs", "Hurricanes", "Panthers", "Lightning", "Capitals", "Blackhawks", "Red Wings", "Predators", "Blues", "Flames", "Avalanche", "Oilers", "Canucks", "Ducks", "Stars", "Kings", "Sharks ", "Blue Jackets", "Wild", "Jets", "Coyotes ", "Golden Knights", "Kraken")
ID_S <- c((1:10), (12:26), (28:30), (52:55))
StatsMap <- data.frame(Teams, ID)

statsapi <- function(team = "", team_id) {
  if (team == "all") {
    for (i in ID_S) {
      url <- paste0("https://statsapi.web.nhl.com/api/v1/teams/",toString(i),"?expand=team.stats")
      get <- GET(url)
      txt <- content(get, "text", encoding = "UTF-8")
      json <- fromJSON(txt, flatten = TRUE)
      if (i == 1) {StatsPull <- as.data.frame(json)} else {StatsPull <- rbind(StatsPull, ... = as.data.frame(json))}}}
  else if (team == "") {
    url <- paste0("https://statsapi.web.nhl.com/api/v1/teams/",toString(team_id),"?expand=team.stats")
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    StatsPull <- as.data.frame(json)}
  else {
    TeamName <- StatsMap %>% filter(TeamsS == team)
    j <- TeamName[1,2]
    url <- paste0(base_url, "https://statsapi.web.nhl.com/api/v1/teams/",toString(j),"?expand=team.stats")
    get <- GET(url)
    txt <- content(get, "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)
    StatsPull <- as.data.frame(json)}
  return(StatsPull)
}
```

The above functions return the individual table of interest, but the
following wrapper function will be the only function needed to pull any
table from above. Example syntax is shown in the latter sections. You
will need to specify a table of interest as well as either a team or
team\_id value if the table contains team-specific data.

``` r
nhl <- function(table,...) {
  if (table == "franchise") {df <- franchise()}
  else if (table == "team_tot") {df <- team_tot()}
  else if (table == "season_rec") {df <- season_rec(...)}
  else if (table == "goalie") {df <- goalie(...)}
  else if (table == "skater") {df <- skater(...)}
  else if (table == "recent") {df <- recent(...)}
  else if (table == "statsapi") {df <- statsapi(...)}
  else {df <- NULL}
  return(df)
}
```

# NHL Data Analysis

## Goalie Records

Do teams keep under-performing goalies around for a long time? I was
curious to see win percentage by number of games played. Of course,
there could be a lot of things going on here, but I am interested in the
lower-right quadrant of the scatterplot of win percentage and games
played. Are there goalies who just have long losing careers? Maybe they
are good, but they are just stuck on losing teams all of the time? Maybe
they aren’t good, and they contribute to losing records, but teams keep
them around?

### Histograms

Before we jump into the analysis, let’s do some data investigation. We
will start with histograms of the variables we are interested in. The
following is a histogram with all teams included:

``` r
#Pulling Goalie data and creating a histogram
GoalieRecords <- nhl("goalie", team = "all") %>% mutate(Win.Percentage = data.wins / data.gamesPlayed)

g1 <- ggplot(GoalieRecords, aes(x = Win.Percentage, , y = ..density..))
g1 + geom_histogram(color = "white", fill = "darkgoldenrod1", bins = 35) + geom_density(color = "blue") + labs(title = "Histogram for Goalie Win Percentage")
```

![](/README_files/figure-gfm/goalierec1-1.png)<!-- -->

There are several players with a win percentage of 0% and a few with
100%. This is because many of these players have only played in a few
games. I think it makes sense to filter out goalies who have played less
than 20 games so that we have appropriate sample sizes. Removing those
players and re-creating the histogram:

``` r
#Updating the data and reprinting the histogram

GoalieRecords <- filter(GoalieRecords, data.gamesPlayed >=20)

g2 <- ggplot(GoalieRecords, aes(x = Win.Percentage, , y = ..density..))
g2 + geom_histogram(color = "white", fill = "darkgoldenrod1", bins = 35) + geom_density(color = "blue") + labs(title = "Histogram for Win Percentage (at least 20 games)")
```

![](/README_files/figure-gfm/goalierec2-1.png)<!-- -->

So let’s see how that distribution compares to the Toronto Maple Leafs.

``` r
#Team-level histogram
tGoalieRecords <- nhl("goalie", team_id = team_id) %>% mutate(Win.Percentage = data.wins / data.gamesPlayed) %>% filter(data.gamesPlayed >=20)

t2 <- ggplot(tGoalieRecords, aes(x = Win.Percentage))
t2 + geom_histogram(color = "white", fill = "darkgoldenrod", bins = 35) + labs(title = paste0("Histogram for Win Percentage (at least 20 games) - ", team))
```

![](/README_files/figure-gfm/tgoalierec2-1.png)<!-- -->

Now that we have removed players with under 20 games, let’s look at a
histogram of games played.:

``` r
#Games Played Histogram

g3 <- ggplot(GoalieRecords, aes(x = data.gamesPlayed, , y = ..density..))
g3 + geom_histogram(color = "white", fill = "darkgoldenrod1", bins = 35) + geom_density(color = "blue") + labs(title = "Histogram for Games Played - All Teams") + xlab("Games Played")
```

![](/README_files/figure-gfm/goalierec3-1.png)<!-- -->

``` r
#Team - level games played

t3 <- ggplot(tGoalieRecords, aes(x = data.gamesPlayed))
t3 + geom_histogram(color = "white", fill = "darkgoldenrod", bins = 35) + labs(title = paste0("Histogram for Games Played - ", team)) + xlab("Games Played")
```

![](C:\Users\rm915\Desktop\ST%20558\NHL\README_files/figure-gfm/tgoalierec3-1.png)<!-- -->

### Scatterplot

I am interested in statistics around goalie career losses and lowest
winning percentages. Players can get stuck on losing teams even if they
are very good (just ask Mike Trout). I want to see who these goalies
might be in the NHL and just how badly their careers went.

We continue the analysis with a scatterplot of win percentage by games
played.

``` r
#Find out who had the most losses
MostLosses <- max(GoalieRecords$data.losses)
Loser <- filter(GoalieRecords, data.losses == MostLosses) %>% select(data.firstName, data.lastName, data.gamesPlayed)

#Lowest winning percentage in over 500 games
FrustratedPct <- min(filter(GoalieRecords, data.gamesPlayed > 500) %>% select(Win.Percentage))
Frustrated <- filter(GoalieRecords, data.gamesPlayed > 500) %>% filter(Win.Percentage == min(FrustratedPct))  %>% select(data.firstName, data.lastName, data.gamesPlayed)

# Lowest winning percentage in the sample set (get max games played if there's a tie)
LowestPct <- min(GoalieRecords$Win.Percentage)
Lowest <- filter(GoalieRecords, Win.Percentage == LowestPct)  %>% select(data.firstName, data.lastName, data.gamesPlayed) %>% filter(data.gamesPlayed == max(data.gamesPlayed))

GoalieRecords <- mutate(GoalieRecords, Who.Stands.Out = ifelse(Win.Percentage == LowestPct , paste0(data.firstName, " ", data.lastName), ifelse(Win.Percentage == FrustratedPct, paste0(data.firstName, " ", data.lastName), ifelse(data.losses == MostLosses, paste0(data.firstName, " ", data.lastName), "Everybody Else"))))



g4 <- ggplot(GoalieRecords, aes(x = data.gamesPlayed, y = Win.Percentage, color = Who.Stands.Out))
g4 + geom_point() + labs(title = "Goalie Win Percentage by Career Games Played - All Teams") + xlab("Games Played") + ylab("Win Percentage") + scale_color_manual(values=c("grey60", "green3", "firebrick3", "darkorange2"))
```

![](/README_files/figure-gfm/goalierec4-1.png)<!-- -->

``` r
#Print notable goalie names based on data input
BiggestLoser <- paste0("The goalie with the most losses is ", Loser[1,1]," " , Loser[1,2], " with ", toString(MostLosses), ".")

MostFrustrated <- paste0("The goalie with the lowest win percentage (over 500 games) is ", Frustrated[1,1]," ", Frustrated[1,2], " at ", toString(round(100 * FrustratedPct, 1)),"% in ", toString(Frustrated[1,3]), " games.")

LowestRate <- paste0("The goalie with the lowest win percentage in the whole sample is ", Lowest[1,1]," ", Lowest[1,2], " at ", toString(round(LowestPct, 1)), "% in ", toString(Lowest[1,3]), " games.")
```

Win percentage is defined as win (not ties) divided by total games.
Total games include ties.

Well, it looks like if you are really a loser, they don’t keep you in
the goal for *that* many games. Three players stand out to me from the
graph:

The goalie with the most losses is Martin Brodeur with 394.

The goalie with the lowest win percentage (over 500 games) is Gump
Worsley at 35.1% in 581 games.

The goalie with the lowest win percentage in the whole sample is Michel
Belhumeur at 0% in 42 games.

Total losses are probably more related to longevity than an extended
poor performance. Having a zero or near-zero win percentage in 40+ games
seems to be quite an anomaly. Even highly likely events can have a
non-zero probability of occurring.

Compare with the scatterplot for the Toronto Maple Leafs.

``` r
#Find out who had the most losses
tMostLosses <- max(tGoalieRecords$data.losses)
tLoser <- filter(tGoalieRecords, data.losses == tMostLosses) %>% select(data.firstName, data.lastName, data.gamesPlayed)  %>% filter(data.gamesPlayed == max(data.gamesPlayed))

#Lowest winning percentage in over 500 games
tFrustratedPct <- min(filter(tGoalieRecords, data.gamesPlayed > 200) %>% select(Win.Percentage))
tFrustrated <- filter(tGoalieRecords, data.gamesPlayed > 200) %>% filter(Win.Percentage == min(tFrustratedPct))  %>% select(data.firstName, data.lastName, data.gamesPlayed)  %>% filter(data.gamesPlayed == max(data.gamesPlayed))

# Lowest winning percentage in the sample set (get max games played if there's a tie)
tLowestPct <- min(tGoalieRecords$Win.Percentage)
tLowest <- filter(tGoalieRecords, Win.Percentage == tLowestPct)  %>% select(data.firstName, data.lastName, data.gamesPlayed) %>% filter(data.gamesPlayed == max(data.gamesPlayed))

tGoalieRecords <- mutate(tGoalieRecords, Who.Stands.Out = ifelse(Win.Percentage == tLowestPct , paste0(data.firstName, " ", data.lastName), ifelse(Win.Percentage == tFrustratedPct, paste0(data.firstName, " ", data.lastName), ifelse(data.losses == tMostLosses, paste0(data.firstName, " ", data.lastName), "Everybody Else"))))
tGoalieRecords$Who.Stands.Out <- factor(tGoalieRecords$Who.Stands.Out, levels = c("Everybody Else", paste0(tLoser[1,1]," " , tLoser[1,2]), paste0(tFrustrated[1,1]," ", tFrustrated[1,2]), paste0(tLowest[1,1]," ", tLowest[1,2])))


t4 <- ggplot(tGoalieRecords, aes(x = data.gamesPlayed, y = Win.Percentage, color = Who.Stands.Out))
t4 + geom_point() + labs(title = paste0("Goalie Win Percentage by Career Games Played - ", team)) + xlab("Games Played") + ylab("Win Percentage") + scale_color_manual(values=c("black", "green3", "firebrick3", "darkorange2"))
```

![](/README_files/figure-gfm/tgoalierec4-1.png)<!-- -->

``` r
#print notable goalie names based on team input
tBiggestLoser <- paste0("The goalie with the most losses for ", team, " is ", tLoser[1,1]," " , tLoser[1,2], " with ", toString(tMostLosses), ".")

tMostFrustrated <- paste0("The goalie with the lowest win percentage for the ", team, " (over 200 games) is ", tFrustrated[1,1]," ", tFrustrated[1,2], " at ", toString(round(100 * tFrustratedPct, 1)),"% in ", toString(tFrustrated[1,3]), " games.")

tLowestRate <- paste0("The goalie with the lowest win percentage for the ", team, " in the whole sample is ", tLowest[1,1]," ", tLowest[1,2], " at ", toString(round(100 * tLowestPct, 1)), "% in ", toString(tLowest[1,3]), " games.")
```

Since some teams don’t have goalies with 500+ games, the parameter is
changed to 200 games.

The goalie with the most losses for Toronto Maple Leafs is Turk Broda
with 222.

The goalie with the lowest win percentage for the Toronto Maple Leafs
(over 200 games) is Allan Bester at 33.7% in 205 games.

The goalie with the lowest win percentage for the Toronto Maple Leafs in
the whole sample is Michel Larocque at 21.6% in 74 games.

### Most Goals Scored Against a Goalie in a Single Game

The data has an interesting statistic that might be worth looking at:
most goals scored on a goalie in a single game. It would be unfair to
judge a player’s career by their worst game, but in the name of data
analysis, let’s see if there are any relationships there.

Keeping with the 20 game minimum, we’ll start with a histogram:

``` r
#Most goals histogram
g5 <- ggplot(GoalieRecords, aes(x = data.mostGoalsAgainstOneGame, , y = ..density..))
g5 + geom_histogram(color = "white", fill = "darkgoldenrod1", bins = 35) + labs(title = "Histogram for Single Game Goals Scored Against - All Teams") + xlab("Most Goals Scored Against in a Single Game")
```

![](/README_files/figure-gfm/goalierec5-1.png)<!-- -->

It seems that there are very few where the number of goals is over 11 or
under 5, so let’s create a set of categorical variables that denote most
goals scored against with categories “&lt;=5”, “6”, “7”,…,“10”, “11+”.

``` r
#Modify data to make categories
ScoredAgainst <- mutate(GoalieRecords, Most.Scored.Against = recode(data.mostGoalsAgainstOneGame,
                                                                    "1" = "<6",
                                                                    "2" = "<6",
                                                                    "3" = "<6",
                                                                    "4" = "<6",
                                                                    "5" = "<6",
                                                                    "6" = "6",
                                                                    "7" = "7",
                                                                    "8" = "8",
                                                                    "9" = "9",
                                                                    "10" = "10",
                                                                    .default = "11+"))

ScoredAgainst$Most.Scored.Against <- factor(ScoredAgainst$Most.Scored.Against, levels = c("<6", "6", "7", "8", "9", "10", "11+"))

df0 <- filter(ScoredAgainst, Most.Scored.Against == "<6")
Defense <- as.data.frame(t(round(summary(df0$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq)  %>% rename("<6" = "Freq")
df1 <- filter(ScoredAgainst, Most.Scored.Against == "6")
A <- as.data.frame(t(round(summary(df1$Win.Percentage),3) )) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("6" = "Freq")
df2 <- filter(ScoredAgainst, Most.Scored.Against == "7")
B <- as.data.frame(t(round(summary(df2$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("7" = "Freq")
df3 <- filter(ScoredAgainst, Most.Scored.Against == "8")
C <- as.data.frame(t(round(summary(df3$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("8" = "Freq")
df4 <- filter(ScoredAgainst, Most.Scored.Against == "9")
D <- as.data.frame(t(round(summary(df4$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("9" = "Freq")
df5 <- filter(ScoredAgainst, Most.Scored.Against == "10")
E <- as.data.frame(t(round(summary(df5$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("10" = "Freq")
df6 <- filter(ScoredAgainst, Most.Scored.Against == "11+")
F <- as.data.frame(t(round(summary(df6$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("11+" = "Freq")

tScoredAgainst <- filter(ScoredAgainst, , data.franchiseName == team)

#Format data for input into table

tdf0 <- filter(tScoredAgainst, Most.Scored.Against == "<6")
tDefense <- as.data.frame(t(round(summary(tdf0$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq)  %>% rename("<6" = "Freq")
tdf1 <- filter(tScoredAgainst, Most.Scored.Against == "6")
tA <- as.data.frame(t(round(summary(tdf1$Win.Percentage),3) )) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("6" = "Freq")
tdf2 <- filter(tScoredAgainst, Most.Scored.Against == "7")
tB <- as.data.frame(t(round(summary(tdf2$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("7" = "Freq")
tdf3 <- filter(tScoredAgainst, Most.Scored.Against == "8")
tC <- as.data.frame(t(round(summary(tdf3$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("8" = "Freq")
tdf4 <- filter(tScoredAgainst, Most.Scored.Against == "9")
tD <- as.data.frame(t(round(summary(tdf4$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("9" = "Freq")
tdf5 <- filter(tScoredAgainst, Most.Scored.Against == "10")
tE <- as.data.frame(t(round(summary(tdf5$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("10" = "Freq")
tdf6 <- filter(tScoredAgainst, Most.Scored.Against == "11+")
tF <- as.data.frame(t(round(summary(tdf6$Win.Percentage),3))) %>% rename(Stat = Var2) %>% select(Stat, Freq) %>% rename("11+" = "Freq")

#Combine data
Defense <- Defense %>% left_join(A, by = "Stat") %>% left_join(B, by = "Stat") %>% left_join(C, by = "Stat") %>% left_join(D, by = "Stat") %>% left_join(E, by = "Stat") %>% left_join(F, by = "Stat")

tDefense <- tDefense %>% left_join(tA, by = "Stat") %>% left_join(tB, by = "Stat") %>% left_join(tC, by = "Stat") %>% left_join(tD, by = "Stat") %>% left_join(tE, by = "Stat") %>% left_join(tF, by = "Stat")

Defense <- Defense %>% rename(" " = "Stat")
tDefense <- tDefense %>% rename(" " = "Stat")

sampsize <- as.data.frame(t(c("n", nrow(df0), nrow(df1), nrow(df2), nrow(df3), nrow(df4), nrow(df5), nrow(df6)))) %>% rename(" " = "V1", "<6" = "V2", "6" = "V3", "7" = "V4", "8" = "V5", "9" = "V6", "10" = "V7", "11+" = "V8")

tsampsize <- as.data.frame(t(c("n", nrow(tdf0), nrow(tdf1), nrow(tdf2), nrow(tdf3), nrow(tdf4), nrow(tdf5), nrow(tdf6)))) %>% rename(" " = "V1", "<6" = "V2", "6" = "V3", "7" = "V4", "8" = "V5", "9" = "V6", "10" = "V7", "11+" = "V8")

Defense <- rbind(Defense, sampsize)
tDefense <- rbind(tDefense, tsampsize)

#Print table

kable(Defense, caption = "Summaries Statistics for Win Percentage by Career 'Most Goals Scored Against' - All Teams", digits = 3) %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Summaries Statistics for Win Percentage by Career ‘Most Goals Scored
Against’ - All Teams
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
&lt;6
</th>
<th style="text-align:left;">
6
</th>
<th style="text-align:left;">
7
</th>
<th style="text-align:left;">
8
</th>
<th style="text-align:left;">
9
</th>
<th style="text-align:left;">
10
</th>
<th style="text-align:left;">
11+
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Min.
</td>
<td style="text-align:left;">
0.132
</td>
<td style="text-align:left;">
0.136
</td>
<td style="text-align:left;">
0.077
</td>
<td style="text-align:left;">
0.111
</td>
<td style="text-align:left;">
0.12
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0.087
</td>
</tr>
<tr>
<td style="text-align:left;">
1st Qu.
</td>
<td style="text-align:left;">
0.383
</td>
<td style="text-align:left;">
0.314
</td>
<td style="text-align:left;">
0.351
</td>
<td style="text-align:left;">
0.29
</td>
<td style="text-align:left;">
0.3
</td>
<td style="text-align:left;">
0.225
</td>
<td style="text-align:left;">
0.303
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:left;">
0.441
</td>
<td style="text-align:left;">
0.414
</td>
<td style="text-align:left;">
0.416
</td>
<td style="text-align:left;">
0.372
</td>
<td style="text-align:left;">
0.383
</td>
<td style="text-align:left;">
0.337
</td>
<td style="text-align:left;">
0.342
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:left;">
0.444
</td>
<td style="text-align:left;">
0.403
</td>
<td style="text-align:left;">
0.413
</td>
<td style="text-align:left;">
0.374
</td>
<td style="text-align:left;">
0.371
</td>
<td style="text-align:left;">
0.314
</td>
<td style="text-align:left;">
0.348
</td>
</tr>
<tr>
<td style="text-align:left;">
3rd Qu.
</td>
<td style="text-align:left;">
0.523
</td>
<td style="text-align:left;">
0.486
</td>
<td style="text-align:left;">
0.485
</td>
<td style="text-align:left;">
0.453
</td>
<td style="text-align:left;">
0.444
</td>
<td style="text-align:left;">
0.4
</td>
<td style="text-align:left;">
0.41
</td>
</tr>
<tr>
<td style="text-align:left;">
Max.
</td>
<td style="text-align:left;">
0.714
</td>
<td style="text-align:left;">
0.727
</td>
<td style="text-align:left;">
0.737
</td>
<td style="text-align:left;">
0.65
</td>
<td style="text-align:left;">
0.655
</td>
<td style="text-align:left;">
0.525
</td>
<td style="text-align:left;">
0.614
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
83
</td>
<td style="text-align:left;">
149
</td>
<td style="text-align:left;">
194
</td>
<td style="text-align:left;">
121
</td>
<td style="text-align:left;">
77
</td>
<td style="text-align:left;">
45
</td>
<td style="text-align:left;">
41
</td>
</tr>
</tbody>
</table>

``` r
kable(tDefense, caption = paste0("Summaries Statistics for Win Percentage by Career 'Most Goals Scored Against' - ", team), digits = 3) %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Summaries Statistics for Win Percentage by Career ‘Most Goals Scored
Against’ - Toronto Maple Leafs
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
&lt;6
</th>
<th style="text-align:left;">
6
</th>
<th style="text-align:left;">
7
</th>
<th style="text-align:left;">
8
</th>
<th style="text-align:left;">
9
</th>
<th style="text-align:left;">
10
</th>
<th style="text-align:left;">
11+
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Min.
</td>
<td style="text-align:left;">
0.354
</td>
<td style="text-align:left;">
0.387
</td>
<td style="text-align:left;">
0.309
</td>
<td style="text-align:left;">
0.316
</td>
<td style="text-align:left;">
0.224
</td>
<td style="text-align:left;">
0.216
</td>
<td style="text-align:left;">
0.4
</td>
</tr>
<tr>
<td style="text-align:left;">
1st Qu.
</td>
<td style="text-align:left;">
0.357
</td>
<td style="text-align:left;">
0.418
</td>
<td style="text-align:left;">
0.351
</td>
<td style="text-align:left;">
0.369
</td>
<td style="text-align:left;">
0.336
</td>
<td style="text-align:left;">
0.276
</td>
<td style="text-align:left;">
0.425
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:left;">
0.4
</td>
<td style="text-align:left;">
0.449
</td>
<td style="text-align:left;">
0.401
</td>
<td style="text-align:left;">
0.392
</td>
<td style="text-align:left;">
0.428
</td>
<td style="text-align:left;">
0.337
</td>
<td style="text-align:left;">
0.45
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:left;">
0.471
</td>
<td style="text-align:left;">
0.449
</td>
<td style="text-align:left;">
0.415
</td>
<td style="text-align:left;">
0.41
</td>
<td style="text-align:left;">
0.39
</td>
<td style="text-align:left;">
0.343
</td>
<td style="text-align:left;">
0.45
</td>
</tr>
<tr>
<td style="text-align:left;">
3rd Qu.
</td>
<td style="text-align:left;">
0.531
</td>
<td style="text-align:left;">
0.48
</td>
<td style="text-align:left;">
0.471
</td>
<td style="text-align:left;">
0.448
</td>
<td style="text-align:left;">
0.461
</td>
<td style="text-align:left;">
0.407
</td>
<td style="text-align:left;">
0.475
</td>
</tr>
<tr>
<td style="text-align:left;">
Max.
</td>
<td style="text-align:left;">
0.714
</td>
<td style="text-align:left;">
0.511
</td>
<td style="text-align:left;">
0.556
</td>
<td style="text-align:left;">
0.547
</td>
<td style="text-align:left;">
0.483
</td>
<td style="text-align:left;">
0.477
</td>
<td style="text-align:left;">
0.5
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
12
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
2
</td>
</tr>
</tbody>
</table>

We can use boxplots to visually see the distribution of win percentage
by each of these categories.

``` r
#Generate boxplots

g6 <- ggplot(ScoredAgainst, aes(x = Most.Scored.Against, y = Win.Percentage, color = Most.Scored.Against))
g6 + geom_boxplot(fill = "white", colour = "black") + labs(title = "Boxplots of Win Percentage by Most Goals Scored Against") + geom_point(position = "jitter") + geom_smooth(formula = (y ~ x), method = lm, se = FALSE, aes(group=1), color = "black")
```

![](/README_files/figure-gfm/goalierec7-1.png)<!-- -->

Interestingly, it appears that goalies with larger max number of goals
scored against them in a game are a little more likely to have lower
career winning percentages. Let’s see if this holds true for Toronto
Maple Leafs:

``` r
#Team - level scatterplot
t6 <- ggplot(tScoredAgainst, aes(Most.Scored.Against, Win.Percentage))
t6 + geom_point() + geom_smooth(formula = y ~ x, method = lm, se = FALSE, col = "red", aes(group=1)) + labs(title = paste0("Win Percentage by Most Goals Scored Against - ", team)) + xlab("Career Most Goals Scored Against") + ylab("Win Percentage")
```

![](/README_files/figure-gfm/tgoalierec7-1.png)<!-- -->

## Player Names

Hockey is a sport that is very popular in North America, particularly
Canada. The names of players is a topic that is interesting because it
may differ significantly from the name distribution of the general
population. What are the most common first and last names for all
players that have played hockey for the Toronto Maple Leafs?

``` r
#Generate player first names table

skaterrec_url <- paste0(base_url, "/franchise-skater-records?cayenneExp=franchiseId=",toString(team_id))
get_skaterrec <- GET(skaterrec_url)
txt_skaterrec <- content(get_skaterrec, "text", encoding = "UTF-8")
json_skaterrec <- fromJSON(txt_skaterrec, flatten = TRUE)
SkaterPull <- as.data.frame(json_skaterrec)

PlayerData <- rbind(select(nhl("goalie", team = team), data.firstName, data.lastName), select(SkaterPull, data.firstName, data.lastName))

FirstNames1 <- count(PlayerData, data.firstName) %>% arrange(desc(n)) %>% head(10) %>% rename(First.Name = data.firstName)
kable(FirstNames1, caption = "10 Most Common First Names") %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
10 Most Common First Names
</caption>
<thead>
<tr>
<th style="text-align:left;">
First.Name
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mike
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
Dave
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
Bob
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
Jack
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
Bill
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
John
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
Paul
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
Ken
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Larry
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
Brian
</td>
<td style="text-align:right;">
10
</td>
</tr>
</tbody>
</table>

The number of last names might be more interesting since that may reveal
more information about nationality than first names.

``` r
#Generate player last names table

LastNames1 <- count(PlayerData, data.lastName) %>% arrange(desc(n)) %>% head(10) %>% rename(Last.Name =data.lastName)
kable(LastNames1, caption = "10 Most Common Last Names") %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
10 Most Common Last Names
</caption>
<thead>
<tr>
<th style="text-align:left;">
Last.Name
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Armstrong
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Martin
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Bailey
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Brown
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Hamilton
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Jackson
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Johnson
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Wilson
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Adams
</td>
<td style="text-align:right;">
3
</td>
</tr>
</tbody>
</table>

Let’s put the top 20 first names and the top 20 last names into bar
charts and see what the relative frequencies are. We will remove the
remainder to make the charts visible.

``` r
#Create bar charts

FirstNames2 <- count(PlayerData, data.firstName) %>% arrange(desc(n)) %>% head(20)  %>% rename(First.Name = data.firstName) 
#%>% mutate(name=factor(name, levels=name))
LastNames2 <- count(PlayerData, data.lastName) %>% arrange(desc(n)) %>% head(20)  %>% rename(Last.Name =data.lastName)

g7 <- ggplot(FirstNames2, aes(x = reorder(First.Name, desc(n))))
g7 + geom_bar(aes(weight = n), color = "white", fill = "darkolivegreen") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("Top 20 First Names") + ylab("Count")
```

![](/README_files/figure-gfm/names3-1.png)<!-- -->

``` r
g8 <- ggplot(LastNames2, aes(x = reorder(Last.Name, desc(n))))
g8 + geom_bar(aes(weight = n), color = "white", fill = "darkolivegreen") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("Top 20 Last Names") + ylab("Count")
```

![](/README_files/figure-gfm/names3-2.png)<!-- -->

# Bonus: Team Reg. Season Win % by Playoff Outcome

This is just because I was curious and wanted to explore more of the
data. The endpoint wasn’t in the list, but I found it in the API
documentation and it had interesting data.

This is a series of boxplots showing the distribution of win percentages
by the furthest point a team got in the playoffs. I would have expected
it to be increasing monotonically, but playoff series being what they
are, it flattens out (declines maybe?) after the first second because
regular season success can be undone by small sample playoff randomness!
I guess the *best* team doesn’t always win the championship.

``` r
#There are 38 team IDs
#Data pull
for (i in 1:38) {
  teamseas_url <- paste0(base_url, "/franchise-season-results?cayenneExp=franchiseId=",toString(i),"&sort=seasonId&dir=DESC")
  get_teamseas <- GET(teamseas_url)
  txt_teamseas <- content(get_teamseas, "text", encoding = "UTF-8")
  json_teamseas <- fromJSON(txt_teamseas, flatten = TRUE)
  if (i == 1) {TeamData <- as.data.frame(json_teamseas)} else {TeamData <- rbind(TeamData, as.data.frame(json_teamseas))}
}
TeamData <- TeamData %>% mutate(Playoff.Series = ifelse(is.na(data.seriesTitle), "No Playoffs", data.seriesTitle))
TeamData$Playoff.Series <- factor(TeamData$Playoff.Series, levels = c("No Playoffs", "Stanley Cup Qualifiers", "1st Round", "2nd Round", "Conference Finals", "Stanley Cup Final"))
#NHL Final and Semifinal occurred only in one season
RegSeason <- TeamData %>% mutate(Win.Percentage = data.wins / data.gamesPlayed) %>% filter(data.gamesPlayed > 30, data.seasonId >= 20132014)

#Create boxplot

g9 <- ggplot(RegSeason, aes(x = Playoff.Series, y = Win.Percentage, color = Playoff.Series))
g9 + geom_boxplot(fill = "white", colour = "black") + geom_point(position = "jitter") + labs(title = "Win Percentage by Playoff Outcome, 2014 - Present") + scale_x_discrete(labels = function(Playoff.Series) str_wrap(Playoff.Series, width = 10))
```

![](/README_files/figure-gfm/teamseas-1.png)<!-- -->

The Stanley Cup Qualifiers was a round instituted in the unusual 2020
season due to COVID-19, hence only a few observations in that bucket are
present.

One very interesting thing about hockey that isn’t often seen in other
major sports is that teams with losing records frequently make the
playoffs. Based on the analysis, they don’t often see tremendous success
in the postseason, but they sometimes make it.
