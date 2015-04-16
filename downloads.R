library(lubridate)
library(XML)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)

source("team.R")
source("player.R")

cleanName <- function(name) {
  name <- gsub(" ","-",tolower(name))
  name <- gsub("\\.","",name)
  name
}

parseLeague <- function() {
  ##con <- file("liga.html")
  ##htmlCode <- readLines(con)
  ##head(htmlCode)
  ##html <- htmlTreeParse("liga.html", useInternalNodes = T)
  ##xpathSApply(html, "", xmlValue)
  
  ## download league
  download.file("http://www.sfl.ch/superleague/matchcenter/","liga.html")
  
  ## parse data
  league.table <- readHTMLTable("liga.html", stringsAsFactors = FALSE)
  standings.total <- league.table[[1]]
  standings.home <- league.table[[2]]
  standings.away <- league.table[[3]]
  
  ## cleanup data
  standings.total <- tbl_df(standings.total)
  standings.total <- select(standings.total, Rang,Team,Spiele,S,U,N,Tore,Punkte)
  standings.total <- mutate(standings.total, rank = as.numeric(Rang))
  standings.total <- mutate(standings.total, Team = gsub("\t", "", Team))
  standings.total <- mutate(standings.total, Team = gsub("\r\n", ",", Team))
  standings.total <- separate(standings.total, Team, c("fullname","shortname"), sep=",")
  standings.total <- mutate(standings.total, games.played = as.numeric(Spiele))
  standings.total <- mutate(standings.total, victories = as.numeric(S))
  standings.total <- mutate(standings.total, draws = as.numeric(U))
  standings.total <- mutate(standings.total, defeats = as.numeric(N))
  standings.total <- mutate(standings.total, points = as.numeric(Punkte))
  standings.total <- separate(standings.total, Tore, c("goals.scored","goals.conceived"))
  
  standings.total <- select(standings.total, rank, fullname, shortname, games.played, 
                                  victories, draws, defeats, points, goals.scored, goals.conceived)
  
  ## plots
}

plotTeams <- function() {
  g <- ggplot(standings.total, aes(goals.scored, goals.conceived)) + 
    geom_point(color = "steelblue", size=4, alpha=1/2) + ggtitle("Swiss Super Leage Goals")
    labs(x = "Goals Scored", y = "Goals Conceived")
    geom_text(aes(label=fullname),hjust=0, vjust=-2)
}

loadAllPlayers <- function() {
  players <- rbind(
    loadTeam("FC Basel 1893"),
    loadTeam("FC Aarau"),
    loadTeam("FC Zuerich"),
    loadTeam("FC Luzern"),
    loadTeam("FC Sion"),
    loadTeam("FC St. Gallen"),
    loadTeam("FC Thun"),
    loadTeam("FC Vaduz"),
    loadTeam("Grasshopper Club"),
    loadTeam("BSC Young Boys")
  )
  players
}

bmiStats <- function(players) {
  players %>% 
    mutate(bmi = weight/(height/100)^2) %>%
    arrange(desc(bmi)) %>%
    print
  
  players %>%
    mutate(bmi = weight/(height/100)^2) %>%
    arrange(desc(bmi))
  
  qplot(players$name, players$bmi)
}

shotStats <- function(players, min.goals = 3) {
  stats <- players %>%
    filter(goals >= min.goals) %>%
    filter(minutes.played > 0) %>%
    ##filter(position == 'Stürmer' | position == 'Mittelfeldspieler') %>%
    mutate(shots.per.goal = shots/goals) %>%
    mutate(minutes.per.shot = minutes.played/shots) %>%
    mutate(minutes.per.goal = minutes.played/goals) %>%
    mutate(bmi = weight/(height/100)^2) %>%
    arrange(shots.per.goal, goals) %>%
    select(name, goals, shots, minutes.played, team, shots.per.goal, minutes.per.shot, minutes.per.goal)

  g <- ggplot(stats, aes(minutes.per.shot, shots.per.goal)) + 
    ggtitle("Analyse Superleague-Torschützen (>3 Tore)") + 
    labs(x = "Minuten pro Torschuss", y = "Schüsse pro Tor") +
    scale_y_continuous(limits = c(3,11), breaks=seq(2,11,0.5)) + 
    scale_x_continuous(limits = c(25,75), breaks=seq(0, 90, 5))  +
    geom_point(size=4, alpha=1/2, color="steelblue") + 
    geom_text(aes(label=paste0(name," (",goals,")")),hjust=0, vjust=-0.5) + 
    geom_hline(yintercept=5.25, linetype="solid", alpha=0.75) +
    geom_vline(xintercept=46.3, linetype="solid", alpha=0.75) +
    geom_rect(xmin=0, xmax=34.3, ymin=0, ymax=Inf, alpha=0.0025, fill='red') +
    geom_rect(xmin=0, xmax=Inf, ymin=0, ymax=4.0, alpha=0.0025, fill='red') +
    geom_text(label="Viele Chancen", x=24.25, y=11.1, colour = "red", hjust=0, alpha=0.075) + 
    #geom_segment(x=33, xend=23.25, y=10.9, yend=10.9, arrow=arrow(), size=1, color="red") +
    geom_text(label="Effizient", x=73.5, y=2.75, colour = "red", hjust=0, alpha=0.075) + 
    #geom_segment(x=75, xend=75, y=3.75, yend=3.0, arrow=arrow(), size=1, color="red") +
    theme(legend.position="none")
  g
  
  #0%       25%       50%       75%      100% 
  ##1.333333  4.000000  5.250000  7.000000 13.333333 
  #0%       25%       50%       75%      100% 
  #23.70000  34.30769  46.31250  54.37500 259.50000 
}

## players %>% group_by(team) %>% 
## summarise(total.goals = sum(goals, na.rm = T), total.shots = sum(shots, na.rm = T)) %>% 
## mutate(shots.per.goal = total.shots/total.goals) %>% 
## arrange(desc(total.goals)

## ggplot(teams, aes(total.goals, shots.per.goal)) + geom_point() + geom_text(aes(label=team), hjust=0, vjust=-0.5)
## ggplot(teams, aes(total.shots, shots.per.goal)) + geom_point() + geom_text(aes(label=team), vjust=-0.5)
