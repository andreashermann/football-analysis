library(lubridate)
library(XML)
suppressMessages(library(dplyr))
library(tidyr)

loadTeam <- function(name) {
  cleanName <- cleanName(name)
  filename <- downloadTeam(cleanName)
  playerLinks <- parseTeamPlayerLinks(filename)
  names <- row.names(playerLinks)
  
  f <- downloadPlayer(names[1],playerLinks[1])
  players <- parsePlayer(names[1], f)
  
  for (i in 2:dim(playerLinks)[1]) {
    #print(paste0("loading player: ",names[i]))
    f <- downloadPlayer(names[i],playerLinks[i])
    p <- parsePlayer(names[i], f)
    players <- rbind(players, p)
  }
  players$team <- name
  players
}

cleanName <- function(name) {
  name <- gsub(" ","-",tolower(name))
  name <- gsub("\\.","",name)
  name
}

downloadTeam <- function(name) {
  url <- paste0("http://www.sfl.ch/superleague/klubs/",name,"/season/201415/")
  filename <- paste0("teams/",name,".html")
  if (!file.exists(filename)) {
    download.file(url, filename)
  }
  filename
}

parseTeamPlayerLinks <- function(filename) {
  document <- htmlTreeParse(filename, useInternalNodes = T)
  names <- xpathApply(document, "//table[@id='team']//a", xmlValue)
  attributes <- xpathApply(document, "//table[@id='team']//a", xmlAttrs)
  links <- sapply(attributes, function(e) { e["href"] })
  c <- cbind(link=paste0("http://www.sfl.ch/",links))
  row.names(c) <- names
  c
}

downloadPlayer <- function(name, link) {
  filename <- paste0("players/", cleanName(name), ".html")
  if (!file.exists(filename)) {
    Sys.sleep(3)
    download.file(link, filename)
  }
  filename
}

parsePlayer <- function(name, filename) {
  tables <- readHTMLTable(filename, stringsAsFactors = FALSE, header = FALSE)
  if (length(tables) == 3) {
    player.by_row <- rbind(tables[[1]], tables[[3]])
  } else {
    ## some player dont have a history table
    player.by_row <- rbind(tables[[1]], tables[[2]])
  }
  player.by_row <- rbind(player.by_row, data.frame(V1 = c("name"), V2 = c(name)))
  row.names(player.by_row) <- player.by_row$V1
  player <- data.frame(t(select(player.by_row, V2)), stringsAsFactors = F)
  player <- tbl_df(player)
  player <- mutate(player, birthday = dmy(Geburtstag))
  if ("Grösse" %in% colnames(player)) {
    player <- mutate(player, height = as.numeric(strsplit(player$Grösse, split = " ", fixed = T)[[1]][1]))
  } else {
    player <- mutate(player, height = NA)
  }
  
  if ("Gewicht" %in% colnames(player)) {
    player <- mutate(player, weight = as.numeric(strsplit(player$Gewicht, split = " ", fixed = T)[[1]][1]))
  } else {
    player <- mutate(player, weight = NA)
  }
  
  if ("Schüsse" %in% colnames(player)) {
    player <- mutate(player, shots = as.numeric(player$Schüsse))
  } else {
    player <- mutate(player, shots = NA)
  }
  
  if ("Tore" %in% colnames(player)) {
    player <- mutate(player, goals = as.numeric(player$Tore))
  } else {
    player <- mutate(player, goals = NA)
  }
  
  if ("Spielminuten" %in% colnames(player)) {
    player <- mutate(player, minutes.played = as.numeric(player$Spielminuten))
  } else {
    player <- mutate(player, minutes.played = NA)
  }
  
  if ("Position" %in% colnames(player)) {
    player <- mutate(player, position = Position)
  } else {
    player <- mutate(player, position = NA)
  }
  
  player %>% select(name, birthday, height, weight, shots, goals, minutes.played, position)
}