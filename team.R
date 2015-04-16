
downloadTeams <- function() {
  download.file("http://www.sfl.ch/superleague/klubs/bsc-young-boys/","bsc-young-boys.html")
  download.file("http://www.sfl.ch/superleague/klubs/fc-basel-1893/","fc-basel-1983.html")
  download.file("http://www.sfl.ch/superleague/klubs/fc-sion/","fc-sion")
  download.file("http://www.sfl.ch/superleague/klubs/fc-st-gallen/","fc-st-gallen.html")
  download.file("http://www.sfl.ch/superleague/klubs/fc-thun/","fc-thun.html")
  download.file("http://www.sfl.ch/superleague/klubs/fc-zuerich/","fc-zuerich.html")
  download.file("http://www.sfl.ch/superleague/klubs/grasshopper-club/","grasshopper-club.html")
  download.file("http://www.sfl.ch/superleague/klubs/fc-vaduz/","fc-vaduz.html")
  download.file("http://www.sfl.ch/superleague/klubs/fc-aarau/","fc-aarau.html")
  download.file("http://www.sfl.ch/superleague/klubs/fc-luzern/","fc-luzern.html")
}

parseAllTeams <- function() {
  all.players <- rbind(
    parseTeam("bsc-young-boys.html","BSC Young Boys"),
    parseTeam("fc-basel-1983.html","FC Basel 1983"),
    parseTeam("fc-sion.html","FC Sion"),
    parseTeam("fc-st-gallen.html","FC St. Gallen"),
    parseTeam("fc-zuerich.html","FC Zürich"),
    parseTeam("grasshopper-club.html","Grasshopper Club"),
    parseTeam("fc-vaduz.html","FC Vaduz"),
    parseTeam("fc-aarau.html","FC Aarau"),
    parseTeam("fc-luzern.html","FC Luzern")
  )
  all.players
}

loadTeam <- function(name) {
  cleanName <- cleanName(name)
  filename <- downloadTeam(cleanName)
  playerLinks <- parseTeamPlayerLinks(filename)
  
  names <- row.names(playerLinks)
  
  f <- downloadPlayer(names[1],playerLinks[1])
  players <- parsePlayer(names[1], f)
  
  for (i in 2:dim(playerLinks)[1]) {
    print(paste0("loading player: ",names[i]))
    f <- downloadPlayer(names[i],playerLinks[i])
    p <- parsePlayer(names[i], f)
    players <- rbind(players, p)
  }
  players$team <- name
  players
}

downloadTeam <- function(name) {
  url <- paste0("http://www.sfl.ch/superleague/klubs/",name,"/season/201415/")
  filename <- paste0("teams/",name,".html")
  if (!file.exists(filename)) {
    download.file(url, filename)
  }
  filename
}

parseTeam2 <- function(name, filename) {
  ## team.tables[[2]] = coaches
  tables <- readHTMLTable(filename, stringsAsFactors = FALSE)
  players <- team.tables[[1]]
  names(players) <- c("number","name","position","birthday","nationality","games.played","goals","assists","yellow.cards","red.cards")
  players <- tbl_df(players)
  
  players <- 
    players %>% 
    ## separate(name, c("fullname","firstname"))
    mutate(birthday = dmy(birthday)) %>%
    mutate(games.played = as.numeric(games.played)) %>%
    mutate(goals = as.numeric(goals)) %>%
    mutate(assists = as.numeric(assists)) %>%
    mutate(yellow.cards = as.numeric(yellow.cards)) %>%
    mutate(red.cards = as.numeric(red.cards)) %>%
    mutate(team.name = team.name)
  players
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