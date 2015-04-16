
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
  
  ##games.played = as.numeric(Spiele), 
  ##minutes.played = as.numeric(Spielminuten),
  ##goals = as.numeric(Tore),
  ##assists = as.numeric(Assists),
  ##substitutes.out = as.numeric(Auswechslungen),
  ##substitutes.in = as.numeric(Einwechslungen),
  ##yellow.cards = as.numeric("Gelbe Karten"),
  ##red.cards = as.numeric("Rote Karten")
  player %>% select(name, birthday, height, weight, shots, goals, minutes.played, position)
}