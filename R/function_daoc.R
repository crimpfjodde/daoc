
exists_guild = function(guild) {
  url <- paste0('https://herald.playphoenix.online/g/', guild)
  temp <- getURL(url)
  if(temp != '') {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

exists_player = function(player) {
  url <- paste0('https://herald.playphoenix.online/c/',player)
  temp <- getURL(url)
  if(temp != '') {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

tbl_4x4_numeric = function(x) {
  for (i in 2:4) {
    x[[i]] <- gsub(',', '', x[[i]])
    x[[i]] <- as.numeric(x[[i]])
  }
  x
}

get_rooster = function(x) {
  for (i in c(3,5:9)) {
    x[[i]] <- gsub(',', '', x[[i]])
    x[[i]] <- as.numeric(x[[i]])
  }
  x
}

time_tbl <- function(li, tid){
  s <- data.frame()
  for(i in 1:5) {
    s <- rbind(s, li[[i]][tid,])
  }
  rownames(s) <- c('RP', 'Deathblows', 'Deaths', 'Kills', 'Solo kills')
  s[,-1]
}

get_player = function(player) {
  url <- paste0('https://herald.playphoenix.online/c/',player)
  temp <- getURL(url)
  x <- readHTMLTable(temp)
  x
}

get_guild = function(guild) {
  url <- paste0('https://herald.playphoenix.online/g/', guild)
  temp <- getURL(url)
  x <- readHTMLTable(temp, as.data.frame = TRUE)
  x[[1]] <- tbl_4x4_numeric(x[[1]])
  x[[2]] <- tbl_4x4_numeric(x[[2]])
  x[[3]] <- tbl_4x4_numeric(x[[3]])
  x[[4]] <- tbl_4x4_numeric(x[[4]])
  x[[5]] <- tbl_4x4_numeric(x[[5]])
  x[[6]] <- tbl_4x4_numeric(x[[6]])
  x[[7]] <- get_rooster(x[[7]])
  x
}


get_deathblow = function(guild) {
  url <- paste0('https://herald.playphoenix.online/characters/deathblows?time-frame=all-time&filter=albion')
  temp <- getURL(url)
  x <- readHTMLTable(temp, as.data.frame = TRUE)[[1]]
  x <- x %>% filter(Guild == guild) %>% slice(1)
  for (i in c(6:9)) {
    x[[i]] <- gsub(',', '', x[[i]])
    x[[i]] <- as.numeric(x[[i]])
  }
  x
}

get_deaths = function(guild) {
  url <- paste0('https://herald.playphoenix.online/characters/deaths?time-frame=all-time&filter=albion')
  temp <- getURL(url)
  x <- readHTMLTable(temp, as.data.frame = TRUE)[[1]]
  x <- x %>% filter(Guild == guild) %>% slice(1)
  for (i in c(6:7)) {
    x[[i]] <- gsub(',', '', x[[i]])
    x[[i]] <- as.numeric(x[[i]])
  }
  x
}

get_solokills = function(guild) {
  url <- paste0('https://herald.playphoenix.online/characters/solokills?time-frame=all-time&filter=albion')
  temp <- getURL(url)
  x <- readHTMLTable(temp, as.data.frame = TRUE)[[1]]

  x <- x %>% filter(Guild == guild) %>% slice(1)
  for (i in c(6:9)) {
    x[[i]] <- gsub(',', '', x[[i]])
    x[[i]] <- as.numeric(x[[i]])
  }
  x
}


motaut <- function(m) {

  if (m == 0) {
    mot = 0.00
  } else if(m == 1) {
    mot = 0.01
  } else if(m == 2) {
    mot = 0.02
  } else if(m == 3) {
    mot = 0.03
  } else if(m == 4) {
    mot = 0.05
  } else if(m == 5) {
    mot = 0.07
  } else if(m == 6) {
    mot = 0.09
  } else if(m == 7) {
    mot = 0.11
  } else if(m == 8) {
    mot = 0.13
  } else {
    mot = 0.15
  }
  mot
}

dexut <- function(d) {

  if (d == 0) {
    mot = 0.00
  } else if(d == 1) {
    mot = 4
  } else if(d == 2) {
    mot = 8
  } else if(d == 3) {
    mot = 12
  } else if(d == 4) {
    mot = 17
  } else if(d == 5) {
    mot = 22
  } else if(d == 6) {
    mot = 28
  } else if(d == 7) {
    mot = 34
  } else if(d == 8) {
    mot = 41
  } else {
    mot = 48
  }
  mot
}

costut <- function(c) {

  if (c == 0) {
    mot = 0
  } else if(c == 1) {
    mot = 1
  } else if(c == 2) {
    mot = 2
  } else if(c == 3) {
    mot = 4
  } else if(c == 4) {
    mot = 7
  } else if(c == 5) {
    mot = 10
  } else if(c == 6) {
    mot = 15
  } else if(c == 7) {
    mot = 20
  } else if(c == 8) {
    mot = 27
  } else {
    mot = 34
  }
  mot
}

dexcalc <- function(dex, augdex) {
  dex + dexut(augdex)
}

castspeed <- function(delve, dex, mota) {
  delve * (1 - ((dex - 60)/600)) * (1 - motaut(mota))
}
cspeed <- function(delve, dex, mota) {
  (delve) * (1 - ((dex - 60)/600)) * (1 - (mota))
}
dex1 <- function() {
  input$currentdex + dexut(input$dexaug1)
}
