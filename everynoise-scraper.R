library(httr)
library(httpuv)
library(rvest)
library(stringr)
library(ggplot2)
library(gridExtra)

### Doing it with one city only

BirminghamReaded <- read_html("http://everynoise.com/everyplace.cgi?root=Birmingham%20GB&scope=all")

BirminghamList <- html_text(html_node(BirminghamReaded, css = "div.note > div"))

BirminghamGenresL <- strsplit(BirminghamList, "\n")

BirminghamGenresV <- unlist(BirminghamGenresL)

### Because the first few characters are \n, which is meaningless, so gives a first empty value, so we remove it

BirminghamGenresV <- BirminghamGenresV[-1]

### As a function now, to apply it to all cities of interest

Cities <- c("London", "Birmingham", "Manchester", "Leeds", "Sheffield", "Glasgow", "Edinburgh", "Bristol", "Liverpool", "Leicester", "Bradford", "Cardiff", "Belfast")

ParticularLikesFUN <- function(x) {
  City <- x
  City_EN_URL <- str_c("http://everynoise.com/everyplace.cgi?root=", City, "%20GB&scope=all", sep = "", collapse = NULL)
  City_Readed <- read_html(City_EN_URL)
  City_Genres <- html_text(html_node(City_Readed, css = "div.note > div"))
  City_GenreL <- strsplit(City_Genres, "\n")
  City_GenreV <- unlist(City_GenreL)
  City_GenreV <- City_GenreV[-1]
  City_GenreV <- City_GenreV[1:20]
  factor(City_GenreV)
  print(City_GenreV)
}

ParticularLikesAllCities <- lapply(Cities, ParticularLikesFUN)
names(ParticularLikesAllCities) <- Cities

CitiesLikes <- ParticularLikesAllCities
CitiesLikesDAT <- as.data.frame(CitiesLikes)

grid.table(CitiesLikesDAT)