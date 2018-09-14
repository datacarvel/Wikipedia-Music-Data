### So here we are! ###

### Opening the packages we are likely to need

library(httr)
library(WikipediR)
library(WikidataQueryServiceR)
library(rlist)
library(WikidataR)
library(jsonlite)

### Or

libraries <- c('httr', 'WikipediR', 'WikidataQueryServiceR', 'rlist', 'WikidataR', 'jsonlite')

lapply(libraries, FUN = function(y) {
  do.call('require', list(y))})

### Tutorial for using WDQS : https://www.wikidata.org/wiki/Wikidata:SPARQL_tutorial

### Getting all 'instance of (P31)' 'music genre (Q188451)' - Helped by this query builder -> https://query.wikidata.org/

sparql_query_music <- 'SELECT DISTINCT ?musicgenre ?musicgenreLabel
WHERE
{
  ?musicgenre wdt:P31 wd:Q188451.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}'

  music_genres_entities <- query_wikidata(sparql_query_music)
  music_genres_entities
  
### Saving the data in a R-friendly format
  
  saveRDS(music_genres_entities, file = "music_genres.csv")
  
### It can be loaded back with readRDS("music_genres.csv")
  
### We can inspect how the data looks like now - it looks like a data.frame, and see the first few entries with head()
  
  str(music_genres_entities)
  head(music_genres_entities)
  
### It would be convenient to group genres by their broader or parent genre, for example putting grunge in rock, black metal in heavy metal, and dubstep in electronic
### The code below will do this... sort of. 
  
  sparql_query_music_with_parent <- 'SELECT ?genre ?genreLabel ?parent ?parentLabel
  WHERE
  {
  ?genre wdt:P31/wdt:P279* wd:Q188451;
  p:P279 [ ps:P279 ?parent ].
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
  }'
  
  music_genres_entities_with_parent <- query_wikidata(sparql_query_music_with_parent)
  head(music_genres_entities_with_parent)
  
### A couple of issues here: from our close-to-1600 previous rows, we now have 942 of them... and this includes duplicates!
### This is likely because some genres have no parent/broader genre. And there are duplicates because some genres are associated with more than one parent genre. For instance, Britpop is considered a subclass or Pop music, Beat music, Power pop and Mod revival. 
### What's more, each of those 4 parent genres can be associated with other broader genres as well. 
### Is there any way to (1) keep orphelin genres in our dataset (we'll sort them out later) and (2) stick to one parent genre only, for instance the broadest?
### For (1), yes; and (2), the issue is that the scope of the upper subclass really varies. While some genres will be directly associated with still-narrow subclasses like "psychedelic rock", others will be straight-forwardly associated with "rock music", which is our ideal broadness level of genre.
### So for now we will (1) make our parentLabel optional so orphelin genres will stay in our data, and (2) we will collect all levels of subclasses associated with a specific, precise genre. 
### For this latter, we will also make sure our script will stop traveling to all subclasses of subclasses at some point, otherwise it would collect data beyond our topic of music genres (like "art", "human expression", etc.). So we will limit our query to "instances of" "music genre" (which can be either specific or very broad, from "glam metal" to "popular music"). 
  
  sparql_query_music_with_parent_full <- 'SELECT ?genre ?genreLabel ?parentLabel
  WHERE
  {
  ?genre wdt:P31/wdt:P279* wd:Q188451.
  ?genre rdfs:label ?genreLabel.
  OPTIONAL {
  ?genre wdt:P279+ ?parent.
  ?parent wdt:P31/wdt:P279* wd:Q188451.
  ?parent rdfs:label ?parentLabel.
  FILTER(LANG(?parentLabel) = "en").
  }
  FILTER(LANG(?genreLabel) = "en").
  }
  ORDER BY ?genreLabel
  '
  
  music_genres_entities_with_parent_full <- query_wikidata(sparql_query_music_with_parent_full)
  head(music_genres_entities_with_parent_full)
  str(music_genres_entities_with_parent_full)
  
### So now, having a given genre spread across multiple rows because of all its associated broader classes of genres isn't very convenient.
### We will therefore try to put them in a single cell, separated by a comma. We can do this using GROUP_CONCAT, and not forgetting to group the data so our code knows which values to concatenate around which other values (GROUP BY):
  
  sparql_query_music_with_parent_full <-'SELECT ?genre ?genreLabel (GROUP_CONCAT(?parentLabel ; SEPARATOR=", ") AS ?parents)
  WHERE
  {
  ?genre wdt:P31/wdt:P279* wd:Q188451.
  ?genre rdfs:label ?genreLabel.
  OPTIONAL {
  ?genre wdt:P279+ ?parent.
  ?parent wdt:P31/wdt:P279* wd:Q188451.
  ?parent rdfs:label ?parentLabel.
  FILTER(LANG(?parentLabel) = "en").
  }
  FILTER(LANG(?genreLabel) = "en").
  }
  GROUP BY ?genre ?genreLabel'
  
  music_genres_entities_with_parent_full <- query_wikidata(sparql_query_music_with_parent_full)
  head(music_genres_entities_with_parent_full)
  str(music_genres_entities_with_parent_full)
  
### So now we are done with querying Wikidata. The other data of interest is in each genre's specific Wikipedia article (and not Wikidata, another project).
### Each article of a genre has a very convenient table on the top right of the page. But before that, we need to know what is the Wikipedia URL of all genres!
  
### A way to do this is using the Wikidata API, from which JSON data we will be able to associate a Wikidata ID code with the URL of the genre's article.
  
### Here is the URL of a query (you may want to install a JSON reader extension in your browser):
### For "electronicore": https://www.wikidata.org/wiki/Special:EntityData/Q2084353.json
  
### So we need to give each genre (row) its respective URL for this JSON data. I'll simply extract the QXXXXXX ID from each row of the first column:
  
  music_genres_gettingURLs <- within(music_genres_entities_with_parent_full_test, genre <- data.frame(do.call('rbind', strsplit(as.character(genre), '/entity/', fixed = TRUE))))
  head(music_genres_gettingURLs)
  
### I'll then concatenate this new genre.X2 data with the base URL for all of our data, in a new column named FullWikiDataUrl:
  
  music_genres_gettingURLs$FullWikiDataURL <- paste("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=", music_genres_gettingURLs$genre$X2, "&props=labels|sitelinks&languages=en&sitefilter=enwiki&format=json&props=sitelinks/urls", sep = "")
  head(music_genres_gettingURLs$FullWikiDataURL)
  
### So now we're got all URL of our JSON data, from which we simply want to extract another URL, this time of the WikiPEDIA page, and not WikiDATA!
  
### I'll just rename my up-to-date dataset with a shorter and clearer name
  
  music_genres_gettingURLs <- MusicDataset
  music_genres_gettingURLs$FullWikiDataURL <- MusicDataset$apiURLwithID ### (because that's what it is actually)
  
### Now what happens when I query a given music genre's Wikidata JSON data?
### Example for the first entry, which is "Electronicore":
  
  fromJSON(MusicDataset$apiURLwithID[1])
  
### I get the full data from my API query, submitted through my URL with parameters and arguments
### But I only want the URL really, but not only this, I'd like to store it, and do it for each row / music genre.
### The information I want is under "$`entities`$`Q2084353`$sitelinks$`enwiki`$`url`", so here I can do the same query by adding this code at the end:
  
  fromJSON(MusicDataset$apiURLwithID[1])$`entities`$`Q2084353`$sitelinks$`enwiki`$`url`
  
### But notice the presence of the QXXXXXXX again: it will change for every row, so we have to tell R to go at some repetitive position in the hierarchy of the data
  
  fromJSON(MusicDataset$apiURLwithID[1])$`entities`[[1]][3]$`sitelinks`$`enwiki`$`url`
  
### It will work also with "Cowpunk" and "Rap rock", but not "Free rock", which has no English wikipedia entry, so it has no URL link and we will be given a NULL. This project will stick to music genres with an English Wikipedia page for now.
  
  fromJSON(MusicDataset$apiURLwithID[7])$`entities`[[1]][3]$`sitelinks`$`enwiki`$`url`
  fromJSON(MusicDataset$apiURLwithID[12])$`entities`[[1]][3]$`sitelinks`$`enwiki`$`url`
  fromJSON(MusicDataset$apiURLwithID[14])$`entities`[[1]][3]$`sitelinks`$`enwiki`$`url`
  
### For some reason, I previously had to use the readLines() function just inside the fromJSON() one in order to work, so if for any reason it isn't working, add it inside fromJSON()!
  
### So now, we can start collecting our music genres' Wikipedia article URLs! We'll store the results in a new colunn:
  
  MusicDatasetTXT <- for (i in MusicDataset$apiURLwithID) {
    if (!is.null(fromJSON(i)$`entities`[[1]][3]$`sitelinks`$`enwiki`$`url`)) {
      print(fromJSON(i)$`entities`[[1]][3]$`sitelinks`$`enwiki`$`url`)
    } else {
      print("NA")
    }
  }
  
  MusicDatasetTXT <- lapply(MusicDataset$apiURLwithID, function(x) {
    if (!is.null(fromJSON(x)$`entities`[[1]][3]$`sitelinks`$`enwiki`$`url`)) {
      y <- fromJSON(x)$`entities`[[1]][3]$`sitelinks`$`enwiki`$`url`
    } else {
      y <- "NA"
    }
    print(y)
  }) 
  
### Adding the new links data to our existing dataset, no need to use cbind() here! This works because the number of rows are exactly the same - 1469. And I took take to make sure the order was not altered in the process, so each link goes with its rightful music genre.
  
  MusicDataset$ArticleURLenglish <- unlist(MusicDatasetTXT)
  head(MusicDataset, n = 20)
  
### Now, I am totally saving all this, in a R-friendly format of course, even though R can be quite user-hostile.
  
  saveRDS(MusicDataset, file = "MusicDatasetR")
  
### Now the next huge part: actually mining/scraping the content I most want in the end. 

  ### So here we are! ###
  
  ### Loading the likely-ish-to-be-needed packages
  
  library(httr)
  library(rlist)
  library(jsonlite)
  library(rvest)
  library(xml2)
  library(stringr)
  library(dplyr)
  library(splus2R)
  
  ### Or
  
  libraries <- c('httr', 'rlist', 'jsonlite', 'rvest', 'xml2', 'stringr')
  
  lapply(libraries, FUN = function(y) {
    do.call('require', list(y))})
  
  ### MusicDataset, containing all links, is loaded in my environment already
  
  MusicDataset <- subset(MusicDataset, select = -apiURLwithID_LIST)
  head(MusicDataset)
  
  ### So, should I create a list? As follow: Each item of the list (item here = music genre), would be storing 4 vectors containing a range of values: Cultural origins, Stylistic origins, Derivative forms, Fusion genres
  ### For instance: CulturalOrigins <- c("Ska", "Punk rock", "Rocksteady", "New wave", "Reggae")
  
  ### But before we are to worry about iterating through all those links, we must know how to mine a single page's content!
  
  ### BUT WAIT AGAIN! It would also be wise to imagine how we'd like our data to look like once scraped. Should we do vectors, lists, dataframes, etc.?
  
  ### My first guess would be creating a dataframe, where each row would be a genre and Cultural origins, Stylistic origins and others would be variables (columns)
  
  ### But I had a second thought and I think it would be much more convenient to create a list (musicgenres) of lists (specific genre) of vectors (stylistic-origins, cultural-origins, derivative-forms, fusion-genres, length-of-article) containing mostly string values. 
  ### It would be more flexible for what follow - creating a network graph, where each edge will be a row.
  ### For instance...
  ### "Two-tone -> Third wave ska"
  ### "Two-tone -> Ska punk"
  ### "Rocksteady -> Two-tone"
  ### "Reggae -> Two-tone"
  
  ### Unfortunately, it seems that the Mediawiki API does not have anything about Infoboxes, which contain our data of interest.
  ### And although dbpedia.org is doing exactly that, it seems the data can be either out of date or incomplete, as trying with Two-tone and Emo was inconclusive, so it will probably be so too many other genres.
  ### So scrape the data, we will. Make sure the rvest package is loaded.
  
  ### For what follows, I just needed to use my browser's developer tool to find my CSS Select, which is done simply by inspecting an element, right-clicking, and copy the CSS Selector, and edit and play with it to get exactly what I need.
  
  ### But no, it's not that simple, because doing so gives you the exact CSS Selector path, which actually differs from article to article, even though it's the same infobox.
  
  ### So I had to play with the CSS selector to find one that will find Stylistic origins in all infoboxes, a good start is to remove the > signs in the path, and replace them with nothing, so my scraper will be looking for data more broadly
  
  # Testing for Stylistic origins with a few random genres, defining their URL and XML first
  
  thrash_metal_test <- read_html("https://en.wikipedia.org/wiki/Thrash_metal")
  wizard_rock_test <- read_html("https://en.wikipedia.org/wiki/Wizard_rock")
  pop_metal_test <- read_html("https://en.wikipedia.org/wiki/Glam_metal")
  chiptune_test <- read_html("https://en.wikipedia.org/wiki/Chiptune")
  electro_house_test <- read_html("https://en.wikipedia.org/wiki/Electro_house")
  psychobilly_test <- read_html("https://en.wikipedia.org/wiki/Psychobilly")
  
  # Then trying them out with a CSS selector.
  
  html_text(html_nodes(wizard_rock_test, css = ".infobox tbody tr:nth-child(2) li"))
  html_text(html_nodes(thrash_metal_test, css = ".infobox tbody tr:nth-child(2) li"))
  html_text(html_nodes(pop_metal_test, css = ".infobox tbody tr:nth-child(2) li"))
  html_text(html_nodes(chiptune_test, css = ".infobox tbody tr:nth-child(2) li"))
  html_text(html_nodes(electro_house_test, css = ".infobox tbody tr:nth-child(2) li"))
  html_text(html_nodes(psychobilly_test, css = ".infobox tbody tr:nth-child(2) li"))
  
  # So here are some problems: Stylistic origins isn't always at the same position within the infobox, lke with Pop metal. Also, Glitch hop is actually in the second infobox of an article, because the genre hasn't got an article of itself only (it's within "Glitch")
  
  # Trying with Cultural origins anyway:
  
  html_text(html_nodes(wizard_rock_test, css = ".infobox th"))
  html_text(html_nodes(thrash_metal_test, css = ".infobox tbody tr:nth-child(3) td"))
  html_text(html_nodes(pop_metal_test, css = ".infobox .hlist tr"))
  html_text(html_nodes(pop_metal_test, css = ".infobox tr td:nth-child(1) ul:nth-child(1) li"))
  html_text(html_nodes(glitch_hop_test, css = ".infobox tbody tr:nth-child(3) td"))
  html_text(html_nodes(chiptune_test, css = ".infobox tbody tr:nth-child(3) td"))
  html_text(html_nodes(electro_house_test, css = ".infobox tbody tr:nth-child(3) td"))
  html_text(html_nodes(psychobilly_test, css = ".infobox tbody tr:nth-child(3) td"))
  
  # Again, does not work with our two problematic pages at the previous step. But actually, all infoboxes aren't the same, Cultural origins can be the third or fourth row of the box, and the available information for all the rest differs as well - some have a "Other names", "Subgenres", "Fusion", etc.
  
  # I'll spare yourself the details of it all, but I spent much time trying things, first with Thrash metal only. Took a nightmarish while to come up with this probably perfectible and inefficient operation below:
  
  # So we got table headers and their values. The former are easy to find: they got the <th> tag. I take them all, notice the commas (I take more than enough, inexisting values will simply stay inexistent, it will not throw me an error):
  # I leave out the first child because it's only the repetition of the music genre label, there's no value with it.
  
  csstest <- ".infobox tr:nth-child(2) th, .infobox tr:nth-child(3) th, .infobox tr:nth-child(4) th, .infobox tr:nth-child(5) th, .infobox tr:nth-child(6) th, .infobox tr:nth-child(7) th, .infobox tr:nth-child(8) th, .infobox tr:nth-child(9) th, .infobox tr:nth-child(10) th, .infobox tr:nth-child(11) th, .infobox tr:nth-child(12) th"
  
  thrash_metal_headers <- html_text(html_nodes(thrash_metal_test, css = csstest))
  
  # Those above will be my headers for my upcoming list of data
  # Now I simply convert it to a list, but a vector would probably be fine too
  
  thrash_metal_headers_list <- as.list(thrash_metal_headers)
  
  # Probably inefficient code, but those multiple lines below will grab the values associated with our previous headers.
  # What's quite convenient is that the data is already coming out as vectors! But some will be empty vectors, although will not be the same as NULL, which is important, I found out.
  # Notice the commas again. All my values can be found as <li> items (for "list" in HTML, a bit like bullet points), except, at least, Cultural origins, which is basic text. I am using the comma here because there is no way of knowing beforehand where this Cultural origins text will be in the infobox. But for each value it's either <li> objects or basic text, never both, so here the code will simply pick the one he finds (although it will duplicate values in a non-vector format, which I'll just remove later, it starts with \n)
  
  thrash_metal_values2 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(2) li, .infobox tr:nth-child(2) td"))
  thrash_metal_values3 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(3) li, .infobox tr:nth-child(3) td"))
  thrash_metal_values4 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(4) li, .infobox tr:nth-child(4) td"))
  thrash_metal_values5 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(5) li, .infobox tr:nth-child(5) td"))
  thrash_metal_values6 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(6) li, .infobox tr:nth-child(6) td"))
  thrash_metal_values7 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(7) li, .infobox tr:nth-child(7) td"))
  thrash_metal_values8 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(8) li, .infobox tr:nth-child(8) td"))
  thrash_metal_values9 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(9) li, .infobox tr:nth-child(9) td"))
  thrash_metal_values10 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(10) li, .infobox tr:nth-child(10) td"))
  thrash_metal_values11 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(11) li, .infobox tr:nth-child(11) td"))
  thrash_metal_values12 <- html_text(html_nodes(thrash_metal_test, css = ".infobox tr:nth-child(12) li, .infobox tr:nth-child(12) td"))
  
  # Putting this band (pun intended) of vectors in a list
  
  thrash_metal_full_list <- list(thrash_metal_values2, thrash_metal_values3, thrash_metal_values4, thrash_metal_values5, thrash_metal_values6, thrash_metal_values7, thrash_metal_values8, thrash_metal_values9, thrash_metal_values10, thrash_metal_values11, thrash_metal_values12)
  
  names(thrash_metal_full_list_with_headers) <- thrash_metal_headers
  
  thrash_metal_full_list_with_headers
  
  # Now you can see that headers and values don't add up. My list has empty vectors of length 0 - not NULL! And some values are not with their rightful header.
  # This is because, if you look at the Thrash metal page infobox, some values are inline with their headers, while some have their header above them. So this means some rows will have only a header, while other will have only values.
  # Long story short, we can just go back to our list of values without the headers, get rid of the list items that are empty vectors, and then the headers and values should add up and match fully. Think of this operation as one in which your spreadsheet has two columns of data, but one of them have some extra empty cells between values for some reason but the number of occupied cells between the two columns is the same, so we delete those empty cells.
  
  thrash_metal_full_list <- list(thrash_metal_values2, thrash_metal_values3, thrash_metal_values4, thrash_metal_values5, thrash_metal_values6, thrash_metal_values7, thrash_metal_values8, thrash_metal_values9, thrash_metal_values10, thrash_metal_values11, thrash_metal_values12)
  
  thrash_metal_full_list <- thrash_metal_full_list[lapply(thrash_metal_full_list, length) > 0]
  thrash_metal_full_list
  
  names(thrash_metal_full_list) <- thrash_metal_headers
  
  thrash_metal_full_list
  
  thrash_metal_full_list_clean <- thrash_metal_full_list
  
  # So now my scraping operation is successful... for Thrash metal. We have to do this for all genres now!
  
  # Not all music genre entries in Wikidata have a Wikipedia page in English. So for the sake of moving on with this projet, we'll simply get rid of genres without an English page - because I don't speak German just yet, nor Dutch, nor Spanish, and so on. 
  
  MusicDatasetEN <- subset(MusicDataset, !MusicDataset$ArticleURLenglish == "NA")
  
  # Trying to make a function out of my Thrash metal test above...
  
  #lapply(MusicDatasetEN$ArticleURLenglish, get_infobox)
  
  get_infobox <- function(x) {
    url <- x
    readed_html <- read_html(url)
    css_selector_headers <- ".infobox tr:nth-child(2) th, .infobox tr:nth-child(3) th, .infobox tr:nth-child(4) th, .infobox tr:nth-child(5) th, .infobox tr:nth-child(6) th, .infobox tr:nth-child(7) th, .infobox tr:nth-child(8) th, .infobox tr:nth-child(9) th, .infobox tr:nth-child(10) th, .infobox tr:nth-child(11) th, .infobox tr:nth-child(12) th, .infobox tr:nth-child(13) th, .infobox tr:nth-child(14) th, .infobox tr:nth-child(15) th, .infobox tr:nth-child(16) th"
    headers <- html_text(html_nodes(readed_html, css = css_selector_headers))
    headers_list <- list(headers)
    readed_html_values_1 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(1) td li, .infobox tr:nth-child(1) td"))
    readed_html_values_2 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(2) td li, .infobox tr:nth-child(2) td"))
    readed_html_values_3 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(3) td li, .infobox tr:nth-child(3) td"))
    readed_html_values_4 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(4) td li, .infobox tr:nth-child(4) td"))
    readed_html_values_5 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(5) td li, .infobox tr:nth-child(5) td"))
    readed_html_values_6 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(6) td li, .infobox tr:nth-child(6) td"))
    readed_html_values_7 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(7) td li, .infobox tr:nth-child(7) td"))
    readed_html_values_8 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(8) td li, .infobox tr:nth-child(8) td"))
    readed_html_values_9 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(9) td li, .infobox tr:nth-child(9) td"))
    readed_html_values_10 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(10) td li, .infobox tr:nth-child(10) td"))
    readed_html_values_11 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(11) td li, .infobox tr:nth-child(11) td"))
    readed_html_values_12 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(12) td li, .infobox tr:nth-child(12) td"))
    readed_html_values_13 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(13) td li, .infobox tr:nth-child(13) td"))
    readed_html_values_14 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(14) td li, .infobox tr:nth-child(14) td"))
    readed_html_values_15 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(15) td li, .infobox tr:nth-child(15) td"))
    readed_html_values_16 <- html_text(html_nodes(readed_html, css = ".infobox tr:nth-child(16) td li, .infobox tr:nth-child(16) td"))
    readed_html_values_list <- list(readed_html_values_1, readed_html_values_2, readed_html_values_3, readed_html_values_4, readed_html_values_5, readed_html_values_6, readed_html_values_7, readed_html_values_8, readed_html_values_9, readed_html_values_10, readed_html_values_11, readed_html_values_12, readed_html_values_13, readed_html_values_14, readed_html_values_15, readed_html_values_16)
    readed_html_values_list <- readed_html_values_list[lapply(readed_html_values_list, length) > 0]
    Sys.sleep(3) # Simply to give the script and the Wiki server some time - 3 seconds break
    names(readed_html_values_list) <- headers
    GenreData <- readed_html_values_list
    print(GenreData)
  }
  
  # *Terrifying drum-roll* Testing my function on a random genre...
  
  GenreListTest <- get_infobox("https://en.wikipedia.org/wiki/Electronicore")
  GenreListTest
  
  # After a few errors because of typos and lambda omissions, this function works! (This is my R function / script baptem here, mind you)
  # I know for a thing that some music genre pages won't have any infobox, we'll just politely get rid of them later.
  # So now we must 'apply' this to all urls, and store the scraped data somewhere. But we'll test on a few before.
  
  # This below works, one way or another
  
  GenreList2ndTest <- c("https://en.wikipedia.org/wiki/Electronicore", "https://en.wikipedia.org/wiki/Dubstep", "https://en.wikipedia.org/wiki/Thrash_metal")
  lapply(GenreList2ndTest, get_infobox)
  
  GenreList3rdTest <- MusicDatasetEN$ArticleURLenglish$V1[1:3]
  lapply(GenreList3rdTest, get_infobox)
  
  # So let's do this. Getting my URLs as a vector and naming this vector's values
  
  GenreURLsWithLabelsVector <- MusicDatasetEN$ArticleURLenglish$V1
  names(GenreURLsWithLabelsVector) <- MusicDatasetEN$genreLabel
  is.vector(GenreURLsWithLabelsVector)
  
  get_infobox(GenreURLsWithLabels[3])
  
  GenreURLsWithLabelsDF <- data.frame(MusicDatasetEN$genreLabel, GenreURLsWithLabels)
  
  # I also need to put this subset's columns back into character strings, because somewhere in the process they apparently became factors
  
  GenreURLsWithLabelsDF_Subset <- subset(GenreURLsWithLabelsDF[1:3,])
  GenreURLsWithLabelsDF_Subset[] <- lapply(GenreURLsWithLabelsDF_Subset, as.character)
  head(GenreURLsWithLabelsDF_Subset)
  
  DataSubsetList <- lapply(GenreURLsWithLabelsDF_Subset$GenreURLsWithLabels, get_infobox)
  names(DataSubsetList) <- GenreURLsWithLabelsDF_Subset$MusicDatasetEN.genreLabel
  str(DataSubsetList)
  
  # Works fine, let's move on to the last few final operations
  # Renaming my two columns in a more intuitive way, and setting them back as character strings rather than factors
  
  head(GenreURLsWithLabelsDF)
  names(GenreURLsWithLabelsDF) <- c("genreLabelString", "URLsString")
  GenreURLsWithLabelsDF[] <- lapply(GenreURLsWithLabelsDF, as.character)
  class(GenreURLsWithLabelsDF$genreLabelString)
  class(GenreURLsWithLabelsDF$URLsString)
  
  MusicDataList <- lapply(GenreURLsWithLabelsDF$URLsString, get_infobox)
  
  # So now my script has mulitple issues. 
  # The first one is that some genre pages have an extra picture in the infobox, below which there is a caption that qualifies as <td> but has not <th>, so it will mess up my order of headers and values.
  # Fortunately, this extra undesired text does not have the .hlist class, which all my other data of interest has, so if you look at my get_infobox function I added a .hlist to all my selectors that are looking for <td>.
  # The second one is that some pages won't have any infobox, but the genre will count has a list item anyway, so I should be able to names() them with no synchronisation issue, so it's not so much of an issue.
  # The third one is a bigger nightmare. Some genres are subgenres that apparently did not deserved their own page, so they were fitted as a section of its parent genre, for instance with Rapcore and Glitch hop, respectively within the Rap rock and Glitch genres pages.
  # And quite unconveniently, while CSS has a convenient pseudo-class feature, the one we would need most isn't implemented in the rvest package.
  # There is a workaround that is imperfect but should be doing the job, although further adjustments may be required (their ":nth-child(n)" position).
  # My solution has been to look at multiple random genre pages and inquire about infoboxes position (nth-child(n)). When there is a second infobox, it was the 16th and 35th child of its parent, while most first infoboxes are typically between the 1st and 6th child of their parent, so I am confident that in the case of two infoboxes of interest within a given page, I can first select the infobox that will be one of the eight first children of its parent, while I can isolately select any second infobox of interest by selecting all infoboxes after the 8th child exclusively. I also narrowed my CSS path, eliminating other infoboxes as explained below, so I should not capture more than those that are relevant to my analysis.
  # In my concrete words, that means I'll add to my css paths in my function, for both headers and values. My ".infobox" will now be ".infobox:nth-child(-n+8)" for grabbing the main infobox (one of the first 8 children) and ".infobox:nth-child(n+9)" for grabbing any other one (all the other children, starting from the 9th one). See the code below for an example.
  # And some infoboxes are infoboxes we don't want here (for instance, the second one in the Reggae page "Music of Jamaica").
  # Fortunately, the .infoboxes we want and those we don't want do not share the same classes. The ones we want are all classified as table.infobox.nowraplinks, while the others are classified as infobox.hlist (do not confuse with our previous .hlist that applied to <td> tags, not on .infobox - can be confusing). Sorted that out by using the CSS Path (not Selector) and looking for differences.
  # The fourth one is that for some infoboxes, there are some extra "table data" with no <th> before them, for instance the Rock music page has a "2018 in rock music" URL at the end of its first infobox. 
  # Again, I sorted that out by looking at the CSS Path, which give much more complete information than CSS Selector. The <td> in this extra bit of info we don't want do not has a .hlist class, so I added that to my CSS selector in my function (td.hlist)
  # And all of this only after looking at a few pages. 
  
  # Example for Glitch and Rap rock, selecting the appropriate children infoboxes
  
  html_text(html_nodes(glitch_test, css = ".infobox:nth-child(-n+8) tr:nth-child(3) td"))    #    infobox of interest is the 2nd child, so looking for infoboxes that will be anywhere in the first 8 child position will grab it. 
  html_text(html_nodes(glitch_test, css = ".infobox:nth-child(n+9) tr:nth-child(3) td"))    #    infobox of interest is the 16th child, so looking for infoboxes that will be anywhere from the 9th position or farther will grab it.
  
  html_text(html_nodes(rap_rock_test, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(3) td"))  #    infobox of interest is the 3rd child, so looking for infoboxes that will be anywhere in the first 8 child position will grab it.  
  html_text(html_nodes(rap_rock_test, css = ".infobox.nowraplinks:nth-child(n+9) tr:nth-child(3) td"))  #    infobox of interest is the 35th child, so looking for infoboxes that will be anywhere from the 9th position or farther will grab it.
  
  # So taking consideration the lengthy explanation below, here the new version of my get_infobox() function. I appreciate this is terrible code and that there probably is a much more efficient, readable way of writing it, but that will be for later - do let me know please!
  
  get_infobox_clean <- function(x) {
    
    url <- x
    
    # From the rvest package, read an html document
    
    readed_html <- read_html(url)
    
    # For when there is more than one Infobox, I did some testing and came up with those css selectors to have each infobox separately from any other in the page. Infoboxes or interest are never immediately following each other
    
    # :nth-child(-n+8)
    # :nth-child(n+9):nth-child(-n+32)
    # :nth-child(n+33):nth-child(-n+39)
    # :nth-child(n+40):nth-child(-n+55)
    # :nth-child(n+56)
    
    # Taking first the data categories (headers) such as "Stylistic origins", "Derivative forms", "Fusion genres", regardless of whether they are on the left or on top of their data
    
    css_selector_headersA_1_8 <- ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(2) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(3) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(4) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(5) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(6) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(7) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(8) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(9) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(10) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(11) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(12) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(13) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(14) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(15) th, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(16) th"
    css_selector_headersB_9_32 <- ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(2) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(3) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(4) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(5) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(6) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(7) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(8) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(9) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(10) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(11) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(12) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(13) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(14) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(15) th, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(16) th"
    css_selector_headersC_33_39 <- ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(2) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(3) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(4) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(5) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(6) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(7) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(8) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(9) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(10) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(11) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(12) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(13) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(14) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(15) th, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(16) th"
    css_selector_headersD_40_55 <- ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(2) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(3) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(4) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(5) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(6) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(7) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(8) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(9) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(10) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(11) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(12) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(13) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(14) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(15) th, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(16) th"
    css_selector_headersE_56_X <- ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(2) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(3) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(4) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(5) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(6) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(7) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(8) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(9) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(10) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(11) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(12) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(13) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(14) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(15) th, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(16) th"
    
    # Storing those headers
    
    headersA <- html_text(html_nodes(readed_html, css = css_selector_headersA_1_8))
    headersB <- html_text(html_nodes(readed_html, css = css_selector_headersB_9_32))
    headersC <- html_text(html_nodes(readed_html, css = css_selector_headersC_33_39))
    headersD <- html_text(html_nodes(readed_html, css = css_selector_headersD_40_55))
    headersE <- html_text(html_nodes(readed_html, css = css_selector_headersE_56_X))
    
    # Getting the main header of each possible infobox
    
    infobox_top_headerA <- html_text(html_nodes(readed_html, css = "#firstHeading")) # Some pages have an infobox with no main heading, but this latter is, for the first infobox only, the same as the article title (except for subtle differences like "Heavy metal music" and "Heavy metal"), so it's simpler and safer to use this #firstHeading ID.
    infobox_top_headerB <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(1) th"))
    infobox_top_headerC <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(1) th"))
    infobox_top_headerD <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(1) th"))
    infobox_top_headerE <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(1) th"))
    
    # Storing them more efficiently
    
    infobox_top_headersA_E <- c(infobox_top_headerA, infobox_top_headerB, infobox_top_headerC, infobox_top_headerD, infobox_top_headerE)
    
    # Now's the unefficient part. Getting all possible data of interest. A reminder, only a few pages have more than one infobox, so mostly be attentive to the first set (the A one)
    
    readed_html_values_1A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(1) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(1) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(1) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(1) td div.description")) # td span?
    readed_html_values_2A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(2) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(2) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(2) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(2) td div.description"))
    readed_html_values_3A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(3) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(3) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(3) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(3) td div.description"))
    readed_html_values_4A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(4) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(4) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(4) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(4) td div.description"))
    readed_html_values_5A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(5) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(5) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(5) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(5) td div.description"))
    readed_html_values_6A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(6) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(6) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(6) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(6) td div.description"))
    readed_html_values_7A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(7) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(7) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(7) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(7) td div.description"))
    readed_html_values_8A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(8) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(8) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(8) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(8) td div.description"))
    readed_html_values_9A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(9) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(9) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(9) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(9) td div.description"))
    readed_html_values_10A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(10) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(10) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(10) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(10) td div.description"))
    readed_html_values_11A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(11) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(11) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(11) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(11) td div.description"))
    readed_html_values_12A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(12) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(12) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(12) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(12) td div.description"))
    readed_html_values_13A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(13) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(13) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(13) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(13) td div.description"))
    readed_html_values_14A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(14) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(14) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(14) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(14) td div.description"))
    readed_html_values_15A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(15) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(15) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(15) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(15) td div.description"))
    readed_html_values_16A <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(-n+8) tr:nth-child(16) td.hlist li, .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(16) td.hlist:not(br):nth-child(1):not(.hlist-separated):not(li):not(ul), .infobox.nowraplinks:nth-child(-n+8) tr:nth-child(16) td:nth-child(2):not(li):not(ul):not(div), .infobox.nowraplinks tr.haudio:nth-child(16) td div.description"))
    
    readed_html_values_1B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(1) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(1) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(1) td:nth-child(2)"))
    readed_html_values_2B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(2) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(2) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(2) td:nth-child(2)"))
    readed_html_values_3B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(3) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(3) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(3) td:nth-child(2)"))
    readed_html_values_4B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(4) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(4) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(4) td:nth-child(2)"))
    readed_html_values_5B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(5) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(5) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(5) td:nth-child(2)"))
    readed_html_values_6B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(6) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(6) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(6) td:nth-child(2)"))
    readed_html_values_7B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(7) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(7) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(7) td:nth-child(2)"))
    readed_html_values_8B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(8) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(8) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(8) td:nth-child(2)"))
    readed_html_values_9B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(9) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(9) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(9) td:nth-child(2)"))
    readed_html_values_10B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(10) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(10) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(10) td:nth-child(2)"))
    readed_html_values_11B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(11) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(11) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(11) td:nth-child(2)"))
    readed_html_values_12B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(12) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(12) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(12) td:nth-child(2)"))
    readed_html_values_13B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(13) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(13) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(13) td:nth-child(2)"))
    readed_html_values_14B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(14) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(14) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(14) td:nth-child(2)"))
    readed_html_values_15B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(15) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(15) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(15) td:nth-child(2)"))
    readed_html_values_16B <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(16) td.hlist li, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(16) td.hlist, .infobox.nowraplinks:nth-child(n+9):nth-child(-n+32) tr:nth-child(16) td:nth-child(2)"))
    
    readed_html_values_1C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(1) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(1) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(1) td:nth-child(2)"))
    readed_html_values_2C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(2) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(2) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(2) td:nth-child(2)"))
    readed_html_values_3C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(3) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(3) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(3) td:nth-child(2)"))
    readed_html_values_4C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(4) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(4) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(4) td:nth-child(2)"))
    readed_html_values_5C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(5) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(5) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(5) td:nth-child(2)"))
    readed_html_values_6C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(6) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(6) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(6) td:nth-child(2)"))
    readed_html_values_7C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(7) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(7) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(7) td:nth-child(2)"))
    readed_html_values_8C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(8) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(8) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(8) td:nth-child(2)"))
    readed_html_values_9C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(9) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(9) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(9) td:nth-child(2)"))
    readed_html_values_10C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(10) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(10) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(10) td:nth-child(2)"))
    readed_html_values_11C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(11) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(11) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(11) td:nth-child(2)"))
    readed_html_values_12C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(12) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(12) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(12) td:nth-child(2)"))
    readed_html_values_13C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(13) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(13) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(13) td:nth-child(2)"))
    readed_html_values_14C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(14) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(14) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(14) td:nth-child(2)"))
    readed_html_values_15C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(15) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(15) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(15) td:nth-child(2)"))
    readed_html_values_16C <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(16) td.hlist li, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(16) td.hlist, .infobox.nowraplinks:nth-child(n+33):nth-child(-n+39) tr:nth-child(16) td:nth-child(2)"))
    
    readed_html_values_1D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(1) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(1) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(1) td:nth-child(2)"))
    readed_html_values_2D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(2) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(2) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(2) td:nth-child(2)"))
    readed_html_values_3D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(3) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(3) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(3) td:nth-child(2)"))
    readed_html_values_4D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(4) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(4) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(4) td:nth-child(2)"))
    readed_html_values_5D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(5) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(5) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(5) td:nth-child(2)"))
    readed_html_values_6D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(6) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(6) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(6) td:nth-child(2)"))
    readed_html_values_7D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(7) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(7) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(7) td:nth-child(2)"))
    readed_html_values_8D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(8) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(8) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(8) td:nth-child(2)"))
    readed_html_values_9D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(9) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(9) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(9) td:nth-child(2)"))
    readed_html_values_10D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(10) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(10) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(10) td:nth-child(2)"))
    readed_html_values_11D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(11) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(11) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(11) td:nth-child(2)"))
    readed_html_values_12D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(12) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(12) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(12) td:nth-child(2)"))
    readed_html_values_13D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(13) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(13) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(13) td:nth-child(2)"))
    readed_html_values_14D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(14) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(14) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(14) td:nth-child(2)"))
    readed_html_values_15D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(15) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(15) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(15) td:nth-child(2)"))
    readed_html_values_16D <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(16) td.hlist li, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(16) td.hlist, .infobox.nowraplinks:nth-child(n+40):nth-child(-n+55) tr:nth-child(16) td:nth-child(2)"))
    
    readed_html_values_1E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(1) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(1) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(1) td:nth-child(2)"))
    readed_html_values_2E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(2) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(2) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(2) td:nth-child(2)"))
    readed_html_values_3E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(3) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(3) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(3) td:nth-child(2)"))
    readed_html_values_4E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(4) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(4) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(4) td:nth-child(2)"))
    readed_html_values_5E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(5) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(5) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(5) td:nth-child(2)"))
    readed_html_values_6E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(6) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(6) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(6) td:nth-child(2)"))
    readed_html_values_7E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(7) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(7) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(7) td:nth-child(2)"))
    readed_html_values_8E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(8) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(8) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(8) td:nth-child(2)"))
    readed_html_values_9E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(9) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(9) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(9) td:nth-child(2)"))
    readed_html_values_10E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(10) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(10) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(10) td:nth-child(2)"))
    readed_html_values_11E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(11) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(11) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(11) td:nth-child(2)"))
    readed_html_values_12E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(12) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(12) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(12) td:nth-child(2)"))
    readed_html_values_13E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(13) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(13) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(13) td:nth-child(2)"))
    readed_html_values_14E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(14) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(14) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(14) td:nth-child(2)"))
    readed_html_values_15E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(15) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(15) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(15) td:nth-child(2)"))
    readed_html_values_16E <- html_text(html_nodes(readed_html, css = ".infobox.nowraplinks:nth-child(n+56) tr:nth-child(16) td.hlist li, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(16) td.hlist, .infobox.nowraplinks:nth-child(n+56) tr:nth-child(16) td:nth-child(2)"))
    
    # Storing all grabbed values (but not mixing them up) in a list.
    
    readed_html_values_listA <- list(readed_html_values_1A, readed_html_values_2A, readed_html_values_3A, readed_html_values_4A, readed_html_values_5A, readed_html_values_6A, readed_html_values_7A, readed_html_values_8A, readed_html_values_9A, readed_html_values_10A, readed_html_values_11A, readed_html_values_12A, readed_html_values_13A, readed_html_values_14A, readed_html_values_15A, readed_html_values_16A)
    readed_html_values_listB <- list(readed_html_values_1B, readed_html_values_2B, readed_html_values_3B, readed_html_values_4B, readed_html_values_5B, readed_html_values_6B, readed_html_values_7B, readed_html_values_8B, readed_html_values_9B, readed_html_values_10B, readed_html_values_11B, readed_html_values_12B, readed_html_values_13B, readed_html_values_14B, readed_html_values_15B, readed_html_values_16B)
    readed_html_values_listC <- list(readed_html_values_1C, readed_html_values_2C, readed_html_values_3C, readed_html_values_4C, readed_html_values_5C, readed_html_values_6C, readed_html_values_7C, readed_html_values_8C, readed_html_values_9C, readed_html_values_10C, readed_html_values_11C, readed_html_values_12C, readed_html_values_13C, readed_html_values_14C, readed_html_values_15C, readed_html_values_16C)
    readed_html_values_listD <- list(readed_html_values_1D, readed_html_values_2D, readed_html_values_3D, readed_html_values_4D, readed_html_values_5D, readed_html_values_6D, readed_html_values_7D, readed_html_values_8D, readed_html_values_9D, readed_html_values_10D, readed_html_values_11D, readed_html_values_12D, readed_html_values_13D, readed_html_values_14D, readed_html_values_15D, readed_html_values_16D)
    readed_html_values_listE <- list(readed_html_values_1E, readed_html_values_2E, readed_html_values_3E, readed_html_values_4E, readed_html_values_5E, readed_html_values_6E, readed_html_values_7E, readed_html_values_8E, readed_html_values_9E, readed_html_values_10E, readed_html_values_11E, readed_html_values_12E, readed_html_values_13E, readed_html_values_14E, readed_html_values_15E, readed_html_values_16E)
    
    # Now I have values such as the Stylistic origins, Derivative forms and others, but with no "Stylistic origins, "Derivative forms" headers just yet
    
    # This below removes any empty list item, in this case the table data (<td>) with nothing inside. 
    # For instance, there is Stylistic origins table data, Derivative forms table data and a couple others, but never up to 16 categories, but the code above looks up to that number, so here we remove the variables that were created but resulted in nothing
    
    readed_html_values_listA <- readed_html_values_listA[lapply(readed_html_values_listA, length) > 0]
    readed_html_values_listB <- readed_html_values_listB[lapply(readed_html_values_listB, length) > 0]
    readed_html_values_listC <- readed_html_values_listC[lapply(readed_html_values_listC, length) > 0]
    readed_html_values_listD <- readed_html_values_listD[lapply(readed_html_values_listD, length) > 0]
    readed_html_values_listE <- readed_html_values_listE[lapply(readed_html_values_listE, length) > 0]
    
    # Now at this point, the number of valid items of table data should match the number of headers (such as Stylistic Origins, Fusion genres, etc.)
    # So we name the valid table data with their corresponding header, and the order should match too.
    
    names(readed_html_values_listA) <- headersA
    names(readed_html_values_listB) <- headersB
    names(readed_html_values_listC) <- headersC
    names(readed_html_values_listD) <- headersD
    names(readed_html_values_listE) <- headersE
    
    # Give my computer and the Wiki servers some break between pages
    
    Sys.sleep(1) 
    
    # Just simplifying our variables' names
    
    GenreDataA <- readed_html_values_listA
    GenreDataB <- readed_html_values_listB
    GenreDataC <- readed_html_values_listC
    GenreDataD <- readed_html_values_listD
    GenreDataE <- readed_html_values_listE
    
    # This step would be unneccesary weren't it from the couple of pages with more than one Infobox. What's convenient here is that we should be able to know in which music genre page we have found an extra Infobox.
    # For instance, where there is more than one infobox, like with Rap rock, the first level will be "Raprock", and then items below will be "Raprock" (yes again, because it's the first infobox) and "Rapcore" (the second infobox, that we know can be found within the Rap rock page)
    # So here below, each "GenreData" is a complete, clean Infobox, with headers and table data.
    
    GenreDataAll <- list(GenreDataA, GenreDataB, GenreDataC, GenreDataD, GenreDataE)
    
    GenreDataAll <- GenreDataAll[lapply(GenreDataAll, length) > 0]
    
    # But in the case of Rap rock and Rapcore, we still have no way to tell which data goes with which Infobox. So ealier in this function we grabbed the name of the Infobox, and now we are matching them.
    # In other words, naming the infoboxes (if applicable) so we know which one refer to which genre within a page
    
    if (length(GenreDataAll) != 0L) {
      print("ok")
      names(GenreDataAll) <- infobox_top_headersA_E
    } else {
      GenreDataAll <- "No-Infobox"
    }
    
    
    #GenreDataAll <- GenreDataAll[lapply(GenreDataAll, length) > 0] # If empty, the B values will at minimum leave one small trace, it will be an empty list item, so this code removes it.
    print(GenreDataAll)
  }
  
  # Testing the clean function before launching the whole scraper
  
  get_infobox_clean(GenreURLsWithLabelsDF$URLsString[184]) # Glitch and Glitch hop (2 infoboxes)
  salut <- get_infobox_clean(GenreURLsWithLabelsDF$URLsString[11]) # Rap rock and Rapcore (2)
  get_infobox_clean(GenreURLsWithLabelsDF$URLsString[1]) # Electronicore (1)
  get_infobox_clean(GenreURLsWithLabelsDF$URLsString[2]) # Youth crew (1)
  get_infobox_clean(GenreURLsWithLabelsDF$URLsString[224]) # This one, Shibuya-kei, was problematic because its "Native name" value was not in a td.hlist but only td with no class. Long story short, I noticed that any data of interest that will not be scraped by my previous function was a td:nth-child(2), since it is the second child of its parent row <tr> - the first one being the header (like "Stylistic origins"). Sometimes the value was more textual, like a sentence, and was not in a .hlist class. 
  get_infobox_clean(GenreURLsWithLabelsDF$URLsString[99])
  
  # Removing the row with no actual URL
  
  GenreURLsWithLabelsDF_Clean <- GenreURLsWithLabelsDF[-1135,]
  
  MusicDataList <- lapply(GenreURLsWithLabelsDF_Clean$URLsString, get_infobox_clean)
  
  # Too scared to modify this MusicDataList directly so I'll just continue with another one
  
  MusicDataList2 <- MusicDataList
  
  # Labels from GenreURLsWithLabelsDF_Clean should fit the items in my list with the right order
  
  names(MusicDataList2) <- GenreURLsWithLabelsDF_Clean$genreLabelString
  
  # And it does, and now I remove any list item that has nothing - every genre that had no infobox, even if a page exists
  # For instance, "German punk" has a wiki page but it has no Infobox, so in my data above it is still present in my list as an object that has no value really.
  
  MusicDataList3 <- MusicDataList2[MusicDataList2 != "No-Infobox"]
  
  # Now for list items who had more than one infobox, the infoboxes are stuck within their parent genre, but we'd like to analyse them separately, so it's best to break them from their parent
  # Very, very long story short, I went to stratospheric lengths to try to do something on my own before noticing that the rlist package might be useful for dealing with lists
  # So not only this function below from the rlist package grabbed all the second-level items I needed, but it also made them all equal, on the same level
  # For instance, Dubstep had only one sub-item - Dubstep again, while Symphonic Metal had Symphonic Metal, Symphonic Gothic Metal and Symphonic Death Metal as sub-items, but now Dubstep and all those Symphonic Metal variations are on the same level.
  
  MusicDataList4 <- list.ungroup(MusicDataList3, level = 1L, group.names = FALSE, sort.names = FALSE)
  
  # So now every genre is a top-level item and for each of them I can navigate easily to find my string data
  
  # The "Cultural origins" one though is different. From it, I really only want the temporal information, typically something like "Late 1990s", "Early 1980s", "1970s". I don't feel like taking the geographical information - it will all be from the US or the UK, mostly, as I have been scraping English pages only.
  # Example for Disco : "Mid-1960s – early 1970s, East Coast of the United States"
  # In virtually all cases the era information is the first element before any other information, and in many cases this will mean before a coma.
  # So here how I cleaned that and store that into a separate data.frame - be sure to have rlist and stringr packages loaded:
  
  # A function that extracts values from a list item, while keeping the name
  
  GenreBirth <- function(x) {
    Genre <- x
    GenreCulturals <- list.extract(Genre, 'Cultural origins')
    print(GenreCulturals)
  }
  
  GenresBirth <- sapply(MusicDataList4, GenreBirth)
  
  
  # Now taking the values directly so the data is now strings in a vector, and selecting only the first item
  
  GenreBirthYear <- function(x) {
    Genre <- x
    GenreExt <- list.extract(Genre, 1)
    if (is.null(GenreExt)) {
      GenreExt <- "NA" 
    }
    GenreCultSplits <- strsplit(GenreExt, ", ")[[1]]
    print(GenreCultSplits[1])
  }
  
  GenresBirthYears <- lapply(GenresBirth, GenreBirthYear)
  
  # Removing the "\n", was totally looking forward for this
  
  GenresBirthYears <- str_replace(GenresBirthYears, "\n", "")
  
  # The music genre to which the temporal information refer has been lost in the process, so after creating my separate data.frame I put back the names and values together
  
  GenresBirthYearsDF <- data.frame(GenresBirthYears)
  GenresBirthYearsDFfull <- cbind(names(GenresBirth), GenresBirthYearsDF)
  GenresYearsOrigin <- GenresBirthYearsDFfull
  
  head(GenresYearsOrigin)
  
  # And now I got my data.frame giving the years of birth of each genre. Arguably this data is still messy, but that'll be for later, as I need to worry about doing my edgelist now!
  # So back with MusicDataList4, which I am going to leave as is and create a copy
  
  MusicDataList5 <- MusicDataList4
  
  # I don't need the "Cultural origins" anymore so I'd rather get rid of them.
  # Long story short again, this is how we do it:
  
  CultureDestroyer <- function(x) {
    x$`Cultural origins` <- NULL
    x$`Cultural Origins` <- NULL
    print(x) 
  }
  
  # Because before I was trying to use list.remove() to remove any occurrence of Cultural Origins but whenever one had no such category (like Funktronica) the lapply stopped the whole process and threw an error, no idea why it does not now.
  # Tired of writing and upgrading MusicDataList, so from now on it's MDL_Links, because what's left is only what will be of use for linking music genres
  
  MDL_Links <- lapply(MusicDataList5, CultureDestroyer)
  
  # Next: cleaning the data one last time before making an edgelist out of it. We have done it before with the Cultural origins separately - it will be pretty much the same here:
  # When I look at the data I notice some pattern: for each genre (list item), and for each of its category of data (sublists: Stylistic origins, Fusion genres, etc.), whenever there is more than one string value, the first one is always a messy extra, like "\nNew York City\nPhiladelphia\nAtlanta\nMiami\nLos Angeles\nMontreal"
  # That is because my scraper was a bit large, it took the lists elements <li> AND the upper element <td>, which itself contains the string as well on its own.
  # Fortunately, as I said, the messy string data always comes first in the vector, so we'll tell R to remove any first string value of all vectors containing more than one.
  
  ExtraRemover <- function(x) {
    lapply(x, function(y) {
      if (length(y) > 1) {
        y <- y[-1] # For the record, long story short again, I first tried to do this with a for loop, never got it working, but will lapply it worked on the first try really
      }
      print(y)
    }) 
  }
  
  ExtraRemoved <- lapply(MDL_Links, ExtraRemover)
  head(ExtraRemoved)
  
  # Splitting those that are still together but 'splitable' by a comma
  
  GenreCharactSplitter  <- function(x) {
    lapply(x, function(y) {
      if (str_detect(y, ", ")) {
        y <- strsplit(y, ", ")
        y <- unlist(y, recursive = TRUE, use.names = FALSE) # Gets extra-listed in the process so I punch back with this fist of unlist
      }
      print(y)
    }) 
  }
  
  GenreCharactSplit <- lapply(ExtraRemoved, GenreCharactSplitter)
  head(GenreCharactSplit)
  
  # Now from this operation above I received several warnings saying it took only the first item for each category like "Subgenres". In our case here it isn't a problem because whenever there are commas it is exactly because there is only one item and it hasn't been split into several items (it was not in <li> tags in the Wikipediua infoboxes, it was only separated by commas in the <td> tag)
  # There are still some data cleaning to be done, mostly about extra "\n" and "[4]" (which are footnotes), It's probably best to do this cleaning later, when I won't be dealing with lists anymore
  # So now I only want to look at problematic or desynchronised values if any
  
  # Here is a very handy line of code to look at the presence of particular strings
  # Handy because it gives the Genre AND its index!
  
  CompleteListBugs <- which(sapply(GenreCharactSplit, FUN=function(X) "\n(complete list)" %in% X))
  
  # Even though a lot of those cases aren't problematic, some are.
  
  GenreCharactSplit[CompleteListBugs]
  
  # But while manually looking at the printed data in the console, I noticed that all issues for our data of interest occur for the "Fusion genres" category, and that in all cases except one, the data that should belong to Fusion genres is in "Other topics"
  # So because of this, we can just replace the former's data with the latter's, and for the unique error one, we'll just change it specifically.
  
  ### DO NOT TOUCH BELOW
  
  CompleteListFixer  <- function(x) {
    if ("Fusion genres" %in% names(x)) {
      x[["Fusion genres"]] <- x[["Other topics"]]
    }
    print(x)
  }
  CompleteListFixed <- lapply(GenreCharactSplit[CompleteListBugs], CompleteListFixer)
  CompleteListFixed
  
  ### DO NOT TOUCH ABOVE
  
  ### But now I figured I'd just add a higher conditional statement to apply this to the whole dataset instead of replacing the problematic genres with the fixed ones
  
  CompleteListFixerMOCK  <- function(x) {
    if ("\n(complete list)" %in% x) {
      if ("Fusion genres" %in% names(x)) {
        x[["Fusion genres"]] <- x[["Other topics"]]
      }
    }
    print(x)
  }
  CompleteListFixedMOCK <- lapply(GenreCharactSplit, CompleteListFixerMOCK)
  head(CompleteListFixedMOCK)
  
  ### And fixing the Psychedelic Trance, which was the only one to have its Fusion genres data put elsewhere, in "Regional scenes"
  
  CompleteListFixedMOCK$`Psychedelic trance`$`Fusion genres` <- GenreCharactSplit$`Psychedelic trance`$`Regional scenes`
  
  MDL_Links2 <- CompleteListFixedMOCK
  head(MDL_Links2)
  
  ### So now my list should be close to ready to be converted in a dataframe - the edgelist!
  
  ### Let's get familiar with the process with one genre
  
  Electronicore <- cbind(list.rbind(MDL_Links2$Electronicore$`Stylistic origins`), names(MDL_Links2[1]))
  
  exists("Other topics", where = MDL_Links2$Electronicore)
  
  list.names(MDL_Links2[1])
  
  ### Now let's go ahead with all of them
  
  ####### STYLISTIC ORIGINS !
  
  Stylistic_ToVar <- function(x) {
    Genre <- x
    if (exists("Stylistic origins", where = Genre)) {
      GenreToVar <- Genre$`Stylistic origins`
      EdgesToVar <- list.rbind(GenreToVar)
    }
  }
  
  StyleToVarList <- lapply(MDL_Links2, Stylistic_ToVar)
  StyleToVarMtx <- as.matrix(unlist(StyleToVarList, recursive = TRUE, use.names = TRUE))
  Edgelist1_StyleToGenre <- cbind(StyleToVarMtx, rownames(StyleToVarMtx))
  rownames(Edgelist1_StyleToGenre) <- NULL
  summary(Edgelist1_StyleToGenre)
  
  # OMG it all seems to work! Now let's totally do this with our 3 other categories. 
  # Editorial decision here: Derivative forms, Subgenres and Fusion genres are all product of the given genre - in my opinion they're all Derivative Forms really. 
  # So in more concrete terms, "Stylistic origins" are what preceded a genre, while the 3 others are what followed a genre.
  # So in even more concrete terms, on my edgelist, "Stylistic origins" of a genre will go on the left column and the given genre on the right (From/To), while it will be the other way around for the 3 others. Given genre is on the left column while its derivatives will be on the right.
  
  # Now for what follow I'll exchange code efficiency for caution with data. I'll treat every one of the three categories in a separate function and matrix - I'll then merge all four matrixes together
  
  ####### DERIVATIVE FORMS !
  
  Derivative_FromVar <- function(x) {
    Genre <- x
    if (exists("Derivative forms", where = Genre)) {
      GenreFromVar <- Genre$`Derivative forms`
      EdgesFromVar <- list.rbind(GenreFromVar)
    }
  }
  
  
  DerivativeFromVarList <- lapply(MDL_Links2, Derivative_FromVar)
  DerivativeFromVarMtx <- as.matrix(unlist(DerivativeFromVarList, recursive = TRUE, use.names = TRUE))
  Edgelist2_DerivativeFromGenre <- cbind(rownames(DerivativeFromVarMtx), DerivativeFromVarMtx)
  rownames(Edgelist2_DerivativeFromGenre) <- NULL
  summary(Edgelist2_DerivativeFromGenre)
  head(Edgelist2_DerivativeFromGenre, n = 20)
  
  ####### FUSION GENRES !
  
  Fusion_FromVar <- function(x) {
    Genre <- x
    if (exists("Fusion genres", where = Genre)) {
      GenreFromVar <- Genre$`Fusion genres`
      EdgesFromVar <- list.rbind(GenreFromVar)
    }
  }
  
  
  FusionFromVarList <- lapply(MDL_Links2, Fusion_FromVar)
  FusionFromVarMtx <- as.matrix(unlist(FusionFromVarList, recursive = TRUE, use.names = TRUE))
  Edgelist3_FusionFromGenre <- cbind(rownames(FusionFromVarMtx), FusionFromVarMtx)
  rownames(Edgelist3_FusionFromGenre) <- NULL
  summary(Edgelist3_FusionFromGenre)
  head(Edgelist3_FusionFromGenre, n = 20)
  
  ####### SUBENRES !
  
  Subgenre_FromVar <- function(x) {
    Genre <- x
    if (exists("Subgenres", where = Genre)) {
      GenreFromVar <- Genre$Subgenres
      EdgesFromVar <- list.rbind(GenreFromVar)
    }
  }
  
  
  SubgenreFromVarList <- lapply(MDL_Links2, Subgenre_FromVar)
  SubgenreFromVarMtx <- as.matrix(unlist(SubgenreFromVarList, recursive = TRUE, use.names = TRUE))
  Edgelist4_SubgenreFromGenre <- cbind(rownames(SubgenreFromVarMtx), SubgenreFromVarMtx)
  rownames(Edgelist4_SubgenreFromGenre) <- NULL
  summary(Edgelist4_SubgenreFromGenre)
  head(Edgelist4_SubgenreFromGenre, n = 20)
  
  ### So now it seems that we have pretty much everything.
  
  ### Let's rowbind the matrixes now!
  
  EdgelistMtx <- rbind(Edgelist1_StyleToGenre, Edgelist2_DerivativeFromGenre, Edgelist3_FusionFromGenre, Edgelist4_SubgenreFromGenre)
  
  summary(EdgelistMtx)
  head(EdgelistMtx)
  ### We already have somewhat interesting but more or less surprising findings here: in the first column, the origins of a genre (this latter the second column), Jazz comes up the most often, followed by Pop, RnB, Hip-Hop, Funk and Soul.
  ### But we still have some cleaning to do, which may slightly change the result: removing all footnote tracks ([1], [4], ...) as well as the "\n", and it is probably a good idea to lowercase everything because R is case-sensitive
  
  ### Removing all "\n"
  
  colnames(EdgelistMtx) <- c("From", "To")
  
  EdgelistMtx2 <- gsub("[\n]", "", EdgelistMtx)
  head(EdgelistMtx2)
  
  ### Removing [, ], and numbers - but by taking care of removing only the ones between square brackets and those at the end of the string - because there are genres like "4-beat" and "2-step garage"
  
  EdgelistMtx3 <- gsub("\\[|\\]", "", EdgelistMtx2)
  head(EdgelistMtx3)
  
  EdgelistMtx4 <- gsub("[1234567890]+$", "", EdgelistMtx3)
  EdgelistMtx4 <- gsub("[1234567890]+$", "", EdgelistMtx4)
  head(EdgelistMtx4)
  
  ### I did notice some less direct duplicates, like "hip hop" and "hip hop music", so I'll remove any mention of " music", with the extra space at the beginning
  ### There are also mentions of "(music genre)"
  
  EdgelistMtx5a <- gsub(" music", "", EdgelistMtx4)
  EdgelistMtx5b <- gsub("music genre", "", EdgelistMtx5a)
  EdgelistMtx5c <- gsub("\\(|\\)", "", EdgelistMtx5b)
  head(EdgelistMtx5c)
  
  ### Lowercasing every value of every column - for which we need the package splus2R, so install and load it first!
  
  library(splus2R)
  
  EdgelistMtxFinal <- lowerCase(EdgelistMtx5c)
  
  head(EdgelistMtxFinal, n = 40)
  
  tail(EdgelistMtxFinal)
  
  summary(EdgelistMtxFinal)
  
  ### So as predicted, there is some change in the summary data. Punk rock apparently is behind more genres than Jazz now. And except for house music, the rest of the top 6 is the same as before.
  ### But it is still intriguing that Punk rock comes higher. Still, this data does not measure popularity, but really the count of music genres. Modern rock and heavy metal music is still a thing, and a lot of genres, if not a majority of them, has some direct or indirect inspiration from punk rock. 
  
  ### Almost forgot: there will be duplicated rows/edges, because a stylistic origin will necessarily be the derivative of something
  
  sum(duplicated.matrix(EdgelistMtxFinal, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE))
  
  ### So above I find 1279 duplicated rows. The code below shows them.
  
  EdgelistMtxFinal[duplicated.matrix(EdgelistMtxFinal, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE),]
  
  # The number of rows below
  
  length(EdgelistMtxFinal[,"To"])
  
  # I just want to make sure below that the code does what I think it does with a subset - removing duplicated rows, no duplicated values!
  
  EmoEL <- EdgelistMtxFinal[EdgelistMtxFinal[, "To"] == "emo",]
  
  
  # So here I see eleven rows matching "emo" in the "To" column. I do see the few duplicates. Like above, the code below shows them
  
  EmoEL[duplicated.matrix(EmoEL, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE),]
  
  # And this code show all unique rows
  
  EmoEL
  
  # So duplicated.matrix() does seem to be taking the duplicated rows WITHOUT grabbing the unique rows that are the object of a duplication - very important!
  
  # So let's remove them! We can either remove them with duplicated.matrix - or just use unique(). We'll compare both outcomes just to be sure.
  
  length(unique(EdgelistMtxFinal)) / 2 # I divide by 2 because it counts all values, not rows, and value are spreaded across 2 columns
  
  length(EdgelistMtxFinal[!duplicated.matrix(EdgelistMtxFinal, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE),]) / 2
  
  # Let's go!
  
  EdgelistMtxVeryFinal <- unique(EdgelistMtxFinal)
  summary(EdgelistMtxVeryFinal)
  
  # A fun tool here: to look up for any specific genre, either in the "From" or "To" column
  
  EdgelistMtxVeryFinal[EdgelistMtxVeryFinal[, "To"] == "music",]
  EdgelistMtxVeryFinal[str_detect(EdgelistMtxVeryFinal, " music")]
  
  # No idea why, but there are still occurrences of "[something] music", so I remove it a second time:
  
  EdgelistMtxVeryFinal1 <- gsub(" music", "", EdgelistMtxVeryFinal)
  EdgelistMtxVeryFinal1[str_detect(EdgelistMtxVeryFinal1, "contradanza")]
  
  Edgelist <- EdgelistMtxVeryFinal1
  summary(Edgelist)
  
  # Now jazz is back at the top of our table for its number of occurrences as the stylistic origin of something!
  # Punk rock is still a close second though - interesting, it's the only rock music genre within the top 6. 
  
  # Now I do have a feeling that there might be occurrences that are partially matching - and this is where I switch to OpenRefine, because it's quite easy!
  
  saveRDS(Edgelist, file = "Edgelist", ascii = FALSE, version = NULL, compress = FALSE, refhook = NULL)
  write.csv(Edgelist, file = "Edgelist.csv")
  
  # Once in OpenRefine and when the data is uploaded and in front of you, select a column's downward arrow, and choose "Edit cells" and "Cluster and edit", and play around with the Method, the Ngram size and the Keying Function. Easy, powerful and effective!
  
  # There are occurrences when there is an extra s, an extra -, an extra space, and other very slight differences where I have to take decisions that will ultimately always be arbitrary. But whenever I have the shadow of a very slight doubt I look up the genre(s) in Wikipedia - like I didn't assume IDM or EBM was a typo in EDM for example. Same with "Filk" and "Folk".
  
  # And you know, even though I got a few thousands edges, there is nothing like human eyes, so I'll admit I did a quick scroll of the post-OpenRefine file through Excel to detect any remaining anomaly. Not reading them one by one, just scrolling rapidly to detect longer than usual values mostly  
  
  # So one last thing before changing script: removing duplicates again, since I did further cleaning!
  
  EdgelistPreClean <- Edgelist.postOpenRefine
  EdgelistClean <- unique(EdgelistPreClean)
  
  nrow(EdgelistClean)
  
  # It changed as a dataframe now, but I don't think this creates any issue.
  # So now I am down to 3967! Some edges were lost in the process as they were actually extra textual information that was not meant to be part of the data, or data that was badly organised so my scraper didn't get it - I discarded them when there were ridiculously long, occurred around 10 or 20 times.
  
  # Now I have an edgelist, a dataset containing genres' broader categories, and their era of birth, which we'll all try to visualise in some way
  
  # See you in the next script - the visualisation part!