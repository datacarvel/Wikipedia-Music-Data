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
