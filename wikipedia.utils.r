install.dependencies <- function() {
  install.packages("networkD3")
  install.packages("rvest")
  library("devtools")
  install.packages("wordcloud")
  install.packages("tm")
  install.packages("dygraphs")
  install_github("bwlewis/rthreejs")
  install.packages("ggmap")
  install_github("rstudio/leaflet")
  install_github("hadley/purrr")
}

library("httr")
library("rvest")
library("tm")
library("wordcloud")
library("purrr")

page.to.words <- function(page) {
  text <- GET(paste("http://en.wikipedia.org/wiki", page, sep="/"))
  body <- httr::content(text) %>% html_nodes("body") %>% html_text
  long.words <- body %>% strsplit("[^a-zA-Z]") %>% unlist %>% keep(~ nchar(.) > 0)
  purrr::map(long.words, tolower)
}

most.common <- function(vec, n) {
  names(sort(table(vec),decreasing=TRUE))[1:n]
}
word.frequencies <- function(observed.words) {
  interesting.words <- observed.words %>% discard(~ . %in% stopwords("english")) %>% unlist(use.names=FALSE)
  tab <- table(interesting.words)
  data.frame(word=names(tab), count=as.vector(tab))
}
wikipedia.word.cloud <- function(page, min.freq=10) {
  df <- page %>% page.to.words %>% word.frequencies
  wordcloud(df$word, df$count, min.freq=min.freq, scale=c(3,1))
}
# wikipedia.word.cloud("Artificial_intelligence", 10)

# Dynamic JS graph of word adjacency for most common 7+ character words
library("networkD3")
word.adjacents <- function(page, words=NULL) {
  observed.words <- page.to.words(page)
  if (is.null(words)) {
    words <- observed.words %>% keep(~ nchar(.) > 6) %>% unlist
    if (length(words) > 10) {
      words <- most.common(words, 10)
    }
  }
  ind <- which(observed.words %in% words)
  
  before <- observed.words[ind[ind > 1]-1]
  after <- observed.words[ind[ind < length(ind)]+1]
  
  src <- observed.words[c(ind[ind > 1], ind[ind < length(ind)])] %>% unlist(use.names=FALSE)
  target <- c(before, after) %>% unlist(use.names=FALSE)
  simpleNetwork(data.frame(src, target))
}
# word.adjacents("Utopia")

# A dygraph with revisions, do http://rstudio.github.io/dygraphs/
library("xts")
library("dygraphs")
get.revision.series <- function(page) {
  response <- GET("http://en.wikipedia.org/w/api.php?", query=list(
    format="json",
    action="query",
    prop="revisions",
    titles=page,
    rvprop="timestamp|user",
    rvlimit=1000
  ))
  pages <- httr::content(response, "parsed")$query$pages
  revisions <- pages[[names(pages)[[1]]]]$revisions
  timestamp <- purrr::map(revisions, ~ .$timestamp) %>% unlist
  ct <- as.POSIXct(timestamp, format = "%Y-%m-%d")
  ts <- xts(rep(1, length(ct)), ct)
  aggregate(as.zoo(ts), time(ts), sum)
}
plot.revisions <- function(page) {
  ats <- cbind(Revisions=get.revision.series(page))
  dygraph(ats, main=page, ylab="Revisions") %>% dyRangeSelector() %>% dyOptions(stackedGraph=TRUE)
}
# plot.revisions("United_States")


# Recent earthquakes on a globe
library("httr")
library("threejs")
globe.earthquakes <- function() {
  now <- as.POSIXct(Sys.time(), "UTC")
  since <- now - 60 * 60 * 24
  since.str <- format(since, "%y-%m-%dT%H:%M:%S")
  resp <- GET("http://earthquake.usgs.gov/fdsnws/event/1/query", query=list(
    format="geojson", 
    starttime=since.str
  ))
  features <- httr::content(resp, "parsed")$features
  mag <- purrr::map(features, ~ .$properties$mag) %>% unlist
  long <- purrr::map(features, ~ .$geom$coordinates[[1]]) %>% unlist
  lat <- purrr::map(features, ~ .$geom$coordinates[[2]]) %>% unlist
  earth <- "/home/sense/land_shallow_topo_2048.jpg"
  globejs(img=earth, bodycolor="#555555", emissive="#444444",
         lightcolor="#555555", bg="#ffffff", lat=lat, long=long,
         color="#FF3333",
         value=mag * 50)
  
}
# globe.earthquakes()

library("ggmap")
library("leaflet")
geo.search <- function(place, radius=10000) {
  Sys.setlocale(category="LC_ALL", locale="en_US.UTF-8")
  latlon <- geocode(place)
  response <- GET("https://en.wikipedia.org/w/api.php", query=list(
    action="query",
    list="geosearch",
    gsradius=radius, 
    gscoord=paste(latlon$lat, latlon$lon, sep="|"), 
    format="json"
  ))
  geosearch <- httr::content(response, "parsed")$query$geosearch
  info <- purrr::unzip(geosearch, c("lon", "lat", "title"))
  attr <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
  template <- 'http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png'
  opts = tileOptions(subdomains='abcd', minZoom=0, maxZoom=20)
  leaflet() %>% addTiles(urlTemplate=template, attribution=attr, options=opts) %>% addCircleMarkers(info$lon, info$lat, popup=info$title)
}
# geo.search("Venice")

