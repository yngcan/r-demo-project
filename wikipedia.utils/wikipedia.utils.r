# Install packages
# ----------------
# 
# This only needs to be done once per project. Once the packages are installed
# to the project, every console launched in the project can use them.
#
# install.packages("networkD3")
# install.packages("rvest")
# library("devtools")
# install.packages("wordcloud")
# install.packages("tm")
# install.packages("dygraphs")
# install_github("bwlewis/rthreejs")
# install.packages("ggmap")
# install_github("rstudio/leaflet")
# install_github("hadley/purrr")

library("httr")
library("rvest")
suppressMessages(library("tm"))
library("wordcloud")
suppressMessages(library("purrr"))
library("networkD3")
suppressMessages(library("xts"))
library("dygraphs")
suppressMessages(library("threejs"))
suppressMessages(library("ggmap"))
library("leaflet")

page.to.words <- function(page, lang="en") {
  text <- GET(paste("http://", lang, ".wikipedia.org/wiki/", page, sep=""))
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

# Dynamic neighborhood graph
# --------------------------

page.links <- function(title, lang="en") {
  
  # Returns the links from the given Wikipedia page as a list
  # of vectors containing the sources and targets of the links.
  
  # Use httr to query the Wikipedia API.
  response <- GET(paste("http://", lang, ".wikipedia.org/w/api.php?", sep=""), query=list(
    format="json",
    action="query",
    prop="links",
    # Keep the number of links per page to 50, to avoid hitting the Wikipedia
    # API too hard.
    pllimit=50,
    titles=title
  ))
  pages <- httr::content(response, "parsed")$query$pages
  
  # Put the data in a more usable format. 
  page <- pages[[names(pages)[[1]]]]
  target <- page$links %>% purrr::map(~ .$title) %>% unlist
  list(target = target, src = rep(page$title, length(target)))
}

page.neighborhood <- function(page, open=T, lang="en") {
  
  # Returns an interactive HTML widget showing the neighborhood
  # of the given Wikipedia page.
  
  # Get all links inside the page's neighborhood.
  links <- page.links(page, lang)
  reducer <- function(sofar, title) {
    new.links <- page.links(title, lang)
    keep <- new.links$target %>% purrr::map(~ (. %in% links$target) || ((. == page) && !open)) %>% unlist
    list(target=c(sofar$target, new.links$target[keep]),
        src=c(sofar$src, new.links$src[keep]))
  }
  if (!open) {
    init = links
  } else {
    init = list(src=c(), target=c())
  }
  all.links <- links$target %>% purrr::reduce(reducer, .init=init)
  
  # Use the htmlwidgets simpleNetwork function to render and
  # return the interactive visualization.
  simpleNetwork(data.frame(all.links))
}

# Compare cumulative revisions
# ----------------------------

get.revision.series <- function(page, lang) {
  
  # Use httr to get the recent revisions for the given page.
  
  response <- GET(paste("http://", lang, ".wikipedia.org/w/api.php?", sep=""), query=list(
    format="json",
    action="query",
    prop="revisions",
    titles=page,
    rvprop="timestamp|user",
    rvlimit=1000
  ))
  pages <- httr::content(response, "parsed")$query$pages
  revisions <- pages[[names(pages)[[1]]]]$revisions
  
  # Convert the revision records to an xts time series of daily
  # revision numbers.
  
  timestamp <- revisions %>% purrr::map(~ .$timestamp) %>% unlist
  ct <- as.POSIXct(timestamp, format = "%Y-%m-%d")
  ts <- xts(rep(1, length(ct)), ct)
  agg <- suppressWarnings(aggregate(as.zoo(ts), time(ts), sum))
  xts(unlist(agg), time(agg))
}
plot.revisions <- function(page, lang="en") {
  ats <- cbind(Revisions=get.revision.series(page, lang))
  dygraph(ats, main=page, ylab="Revisions") %>% dyRangeSelector() %>% dyOptions(stackedGraph=TRUE)
}
compare.cumulative.revisions <- function(page1, page2, lang="en") {
  
  # Get the daily revision numbers for the two given pages.
  
  tss <- c(page1, page2) %>% purrr::map(~ get.revision.series(., lang))
  
  # Conform the two time series.
  
  first.i <- tss %>% purrr::map(~ time(first(.))) %>% which.max
  first.t <- time(first(tss[[first.i]]))
  data <- cbind(p1=tss[[1]], p2=tss[[2]])
  data[is.na(data)] <- 0
  data <- data[time(data) >= first.t]
  
  # Cumulate the time series.
  
  data <- cumsum(data)
  
  # Use the htmlwidgets dygraph function to create and return
  # a dynamic visualization of the two pages' revisions.
  
  dygraph(data) %>%
  dySeries(names(data)[1], label = page1) %>%
  dySeries(names(data)[2], label = page2) %>%
  dyRangeSelector() %>% dyOptions(stackedGraph=TRUE)
}

# Recent earthquakes on a globe
# -----------------------------

previous.day.earthquakes <- function() {
  
  # Use HTTR to query the last 24 hours' earthquakes from USGS.
  
  now <- as.POSIXct(Sys.time(), "UTC")
  since <- now - 60 * 60 * 24
  since.str <- format(since, "%y-%m-%dT%H:%M:%S")
  resp <- GET("http://earthquake.usgs.gov/fdsnws/event/1/query", query=list(
    format="geojson", 
    starttime=since.str
  ))
  features <- httr::content(resp, "parsed")$features
  
  # Extract latitude, longitude and magnitude for each earthquake.
  
  mag <- features %>% purrr::map(~ .$properties$mag) %>% unlist
  long <- features %>% purrr::map(~ .$geom$coordinates[[1]]) %>% unlist
  lat <- features %>% purrr::map(~ .$geom$coordinates[[2]]) %>% unlist
  
  # Use the htmlwidgets globejs function to create and return an
  # interactive globe.
  
  earth <- "/home/sense/wikipedia.utils/land_shallow_topo_2048.jpg"
  globejs(img=earth, bodycolor="#555555", emissive="#444444",
         lightcolor="#555555", bg="#ffffff", lat=lat, long=long,
         color="#FF3333",
         value=mag * 50)
  
}

# Map of nearby pages
# -------------------

nearby.pages <- function(place, radius=10000) {
  
  # Use ggmap to attempt to geocode (get latitude and longitude for)
  # the given place.
  
  latlon <- geocode(place)
  
  # Use httr to get up to ten Wikipedia pages within 10km of the given
  # point.
  
  response <- GET("https://en.wikipedia.org/w/api.php", query=list(
    action="query",
    list="geosearch",
    gsradius=radius, 
    gscoord=paste(latlon$lat, latlon$lon, sep="|"), 
    format="json"
  ))
  geosearch <- httr::content(response, "parsed")$query$geosearch
  
  # Extract the latitude, longitude and title of each page.
  
  info <- purrr::unzip(geosearch, c("lon", "lat", "title"))
  
  # Use the htmlwidgets leaflet function to create and return an
  # interacive map with markers.
  
  attr <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
  template <- 'http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png'
  opts = tileOptions(subdomains='abcd', minZoom=0, maxZoom=20)
  # Gritty black-and-white tiles look good with Venice
  leaflet() %>% addTiles(urlTemplate=template, attribution=attr, options=opts) %>% addCircleMarkers(info$lon, info$lat, popup=info$title)
  
  # Standard OpenStreetMap tiles look good with Samarkand
  # leaflet() %>% addTiles(options=opts) %>% addCircleMarkers(info$lon, info$lat, popup=info$title)
}
