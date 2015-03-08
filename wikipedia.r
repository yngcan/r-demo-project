install.dependencies <- function() {
  install.packages("WikipediR")
  install.packages("networkD3")
  install.packages("rvest")
  library("devtools")
  install.packages("wordcloud")
  install.packages("tm")
  install.packages("dygraphs")
  install_github("bwlewis/rthreejs")
  install.packages("ggmap")
  install_github("rstudio/leaflet")
}

library("WikipediR")
library("rvest")
library("tm")
library("networkD3")
library("wordcloud")
library("dygraphs")
library("xts")
library("threejs")
library("ggmap")
library("leaflet")

con <- wiki_con("en", "wikipedia")
page.to.words <- function(page) {
  pg <- wiki_page(con, page)
  pg.text <- pg$parse$text[[1]]
  body <- html(pg$parse$text[[1]]) %>% html_nodes("body") %>% html_text
  w <- Map(tolower, Filter(function(w) {nchar(w) > 0}, unlist(strsplit(body, "[^a-zA-Z]"))))
}
most.common <- function(vec, n) {
  names(sort(table(vec),decreasing=TRUE))[1:n]
}
word.frequencies <- function(observed.words) {
  '%nin%' <- Negate('%in%')
  interesting.words <- unlist(observed.words[observed.words %nin% stopwords("english")], use.names=FALSE)
  tab <- table(interesting.words)
  data.frame(word=names(tab), count=as.vector(tab))
}

# Word cloud for any page
word.cloud <- function(page, min.freq=10) {
  df <- word.frequencies(page.to.words(page))
  wordcloud(df$word, df$count, min.freq=min.freq, scale=c(3,1))
}
word.cloud("Artificial_intelligence", 10)


# Dynamic JS graph of word adjacency for most common 7+ character words
word.adjacents <- function(page, words=NULL) {
  observed.words <- page.to.words(page)
  if (is.null(words)) {
    words <- unlist(Filter(function(x) {nchar(x) > 6}, observed.words))
    if (length(words) > 10) {
      words <- most.common(words, 10)
    }
  }
  ind <- which(observed.words %in% words)
  
  before <- observed.words[ind[ind > 1]-1]
  after <- observed.words[ind[ind < length(ind)]+1]
  
  src <- unlist(observed.words[c(ind[ind > 1], ind[ind < length(ind)])], use.names=FALSE)
  target <- unlist(c(before, after), use.names=FALSE)
  simpleNetwork(data.frame(src, target))
}
word.adjacents("Utopia")


# A dygraph with revisions, do http://rstudio.github.io/dygraphs/
get.revision.series <- function(page) {
  revisions <- WikipediR:::wiki_call(paste("http://en.wikipedia.org/w/api.php?format=json&action=query&prop=revisions&titles=", page, "&rvprop=timestamp|user&rvlimit=1000", sep=""))
  revisions <- revisions$query$pages[[names(revisions$query$pages)[[1]]]]$revisions
  timestamp <- unlist(Map(function(r) {r$timestamp}, revisions))
  initial.ts <- 
  ct <- as.POSIXct(timestamp, format = "%Y-%m-%d")
  ts <- xts(rep(1, length(ct)), ct)
  ats <- aggregate(as.zoo(ts), time(ts), sum)
  cbind(Revisions=ats)
}
plot.revisions <- function(page) {
  ats <- get.revision.series(page)
  dygraph(ats, main=page, ylab="Revisions") %>% dyRangeSelector() %>% dyOptions(stackedGraph=TRUE)
}
plot.revisions("United_States")
# Two looks like unless the axes are lined up, which they aint.
#plot.two.revisions <- function(page1, page2) {
#  ats1 <- get.revision.series(page1)
#  ats2 <- get.revision.series(page2)
#  dygraph(cbind(ats1, ats2)) %>%
#    dySeries("ats1", label = page1) %>%
#    dySeries("ats2", label = page2) %>%
#    dyRangeSelector() %>% dyOptions(stackedGraph=TRUE)
#}


# Space: http://simia.net/wikiglobe/ .
# Doesn't look great. Consider using geosearch and putting it on Google Maps
# you can get lat/lon for places with: http://andybeger.com/2013/08/06/finding-coordinates-for-cities-etc-with-r/
globe.recent.changes <- function() {
  changes <- WikipediR:::wiki_call("http://en.wikipedia.org/w/api.php?format=json&action=query&list=recentchanges&rctype=edit&rcprop=title|user|timestamp&rclimit=200")
  changes <- changes$query$recentchanges
  titles = unlist(Filter(function(c) {!grepl("/", c)}, Map(function(c) {c$title}, changes)))
  
  folded.titles <- matrix(titles, ncol=50)
  space.results <- list()
  for (i in 1:nrow(folded.titles)) {
    title.url <- paste(folded.titles[i,], collapse="|")
    # This needs to be batched up into batches of 50.
    results <- WikipediR:::wiki_call(paste("https://en.wikipedia.org/w/api.php?action=query&prop=coordinates&titles=", title.url, "&format=json&limit=500", sep=""))
    space.results <- c(space.results, Filter(function(r) {!is.null(r$coordinates)}, results$query$pages))
    print(length(space.results))
  }
  print("total")
  print(length(space.results))
  lat <- unlist(Map(function(res) {as.numeric(res$coordinates[[1]]$lat)}, space.results), use.names=FALSE)
  long <- unlist(Map(function(res) {as.numeric(res$coordinates[[1]]$lon)}, space.results), use.names=FALSE)
  value <- rep(100, length(long))
  earth <- texture(system.file("images/world.jpg",package="threejs"))
  globejs(img=earth, lat=lat, value=value, long=long, bg="#FFFFFF")
}
globe.recent.changes()

globe.earthquakes <- function() {
  # http://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson
}


geo.search <- function(place, radius=10000) {
  Sys.setlocale(category="LC_ALL", locale="en_US.UTF-8")
  latlon <- geocode(place)
  print(paste("https://en.wikipedia.org/w/api.php?action=query&list=geosearch&gsradius=", radius, "&gscoord=", latlon$lat, "|", latlon$lon, sep=""))
  results <- WikipediR:::wiki_call(paste("https://en.wikipedia.org/w/api.php?action=query&list=geosearch&gsradius=", radius, "&gscoord=", latlon$lat, "|", latlon$lon, "&format=json&limit=500", sep=""))
  lon <- sapply(results$query$geosearch, `[[`, "lon")
  lat <- sapply(results$query$geosearch, `[[`, "lat")
  title <- sapply(results$query$geosearch, `[[`, "title")
  attr <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
  template <- 'http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png'
  opts = tileOptions(subdomains='abcd', minZoom=0, maxZoom=20)
  leaflet() %>% addTiles(urlTemplate=template, attribution=attr, options=opts) %>% addCircleMarkers(lon, lat, popup=title)
}
geo.search("Venice")

# want a bubble chart like http://bl.ocks.org/mbostock/4063269
# but not trivial
word.frequency <- function(page) {
  observed.words <- page.to.words(page)
  hist(table(observed.words))
}
#library("rCharts")
#tab <- table(observed.words)
#df <- data.frame(word=names(tab), count=as.vector(tab))
#
## doesnt work
#d1 <- dPlot(
#  y="word",
#  x="count",
#  groups="word",
#  data=df,
#  type="bar",
#  height=600,
#  width=800#,
##  bounds = list(x=200, y=30, width=600, height=500)
#)
#d1$xAxis(type="addMeasureAxis", outputFormat="#,")
#d1$yAxis(type="addCategoryAxis")
#d1
##df <- data.frame(uspop =tmp$uspop, year = unlist(Map(function(x) {paste(toString(x), "-blarg") }, tmp$year)))
#
## works but not right, has axes
#a <- hPlot(Pulse ~ Height, data = MASS::survey, type = "bubble", title = "Zoom demo", subtitle = "bubble chart", size = "Age", group = "Exer")
#a$chart(zoomType = "xy")
#a$exporting(enabled = T)
#a
#
#word.frequency("Muhammad_Ali")
