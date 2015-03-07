install.dependencies <- function() {
  install.packages("WikipediR")
  install.packages("networkD3")
  install.packages("rvest")
  library("devtools")
#  install_github("ramnathv/rCharts")
#  install.packages("base64enc")
#  install.packages("knitr")
  install.packages("wordcloud")
  install.packages("tm")
  install.packages("dygraphs")
}

library("WikipediR")
library("rvest")
library("tm")
library("networkD3")
library("wordcloud")
library("dygraphs")
library("xts")

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
# word.cloud("Artificial_intelligence", 10)


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
# word.adjacents("Utopia")


# A dygraph with revisions, do http://rstudio.github.io/dygraphs/
revisions <- WikipediR:::wiki_call("http://en.wikipedia.org/w/api.php?format=json&action=query&prop=revisions&titles=Uzbekistan&rvprop=timestamp|user&rvlimit=500")
revisions <- revisions$query$pages$`31853`$revisions
timestamp <- unlist(Map(function(r) {r$timestamp}, revisions))
initial.ts <- 
ct <- as.POSIXct(timestamp, format = "%Y-%m-%d")
ts <- xts(rep(1, length(ct)), ct)
ats <- aggregate(as.zoo(ts), time(ts), sum)
dygraph(ats)


# Space: http://simia.net/wikiglobe/ . Note, whatever json WikipediR
# does kills the coordinates, so can't use that. Just use httr.
changes <- WikipediR:::wiki_call("http://en.wikipedia.org/w/api.php?format=json&action=query&list=recentchanges&rctype=edit&rcprop=title|user|timestamp&rclimit=50")
changes <- changes$query$recentchanges
titles = unlist(Filter(function(c) {!grepl("/", c)}, Map(function(c) {URLencode(c$title)}, changes)))
title.url <- paste(titles, collapse="|")
space <- WikipediR:::wiki_call(paste("https://en.wikipedia.org/w/api.php?action=query&prop=coordinates&titles=", title.url, "&format=json", sep=""))
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
