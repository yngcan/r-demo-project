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
}

library("httr")
library("rvest")
library("tm")
library("wordcloud")
page.to.words <- function(page) {
  text <- GET(paste("http://en.wikipedia.org/wiki", page, sep="/"))
  body <- httr::content(text) %>% html_nodes("body") %>% html_text
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

wikipedia.word.cloud <- function(page, min.freq=10) {
  df <- word.frequencies(page.to.words(page))
  wordcloud(df$word, df$count, min.freq=min.freq, scale=c(3,1))
}
# wikipedia.word.cloud("Artificial_intelligence", 10)