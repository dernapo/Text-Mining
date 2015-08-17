
install("wordcloud")
install("tm")

#Load data
lords <- Corpus (DirSource("doc/"))
#inspect(lords)
lords <- tm_map(lords, stripWhitespace)
lords <- tm_map(lords, tolower)
lords <- tm_map(lords, removeWords, stopwords("english"))
lords <- tm_map(lords, stemDocument)
lords <- tm_map(lords, removeNumbers)
lords <- tm_map(lords, removePunctuation)

lords <- tm_map(lords, PlainTextDocument)


wordcloud(lords, scale=c(5,0.5), max.words=100, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))