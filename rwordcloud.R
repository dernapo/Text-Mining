
# My first steps creating a Word Cloud and more.
# thanks to Thierry_G
# https://georeferenced.wordpress.com/2013/01/15/rwordcloud/

library("wordcloud")
library("tm")


#Load data
#Data is just a copy and past from:
#http://www.publications.parliament.uk/pa/ld201213/ldhansrd/text/130110-0002.htm#13011048001187
lords <- Corpus (DirSource("doc/"))

#inspect(lords)

#Clean up
lords <- tm_map(lords, stripWhitespace)
lords <- tm_map(lords, tolower)
lords <- tm_map(lords, removeWords, stopwords("english"))
lords <- tm_map(lords, stemDocument)
lords <- tm_map(lords, removeNumbers)
lords <- tm_map(lords, removePunctuation)

lords <- tm_map(lords, PlainTextDocument)

#Print Cloud
wordcloud(lords, scale=c(5,0.5), max.words=100, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))

