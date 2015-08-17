
###################
# Get data
###################
dir<-getwd()
file <- "Marwick_DMAR_chapter_3.csv" # name of the CSV file
df <- read.csv(paste(dir, file, sep = "/"), stringsAsFactors = FALSE)

# get column names to see structure of the data
names(df) 
# look at the first three rows to check content
head(df,3) 

# some basic and widely-used text mining techniques 
require(tm) 
a <- Corpus(VectorSource(df$text)) # create corpus object
a <- tm_map(a, tolower) # convert all text to lower case
a <- tm_map(a, removePunctuation) 
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removeWords, stopwords("english")) # this list needs to be edited and this function repeated a few times to remove high frequency context specific words with no semantic value 
require(rJava) # needed for stemming function 
require(SnowballC) # also needed for stemming function 
a <- tm_map(a, stemDocument, language = "english") # converts terms to tokens
a <- tm_map(a, PlainTextDocument)
a.tdm <- TermDocumentMatrix(a, control = list(minWordLength = 3)) # create a term document matrix, keepiing only tokens longer than three characters, since shorter tokens are very hard to interpret
inspect(a.tdm[1:10,1:10]) # have a quick look at the term document matrix
findFreqTerms(a.tdm, lowfreq=30) # have a look at common words, in this case, those that appear at least 30 times, good to get high freq words and add to stopword list and re-make the dtm, in this case add aaa, panel, session
findAssocs(a.tdm, "scienc", 0.3) # find associated words and strength of the common words. I repeated this function for the ten most frequent words.


a.tdm.sp <- removeSparseTerms(a.tdm, sparse=0.989)  # I found I had to iterate over this to ensure the tdm doesn't get too small... for example: 0.990 nrow=88, 0.989, nrow=67, 0.985, nrow=37, 0.98 nrow=23, 0.95 nrow=6
a.tdm.sp.df <- as.data.frame(inspect(a.tdm.sp )) # convert document term matrix to data frame
nrow(a.tdm.sp.df) # check to see how many words we're left with after removing sparse terms
# this analysis is based on http://www.statmethods.net/advstats/cluster.html 
# scale and transpose data for cluster analysis
a.tdm.sp.df.sc.t <- t(scale(a.tdm.sp.df))
require(pvclust)
fit <- pvclust(a.tdm.sp.df.sc.t, method.hclust = "average", method.dist = "correlation", nboot = 10) # this method may take a few hours the bootstraping, you can reduce the nboot value for a quicker result
plot(fit, cex = 1.5, cex.pv = 1.2, col.pv = c(1,0,0), main="", xlab="", sub="")  # draw the dendrogram

require(slam)
a.tdm.sp.t <- t(a.tdm.sp) # transpose document term matrix, necessary for the next steps using mean term frequency-inverse document frequency (tf-idf) to select the vocabulary for topic modeling
summary(col_sums(a.tdm.sp.t)) # check median...
term_tfidf <- tapply(a.tdm.sp.t$v/row_sums(a.tdm.sp.t)[a.tdm.sp.t$i], a.tdm.sp.t$j,mean) * log2(nDocs(a.tdm.sp.t)/col_sums(a.tdm.sp.t>0)) # calculate tf-idf values
summary(term_tfidf) # check median... note value for next line... 
a.tdm.sp.t.tdif <- a.tdm.sp.t[,term_tfidf>=1.0] # keep only those terms that are slightly less frequent that the median
a.tdm.sp.t.tdif <- a.tdm.sp.t[row_sums(a.tdm.sp.t) > 0, ]
summary(col_sums(a.tdm.sp.t.tdif)) # have a look

# Before going right into generating the topic model and analysing the output, we need to decide on the number of topics that the model should use
# Here's a function to loop over different topic numbers, get the log liklihood of the model for each topic number and plot it so we can pick the best one
# The best number of topics is the one with the highest log liklihood value.

require(topicmodels)
best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(a.tdm.sp.t.tdif, d)}) # this will make a topic model for every number of topics between 2 and 50... it will take some time! 
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))  # this will produce a list of logLiks for each model... 

# plot the distribution of logliklihoods by topic
best.model.logLik.df <- data.frame(topics=c(2:50), LL = as.numeric(as.matrix(best.model.logLik)))
ggplot(best.model.logLik.df, aes(x = topics, y = LL)) + 
        xlab("Number of topics") + 
        ylab("Log likelihood of the model") + 
        geom_line() + 
        theme_bw()  + 
        opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) + 
        opts(axis.title.y=theme_text(size = 14, angle=90, vjust= -0.25)) + 
        opts(plot.margin = unit(c(1,1,2,2), "lines"))  

# ggsave(file = "model_LL_per_topic_number.pdf") # export the plot to a PDF file
# it's not easy to see exactly which topic number has the highest LL, so let's look at the data...
best.model.logLik.df.sort <- best.model.logLik.df[order(-best.model.logLik.df$LL), ] # sort to find out which number of topics has the highest loglik, in this case 23 topics. 
best.model.logLik.df.sort # have a look to see what's at the top of the list, the one with the highest score
ntop <- best.model.logLik.df.sort[1,]$topics


lda <- LDA(a.tdm.sp.t.tdif, ntop) # generate a LDA model the optimum number of topics
get_terms(lda, 5) # get keywords for each topic, just for a quick look
get_topics(lda, 5) # gets topic numbers per document
lda_topics<-get_topics(lda, 5) 
beta <- lda@beta # create object containing parameters of the word distribution for each topic
gamma <- lda@gamma # create object containing posterior topic distribution for each document
terms <- lda@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(beta) <- terms # puts the terms (or words) as the column names for the topic weights.
id <- t(apply(beta, 1, order)) # order the beta values
beta_ranked <- lapply(1:nrow(id),function(i)beta[i,id[i,]])  # gives table of words per topic with words ranked in order of beta values. Useful for determining the most important words per topic

