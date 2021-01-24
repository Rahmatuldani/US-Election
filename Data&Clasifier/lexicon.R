

tweets <- tweets %>% rename (doc_id = id)
ClintonTweets <- tweets %>% filter(is_retweet=="FALSE" & handle=="Clinton")
TrumpTweets <- tweets %>% filter(is_retweet=="FALSE" & handle=="Trump")

TrumpCorpus <- DataframeSource(TrumpTweets)
TrumpCorpus <- VCorpus(TrumpCorpus)

ClintonCorpus <- DataframeSource(ClintonTweets)
ClintonCorpus <- VCorpus(ClintonCorpus)

inspect(TrumpCorpus[1:2])

CleanCorpus <- function(x){
  x <- tm_map(x, content_transformer(tolower))
  x <- tm_map(x, removeNumbers) #remove numbers before removing words. Otherwise "trump2016" leaves "trump"
  x <- tm_map(x, removeWords, tidytext::stop_words$word)
  x <- tm_map(x, removePunctuation)
  x <- tm_map(x, stripWhitespace)
  return(x)
}

RemoveNames <- function(x) {
  x <- tm_map(x, removeWords, c("donald", "hillary", "clinton", "trump", "realdonaldtrump", "hillaryclinton"))
  return(x)
}

CreateTermsMatrix <- function(x) {
  x <- TermDocumentMatrix(x)
  x <- as.matrix(x)
  y <- rowSums(x)
  y <- sort(y, decreasing=TRUE)
  return(y)
}

TrumpCorpus <- CleanCorpus(TrumpCorpus)
TermFreqTrump <- CreateTermsMatrix(TrumpCorpus)

content(TrumpCorpus[[1]])


set.seed(2020)

TrumpCorpus1 <- RemoveNames(TrumpCorpus)
TermFreqTrump <- CreateTermsMatrix(TrumpCorpus1)
TrumpDF <- data.frame(word=names(TermFreqTrump), count=TermFreqTrump)

wordcloudtrump<-wordcloud(TrumpDF$word, TrumpDF$count, max.words = 100, scale=c(2.5,.5), random.color = TRUE, colors=brewer.pal(9,"Set1"))

ClintonCorpus <- CleanCorpus(ClintonCorpus)
TermFreqClinton <- CreateTermsMatrix(ClintonCorpus)

ClintonCorpus1 <- RemoveNames(ClintonCorpus)
TermFreqClinton <- CreateTermsMatrix(ClintonCorpus1)
ClintonDF <- data.frame(word=names(TermFreqClinton), count=TermFreqClinton)

wordcloudclinton<-wordcloud(ClintonDF$word, ClintonDF$count, max.words = 100, scale=c(2.5,.5), random.color = TRUE, colors=brewer.pal(9,"Set1"))



TrumpTidy <- tidy(TrumpCorpus)
ClintonTidy <- tidy(ClintonCorpus)
TrumpTidy1 <- tidy(TrumpCorpus1) #without names
ClintonTidy1 <- tidy(ClintonCorpus1) #without names

plotBigrams <- function(tibble, topN=20, title="", color="#FF1493"){
  x <- tibble %>% select(text) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  y <- x %>% count(bigram, sort = TRUE) %>% top_n(topN, wt=n) %>%
    ggplot(aes(x=reorder(bigram, n), y=n)) +
    geom_bar(stat='identity', fill=color) + coord_flip() +
    theme(legend.position="none") + labs(x="", title=title)
}

b1 <- plotBigrams(TrumpTidy, title="With names", color="blue")
b2 <- plotBigrams(TrumpTidy1, title="Without names", color="blue")

b1c <- plotBigrams(ClintonTidy, title="With names")
b2c <- plotBigrams(ClintonTidy1, title="Without names")


DocMetaTrump1 <- meta(TrumpCorpus1)
DocMetaTrump1$date <- date(DocMetaTrump1$time)
TrumpTidy1$date <- DocMetaTrump1$date

DocMetaClinton1 <- meta(ClintonCorpus1)
DocMetaClinton1$date <- date(DocMetaClinton1$time)
ClintonTidy1$date <- DocMetaClinton1$date

NoNamesTidy <- bind_rows(trump=TrumpTidy1, clinton=ClintonTidy1, .id="candidate")
Words <- NoNamesTidy %>% unnest_tokens(word, text)

Bing <- Words %>% inner_join(get_sentiments("bing"), by="word")

b1s <- Bing %>% filter(candidate=="trump") %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="number of times used", title="Donald Trump's most used words") +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))
b2s <- Bing %>% filter(candidate=="clinton") %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="number of times used", title="Hillary Clinton's most used words") +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))



t1 <- Bing %>% filter(candidate=="trump") %>% group_by(date) %>% count(sentiment) %>%
  spread(sentiment, n) %>% mutate(score=positive-negative) %>%
  ggplot(aes(x=date, y=score)) +
  scale_x_date(limits=c(as.Date("2016-01-05"), as.Date("2016-09-27")), date_breaks = "1 month", date_labels = "%b") +
  geom_line(stat="identity", col="blue") + geom_smooth(col="red") + labs(title="Sentiment Donald Trump")

t2 <- Bing %>% filter(candidate=="clinton") %>% group_by(date) %>% count(sentiment) %>%
  spread(sentiment, n) %>% mutate(score=positive-negative) %>%
  ggplot(aes(x=date, y=score)) +
  scale_x_date(limits=c(as.Date("2016-01-05"), as.Date("2016-09-27")), date_breaks = "1 month", date_labels = "%b") +
  geom_line(stat="identity", col="blue") + geom_smooth(col="red") + labs(title="Sentiment Hillary Clinton")

  



n1 <- Nrc %>% filter(candidate=="trump") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=2500)) +
  labs(x="", y="", title="Trump")
n2 <- Nrc %>% filter(candidate=="clinton") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=2500)) +
  labs(x="", y="", title="Clinton")


