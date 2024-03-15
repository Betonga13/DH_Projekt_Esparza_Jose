install.packages("readtext")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("tm")
install.packages("tmap")
install.packages("RColorBrewer")
install.packages("leaflet")
install.packages("NLP")
install.packages("manifestoR")

library(readtext)
library(wordcloud)
library(wordcloud2)
library(tm)
library(tmap)
library(RColorBrewer)
library(leaflet)
library(quanteda)
library(tidyverse)
library(manifestoR)

mp_setapikey("C:/R/DigitalHumanities/manifesto1/manifesto_apikey.txt")
mp_maindataset()

corpus_601 <- mp_corpus(countryname == "Germany" & edate > as.Date("1990-01-01"), codefilter = c(601.2))
head(codes(corpus_601))
print(corpus_601)
meta(corpus_601)
write.csv(corpus_601, "corpus_601.csv")
print(corpus_601)

textdata<-read.csv("C:/R/DigitalHumanities/manifesto1/corpus_601.csv",sep= ",",encoding="UTF-8")
stopwords_extended <-readLines("C:/R/DigitalHumanities/manifesto1/german_stopwords_full.txt", encoding ="UTF-8")

corpus_data <- quanteda::corpus(textdata$text, docnames=textdata$doc_id)

corpus_tokens <-corpus_data%>%
  tokens(remove_punct=TRUE, remove_numbers= TRUE, remove_symbols= TRUE)%>%
  tokens_tolower() %>%
  tokens_remove(pattern= stopwords_extended,padding= T) %>%
  tokens_wordstem(language = "de") %>%
  tokens_remove("")
  
DTM <- dfm(corpus_tokens) %>% dfm_trim(min_docfreq = 3)

top10_terms<-c("deutschland", "europa")
DTM<-DTM[, !(colnames(DTM) %in%top10_terms)]

#dtm<-TermDocumentMatrix(corpus)
#m<-as.matrix(dtm)
v<-sort(colSums(DTM),decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)
#head(d,10)

wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words = 1000, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(11, "Dark2"))

sel_idx<-rowSums(DTM) >0
DTM<-DTM[sel_idx, ]
textdata<-textdata[sel_idx,]

require(topicmodels)

K <-10

topicModel<-LDA(DTM,K, method="Gibbs",control=list(
  iter= 500,
  seed= 1,
  verbose= 25,
  alpha= 0.02))

terms(topicModel, 10)

tmResult<-posterior(topicModel)
beta<-tmResult$terms 
theta <-tmResult$topics

library(LDAvis)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), 
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)
