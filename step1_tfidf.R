library(jsonlite)
library(tm)
library(wordcloud)
library(wordcloud2)

#read data (review dataset)
review_2013 <- stream_in(file("D:\\project\\yelp-recruiting\\yelp_training_set\\yelp_training_set_review.json"))

#flatten column(vote)
column_vote <- review_2013$votes
funny <- column_vote[, 1]
useful <- column_vote[, 2]
cool <- column_vote[, 3]
column_vote <- as.data.frame(cbind(funny, useful, cool))

without_vote <- review_2013[, -c(1)]
#without_vote <-review_2013[,-c(1,ncol(data))] 
review_after_flatten <- cbind(column_vote, without_vote)

#get required columns
user_id <- as.data.frame(review_after_flatten[, 4])
text <- as.data.frame(review_after_flatten[, 8]) 
business_id <- as.data.frame(review_after_flatten[, 10])
review.funny <- as.data.frame(review_after_flatten[, 1])
review.useful <- as.data.frame(review_after_flatten[, 2])
review.cool <- as.data.frame(review_after_flatten[, 3])
data_text <- cbind(business_id, user_id, review.funny, review.useful, review.cool, text) 
colnames(data_text) <- c("business_id", "user_id", "funny", "useful", "cool", "text")

#read data (business dataset)
business <- read.csv("F:\\project_python\\cat_business.csv", header = TRUE)
business_id2 <- business$business_id

#get the rows related to the restaurant
data <- data_text[which(data_text$business_id %in% business_id2), ]
#export to csv
write.csv(data,"review_2013_new.csv",row.names = FALSE)


##############################################################################
#tf_idf


#Convert text to Corpus format
review_corpus <- Corpus(VectorSource(data$text))
#Clear punctuation
review_corpus <- tm_map(review_corpus, removePunctuation)
#transform to lowercase
review_corpus <- tm_map(review_corpus, content_transformer(tolower))
#clear stopwords
review_corpus <- tm_map(review_corpus, removeWords, c("the", "and", "ive", "really", "also", "didnt", "cant", "dont", "will", "just", "back", stopwords("english")))
#clear \n
review_corpus <- tm_map(review_corpus, function(word){
  gsub("\n", "", word)
})

#Generate document-term matrix(DTM)
review_dtm <- DocumentTermMatrix(review_corpus)
#reject some words with lower weight values
review_dtm <- removeSparseTerms(review_dtm, 0.99)
#word frequency
freq <-  data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))

#add new column(word)
freq['word'] <- rownames(freq)
#make a new frequency table
freq1 <- cbind(freq['word'], freq[, 1])
write.csv(freq1,'freq1.csv',row.names = FALSE)
#read freq1(remove rownames)
freq1 <- read.csv("F:\\project_python\\text-mining\\freq1.csv", header = TRUE)

#wordcloud
wordcloud(rownames(freq), freq[,1], max.words=100,random.order=FALSE, rot.per=0.35, colors=brewer.pal(7, "Dark2"))
#wordcloud2
hw <- wordcloud2(freq1, color = "random-dark", backgroundColor = "#F2FFFF", minSize = 10)
#lettercloud
letterCloud(freq1, color = "random-dark", wordSize = 2, word = "yelp")

#save as html
library(webshot)
library(htmlwidgets)
saveWidget(hw,"1.html",selfcontained = F)
webshot::webshot("1.html","1.jpg",vwidth = 3000, vheight = 3000, delay =10)

#Use tf_idf as the word frequency weight
review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
#reject some words with lower weight values
review_dtm_tfidf <-  removeSparseTerms(review_dtm_tfidf, 0.95)
#word frequency
freq2 <- data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing = TRUE))
#add new column(word)
freq2['word'] <- rownames(freq2)
#make a new frequency table
freq3 <- cbind(freq2['word'], freq2[, 1])
write.csv(freq3, "freq3.csv", row.names = FALSE)

#read freq3(remove rownames)
freq3 <- read.csv("F:\\project_python\\text-mining\\freq3.csv", header = TRUE)

#wordcloud2
hw2 <- wordcloud2(freq3, color = "random-dark", backgroundColor = "#F2FFFF", minSize = 10)
#save as html
saveWidget(hw2,"2.html",selfcontained = F)
webshot::webshot("2.html","2.jpg",vwidth = 3000, vheight = 3000, delay =10)

#change colnames
colnames(freq3) <- c("word", "frequency")

#export to csv
write.csv(freq3,"tf_idf_output.csv",row.names = FALSE)

