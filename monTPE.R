#####################################                 #####################################
###############################     TEXT MINING AVEC R    #################################
#####################################                 #####################################

# Le traitementt de texte necessites des packages et des librairies
# Nous avons des packages specifiques obligatoires qui doivent être  installés et appelés par
# des librairies


library("tm") # Packages pour le text mining

library("SnowballC") # Packages pour la Racinisation

library("wordcloud") # Packages pour les nuages de mots

library("RColorBrewer") # Packages pour les palettes couleurs
library("plot3D")
library("CharFun")
library("charlatan")
library("rattle")
library("RGtk2")
library("devtools")

 
filePath <-  "C:/Users/dietrich/Desktop/TPE-R-PROJET/Test_TPE.txt"
text <- readLines(filePath)
docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
inspect(docs)
docs <- tm_map(docs, removeNumbers)
inspect(docs)
docs <- tm_map(docs, toSpace, "@")
inspect(docs)
docs <- tm_map(Corpus, tolower)
inspect(docs)
docs <- tm_map(docs, toSpace, "\ | ")
inspect(docs)
docs <- tm_map(docs, removeWords, c("its" , "out"  , "these" , "These"  , "those"  , "they" , "protein" , "and" , "in" , "the" , "at" , "this"
                                     , "for" , "is" , "us" , "are" , "the" , "The"  , "be" , "been" , "from"  , "have"   , "being" , "to" , "of" , "that" , "was" , "were" , "but"))
inspect(docs)
docs <- tm_map(docs, removePunctuation)
inspect(docs)
docs <- tm_map(docs, stripWhitespace)
inspect(docs)
#docs <- tm_map(docs, stemDocument)
inspect(docs)
dtm <- TermDocumentMatrix(docs)
inspect(dtm)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
inspect(dtm)

head(d, 300)


set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 1)

head(d, 100)

findAssocs(dtm, terms = "protein", corlimit = 0.3)

barplot(d[1:30,]$freq, las = 2, names.arg = d[1:30,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

