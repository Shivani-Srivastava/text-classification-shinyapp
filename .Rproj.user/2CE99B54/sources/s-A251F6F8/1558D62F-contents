install.packages("readtext")
install.packages("quanteda.textplots")
library("readtext")
library("quanteda.textplots")
df <- read.csv("data/tweets_sentiment.csv")
#df <- read.csv("C://Users/31202/Downloads//Airline-Sentiment-2-w-AA (1).csv")
test_csv <- df
x_n0 = "text"
y_n0 = "sentiment"

train.index <- createDataPartition(test_csv[,y_n0], p = 0.7, list = FALSE)
train <- test_csv[ train.index,]
test  <- test_csv[-train.index,]



# building DFM
test_corpus <- corpus(x) # constructing a corpus
dfm_test = test_corpus %>% tokens(., remove_punct = TRUE) %>% # tokenize
  tokens_select(., pattern = stopwords("en"), selection = "remove") %>% # pre-proc
  dfm(.) %>% dfm_trim(., min_termfreq = 5)  # build DFM

# generate 70% sample for training, sample numbers without replacement
train_n0 = 0.7*nrow(dfm_test) %>% round(.,0); train_n0
id_train = sample(seq(1:nrow(dfm_test)), train_n0, replace = FALSE); length(id_train)  
id_test = as.matrix(seq(1:nrow(dfm_test)))[-id_train, 1]; length(id_test) 
test_corpus$id_numeric <- 1:ndoc(test_corpus) # create docvar with ID

# get training & test sets
dfm_training <- test_corpus %>% corpus_subset(., id_numeric %in% id_train) %>% 
  tokens(., remove_punct = TRUE) %>%
  dfm(., remove = stopwords("en"), stem = TRUE)

dfm_test <- test_corpus %>% corpus_subset(., !id_numeric %in% id_train) %>% 
  tokens(., remove_punct = TRUE) %>%  
  dfm(remove = stopwords("en"), stem = TRUE)



train.dfm <- build_dfm(train$review)
test.dfm <- build_dfm(test$review)


tmod_nb <- textmodel_nb(dfm_training, y[id_train])
tmod_nb_1 <- textmodel_nb(train.dfm, train$stars)

train$stars

dfm_matched <- dfm_match(test.dfm, features = featnames(train.dfm))  # match tokens from trg & test DFMs
actual_class <- test[,y_n0]; length(actual_class)  # Y value must be taken based on user-input or default
predicted_class <- predict(tmod_nb_1, newdata = dfm_matched); # head(predicted_class)
tab_class <- table(actual_class,predicted_class)
outp0 = confusionMatrix(tab_class, mode = "everything")


# table(actual_class,predicted_class)
# tab_class <- table(factor(actual_class,sort(unique(test_csv[,y_n0]))), factor(predicted_class,sort(unique(test_csv[,y_n0])))); 
# 
# if (nrow(tab_class) == ncol(tab_class)){ outp0 = confusionMatrix(tab_class, mode = "everything")
# } else {outp0 = tab_class}
# outp0  # output for display in app

# display feature based results
classif_probs = tmod_nb$param # dim(tmod_nb$param)
list00 = vector(mode = "list", length = nrow(classif_probs))


for (i0 in 1:nrow(classif_probs)){
  
  a00 = classif_probs[i0,]; head(a00)
  a01 = sort(a00, decreasing = TRUE, index.return=TRUE)
  a02 = a00[a01$ix[1:n00]]
  
  df00 = data.frame( tokens = names(a02)[1:n00], proby = round(a02[1:n00], 3))
  rownames(df00) = NULL
  colnames(df00) = sapply(colnames(df00), function(x) {paste0(x, "_", i0)}) %>% as.character(.)
  
  list00[[i0]] = df00      } # i0 loop ends

list00 <- setNames(list00,rownames(classif_probs))

df0 = as.data.frame(list00); df0  # this becomes output for display






set.seed(100)
textplot_wordcloud(dfm,color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))

df$text <- as.character(df$text)
text_corpus <- corpus(df,text_field = 'review')

corpus_subset(text_corpus, 
              text %in% c(unique(df$text)) %>%
  dfm(groups = "text", remove = stopwords("english"), remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 5, verbose = FALSE) %>%
  textplot_wordcloud(comparison = TRUE))
t <- data_corpus_inaugural

ol<- textclassif_nb(test_csv = df, # input file with text and Y colms
                    y_n0= "airline_sentiment",     # position of Y colm in the input DF 
                    x_n0="text",     # position of X or text colm
                    trg_propn = 0.70,   # default and slider for user input
                    n00 = 100,
                    model="nb",
                    user_stpw = c("will,can"),#,amazon,great"),
                    rem_punct = TRUE
              )   # num_term coeffs to display for each class  

ol[[1]][['table']]
#class(ol[[1]])



# Save confusion matrix as data frame
confusion.data <- as.data.frame( ol[[1]][['table']])
names(confusion.data) <- c('actual_class','predicted_class ','Freq')

ggplot(confusion.data, aes(x=Var1, y=Var2, fill=Freq)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette="Greens", direction=1) +
  guides(fill=F) + # removing legend for `fill`
  labs(title = "Value distribution") + # using a title instead
  geom_text(aes(label=Freq), color="black")+ylab("Predicted")+xlab("Actual") # printing values



# Reverse the order
level_order_y <-
  factor(confusion.data$Var1,
         level = unique(confusion.data$Var1))
ggplot(confusion.data,
       aes(x = Var1, y = level_order_y, fill = Freq)) +
  xlab("Predicted class") +
  ylab("Actual class") +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_x_discrete(labels = c("Brazil", "France", "Great \n Britain", "Japan", "USA")) +
  scale_y_discrete(labels = c("USA", "Japan", "Great \n Britain", "France", "Brazil"))



ggplot(confusion.data, aes(x=Var1, y=Var2, fill=Freq)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette="Greens", direction=1) +
  guides(fill=F) + # removing legend for `fill`
  labs(title = "Value distribution") + # using a title instead
  geom_text(aes(label=Freq), color="black") # printing values


























