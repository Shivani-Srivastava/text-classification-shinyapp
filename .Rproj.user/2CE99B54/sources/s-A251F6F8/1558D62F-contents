install.packages("readtext")
install.packages("quanteda.textplots")
library("readtext")
library("quanteda.textplots")
df <- read.csv("data/Amazon - Fire.csv")
df <- read.csv("C://Users/31202/Downloads//Airline-Sentiment-2-w-AA (1).csv")


dfm <- corpus(df$review) %>% 
  dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 10, verbose = FALSE)

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


























