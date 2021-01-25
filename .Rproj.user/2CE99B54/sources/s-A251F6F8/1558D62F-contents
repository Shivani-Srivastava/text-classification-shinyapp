install.packages("readtext")
install.packages("quanteda.textplots")
library("readtext")
library("quanteda.textplots")
#df <- read.csv("data/Amazon - Fire.csv")
df <- read.csv("C://Users/31202/Downloads//")


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

ol<- textclassif_nb(df, # input file with text and Y colms
               "stars",     # position of Y colm in the input DF 
               "review",     # position of X or text colm
               trg_propn = 0.70,   # default and slider for user input
               n00 = 100,
               model="lr")   # num_term coeffs to display for each class  
class(ol[[1]])
