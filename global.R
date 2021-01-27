# functionizing

textclassif_nb <- function(test_csv, # input file with text and Y colms
                           y_n0,     # position of Y colm in the input DF. Need user-input  
                           x_n0,     # position of X or text colm. User input
                           trg_propn = 0.70,   # default and slider for user input
                           n00 = 20,
                           model="nb",
                           user_stpw = stopw_list,
                           rem_punct = punct
                           
                           ){   # num_term coeffs to display for each class 
  set.seed(123)
  x = test_csv[, x_n0]
  y = test_csv[, y_n0]
  
  # building DFM
  test_corpus <- corpus(x) # constructing a corpus
  dfm_test = test_corpus %>% tokens(., remove_punct = rem_punct) %>% # tokenize
    tokens_select(., pattern = c(stopwords("en"),user_stpw), selection = "remove") %>% # pre-proc
    dfm(.) %>% dfm_trim(., min_termfreq = 5)  # build DFM
  
  # generate 70% sample for training, sample numbers without replacement
  train_n0 = trg_propn*nrow(dfm_test) %>% round(.,0); train_n0
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
  
  ## Train the naive Bayes classifier using textmodel_nb().
  if(model=="nb"){
    tmod_nb <- textmodel_nb(dfm_training, y[id_train])
  }
  if(model=="lr"){
    tmod_nb <- textmodel_lr(dfm_training, y[id_train])
  }
 
  #summary(tmod_nb)
  
  dfm_matched <- dfm_match(dfm_test, features = featnames(dfm_training))  # match tokens from trg & test DFMs
  actual_class <- y[id_test]; length(actual_class)  # Y value must be taken based on user-input or default
  predicted_class <- predict(tmod_nb, newdata = dfm_matched); # head(predicted_class)
  
  tab_class <- table(factor(actual_class,sort(unique(y))), factor(predicted_class,sort(unique(y)))); # tab_class  # confusion matrix
  
  if (nrow(tab_class) == ncol(tab_class)){ outp0 = confusionMatrix(tab_class, mode = "everything")
  } else {outp0 = tab_class}
  outp0  # output for display in app
  
  # display feature based results
  classif_probs = tmod_nb$param # dim(tmod_nb$param)
  list00 = vector(mode = "list", length = nrow(classif_probs))
  
  # n00 = 20 # top n00 terms that impact a class to be displayed => from user input
  
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
  
  return(list(outp0, list00)) } # func ends
