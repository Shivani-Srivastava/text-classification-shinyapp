# # functionizing
# 
# build_tokens <- function(data,n_grams=1,stopw_list){
#   tokens <- tokens(
#     data,
#     remove_punct = TRUE,
#     remove_numbers = TRUE,
#     remove_symbols = TRUE,
#     split_hyphens = TRUE,
#   )
#   
#   tokens <- tokens_tolower(tokens)
#   tokens <- tokens_select(tokens,c(stopwords("en"),stopw_list),selection="remove")
#   tokens <- tokens_wordstem(tokens,language="english")
#   tokens <- tokens_ngrams(tokens,n=1:n_grams)
#   return(tokens)
# }

#------Function 1: Train Test Split------
train_test_split <- function(df,x_n0,y_n0,trg_propn){
  set.seed(123)
  #print(df[,y_n0])
  train.index <- createDataPartition(df[,y_n0], p = trg_propn, list = FALSE)
  train <- df[ train.index,]
  test  <- df[-train.index,]
 # train_size <<- dim(train)
 #  test_size <<- dim(test)
  return(list(train,test))
}


#------Function 2: Build DFM----
build_dfm <- function(data,n_grams=1,
                      remove_punct=remove_punct,
                      remove_numbers=remove_numbers,
                      remove_symbols=remove_symbols,
                      split_hyphens=split_hyphens,
                      stem=stem,
                      lemma =lemma,
                      #remove_url = remove_url,
                      user_list = stopw_list )
  {
  tokens <- tokens(
    data,
    remove_punct = remove_punct,
    remove_numbers = remove_numbers,
    remove_symbols = remove_symbols,
    split_hyphens = split_hyphens
   # remove_url = remove_url,
  )
  
  tokens <- tokens_tolower(tokens)
  tokens <- tokens_select(tokens,c(stopwords("en"),user_list),selection="remove",min_nchar = 2)
  if(stem==TRUE){
    tokens <- tokens_wordstem(tokens,language="english")
  }else{
    
  }
  if(lemma==TRUE){
    tokens <- tokens_replace(tokens, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
  }
  tokens <- tokens_ngrams(tokens,n=1:n_grams) # we can add n_gram feature later, but it will increase training time as feature set increases
  return(dfm(tokens))
}


#------Function 3: Train & Eval-------
train_and_evaluate_model <- function(dfm_train,y_train,dfm_test,y_test,model="nb",n00=100){
  ## Train the naive Bayes classifier using textmodel_nb().
  if(model=="nb"){
    tmod <- textmodel_nb(dfm_train, y_train)
  }
  if(model=="lr"){
    tmod <- textmodel_lr(dfm_test, y_train)
  }
  
  #summary(tmod_nb)
  
  dfm_matched <- dfm_match(dfm_test, features = featnames(dfm_train))  # match tokens from trg & test DFMs
  actual_class <- y_test; length(actual_class)  # Y value must be taken based on user-input or default
  predicted_class <- predict(tmod, newdata = dfm_matched); # head(predicted_class)
  
  tab_class <- table(actual_class,predicted_class) # tab_class  # confusion matrix
  outp0 <- confusionMatrix(tab_class,mode="everything")
  # display feature based results
  classif_probs = tmod$param # dim(tmod_nb$param)
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
  
  return(list(outp0, list00))
}






#------Function 4: Train Model-------
textclassif <- function(test_csv, # input file with text and Y colms
                           y_n0,     # position of Y colm in the input DF. Need user-input  
                           x_n0,     # position of X or text colm. User input
                           trg_propn = 0.70,   # default and slider for user input
                           n00 = 20,
                           model="nb",
                           stopw_list = stopw_list
                           ){   # num_term coeffs to display for each class 
  set.seed(123)
  train.index <- createDataPartition(test_csv[,y_n0], p = trg_propn, list = FALSE)
  train <- test_csv[ train.index,]
  test  <- test_csv[-train.index,]
  
  
  # building DFM
  dfm_training <- build_dfm(train[,x_n0],stopw_list)
  dfm_test <- build_dfm(test[,x_n0],stopw_list)

  ## Train the naive Bayes classifier using textmodel_nb().
  if(model=="nb"){
    tmod_nb <- textmodel_nb(dfm_training, train[,y_n0])
  }
  if(model=="lr"){
    tmod_nb <- textmodel_lr(dfm_training, train[,y_n0])
  }
 

  #summary(tmod_nb)
  
  dfm_matched <- dfm_match(dfm_test, features = featnames(dfm_training))  # match tokens from trg & test DFMs
  actual_class <- test[,y_n0]; length(actual_class)  # Y value must be taken based on user-input or default
  predicted_class <- predict(tmod_nb, newdata = dfm_matched); # head(predicted_class)
  tab_class <- table(actual_class,predicted_class)
  outp0 = confusionMatrix(tab_class, mode = "everything")
 # tab_class <- table(factor(actual_class,sort(unique(y))), factor(predicted_class,sort(unique(y)))); # tab_class  # confusion matrix
  
  # if (nrow(tab_class) == ncol(tab_class)){ outp0 = confusionMatrix(tab_class, mode = "everything")
  # } else {outp0 = tab_class}
  # outp0  # output for display in app
  
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



