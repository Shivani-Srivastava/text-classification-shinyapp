library(quanteda)
library(magrittr)
library(caret)
library(quanteda.textmodels)
library(shinyWidgets)
library(DT)
library(e1071)
library(wordcloud)
library(RColorBrewer)
#library(lexicon)
hash_lemmas<-readRDS('hash_lemmas.rds')
shinyUI(fluidPage(
  title = "Text Classification",
  titlePanel(title=div(img(src="logo.png",align='right'),"Text Classification")),
  
  # Input in sidepanel:
  sidebarPanel(
    
    pickerInput(
      inputId = "sample",
      label = "Load Sample Data", 
      choices = sub('\\.csv$', '',list.files('data')), 
      choicesOpt = list(icon = c("plane-departure", "amazon", 'twitter','refresh')),
      options =list(`live-search` = TRUE,style = "btn-primary")
    ),
    
  
    
    #selectInput("sample","Load Sample Data",choices = sub('\\.csv$', '',list.files('data'))),
    p("OR"),
    fileInput("file", "Upload Data"),
    #actionButton("load","Load Data"),
    actionButton(
      inputId = "load",
      label = "Load Data", 
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
      color = "default",size = 's',icon=icon('cloud-upload-alt')
      ),
    
   # h5("select X"),
   hr(),
   strong(p("Variable Selection")),
   uiOutput('inp_var'),
   # h5("select Y"),
   uiOutput('tar_var'),
   hr(),
   strong(p("Data Preprocessing")),
   helpText("Note: Remove symbols does'nt remove @ from twitter handles"),
   
   fluidRow(
            column(4,align="left",
                   prettyCheckbox('remove_punct', 'Remove Punctuation', value = TRUE,status = 'primary',icon = icon("check")),
                   prettyCheckbox('split_hyphens', 'Split Hyphen', value = TRUE,status = 'primary',icon = icon("check"))),
            column(4,align="left",
                   prettyCheckbox('remove_symbols', 'Remove Symbols', value = TRUE,status = 'primary',icon = icon("check")),
                   prettyCheckbox('stem', 'Stemming', value = FALSE,status = 'primary',icon = icon("check"))),
            column(4,align="left",
                   prettyCheckbox('remove_numbers', 'Remove Numbers', value = TRUE,status = 'primary',icon = icon("check")),
                   prettyCheckbox('lemma', 'Lemmatization', value = FALSE,status = 'primary',icon = icon("check"))
                   ),
                ),
   
   
   textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
   #actionButton('plotwc',"Plot WordCloud"),
   actionButton(
     inputId = "plotwc",
     label = "Plot WordCloud",
     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
     color = "default",size = 's'),
  # ),
   
   
   
   # h5("select training data (in percentage)"),
   hr(),
   strong(p("Train-Test Split")),
    sliderInput("tr_per",label = "select training data (in percentage)",min = 0,max = 100,value = 70,step = 1),
   
    hr(),
    #h5("select classification algorithm"),
    strong(p("Model Selection")),
    selectInput("algo",label = "select algorithm",choices = c("Naive Bayes"="nb","Logistic Regression"="lr")),
    
    #actionButton("apply","Train Model"),
    
  actionButton(
    inputId = "apply",
    label = "Train Model", 
    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
    color = "default",size = 's',icon=icon('cogs')
  ),
  
  
     progressBar(id = "pb6", value = 0, status = "success", size = "xs")
  
  ),
  
  # Main Panel:
  mainPanel( 
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview & Example Dataset",h4(p("How to use this App")),
                         
                         p("To use this app you need a document corpus in csv file format. Make sure each document is separated from another document with a new line character.
                          Dataset should contain atleast two columns text (used as input) and targert (used for predictions).
                           To do basic Text classfication on your text corpus, click on Browse in left-sidebar panel and upload the file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                         p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refreshed
                           ", align = "Justify"),
                         h5("Note"),
                         p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the computations
                           are over in back-end results will be refreshed",
                           align = "justify"),
                         #, height = 280, width = 400
                         br(),
                         h4(p("Download Sample file")),
                         selectInput("downloads","Select Sample file to download",choices = sub('\\.csv$', '',list.files('data'))),
                         downloadButton('downloadData1', 'Download'),br(),br(),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png")
                )
                ,
               
                tabPanel("Data",
                         h5("Data Dimensions"),
                         verbatimTextOutput('dim'),
                         h5("Distribution of Target(Y)"),
                         verbatimTextOutput('y_dis'),
                         hr(),
                         h5("Sample dataset"),
                         dataTableOutput("sampleData"),hr(), 
                         h4("Word Cloud"),
                         dropdownButton(
                           
                          tags$h3("List of Inputs"),
                           
                          sliderInput(inputId = 'minword',
                                       label = 'Min Term Frequency',
                                       value = 10,
                                       min = 1,
                                       max = 50),
                           
                           circle = TRUE, status = "danger",
                           icon = icon("gear"), width = "300px",
                           
                           tooltip = tooltipOptions(title = "Click to see inputs !")
                         ),
                         
                         plotOutput("wordcloud",height = 700, width = 700),br(),
                         #textInput("in",label = "text"),
                         
                        ),
                        tabPanel("Training Report",
                                 h4("Confusion Matrix"),
                                 textOutput("cf_text"),
                                 plotOutput("cf_matrix"),
                                 hr(),
                                 verbatimTextOutput('cf'),
                                 hr(),
                                 uiOutput("tokens"),
                                 
                                 dropdownButton(
                                   
                                   tags$h3("List of Inputs"),
                                   
                                   sliderInput(inputId = 'maxword',
                                               label = 'Max words to display',
                                               value = 20,
                                               min = 1,
                                               max = 100),
                                   
                                   circle = TRUE, status = "danger",
                                   icon = icon("gear"), width = "300px",
                                   
                                   tooltip = tooltipOptions(title = "Click to see inputs !")
                                 ),
                                 
                                 plotOutput("token_wc",height = 700, width = 700),br(),
                                 
                                 
                                  dataTableOutput("token_table")
                                 
                                )
    ),
    
    )
  )
  )



