library(ChemmineR)
library(ChemmineOB)
library(shiny)
library(DT)
library(dplyr)
library(shinyWidgets)
#-------------------------------------------------------------
#coumarins
cou<-read.SDFset("coumarin.sdf")
cou1<-propOB(cou)
cou1<-cou1 %>% select(-InChI)
#reading .csv file
cou2<-read.csv("coumarin.csv", header = T, sep = ",")
#merging both dataframes(b and c)
cou3<-merge(cou1,cou2, by = "title")
cou4<-cou3 %>% select(-cansmiNS)
cou5<-cou4 %>% select(-title)
cou6<-cou5 %>% relocate(Chemical.Name) #Move Col to front
cou6$cansmiName = paste(cou6$cansmi, cou6$Chemical.Name, sep=" ")# joining two cols
cou6<-cou6 %>% relocate(cansmiName)
#------------------------------------------------------------
#Benzothiozole
ben<-read.SDFset("BenzothiazoleScaffold.sdf")
ben1<-propOB(ben)
ben1<-ben1 %>% select(-InChI)
#reading .csv file
ben2<-read.csv("Benzo.csv", header = T, sep = ",")
#merging both dataframes(b and c)
ben3<-merge(ben1,ben2, by = "title")
ben4<-ben3 %>% select(-cansmiNS)
ben5<-ben4 %>% select(-title)
ben6<-ben5 %>% relocate(Chemical.Name) #Move Col to front
ben6$cansmiName = paste(ben6$cansmi, ben6$Chemical.Name, sep=" ")# joining two cols
ben6<-ben6 %>% relocate(cansmiName)
#-----------------------------------------------------
#Pyrazole
pyr<-read.SDFset("PyrazoleScaffold.sdf")
pyr1<-propOB(pyr)
pyr1<-pyr1 %>% select(-InChI)
#reading .csv file
pyr2<-read.csv("Pyrazole.csv", header = T, sep = ",")
#merging both dataframes(pyr1 and pyr2)
pyr3<-merge(pyr1,pyr2, by = "title")
pyr4<-pyr3 %>% select(-cansmiNS)
pyr5<-pyr4 %>% select(-title)
pyr6<-pyr5 %>% relocate(Chemical.name) #Move Col to front
pyr6$cansmiName = paste(pyr6$cansmi, pyr6$Chemical.name, sep=" ")# joining two cols
pyr6<-pyr6 %>% relocate(cansmiName)
#-----------------------------------------------------


shinyUI(fluidPage(
  title = 'CBPDdb',
  titlePanel(title = h1("Coumarin-Benzothiazole-Pyrazole Derivatives Database (CBPDdb)", align ="center",style = "color: DarkBlue")),
  br(),
  h1("1.Full Dataset with Filters",align ="center",style = "color: DarkBlue"),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel( 
      tags$style(type='text/css', ".selectize-input { font-size: 30px; line-height: 30px;} .selectize-dropdown { font-size: 28px; line-height: 28px;background-color: #dab4c1;}"),
      
      #h1("Colored Tabs"),
      tags$style(HTML("
    
    .tabbable > .nav > li > a[data-value='Coumarin'] {background-color: brown;   color:white}
    .tabbable > .nav > li > a[data-value='Benzothiozole'] {background-color: magenta;  color:white}
    .tabbable > .nav > li > a[data-value='Pyrazole'] {background-color: lime;  color:white}
    
    .tabbable > .nav > li[class=active]    > a {background-color: blue; color:white}
  ")),
      
      (h1( "*SKETCHER*",align ="center",style = "color: DarkBlue")),
      #font
      tags$style("body{background-color:#e4f0e7; color:green}"),
      includeHTML('JSME_testt.html'),
      br(),
      br(),
      br(),
      #------
      
      (h4(strong(tags$span(style="color:DarkOrchid", "SCREENING PARAMETERS")))),
      
      selectInput("var","Select the vaiable from dataset",choices=c("MW "=10 ,"LogP"=8, "NumHDonors" = 7, " NumHAcceptors1" =5, "TPSA" = 12, "nF" = 11),selected=11),
      br(),
      br(),
      br(),
      
      #sliderColours and types
      chooseSliderSkin("Big"),
      
      setSliderColor(c("DeepPink ", "PaleVioletRed", "Gold", "Teal","Peru"), c(1, 2, 3,4,5)),
      
      #(h4(strong(tags$span(style="color:DarkOrchid", "SCREENING PARAMETERS")))),
      #br(),
      sliderInput("bin","Select the number of bins", min=0, max=30, value = 15),
      
      br(),
      br(),
      #to center
      (h3( "****************************",align ="center",style = "color: DarkBlue")),
      tags$figure(
        align = "center",
        tags$img(src = 'R.png',height = 100,width=100, align="middle"),
        
        tags$img(src = 'dplyr.png', height=100,width=100,align = 'center'),
        
        tags$img(src = 'shiny.jpeg', height=100,width=100,align = 'center')),
      br(),
      br(),
      br(),
      br(),
      (h3("For any queries, suggestions and feedback please contact shailima.rampogu@gmail.com"))
      
      
    ),
    
    mainPanel(
      tags$head(
        tags$style(type='text/css', 
                   ".nav-tabs {font-size: 30px};")),
      tabsetPanel(type="tabs",
                  
                  # fontawesome.com                                 
                  # Full dataset
                  tabPanel("Coumarin",DT::dataTableOutput("cum"),icon = icon("seedling"),
                           
                           downloadButton(outputId = "cum_filtered",
                                          label = "cum2Compounds",class = "good"),
                           downloadButton(outputId = "cumSDF",
                                          label = "cum2SDF",class = "good"),
                           tags$head(tags$style(h5(".good{background:white;} .good{color: pink;font-size: 20px}")))),
                  
                  #--------------------------       
                  tabPanel("Benzothiozole",DT::dataTableOutput("benzo"),icon = icon("chalkboard"),
                           
                           downloadButton(outputId = "benzo_filtered",
                                          label = "benzo2Compounds",class = "basic"),
                           downloadButton(outputId = "benzoSDF",
                                          label = "benzo2SDF", class = "basic"),
                           tags$head(tags$style(h5(".basic{background:white;} .basic{color: magenta;font-size: 20px}")))),
                  #---------------------------------------------------------
                  tabPanel("Pyrazole",DT::dataTableOutput("pyr"),icon = icon("atom"),
                           
                           downloadButton(outputId = "Pyro_filtered",
                                          label = "Pyro2Compounds", class = "orange"),
                           downloadButton(outputId = "pyroSDF",
                                          label = "Pyro2SDF",class = "orange"),
                           tags$head(tags$style(h5(".orange{background:white;} .orange{color: orange;font-size: 20px}")))),
                  
                  br(),
                  br()
      ),
      
      #----------------------------------------------------------------
      
      
      (h1( "2.Full Dataset Graphical Frequency Analysis of Descriptors",align ="center",style = "color: DarkBlue")),
      fluidRow(
        column(4,
               
               plotOutput("mhist")),
        
        column(4,
               plotOutput("benhist")),
        column(4,
               plotOutput("cumhist"))
      ),
      br(),
      br(),
      #FILTERED DATA EXTRACTION
      (h1("3.Extracting cansmi(smiles) Column:Filtered Data",align ="center",style = "color: DarkBlue")),
      br(),
      br(),
      tabsetPanel(type="tabs",           
                  tabPanel("FilteredCoumarin",DT::dataTableOutput("cumfiltered"),icon = icon("seedling"),
                           
                           downloadButton(outputId = "cumd",
                                          label = "Filteredcum2csv",class = "basic"),
                           
                           downloadButton(outputId = "cumsdf",
                                          label = "Filteredcum2sdf",class = "basic"),
                           tags$head(tags$style(h5(".basic{background:white;} .basic{color: magenta;font-size: 20px}"))))
                  
      ),
      
      br(),
      br(),
      
      #------------Filtered Benzo-------------------------
      tabsetPanel(type="tabs",           
                  tabPanel("FilteredBenzothiozole",DT::dataTableOutput("Filteredbenz"),icon = icon("chalkboard"),
                           
                           downloadButton(outputId = "bend",
                                          label = "Filteredben2csv",class = "basic"),
                           
                           downloadButton(outputId = "bensdf",
                                          label = "Filteredben2sdf",class = "basic"),
                           tags$head(tags$style(h5(".basic{background:white;} .basic{color: magenta;font-size: 20px}"))))
                  
      ),
      br(),
      br(),
      #----------filtered Pyro-----       
      tabsetPanel(type="tabs",           
                  tabPanel("FilteredPyrazole",DT::dataTableOutput("Filteredpyro"),icon = icon("atom"),
                           
                           downloadButton(outputId = "pyrod",
                                          label = "Filteredpyro2csv",class = "basic"),
                           
                           downloadButton(outputId = "pyrosdf",
                                          label = "Filteredpyro2sdf",class = "basic"),
                           tags$head(tags$style(h5(".basic{background:white;} .basic{color: magenta;font-size: 20px}"))))
                  
      )            
      
      
    )
  ))
)
