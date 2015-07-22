library(shiny)

####################
#### Define UI #####
####################
shinyUI(navbarPage("Scoring study tool",
    tabPanel("Main",
        fluidRow(
            column(4, h4("Upload"),
                         fileInput('file1','Choose score file, CSV format',
                              accept=c('text/csv','text/comman-seperated-values,text/plain','.csv')),
                    helpText("Note: columns in the dataset should follow the order below",
                             tags$br(),
                             "TEID,agegroup,item_1 score, item_2 score....."
                    ),
                    tags$hr(),
                    #checkboxInput('header','Header',TRUE),
                    h4("Check Outlier"),
                    numericInput('age','Which age group you want to check outliers for?',value='1'),
                    numericInput('change','Threshold value',value='0.005'),
                    actionButton("check","Check Outlier")  
             ),
             column(4, h4("Item Selection"),
                    numericInput("ntotal","# of items you have in total",value=20),
                    numericInput('ndelete','How many items need to be dropped?',value=1),
                    tags$hr(),
                    helpText("You don't need to fill out all three boxes below",
                             tags$br(),
                             "But the more information you give,",
                             "the shorter it takes to calculate."),
                    tags$hr(),
                    textInput('opt',"Potential drops you want to consider(use comma to seperate)",value="1,2,3"),
                    textInput('keep','Items you want to keep (use comma to separate)',value='1,2,3'),
                    textInput('drop','Items you want to drop for sure(use comma to separate',value=''),
                    div(style="display:inline-block",actionButton("est","Estimate time")),
                    div(style="display:inline-block",actionButton("cal","Calculate")),
                    helpText("Always estimate calculation time before you calculate solutions")
             ),
             column(4,h4("Summary of original document"),
                    textOutput("v2"),
                    tableOutput('vars'),
                    h4("Time Estimate"),
                    textOutput('estim'),
                    h4("Top Solutions"),
                    textOutput('regis'),
                    textOutput('finish'),
                    h4("Potential Outliers"),
                    htmlOutput('outl')
             )
           )
      ),
    tabPanel("Top Solutions",
           dataTableOutput('topsl')
    )
))
