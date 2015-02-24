library(shiny)
library(markdown)

# Define UI for application that draws a histogram of features
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Statistics of features (WND-CHARM)"),

      fileInput("file", "Select file with features:", multiple = FALSE, 
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      includeHTML("include.html"),
      tabsetPanel(type = "tabs", 
                  
                  tabPanel("All Features", plotOutput("distPlot", width="95%"), 
                           uiOutput("ui_All"),
                           tableOutput("table_features_all")), 
 
                  tabPanel("All Features (Grid)", plotOutput("distPlotMartix", height="850px"), 
                          uiOutput("ui_Grid")
                  ), 
                  
                  tabPanel("Sorted Features", plotOutput("distPlot2", width ="95%"), 
                          uiOutput("ui_Sorted"),
                          tableOutput("table_features")), 
                  
                  tabPanel("List of Features", tableOutput("table")),
                  
                  tabPanel("List of Sorted Features", tableOutput("table2")),
                  
                  tabPanel("Word Cloud", helpText("Word Cloud, based on cumulative feature weights per category"), 
                           plotOutput("wordCloudPlot"),
                           uiOutput("ui_WordCloud")
                  ),
                  
                  tabPanel("Feature Classes (Cumulative)", helpText("Cumulative feature weights per category"), 
                           plotOutput("wordCloudHistogram", height="500px"),
                           tableOutput("table_features_cumulative") 
                           ),
                  
                  tabPanel("Feature Classes (Average)", helpText("Average feature weights per category"), 
                           plotOutput("wordCloudHistogramAverage", height="500px"),
                           tableOutput("table_features_average")
                  ),
                  
      #           tabPanel("GoogleCharts", helpText("Test example of using google's librequire(devtools)"), 
      #                     htmlOutput("gvis")
      #           ),
      
                  tabPanel("Source", helpText("Sourcecode of the required ui.R and server.R files, together with the additional include.html and version.html files."), helpText("ui.R"),
                           includeMarkdown("ui.Rmd"),  
                           helpText("server.R"),
                           includeMarkdown("server.Rmd"),
                           helpText("include.html"),
                           includeMarkdown("include.html")
                  )
      ),
      includeHTML("version.html")
    )
  )

