library(shiny)
library(wordcloud)
library(tm)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #This function is repsonsible for loading in the selected file
  dataInput <- reactive({
    infile <- input$file
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    fVals<-numeric(0)
    fNames<-character(0)
    fCategories<-character(0)
    
    con<- file(infile$datapath, 'r') 
    dat<-readLines(con)
    for(i in seq(1, length(dat))) {
      candidate <- unlist(strsplit(as.matrix(dat[i]), split=' '))
      fVals <- rbind(fVals, as.numeric(candidate[1]))
      #fVals <- rbind(fVals, 10)
      fNames <-  rbind(fNames, paste(candidate[2:length(candidate)], collapse=" "))
      fCategories <- rbind(fCategories, paste(candidate[2:(length(candidate)-1)], collapse=" "))
    }
    close(con)
    
    df<- data.frame(fVals, fNames, fCategories)
    uniqueLabels <- unique(df$fCategories)
    n.unique <- length(uniqueLabels)
    n_scale <- 8
    if (n.unique %% 2 == 0) {
      n_scale <- n_scale + 1 
    } 
    col.rainbow <- rainbow(n.unique)
    col.array <- (seq(0,  n_scale * n.unique, n_scale) %% n.unique )+1
    col.ids <- col.array[1:(length(col.array)-1)]
    col.new <- col.rainbow[col.ids]
    
    df <- cbind(df, data.frame(fClass=match(df$fCategories, uniqueLabels)))
    df <- cbind(df, data.frame(fColors=col.new[df$fClass]))
    
    })
  
  ##################################################### DYNAMIC GUIwith SLIDERS #########################################################
  # plot sliders in All Features plot, dynamically
  output$ui_All <- renderUI({
    df <- dataInput()
    if(!is.null(df)) {
        max.fVal <- round(max(df$fVals)) + 1
        size.fVal <- length(df$fVals)
        fluidRow(
          column(3, sliderInput("range", "Range for all features:", min = 1, max = size.fVal, value = c(1, size.fVal))  ),
          column(3, sliderInput("featureMax", label = "Max feature value:", min = 0, max = 1.2 * max.fVal, value = c(0, max.fVal))  )
        )
    }
  })
  
  # plot Grid (second tab) sliders dynamically
  output$ui_Grid <- renderUI({
    df <- dataInput()
    if(!is.null(df)) {
      max.fVal <- round(max(df$fVals)) + 1
      size.fVal <- length(df$fVals)
      fluidRow(
        column(3,sliderInput("featureMaxMatrix", label = "Max feature value:", min = 0, max = 1.2 * max.fVal, value = c(0, max.fVal))),
        column(3,numericInput("num.plots", label = "Number of plots per row:", value = 6, min=3, max=9, step=1))
      ) 
    }
  })
  
  # plot sliders for plot with SORTED features
  output$ui_Sorted <- renderUI({
    df <- dataInput()
    if(!is.null(df)) {
      df.sorted <- df[ order(df$fVals, decreasing=TRUE), ]
      max.fVal <- round(max(df$fVals)) + 1
      size.fVal <- sum(df.sorted$fVals!=0)
      fluidRow(
        column(3, sliderInput("range2", "Range for SORTED features:", min = 1, max = size.fVal, value = c(1, size.fVal %/% 2)) ),
        column(3, sliderInput("featureMax2", label = "Max feature value:", min = 0, max = 1.2 * max.fVal, value = c(0, max.fVal)) )
      )
    }
  })
  
  # plot sliders for WordCLoud
  output$ui_WordCloud <- renderUI({
    df <- dataInput()
    if(!is.null(df)) {
      
      n.categories <- df$fClass[length(df$fCategories)]
      weights <- aggregate(df[,1],by=list(df$fClass),FUN=sum)
      max.weight <- round(max(weights)) - 1
      fluidRow(
        column(3, sliderInput("wc_freq", "Minimum Frequency:", min = 0,  max = max.weight, value = 0) ),
        column(3, sliderInput("wc_max", "Maximum Number of Words:", min = 1,  max = n.categories,  value = n.categories))
      )     
    }
  })
  
  
  ####################################################### PLOTS ################################################################ 
  # Plot window 1 (all the features)
  output$distPlot <- renderPlot({
    
    df <- dataInput()
    #par(mar=c(2.5,2,1.9,5))
    if(!is.null(df) && !is.null(input$range)) {
      brp <- barplot(df$fVals[input$range[1]:input$range[2]], 
              #names.arg=df$fClass[input$range[1]:input$range[2]],
              ylim=input$featureMax, las=2, col=as.character(df$fColors[input$range[1]:input$range[2]]), 
              border=as.character(df$fColors[input$range[1]:input$range[2]])
              , cex.names=0.8, xpd=FALSE
              )
      axis(1, at=brp, labels=df$fClass[input$range[1]:input$range[2]])
      box()
      
      #bar.width <- length(df$fClass) * 0.9 / (input$range[2] - input$range[1])
      #plot(df$fVals, type="h", lwd=bar.width, las=2, col=as.character(df$fColors), xlim=input$range, ylim=input$featureMax, cex.axis=0.8, 
      #     xlab="Feature Index", ylab="Feature Value", frame.plot=TRUE, yaxt="n", xaxt="n")
      #axis(2)
      #axis(side=1, at=seq(1:length(df$fClass)),labels=df$fClass)
      grid(NA,NULL)
    }
  })
  
  # Plot window 2 (sorted features)
  output$distPlot2 <- renderPlot({
      df <- dataInput()
      if(!is.null(df) && !is.null(input$range2)) {
        df.sorted <- df[ order(df$fVals, decreasing=TRUE), ]
        size.sorted.features <- sum(df.sorted$fVals!=0)
        
        brp1 <- barplot(df.sorted$fVals[input$range2[1]:input$range2[2]], ylim=input$featureMax2, 
                #names.arg=df.sorted$fClass[input$range2[1]:input$range2[2]], 
            las=2, col = as.character(df.sorted$fColors[input$range2[1]:input$range2[2]]), border=NA, cex.names=0.8,
            xpd=FALSE)
        axis(1, at=brp1, labels=df.sorted$fClass[input$range2[1]:input$range2[2]], las=2, cex.axis=0.85)
        box()
        #bar.width <- size.sorted.features * 5.0 / (input$range2[2] - input$range2[1])
        #plot(df.sorted$fVals[1:size.sorted.features], type="h", lwd=bar.width , las=2, 
        #     col=as.character(df.sorted$fColors[1:size.sorted.features]), xlim=input$range2, ylim=input$featureMax2,
        #     cex.axis=0.8, 
        #     xlab="Feature Index", ylab="Feature Value", frame.plot=FALSE, yaxt="n", xaxt="n")
        #axis(2)
        #axis(side=1, at=seq(1:sum(df.sorted$fVals!=0)),labels=df.sorted$fClass[1:size.sorted.features], las=2, cex.axis=0.8)
        
        grid(NA,NULL)
      }
  })
  
  # table with all features in a separate tab on the webpage
  output$table <- renderTable({
    df <- dataInput()
    if(!is.null(df)) {
      names(df)[1] <- "Value"
      names(df)[2] <- "Feature"
      names(df)[4] <- "ClassID"
      data.frame(subset(df, select=c(1,2,4)))
    }
  })
  
  # table with sorted features in a separate tab on the webpage
  output$table2 <- renderTable({
    df <- dataInput()
    if(!is.null(df)) {
      df.sorted <- df[ order(df$fVals, decreasing=TRUE), ]
      names(df.sorted)[1] <- "Value"
      names(df.sorted)[2] <- "Feature"
      names(df.sorted)[4] <- "ClassID"
      data.frame(subset(df.sorted, select=c(1,2,4)))
    }
  })
  
  # table below the plot with SORTED features 
  output$table_features <- renderTable({
    df <- dataInput()
    if(!is.null(df)) {
      df.sorted <- df[ order(df$fVals, decreasing=TRUE), ]
      names(df.sorted)[1] <- "Value"
      names(df.sorted)[2] <- "Feature"
      names(df.sorted)[4] <- "ClassID"
      data.frame(subset(df.sorted, select=c(1,2,4)))
    }
  })  
  
  # table below the plot with Histogram of Cumulative features 
  output$table_features_cumulative <- renderTable({
    df <- dataInput()
    if(!is.null(df)) {
      weights <- aggregate(df[,1],by=list(df$fClass),FUN=sum)
      labels <- sapply(unique(df$fCategories), function(x) {
        gsub("Coefficients","Coefs.",x)
        gsub(" ()","",x, fixed=T)
      })
      data.frame(Weight=weights$x, Features=labels)
    }
  })  
  
  # table below the plot with Histogram of Average features 
  output$table_features_average <- renderTable({
    df <- dataInput()
    if(!is.null(df)) {
      weights <- aggregate(df[,1],by=list(df$fClass),FUN=sum)
      sizes <- aggregate(df[,1],by=list(df$fClass),FUN=length)
      labels <- sapply(unique(df$fCategories), function(x) {
        gsub("Coefficients","Coefs.",x)
        gsub(" ()","",x, fixed=T)
      })
      data.frame(Weight=weights$x / sizes$x, Features=labels)
    }
  })  
  
  
  # table below the plot with All features 
  output$table_features_all <- renderTable({
    df <- dataInput()
    if(!is.null(df)) {
      df.unique <- data.frame(unique(df$fCategories))
      names(df.unique)[1] <- "Unique Names"
      df.unique
    }
  })  
  
  # Word Cloud plot 
  output$wordCloudPlot <- renderPlot({
    df <- dataInput()
    if(!is.null(df) && !is.null(input$wc_freq)) {
      weights <- aggregate(df[,1],by=list(df$fClass),FUN=sum)
      labels <- sapply(unique(df$fCategories), function(x) {
        gsub("Coefficients","Coefs.",x)
        gsub(" ()","",x, fixed=T)
      })
      wordcloud(labels,  weights$x, min.freq = input$wc_freq, max.words=input$wc_max,
                colors=as.character(unique(df$fColors)), ordered.colors=T, scale=c(2,0.8), random.order=F)
    }

  })
  
  # Category Histogram plot for cumulative weights
  output$wordCloudHistogram <- renderPlot({
    df <- dataInput()
    if(!is.null(df)) {
      op0 = par()    # Get current graphical parameters
      op1 = op0$mar  # Get current margins in lines
      op1[1] = 18
      par(mar = op1)
      weights <- aggregate(df[,1],by=list(df$fClass),FUN=sum)
      labels <- sapply(unique(df$fCategories), function(x) {
        gsub("Coefficients","Coefs.",x)
        gsub(" ()","",x, fixed=T)
        })
      barplot(weights$x, col=as.character(unique(df$fColors)), names.arg=labels, las=3, cex.names=0.9)
    }
  })
  
  # Category Histogram plot for cumulative weights
  output$wordCloudHistogramAverage <- renderPlot({
    df <- dataInput()
    if(!is.null(df)) {
      op0 = par()    # Get current graphical parameters
      op1 = op0$mar  # Get current margins in lines
      op1[1] = 18
      par(mar = op1)
      weights <- aggregate(df[,1],by=list(df$fClass),FUN=sum)
      sizes <- aggregate(df[,1],by=list(df$fClass),FUN=length)
      
      
      labels <- sapply(unique(df$fCategories), function(x) {
        gsub("Coefficients","Coefs.",x)
        gsub(" ()","",x, fixed=T)
      })
      barplot(weights$x / sizes$x, col=as.character(unique(df$fColors)), names.arg=labels, las=3, cex.names=0.9)
    }
  })
  
  # Category Grid with histograms of features 
  output$distPlotMartix <- renderPlot({
    df <- dataInput()
    if(!is.null(df) && !is.null(input$num.plots)) {
      par(mar=c(1.5,0.5,1.9,0.5))
      par(oma=c(2,1,1,1))
      par(mgp=c(3,0.2,0))
      number.plots  <- df$fClass[length(df$fClass)]
      #substitutions to make the names of the features shorter and fit to the histograms-titles 
      labels <- sapply(unique(df$fCategories), function(x) gsub("Coefficients","Coefs.",x))
      labels <- sapply(labels, function(x) gsub("Fourier","F",x))
      labels <- sapply(labels, function(x) gsub("Wavelet","W",x))
      labels <- sapply(labels, function(x) gsub("Chebyshev", "Ch", x))
      labels <- sapply(labels, function(x) gsub("Zernike","Z",x))
      labels <- sapply(labels, function(x) gsub("Histogram","Hist.",x))
      labels <- sapply(labels, function(x) gsub("Textures","Text.",x))
      labels <- sapply(labels, function(x) gsub("Features","Feat.",x))
      labels <- sapply(labels, function(x) gsub(" ()","",x, fixed=T))
      
      n.x <- input$num.plots
      n.y <- number.plots %/% n.x + 1
      par(mfrow=c(n.y, n.x))
      
      for (id.class in 1:number.plots) {
        barplot(df$fVals[df$fClass==id.class], names.arg=seq(0, length(df$fVals[df$fClass==id.class])-1),
                col=as.character(df$fColors[df$fClass==id.class]), 
                #xlim=c(0,length(df$fVals[df$fClass==id.class])), 
                ylim=input$featureMaxMatrix,
                cex.names=0.99, cex.axis=0.99, tck=0, main=labels[id.class], xpd=FALSE
                #, border=as.character(df$fColors[df$fClass==id.class])
                )
        grid(NA,NULL)
      }
    }
  })

#   ## test of google charts
#   output$gvis <- renderGvis({
#     df <- dataInput()
#     if(!is.null(df)) {
#       df.sorted <- df[ order(df$fVals, decreasing=TRUE), ]
#       size.sorted.features <- sum(df.sorted$fVals!=0) -100
#       colors.short <- sapply(df.sorted$fColors, function(x) substr(x, 1, 7))
#       gvisBarChart(df.sorted[1:size.sorted.features,], yvar=c("fVals", "fVals"), xvar="fCategories",                  
#                    options=list(width=1100, height=600, legend='none', 
#                                 colors="['#ff0000', '#00ff00']", 
#                                 fontSize=10, orientation='horizontal'))
#       
#       #gvisGeoChart(df,
#       #           locationvar="fVals", colorvar="fColors",
#       #           options=list(region="US", displayMode="regions", 
#       #                        resolution="provinces",
#       #                        width=400, height=380,
#       #                        colorAxis="{colors:['#FFFFFF', '#0000FF']}"
#       #           )) 
#     }     
#   })
  
  
})