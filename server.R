
#******************************************************
# Tree DBH-Height Modelling Tool
# (c) Lauri Vesa, FAO
# version 09th April 2020
#
# Reads input data from CSV file that contains 3-4 columns: 
# cluster, plot, tree_dbh, tree_height
#
# Edited: 15.4.2021
# -added updated manual 
#******************************************************


library('tidyr')
library('dplyr')
library('lmfor')
#library('ggplot2') # included in plotly
library('gridExtra')
library('grid') # rasterGrob
library('stringr')
library('png')
library('DT')
library('skimr')     # summary statistics (see e.g. https://dabblingwithdata.wordpress.com/2018/01/02/my-favourite-r-package-for-summarising-data/)

Resultfolder <- paste0(normalizePath("~/module_results/lmfor"),"/")

dir.create(Resultfolder, showWarnings = FALSE, recursive = TRUE)

source("Estimate height.R")

DF_Model = data.frame(
  ModelIndex         = c(0,0,0),
  ModelName          = c("", "", ""),
  Converge           = c(FALSE, FALSE, FALSE), 
  Iterations         = c("111","111","111"),
  stringsAsFactors   = FALSE)


get_colors <- function(tree2, var_name) {
  
  # create unique temporary plot ID as text variable
  tree2$plot_id <- paste( tree2$cluster, tree2$plot, sep = '_' )
  
  # colors for graphs: grouped by plots
  if (var_name=="plot_id") {
    cl <- as.data.frame(unique(tree2$plot_id))
  } else if (var_name=="plot") {
    cl <- as.data.frame(unique(tree2$plot))
  } else {
    cl <- as.data.frame(unique(tree2$cluster))
  }
  
  
  names(cl)   <- var_name
  cl[,1]      <- as.character(cl[,1]) 
  cl$colorNo  <- rep(2 : length(colors()), len=nrow(cl))
  cl$color_id <- colors()[cl$colorNo] 
  cl$colorNo  <- NULL
  print(nrow(cl))
  
  # https://stackoverflow.com/questions/54823846/dplyr-left-join-does-not-work-with-a-character-objects-as-the-lhs-variable
  join_cols        =  c(var_name)
  names(join_cols) <- var_name
  
  tree2 <- tree2 %>% 
    left_join(cl, by= join_cols ) 
  
  return(tree2)
}


plot_estimates <- function(treedata, col_index, modelname, var_type, var_name, Max_values) {
  treedata$tree_height <- NULL
  
  treedata$color_id    <- NULL
  treedata <- get_colors(treedata, var_name) 
  
  # print("COLO")
  # print(unique(treedata$color_id))
  # rm(cl)
  
  maxD <- Max_values$maxD
  maxH <- Max_values$maxH
  
  maxD <- ifelse(maxD > 0, maxD, 10* round((5 + as.integer(max(treedata$tree_dbh   )))/10,0) )
  maxH <- ifelse(maxH > 0, maxH, 10* round((5 + as.integer(max(treedata$tree_height)))/10,0) )
  

  pr <- renderPlot({  
    
    # unique group number
    tree3  <- subset(treedata, imputed==TRUE)
    tree3  <- transform(tree3, id1 = as.numeric(factor(plot_id)))
    tree3  <- transform(tree3, id2 = as.numeric(factor(cluster)))
    tree3  <- tree3 %>% arrange(id2, id1, tree_dbh)    
    
    if (is.na(mean(tree3$h1))) {
     return(NA)
      
    } else {
      if (var_name=="plot_id" & mean(tree3$h1) > 1.3) {
        pp1 <- ggplot(data = tree3, aes(x = tree_dbh, y = h1, group= id1 )) 
      }  else if (mean(tree3$h2)>1.3) {
        pp1 <- ggplot(data = tree3, aes(x = tree_dbh, y = h2, group= id2 )) 
      }
      pp1 <- pp1 +
        geom_point(aes(colour= color_id), size = 0.5, show.legend = FALSE) + 
        xlim(0, maxD) + ylim(0, maxH) +
        geom_line(aes(colour= color_id )) +   
        xlab("Diameter, cm") + ylab("Height, m") + 
        ggtitle(paste0( modelname, ": Fixed + ", var_type )) +
        theme(legend.position = "none")
    }
    
    rm(tree3)
    pp1
    
  })
  if (is.null(pr)) return( NA )
  
  return(pr)
}


tree_plots <- function( tree, model_list, theta, Max_values ) 
{
  
  tree2 <- subset(tree, !is.na(tree_dbh) & tree_height>1.3 & tree_height<70)  
  # graphic output file
  png(paste0(Resultfolder,"height_models.png"), width=600) # ,height=7,pointsize = 12)
  
  
  maxD <- Max_values$maxD
  maxH <- Max_values$maxH
  
  maxD <- ifelse(maxD > 0, maxD, 10* round((5 + as.integer(max(tree2$tree_dbh   )))/10,0) )
  maxH <- ifelse(maxH > 0, maxH, 10* round((5 + as.integer(max(tree2$tree_height)))/10,0) )
  
  plot(tree2$tree_dbh, tree2$tree_height,
       main= "Fixed part models", xlab="DBH, cm", ylab="Height, m",
       xlim=c(1, maxD), ylim=c(0, maxH))
  
  d           <- seq(0, maxD)
  plot_colors <- c( "blue", "red", "green")
  tr          <- array()
  i = 0
  
  for (j in model_list) {
    i = i + 1
    #    print(paste0("model_list: ", model_list))
    
    
    if (theta[[i]][1] != 0 & theta[[i]][2] != 0) {
      if (1  == j) lines(d, HDnaslund(   d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (2  == j) lines(d, HDcurtis(    d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (3  == j) lines(d, HDmichailoff(d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (4  == j) lines(d, HDmeyer(     d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (5  == j) lines(d, HDpower(     d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (6  == j) lines(d, HDnaslund2(  d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (7  == j) lines(d, HDnaslund3(  d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (8  == j) lines(d, HDnaslund4(  d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (9  == j) lines(d, HDmicment(   d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (10 == j) lines(d, HDmicment2(  d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (11 == j) lines(d, HDwykoff(    d, a= theta[[i]][1], b=theta[[i]][2]), col=plot_colors[i], lwd=2)
      if (12 == j) lines(d, HDprodan(    d, a= theta[[i]][1], b=theta[[i]][2], c=theta[[i]][3]), col=plot_colors[i], lwd=2)
      if (13 == j) lines(d, HDlogistic(  d, a= theta[[i]][1], b=theta[[i]][2], c=theta[[i]][3]), col=plot_colors[i], lwd=2)
      if (14 == j) lines(d, HDrichards(  d, a= theta[[i]][1], b=theta[[i]][2], c=theta[[i]][3]), col=plot_colors[i], lwd=2)
      if (15 == j) lines(d, HDweibull(   d, a= theta[[i]][1], b=theta[[i]][2], c=theta[[i]][3]), col=plot_colors[i], lwd=2)
      if (16 == j) lines(d, HDgomperz(   d, a= theta[[i]][1], b=theta[[i]][2], c=theta[[i]][3]), col=plot_colors[i], lwd=2)
      if (17 == j) lines(d, HDsibbesen(  d, a= theta[[i]][1], b=theta[[i]][2], c=theta[[i]][3]), col=plot_colors[i], lwd=2)
      if (18 == j) lines(d, HDkorf(      d, a= theta[[i]][1], b=theta[[i]][2], c=theta[[i]][3]), col=plot_colors[i], lwd=2)
      if (19 == j) lines(d, HDratkowsky( d, a= theta[[i]][1], b=theta[[i]][2], c=theta[[i]][3]), col=plot_colors[i], lwd=2)
      if (20 == j) lines(d, HDhossfeldIV(d, a= theta[[i]][1], b=theta[[i]][2], c=theta[[i]][3]), col=plot_colors[i], lwd=2)
      
      tr[i] <- ifelse(j < 12, paste(str_to_title(model_names[j]), "(a=", round(theta[[i]][1],6),", b=", round(theta[[i]][2],6),")"),
                      paste(str_to_title(model_names[j]), "(a=", round(theta[[i]][1],6),", b=", round(theta[[i]][2],6), " c=", round(theta[[i]][3],6),")"))
    } else {
      tr[i] <- " "
    }
    
  } # for
  
  legend("topleft", tr, cex=0.8, col=plot_colors, lty=1, lwd=2, bty="n")
  graphics.off()
  
  #  return( NA )    
}


server = function(input, output, session) {
  
  #    library(dplyr)
  # https://stackoverflow.com/questions/47248534/dynamically-list-choices-for-selectinput-from-a-user-selected-column
  
  mydata <- ""
  
  resultvalues      <- reactiveValues(df_data = NULL)
  model_parameters  <- reactiveValues(c1 = 0.0, c2 = 0.0, c3 = 0.0)
  Max_values        <- reactiveValues(maxD = 0.0, maxH = 0.0) 
  df_models         <- reactiveValues(data=DF_Model)
  
  
  clear_outputs <- function(){
    # updateTextInput(session, "txtout1", value = NULL)
    updateTextInput(session, "summary", value = NULL)
    
    df                    <- data.frame()
    output$df_result_data <- NULL
    output$contents       <- NULL
    resultvalues$df_data  <- NULL
    model_parameters$c1   <- 0.0
    model_parameters$c2   <- 0.0
    model_parameters$c3   <- 0.0
    Max_values$maxD       <- 0.0
    Max_values$maxH       <- 0.0
    
    sResults              <- NULL
    
    hideTab(inputId = "tabs", target = "Predictions -3")
    hideTab(inputId = "tabs", target = "Predictions -2")
    
    fn <- "./www/dataestimates1.csv"
    if (file.exists(fn)) file.remove(fn)
    fn <- "./www/dataestimates2.csv"
    if (file.exists(fn)) file.remove(fn)
    fn <- "./www/dataestimates3.csv"
    if (file.exists(fn)) file.remove(fn)
    rm(fn)
    
    #    })
  }
  
  
  
  rawData <- eventReactive(input$file1, {
    #    clear_outputs()
    if(is.null(input$file1)){
      return()
    }
    
    
    a1 <- read.csv(input$file1$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    #    a1 <- a1[, -1] # remove index column
    # missing cluster column
    '%ni%' <- Negate('%in%')
    if ("Cluster" %in% colnames(a1)) names(a1)[names(a1) == 'Cluster'] <- 'cluster'
    if ("Plot"    %in% colnames(a1)) names(a1)[names(a1) == 'Plot']    <- 'plot'
    if ("cluster" %ni% colnames(a1)) a1$cluster <- "A"
    
    a1$cluster <- as.character(a1$cluster)
    
    # missing cluster data
    if (is.na(unique(a1$cluster))) a1$cluster <- "A" 
    
    a1$plot    <- as.character(a1$plot)
    
    output$contents <- renderDataTable({
      M <- DT::datatable(data_analysis, options = list(scrollX = TRUE))
      M
    })
    
    return(a1)
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(rawData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  ## update 'column' selector
  observeEvent(rawData(), {
    data_analysis <- rawData()
    
    max_n <- nrow(data_analysis)
    #  print(paste("N: ", max_n))
    
    output$contents <- renderDataTable({
      M <- DT::datatable(data_analysis, options = list(scrollX = TRUE))
      M
    })
    
  })
  
  
  
  output$html_link <- renderUI({
    a("Find out more about lmfor in CRAN", href="https://cran.r-project.org/web/packages/lmfor/index.html", target="_blank") 
  })  
  
  output$CJFR_link <- renderUI({
    a("CJFR: Modeling height-diameter curves for prediction", href="https://www.nrcresearchpress.com/doi/abs/10.1139/cjfr-2015-0054#.Xs5zFjozY2w", target="_blank") 
  })  
  
  output$html_link2 <- renderUI({
    a("A short User's Guide is here", href="https://drive.google.com/file/d/1f9FvW-F8bVL_Fx9jbvVK6Luumj_Ff4Tg/view", target="_blank") 
  })  
  
  output$html_link3 <- renderUI({
    a("Download sample plot data for testing the application", href="https://drive.google.com/open?id=1-ixDxXvqMzZjTDjkVPZwRVGF3n-Jvjxg", target="_blank") 
  })  
  
  
  
  
  observeEvent(input$action_plot, {
    
    data_analysis <-  reactive({
      data_analysis <- rawData()
      
      data_analysis <- subset(data_analysis, !is.na(tree_dbh)) 
      #      data_analysis <- subset(data_analysis, tree_height > 1.35) 
      
      return(data_analysis)
    })
    
    model_list      <- input$numSelector
    model_list_len  <- length(model_list)
    
    showTab(inputId = "tabs", target = "Predictions -2")
    showTab(inputId = "tabs", target = "Predictions -3")
    
    if (model_list_len==2) { hideTab(inputId = "tabs", target = "Predictions -3") }
    if (model_list_len==1) {
      hideTab(inputId = "tabs", target = "Predictions -3")
      hideTab(inputId = "tabs", target = "Predictions -2")
    }
    
    # if more than 3 models, pick up first 3 models
    if (model_list_len>3) {
      model_list      <- model_list[1:3]
      model_list_len  <- 3
    }
    
    Max_values$maxD <- as.numeric(input$d_max)
    Max_values$maxH <- as.numeric(input$h_max)
    
    #  print(model_list_len)
    input_data <- data_analysis()
    
    max_n <- nrow(data_analysis())
    #  print(paste("N - trees with heights:", max_n))
    
    
    if ( nrow(input_data) & model_list_len > 0) {
      
      withProgress(message = 'Application running', value = 0.1, {
        
        model_list <- as.integer( model_list ) 
        tulos_all  <- list()
        tulos      <- list()
        
        j = 0
        for (i in 1:model_list_len) {
          
          incProgress(0.5/model_list_len - 0.05)
          
          j = j + 1
          df_models$ModelIndex[j] = i
          df_models$ModelName[j]  = model_names[i]
          
          tulos_all[i] <- tree_modelling(
            model_index   = i,
            model_list    = model_list,
            tree          = input_data,
            random_effect = input$random_effect,
            Resultfolder  = Resultfolder,
            nrap          = as.integer(input$sel_nrap),
            useVariance   = input$check_variance
          )
          
          if (!is.na(tulos_all[i])) 
          { tulos[i] <- tulos_all[[i]][1]
          tree2    <- as.data.frame(tulos_all[[i]][2])
          # print(str(tree2))
          } else {
            tulos[i] <- NA
            tree2    <- NA 
          }
          
          if (!is.na(tulos[i])) {
            para_a <- as.numeric( tulos[[i]][1] )
            df_models$Converge[[j]]   = ifelse(para_a != 0, TRUE, FALSE)
            df_models$Iterations[[j]] = tulos[[i]][4] 
            tulos[[i]] <- as.numeric( tulos[[i]] )
            
            if (j==1) {
              n1 <- model_names[model_list[j]]
              output$plot_estimates4 <- plot_estimates( subset(tree2, imputed), 6, n1, "Cluster", "cluster", Max_values) 
              output$plot_estimates1 <- plot_estimates( subset(tree2, imputed), 5, n1, "Plot",    "plot_id", Max_values)
              write.csv(tree2, "./www/dataestimates1.csv", row.names = FALSE)
            }
            if (j==2) {
              n2 <- model_names[model_list[j]]
              output$plot_estimates5 <- plot_estimates( subset(tree2, imputed), 6, n2, "Cluster", "cluster", Max_values) 
              output$plot_estimates2 <- plot_estimates( subset(tree2, imputed), 5, n2, "Plot",    "plot_id", Max_values) 
              write.csv(tree2, "./www/dataestimates2.csv", row.names = FALSE)
            }
            if (j==3) { 
              n3 <- model_names[model_list[j]]
              output$plot_estimates6 <- plot_estimates( subset(tree2, imputed), 6, n3, "Cluster", "cluster", Max_values)           
              output$plot_estimates3 <- plot_estimates( subset(tree2, imputed), 5, n3, "Plot",    "plot_id", Max_values) 
              write.csv(tree2, "./www/dataestimates3.csv", row.names = FALSE)
            }
          } else {
            para_a <- 0.0
            df_models$Converge[[j]]   = FALSE
            df_models$Iterations[[j]] = "000" 
            tulos[[i]] <- c( 0, 0, 0, 0 )
            
            if (i==1) output$plot_estimates1 <- NULL
            if (i==1) output$plot_estimates4 <- NULL
            if (i==2) output$plot_estimates2 <- NULL
            if (i==2) output$plot_estimates5 <- NULL
            if (i==3) output$plot_estimates3 <- NULL
            if (i==3) output$plot_estimates6 <- NULL
          }
          
          if (i==3) break
          
          incProgress(0.5/model_list_len - 0.05)
          
        }
        
      })
      
      if (!is.na(tulos)) {
        tree_plots(input_data, model_list, tulos, Max_values )
      }
      
      
      #https://stackoverflow.com/questions/58942332/side-by-side-images-in-r-shiny
      if ( df_models$Iterations[[1]] != "000" )   {
        output$image_residuals1 <-   renderImage({
          ifi <- paste0(Resultfolder, "fitted_residual_", as.character(1), ".png")
          list(src = ifi, contentType = 'image/png', alt = "LMFOR output")
        }, deleteFile = TRUE)
      } else {
        output$image_residuals1 <-   renderImage({
          ifi <- "./converge_message.png"
          list(src = ifi, contentType = 'image/png',  alt = "LMFOR output")
        }, deleteFile = FALSE)
      }
    }
    
    
    if ( model_list_len > 1 ) {
      shinyjs::show( "image_residuals2" )
      if ( df_models$Iterations[[2]] != "000" )   {
        output$image_residuals2 <- renderImage({
          ifi <- paste0(Resultfolder, "fitted_residual_", as.character(2), ".png")
          list(src = ifi, contentType = 'image/png', alt = "LMFOR output")
        }, deleteFile = TRUE)
      } else {
        output$image_residuals2 <-   renderImage({
          ifi <- "./converge_message.png"
          list(src = ifi, contentType = 'image/png', alt = "LMFOR output")
        }, deleteFile = FALSE)
      }
    } else {
      shinyjs::hide( "image_residuals2" )
      shinyjs::hide( "image_residuals3" )
    }
    
    if ( model_list_len > 2 ) {
      shinyjs::show( "image_residuals3" )
      if ( df_models$Iterations[[3]] != "000" )   {
        output$image_residuals3 <- renderImage({
          ifi <- paste0(Resultfolder, "fitted_residual_", as.character(3), ".png")
          list(src = ifi, contentType = 'image/png', alt = "LMFOR output")
        }, deleteFile = TRUE)
      } else {
        output$image_residuals3 <-   renderImage({
          ifi <- "./converge_message.png"
          list(src = ifi, contentType = 'image/png', alt = "LMFOR output")
        }, deleteFile = FALSE)
      }
    }else {
      shinyjs::hide( "image_residuals3" )
    }
    
    
    output$image_curves <- renderImage({
      ifi <- paste0(Resultfolder, "height_models.png")
      list(src = ifi, contentType = 'image/png', 
           alt = "Fitted curves")
    }, deleteFile = TRUE)
    
    showNotification("Processing completed", type = "message")
    
  })
  
  # end of observeEvent(input$action_plot
  
  # https://stackoverflow.com/questions/33416557/r-shiny-download-existing-file
  output$downloadData1 <-  downloadHandler(
    filename <- function() {
      paste("heightdata_1", "csv", sep=".")
    },
    content <- function(file) {
      file.copy("./www/dataestimates1.csv", file)
    },
    contentType = "application/csv"
  )
  
  output$downloadData2 <-  downloadHandler(
    filename <- function() {
      paste("heightdata_2", "csv", sep=".")
    },
    content <- function(file) {
      file.copy("./www/dataestimates2.csv", file)
    },
    contentType = "application/csv"
  )
  
  output$downloadData3 <-  downloadHandler(
    filename <- function() {
      paste("heightdata_3", "csv", sep=".")
    },
    content <- function(file) {
      file.copy("./www/dataestimates3.csv", file)
    },
    contentType = "application/csv"
  )
  
  output$summary <- renderPrint({
    # https://stackoverflow.com/questions/51340323/skimr-how-to-remove-histogram/51341508
    skimr::skim_without_charts(rawData())
  })
  
  
  output$plot1 <- renderPlotly({  
    df <- rawData()
    #     print(names(df))
    
    # create unique temporary plot ID as text variable
    df$plot_id <- paste( df$cluster, df$plot, sep = '_' )
    
    df <- get_colors(df, input$data_color) 
    
    maxD <- Max_values$maxD
    maxH <- Max_values$maxH
    
    maxD <- ifelse(maxD > 0, maxD, 10* round((5 + as.integer(max(df$tree_dbh   )))/10,0) )
    maxH <- ifelse(maxH > 0, maxH, 10* round((5 + as.integer(max(df$tree_height)))/10,0) )
    
    p1 <- ggplot(data = df, aes(label = plot_id) )
    p1 <- p1 + geom_point(aes(x = tree_dbh, y = tree_height, colour= color_id), size = 0.7, show.legend = FALSE) 
    p1 <- p1 + xlim(0, maxD) + ylim(0, maxH)
    p1 <- p1 + xlab("Diameter, cm") + ylab("Height, m") # + ggtitle("DBH-Height relationship")
    # p1 <- p1 + stat_function(fun = h_model, colour = "red")
    p1 <- p1 + theme(legend.position = "none")
    
    p1 <- ggplotly(p1, tooltip = c("tree_dbh","tree_height","plot_id")) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c(
               'sendDataToCloud',
               'autoScale2d',
               'resetScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'zoom2d','pan2d','select2d','lasso2d','zoomIn2d', 'zoomOut2d'
             )) 
    p1
    
  })
  
}

