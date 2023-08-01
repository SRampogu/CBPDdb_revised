library(ChemmineR)
library(ChemmineOB)
library(shiny)
library(DT)
library(dplyr)
library(shinyWidgets)
shinyServer(function(input,output){
  
  #cur-------------
  make_link <- function(x){
    paste0("<a target = '_blank' href= ", x,">",x,"</a>")
  }
  cou6$`References` <- make_link(cou6$`References`)
  
#_---------------------------------------  
  output$cum = DT::renderDataTable({
    datatable(cou6, extensions = 'Select',
              selection = list(target = "column"),
              filter = "top",
              escape=FALSE, 
              class = 'cell-border stripe',
              options = list(searchHighlight = TRUE, autoWidth = TRUE))
    
  })
  #-----------------------------------------------------
  
  #----------------------------------------------------------
  output$cumfiltered<- DT::renderDataTable({
    subset_table <- cou6[input$cum_rows_all, input$cum_columns_selected, drop = F]
    datatable(subset_table,rownames = FALSE)
  })
  
  #=========Filtered========= 
  
  output$cumd <- downloadHandler(
    filename = "Filteredcum.csv",
    content = function( file){
      write.csv(cou6[input$cum_rows_all, input$cum_columns_selected, drop = F],
                file)
    }
  )
  #------sdf-------------------
  output$cumsdf <- downloadHandler(
    filename = "Filteredcum.sdf",
    content = function( file){
      write.SDF(smiles2sdf(cou6[input$cum_rows_all, input$cum_columns_selected]),
                file)
    }
  )  
  
  
  #------------*****
  output$cumhist <- renderPlot({
    col <- as.numeric(input$var)
    hist(cou6[,col], breaks =seq(-4, max(cou6[,col]),l=input$bin+1),col="MediumVioletRed",
         main="Histogram of Coumarins", ylim = c (0,100), xlab=names(cou6[col]))
  })
  
  #-----Full/filtered-----------
  
  output$cum_filtered <- 
    downloadHandler(
      filename = "cum.csv",
      content = function(file){
        write.csv(cou6[input[["cum_rows_all"]], ],
                  file)
      }
    )
  # Full dataset
  output$cumSDF <- 
    downloadHandler(
      filename = "cumFull.sdf",
      content = function(file){
        write.SDF(cou,file)
        
      }
    )
  #------BENZo---------------------------------------
  
  
  #--------------------------------------------------------
  
  # Click to open Links
  clickable_ref <- function(x){
    paste0("<a target = '_blank' href= ", x,">",x,"</a>")
  }
  ben6$'References' <- clickable_ref(ben6$'References')
  
  
  #-----------------------------------------------------------
  #Full Datatable
  output$benzo = DT::renderDataTable({
    datatable(ben6,extensions = 'Select',
              selection = list(target = "column"),
              filter = "top",
              escape=FALSE,
              class = 'cell-border stripe',
              options = list(searchHighlight = TRUE, autoWidth = TRUE))
    
  })
  #------------------#Filtered table-----------------------------------------------
  
  output$Filteredbenz <- DT::renderDataTable({
    subset_table <- ben6[input$benzo_rows_all, input$benzo_columns_selected, drop = F]
    datatable(subset_table,rownames = FALSE)
  })
  
  #================== 
  
  output$bend <- downloadHandler(
    filename = "Filteredben.csv",
    content = function( file){
      write.csv(ben6[input$benzo_rows_all, input$benzo_columns_selected, drop = F],
                file)
    }
  )
  #------sdf
  output$bensdf <- downloadHandler(
    filename = "Filteredben.sdf",
    content = function( file){
      write.SDF(smiles2sdf(ben6[input$benzo_rows_all, input$benzo_columns_selected]),
                file)
    }
  )  
  
  #---------Plot Benzo---
  output$benhist <- renderPlot({
    col <- as.numeric(input$var)
    hist(ben6[,col], breaks =seq(-4, max(ben6[,col]),l=input$bin+1),col="Bisque",
         main="Histogram of Benzothiozole", ylim = c (0,400), xlab=names(ben6[col]))
  })
  
  #------Full/Filtered----------
  
  output$benzo_filtered <- 
    downloadHandler(
      filename = "Benzo.csv",
      content = function(file){
        write.csv(ben6[input[["benzo_rows_all"]], ],
                  file)
      }
    )
  #Full SDF
  output$benzoSDF <- 
    downloadHandler(
      filename = "benzoFull.sdf",
      content = function(file){
        write.SDF(ben,file)
        
      }
    )
  #----------------------
  
  #--------pyr-----------------------
  
 # Clickable References
  clickable_ref <- function(x){
    paste0("<a target = '_blank' href= ", x,">",x,"</a>")
  }
  pyr6$'References' <- clickable_ref(pyr6$'References')
  
  
 #------------------------------------------- 
  
  #__________________________
  #Full Dataset
  output$pyr = DT::renderDataTable({
    datatable(pyr6,extensions = 'Select',
              selection = list(target = "column"),
              filter = "top",
              escape=FALSE, 
              class = 'cell-border stripe',
              options = list(searchHighlight = TRUE, autoWidth = TRUE))
  })
  
  #------------------#Filtered table-----------------------------------------------
  
  output$Filteredpyro <- DT::renderDataTable({
    subset_table <- pyr6[input$pyr_rows_all, input$pyr_columns_selected, drop = F]
    datatable(subset_table,rownames = FALSE)
  })
  
  #================== 
  
  output$pyrod <- downloadHandler(
    filename = "FilteredPyro.csv",
    content = function( file){
      write.csv(pyr6[input$pyr_rows_all, input$pyr_columns_selected, drop = F],
                file)
    }
  )
  #------sdf
  output$pyrosdf <- downloadHandler(
    filename = "FilteredPyro.sdf",
    content = function( file){
      write.SDF(smiles2sdf(pyr6[input$pyr_rows_all, input$pyr_columns_selected]),
                file)
    }
  )  
  
  #-----PLOT-------
  output$mhist <- renderPlot({
    col <- as.numeric(input$var)
    hist(pyr6[,col], breaks =seq(-4, max(pyr6[,col]),l=input$bin+1),col="brown",
         main="Histogram of Pyrozole", ylim = c (0,150),  xlab=names(pyr6[col]))
  })
  
  #----------------
  output$Pyro_filtered <- 
    downloadHandler(
      filename = "Pyro.csv",
      content = function(file){
        write.csv(pyr6[input[["pyr_rows_all"]], ],
                  file)
      }
    )
  
  
  output$pyroSDF <- 
    downloadHandler(
      filename = "PyroFull.sdf",
      content = function(file){
        write.SDF(pyr,file)
        
      }
    )
  
})
