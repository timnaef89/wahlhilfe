# Load all necessary packages

library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(cookies)
library(fontawesome)
library(shinyWidgets)
library(shinyjs)


# server function
function(input, output, session) {
  
  
  
  # catch click on first yes-button
  observeEvent(input$button_yes, {
    
   
   
    
      # if first yes-button is clicked --> render follow-up question and cantonsquery
      
      output$frage1_1 <- renderUI({
        
        
        # use list for multiple elements
        list(
          
          #follow-up question 1 (as paragraph)
        tags$p(class="lead",  "Die Wahlen finden in den Kantonen statt. In welchem Kanton leben Sie und sind Sie somit wahlberechtigt?")
      ,
    
        # canton selection with seletizeInput wrapped in a div for costumacation 
      tags$div(class="knt_auswahl",
      
        selectizeInput(inputId = "knt_auswahl", label = "Kanton", selected = "placeholder", choices =  c("SG", "AG", "LU"),
                     options = list(
                       placeholder = "Wählen Sie einen Kanton",
                       
                       # add preview-text
                       onInitialize = I('function() { this.setValue(""); this.$control.addClass("knt_auswahl"); }')
                     ))
      )
        )
      })
      
  })
  
  
    #if first yes-button is clicked --> disable first no-button with shinyjs (Javascript)
  observeEvent(input$button_yes, {
    shinyjs::disable("button_no")
  })

  
  
  
    # when the canton is selected, point the link to the correct article
  observeEvent(input$knt_auswahl, {
    if (input$knt_auswahl %in% c("SG", "AG", "LU")) {
      output$linkOutput <- renderUI({
        tags$h4(
          class="link",
          tags$a(href="https://www.tagblatt.ch", paste0("Hier finden Sie eine Übersicht aller Kandidatinnen und Kandidaten für den Nationalrat im Kanton ", input$knt_auswahl))
        )
      })
    } else {
      
      # if no canton is selected, don't show anything
      output$linkOutput <- renderUI(NULL)
    }
  })


  # if no-button is clicked, show text
  observeEvent(input$button_no, {

    output$frage1_1 <- renderUI({
      
      
      tags$h4(
      class="link", 
      
      "Natürlich wissen Sie das bereits. Gut gemacht."
      )
      
      
      
      
      
    })
    
    
    })
    
    ##########################################################################
  #   
  # observeEvent(input$button_yes2, {
  #   
  #   
  #   
  #   
  #   output$linkOutput2 <- renderUI({
  #       tags$h4(
  #         class="link",
  #         tags$a(href="https://www.tagblatt.ch", paste0("Wir zeigen Ihnen hier, was die Amtsinhaber erreicht haben und wer die Herausforderer sind"))
  #       )
  #     })
  #   
  #   
  # })
  
  # 1 = ja inkl. knt auswahl
  
  observeEvent(input$button_yes2, {
    
   
    
    
    output$linkOutput3 <- renderUI({
      tags$h4(
        class="link",
        tags$a(href="https://www.tagblatt.ch", paste0("Wir zeigen Ihnen hier, was die Amtsinhaber erreicht haben und wer die Herausforderer sind", "OUTPUT3"))
      )
    })
   
    
  })
  
  
  
  observeEvent(input$button_yes2,{
    
    
    if(is.null(input$knt_auswahl) || !input$knt_auswahl %in% c("SG", "AG")){
      
      
      output$frage2_1 <- renderUI({
        
        
        # use list for multiple elements
        list(
          
          #follow-up question 1 (as paragraph)
          tags$p(class="lead",  "Die Wahlen finden in den Kantonen statt. In welchem Kanton leben Sie und sind Sie somit wahlberechtigt?")
          ,
          
          # canton selection with seletizeInput wrapped in a div for costumacation 
          tags$div(class="knt_auswahl",
                   
                   selectizeInput(inputId = "knt_auswahl2", label = "Kanton", selected = "placeholder", choices =  c("SG", "AG", "LU"),
                                  options = list(
                                    placeholder = "Wählen Sie einen Kanton",
                                    
                                    # add preview-text
                                    onInitialize = I('function() { this.setValue(""); this.$control.addClass("knt_auswahl"); }')
                                  ))
          )
        )
      })
      
      
      
    }
    
  })
      
      
  #if second yes-button is clicked --> disable second no-button with shinyjs (Javascript)
  observeEvent(input$button_yes2, {
    shinyjs::disable("button_no2")
  })
  
  
  
  observeEvent(input$knt_auswahl2, {
    if (input$knt_auswahl2 %in% c("SG", "AG", "LU")) {
      output$linkOutput2 <- renderUI({
        tags$h4(
          class="link",
          tags$a(href="https://www.tagblatt.ch", paste0("Wir zeigen Ihnen hier, was die Amtsinhaber erreicht haben und wer die Herausforderer sind", "OUTPUT2"))
        )
      })
    } else {

      # if no canton is selected, don't show anything
      output$linkOutput2 <- renderUI(NULL)
    }
  })
  


  
  
  
  output$test <- renderPrint({
    
    if(is.null(input$knt_auswahl) || !input$knt_auswahl %in% c("SG", "AG")){
      
      print("nichts ausgewählt")
      
      
    }else{
      
      print("jetzt schon")
      
      
    }
    
  })
  
  
  
  
################################################################################ 
  
                              #  test for mobile  #
  
################################################################################  
  
  output$test2 <- renderUI(
    {
      
      if(isFALSE(input$isMobile)){
        
        tags$h4(class="link", "Hier kommt eine wunderschöne Linkbox zum Erklärartikel vom Inlandressort")
        
      }else{
        
        tags$h4(class="link", "Natürlich wissen Sie das bereits. Gut gemacht.")
        
      }
      
    }
  )
  
  

  
}
