library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(cookies)
library(fontawesome)
library(shinyWidgets)
library(shinyjs)

library(htmltools)



function(input, output, session) {
  
  observeEvent(input$button_yes, {
    
   
   
    
      
      
      output$frage1_1 <- renderUI({
        
        
        list(
        tags$p(class="lead",  "Die Wahlen finden in den Kantonen statt. In welchem Kanton leben Sie und sind Sie somit wahlberechtigt?")
      ,
    
      tags$div(class="knt_auswahl",
      
        selectizeInput(inputId = "knt_auswahl", label = "Kanton", selected = "placeholder", choices =  c("SG", "AG", "LU"),
                     options = list(
                       placeholder = "Wählen Sie einen Kanton",
                       onInitialize = I('function() { this.setValue(""); this.$control.addClass("knt_auswahl"); }')
                     ))
      )
        )
      })
      
  })
  
  observeEvent(input$button_yes, {
    shinyjs::disable("button_no")
  })

  
  
    
  observeEvent(input$knt_auswahl, {
    if (input$knt_auswahl == 'AG') {
      output$linkOutput <- renderUI({
        tags$h4(
          class="link",
          tags$a(href="https://www.tagblatt.ch", "Link auf die jeweiligen Kandidierenden")
        )
      })
    } else {
      output$linkOutput <- renderUI(NULL)
    }
  })



      
  
    
    

    

    
  
  
  
  #   

    
    
  
    
    
  
  observeEvent(input$button_no, {

    output$frage1_1 <- renderUI({
      
      
      tags$h4(
      class="link", 
      
      "Natürlich wissen Sie das bereits. Gut gemacht."
      )
      
      
      
      
      
    })
    
    
    })
    
    
    
  observeEvent(input$button_yes2, {
    
    output$testlink2 <- renderUI({
      
      tags$h4(
        class="link", 
        
        tags$a(href="https://www.tagblatt.ch", "Hier finden einen Erklärartikel zu Majorz und Proporz.")
      )
      
    })
  })
  
  observeEvent(input$button_no2, {
    
    output$testlink2 <- renderUI({
      
      
      tags$h4(
        class="link", 
        
        "Sie wissen natürlich bereits, was Majorz und was Proporz ist."
      )
      
      
    })
    
    
    
  })
  
  
  
  
  
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
