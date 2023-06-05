library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(cookies)
library(fontawesome)
library(shinyWidgets)

library(htmltools)



function(input, output, session) {
  
  observeEvent(input$button_yes, {
    
    output$testlink <- renderUI({
      
      tags$h4(
        class="link", 
        
        tags$a(href="https://www.tagblatt.ch", "Hier kommt eine wunderschöne Linkbox zum Erklärartikel vom Inlandressort")
        )
      
    })
  })
  
  observeEvent(input$button_no, {

    output$testlink <- renderUI({
      
      
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
