# Load all necessary packages

library(shiny)
library(tidyverse)
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
        renderTeaser(
          teaserimage = "https://img.chmedia.ch/2022/04/24/18046014-3ea0-4f83-b41e-132fecfa140e.jpeg?width=1360&height=906&fit=bounds&quality=75&auto=webp&crop=1024,683,x0,y0",
          spitzmarke = "SPITZMARKE",
          title = "Jeder Fünfte leidet an Heuschnupfen – mit diesen Tipps kommen Sie besser durch die Pollensaison",
          link = "https://www.tagblatt.ch/ostschweiz/ressort-ostschweiz/heuschnupfen-allergologe-apotheken-ostschweiz-ld.2472228",
          place = "Vorname, Nachname",
          time = "",
          level = ""
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
  
  
  
# folgende codezeilen auskommentieren, um dropdown auch nach der auswahl anzuzeigen  
  
  observeEvent(input$knt_auswahl, {
    if (input$knt_auswahl %in% c("SG", "AG", "LU")) {
      shinyjs::hide("knt_auswahl")
    }
  })
  
  
  observeEvent(input$knt_auswahl2, {
    if (input$knt_auswahl2 %in% c("SG", "AG", "LU")) {
      shinyjs::hide("knt_auswahl2")
    }
  })
  
  #
  
  ##############################################################################
   # Question 2
  ##############################################################################
  
  
  # linkoutput for q2 if knt is already selected in q1
  observeEvent(input$button_yes2, {
    
    
    
    
    output$linkOutput3 <- renderUI({
      renderTeaser(
        teaserimage = "https://img.chmedia.ch/2022/04/24/18046014-3ea0-4f83-b41e-132fecfa140e.jpeg?width=1360&height=906&fit=bounds&quality=75&auto=webp&crop=1024,683,x0,y0",
        spitzmarke = "SPITZMARKE",
        title = "Jeder Fünfte leidet an Heuschnupfen – mit diesen Tipps kommen Sie besser durch die Pollensaison",
        link = "https://www.tagblatt.ch/ostschweiz/ressort-ostschweiz/heuschnupfen-allergologe-apotheken-ostschweiz-ld.2472228",
        place = "Vorname, Nachname",
        time = "",
        level = ""
      )
    })
    
    
  })
  
  
  # if no knt is selected in q1, follow-up-q and knt-choices are displayed
  observeEvent(input$button_yes2,{
    
    
    if(is.null(input$knt_auswahl) || !input$knt_auswahl %in% c("SG", "AG", "LU")){
      
      
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
  
  
  # display second link if knt-choice in q2 is selected
  observeEvent(input$knt_auswahl2, {
    if (input$knt_auswahl2 %in% c("SG", "AG", "LU")) {
      output$linkOutput2 <- renderUI({
        renderTeaser(
          teaserimage = "https://img.chmedia.ch/2022/04/24/18046014-3ea0-4f83-b41e-132fecfa140e.jpeg?width=1360&height=906&fit=bounds&quality=75&auto=webp&crop=1024,683,x0,y0",
          spitzmarke = "SPITZMARKE",
          title = "Jeder Fünfte leidet an Heuschnupfen – mit diesen Tipps kommen Sie besser durch die Pollensaison",
          link = "https://www.tagblatt.ch/ostschweiz/ressort-ostschweiz/heuschnupfen-allergologe-apotheken-ostschweiz-ld.2472228",
          place = "Vorname, Nachname",
          time = "",
          level = ""
        )
      })
    } else {
      
      # if no canton is selected, don't show anything
      output$linkOutput2 <- renderUI(NULL)
    }
  })
  
  

  
  # if no-button is clicked, show text
  observeEvent(input$button_no2, {
    
    output$frage2_1 <- renderUI({
      
      
      tags$h4(
        class="link", 
        
        "Natürlich wissen Sie das bereits. Gut gemacht."
      )
      
      
      
      
      
    })
  
    
  })
  
  
  ##############################################################################
  # Question 3
  ##############################################################################
  
  

    # catch click on first yes-button
    observeEvent(input$button_yes3, {






# if first yes-button is clicked --> render follow-up question and cantonsquery

output$frage3_1 <- renderUI({

  # use list for multiple elements
  list(

    #follow-up question 3 (as paragraph)
    tags$p(class="lead",  "Ukraine-Krieg, CS-Debakel oder doch Krankenkassen und Inflation - was beschäftigt uns im Wahlkampf am meisten."),

    # topic selection with seletizeInput wrapped in a div for costumacation
    tags$div(class="knt_auswahl",

             selectizeInput(inputId = "topic_choice",
                            label = "Kanton",
                            selected = "placeholder",
                            choices =  c("Ukraine-Krieg", "CS-Debakel", "Krankenkassen", "Inflation"),
                            options = list(
                              placeholder = "Wählen Sie ein Thema",

                              # add preview-text
                              onInitialize = I('function() { this.setValue(""); this.$control.addClass("knt_auswahl"); }')
                            ))
    )
  )
})


    })




  # display second link if knt-choice in q2 is selected
  observeEvent(input$topic_choice, {
    if (input$topic_choice %in% c("Ukraine-Krieg", "CS-Debakel", "Krankenkassen", "Inflation")) {
      output$linkOutput4 <- renderUI({
        renderTeaser(
          teaserimage = "https://img.chmedia.ch/2022/04/24/18046014-3ea0-4f83-b41e-132fecfa140e.jpeg?width=1360&height=906&fit=bounds&quality=75&auto=webp&crop=1024,683,x0,y0",
          spitzmarke = "SPITZMARKE",
          title = "Jeder Fünfte leidet an Heuschnupfen – mit diesen Tipps kommen Sie besser durch die Pollensaison",
          link = "https://www.tagblatt.ch/ostschweiz/ressort-ostschweiz/heuschnupfen-allergologe-apotheken-ostschweiz-ld.2472228",
          place = "Vorname, Nachname",
          time = "",
          level = ""
        )
      })
    } else {
      
      # if no canton is selected, don't show anything
      output$linkOutput4 <- renderUI(NULL)
    }
  })
  
  
  
  # if no-button is clicked, show text
  observeEvent(input$button_no3, {
    
    output$frage3_1 <- renderUI({
      
      
      tags$h4(
        class="link", 
        
        "Natürlich wissen Sie das bereits. Gut gemacht."
      )
      
      
      
      
      
    })
    
    
  })
  
  observeEvent(input$button_yes3, {
    shinyjs::disable("button_no3")
  })
  
  
  
  ##############################################################################
  # Question 4
  ##############################################################################
  
  
  
  
  
  
  # display second link if knt-choice in q2 is selected
  observeEvent(input$button_yes4, {
      output$linkOutput5 <- renderUI({
        renderTeaser(
          teaserimage = "https://img.chmedia.ch/2022/04/24/18046014-3ea0-4f83-b41e-132fecfa140e.jpeg?width=1360&height=906&fit=bounds&quality=75&auto=webp&crop=1024,683,x0,y0",
          spitzmarke = "SPITZMARKE",
          title = "Jeder Fünfte leidet an Heuschnupfen – mit diesen Tipps kommen Sie besser durch die Pollensaison",
          link = "https://www.tagblatt.ch/ostschweiz/ressort-ostschweiz/heuschnupfen-allergologe-apotheken-ostschweiz-ld.2472228",
          place = "Vorname, Nachname",
          time = "",
          level = ""
        )
      })
  })
  
  
  
  # if no-button is clicked, show text
  observeEvent(input$button_no4, {
    
    output$linkOutput5 <- renderUI({
      
      
      tags$h4(
        class="link", 
        
        "Natürlich wissen Sie das bereits. Gut gemacht."
      )
      
      
      
      
      
    })
    
    
  })
  
  observeEvent(input$button_yes4, {
    shinyjs::disable("button_no4")
  })
  
  
  
  ##############################################################################
  # Question simple yes/no questions
  ##############################################################################
  generateShinyServer(input, output, session,
                      yes = "button_yes9",
                      no = "button_no9",
                      outputlink = "linkOutput10",
                      link = "www.vi.nl",
                      linktext = "Dafür haben wir einen Ratgeber-Artikel für Sie",
                      nein_text = "natürlich.")
  
  
  generateShinyServer(input, output, session,
                      yes = "button_yes10",
                      no = "button_no10",
                      outputlink = "linkOutput11",
                      link = "www.vi.nl",
                      linktext = "Wir bringen hier Licht ins Dickicht dieser Tools und Hilfsmittel",
                      nein_text = "natürlich.") 
  
  generateShinyServer(input, output, session,
                      yes = "button_yes11",
                      no = "button_no11",
                      outputlink = "linkOutput12",
                      link = "www.vi.nl",
                      linktext = "Das sagen Umfragen und Prognosen zum Ausgang der Wahlen, mit diesen Gewinnen rechnen die Parteien",
                      nein_text = "natürlich.")  
  
  

  generateShinyServer(input, output, session,
                      yes = "button_yes12",
                      no = "button_no12",
                      outputlink = "linkOutput13",
                      link = "www.vi.nl",
                      linktext = "Hier finden Sie ihren Guide für den grossen Tag",
                      nein_text = "natürlich.")  
  
    

  
  ##############################################################################
  # Question DROPDOWN yes/no questions
  ##############################################################################
  
  
  generateShinyServer_dropdown(input, output, session,
                               yes = paste0("button_yes", 13),
                               no = paste0("button_no", 13),
                               outputlink = "linkOutput14",
                               link = "www.bfs.ch",
                               linktext = "Das steht für diese Parteien auf dem Spiel und das sagen die Parteipräsidenten.",
                               nein_text = "Sie wissen bereits alles über die Parteien.",
                               number = 13,
                               choices = c("SP", "SVP", "Mitte", "FDP", "Grüne"))
  
  
  
      ##############################################################################
    # DEBUGGING STUFF #
    ##############################################################################
    
    
    output$test <- renderPrint({
      
      if(is.null(input$knt_auswahl) || !input$knt_auswahl %in% c("SG", "AG")){
        
        print("nichts ausgewählt")
        
        
      }else{
        
        print("jetzt schon")
        
        
      }
      
    })
  
  

    output$testtest <- renderPrint({
      print("asdf")
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