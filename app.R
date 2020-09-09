# This shiny webapp is designed to search for terms and return a list of all GO terms which match, along with GO IDs
# Richard Browne, 2020

##---- LOAD PACKAGES ----
library(shinydashboard)
library(shiny)
library(DT)
library(data.table)
library(tidyverse)
library(plotrix)
library(markdown)

##---- UI ----
ui <- navbarPage('GO Search',
                 ##PLOT TAB----                 
                 tabPanel('Searcher',
                          sidebarPanel(
                            h3("Info"),
                            tags$li('Enter one or more terms or partial terms (eg: heat) below and all GO terms containing that phrase will be shown.'),
                                     tags$li('Entering lines that are empty or only contain spaces/other special characters may break the tool'),
                                     tags$li('You can choose which columns to show and can download the full table.'),
                                     
                            actionButton("show", "More Info"),
                            br(),
                            br(),
                            checkboxInput("WHOLEWORDS", label = "Search whole words only", value = TRUE),
                            helpText("Ignores partial words. De-select for broader search terms or where you may be using partial terms (i.e: 'carboxyl-')."),
                            fluidRow(
                              column(6, h4("Input GO terms"),
                                        tags$textarea(id = 'input_text',placeholder = 'Type here', rows = 8, "heat")),
                              column(6, checkboxInput("IDCHECK", label = "Include GO IDs", value = TRUE),
                                        checkboxInput("TYPECHECK", label = "Include GO Type", value = TRUE),
                                        radioButtons("summhead", "Show Table",
                                           choices = c(Head = "head",
                                                       All = "all"),
                                           selected = "head",
                                           inline = TRUE),
                                        downloadButton("downloadMult", "Download Table"))),
                            br(),br(),br(),br(),br()),
                          mainPanel(
                            h3('Matching GO Terms'),
                            tableOutput("table"),
                          br(),br(),br(),br()
                                    )
                          ),
                 tags$footer("Created by Richard Browne, 2020. Last updated 10/09/2020.", align = "right", style = "
  position:fixed;
  min-height: 5vh;
  bottom:0;
  width:100%;
  height:40px;
  color: grey;
  padding: 10px;
  background-color: black;
  z-index: 1000;")
)

##---- SERVER ----
server <- function(input, output, session) {
  
  ## Instructions and citation modals ----
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "About this tool",
      tags$li("This tool utilises a list of Gene Ontology terms collected from the Gene Ontology Consortium. The list was last updated here on 10 September, 2020"), br(),
      tags$li("For more information on the GO Consortium or gene ontology terms visit www.geneontology.org. This webapp is unaffiliated with the GO Consortium."), br(),
      tags$li("The code for this webapp as well as the data file utilised is housed at https://github.com/richardbrowne/GOSearch"), br(),
      easyClose = TRUE
    ))
  })
  
  
## Create table of matches to inputs----
  output$table <- renderTable({
    if(input$IDCHECK == TRUE && input$TYPECHECK == TRUE) {
      table <- read.delim("./GO_list.txt", header=FALSE) %>% select(1,2,3)
      table <- tibble::as_tibble(table)
      colnames(table) <- c('GO ID', 'Term', 'Type')
    } else {
      if(input$IDCHECK == FALSE && input$TYPECHECK == TRUE) {
        table <- read.delim("./GO_list.txt", header=FALSE) %>% select(2,3)
        table <- tibble::as_tibble(table)
        colnames(table) <- c('Term', 'Type')
      } else {
        if(input$IDCHECK == TRUE && input$TYPECHECK == FALSE) {
          table <- read.delim("./GO_list.txt", header=FALSE) %>% select(1,2)
          table <- tibble::as_tibble(table)
          colnames(table) <- c('GO ID', 'Term')
        } else {
          table <- read.delim("./GO_list.txt", header=FALSE) %>% select(2)
          table <- tibble::as_tibble(table)
          colnames(table) <- c('Term')
        }
      }
    }
  #list of terms to search
    list <- as.data.frame(as.list(strsplit(input$input_text, '\n')))
  #create empty dataframe
    res <- data.frame()
  #search GO list with each item in list, then push to above df
    for (i in list[,]) {
      if(input$WHOLEWORDS == TRUE) {
      out <- table %>% filter(grepl(paste('\\<',i,'\\>', sep='', collapse=''),Term, ignore.case=TRUE))
      res <- rbind(res,out)
      remove(out)
      } else {
        out <- table %>% filter(grepl(i,Term))
        res <- rbind(res,out)
        remove(out)
      }
    }
  #Remove duplicates
    res <- res[!duplicated(res), ]
  #Aims to remove cases where special characters cause whole GO list to print  
    if(input$input_text == " ") {
      return(head(res, 0))
    } else {
      if(input$input_text == "") {
        return(head(res,0))
      } else {
        if(input$input_text == ".") {
          return(head(res,0))
        } else {
          if(input$input_text == "$") {
            return(head(res,0))
          } else {
            if(input$input_text == "^") {
              return(head(res,0))
            } else {
              if(input$input_text == "*") {
                return(head(res,0))
              } else {
  #Allows to only show top 10 results            
                if(input$summhead == "head") {
                  return(head(res, 10))
                } else {  
                  return(res)
                }
              }
            }
          }
        }
      }
    }
  })  

 
  #Load table for download ----
  downMult <- reactive({
  if(input$IDCHECK == TRUE && input$TYPECHECK == TRUE) {
    table <- read.delim("./GO_list.txt", header=FALSE) %>% select(1,2,3)
    table <- tibble::as_tibble(table)
    colnames(table) <- c('GO ID', 'Term', 'Type')
  } else {
    if(input$IDCHECK == FALSE && input$TYPECHECK == TRUE) {
    table <- read.delim("./GO_list.txt", header=FALSE) %>% select(2,3)
    table <- tibble::as_tibble(table)
    colnames(table) <- c('Term', 'Type')
    } else {
      if(input$IDCHECK == TRUE && input$TYPECHECK == FALSE) {
          table <- read.delim("./GO_list.txt", header=FALSE) %>% select(1,2)
          table <- tibble::as_tibble(table)
          colnames(table) <- c('GO ID', 'Term')
      } else {
          table <- read.delim("./GO_list.txt", header=FALSE) %>% select(2)
          table <- tibble::as_tibble(table)
          colnames(table) <- c('Term')
      }
    }
  }

  #list of terms to search
  list <- as.data.frame(as.list(strsplit(input$input_text, '\n')))
  ## create empty dataframe
  res <- data.frame()
  ## search df with each item in list, then push to file
  for (i in list[,]) {
    if(input$WHOLEWORDS == TRUE) {
      out <- table %>% filter(grepl(paste('\\<',i,'\\>', sep='', collapse=''),Term, ignore.case=TRUE))
      res <- rbind(res,out)
      remove(out)
    } else {
      out <- table %>% filter(grepl(i,Term))
      res <- rbind(res,out)
      remove(out)
    }
  }
  ## remove duplicates
  res <- res[!duplicated(res), ]
  
  if(input$input_text == " ") {
    return(head(res, 0))
  } else {
    if(input$input_text == "") {
      return(head(res,0))
    } else {
      if(input$input_text == ".") {
        return(head(res,0))
      } else {
        if(input$input_text == "$") {
          return(head(res,0))
        } else {
          if(input$input_text == "^") {
            return(head(res,0))
          } else {
            if(input$input_text == "*") {
              return(head(res,0))
            } else {
                return(res)
              }
            }
          }
        }
      }
    }
  })
  
  ##Download Button ----
  output$downloadMult <- downloadHandler(
    filename = function(){
      paste('GOTerms.csv')
    },
    content = function(file) {
      write.csv(downMult(), file, row.names = FALSE)
    }
  )
}

##---- RUN APP ---- 
shinyApp(ui, server)
