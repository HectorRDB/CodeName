# options(repos = BiocManager::repositories())
library(ggplot2)
library(RColorBrewer)
library(DT)
library(shinyjs)
library(tidyr)
library(stringr)
library(dplyr)
library(here)
names <- read.csv("names.csv", header = FALSE, stringsAsFactors = FALSE)
names <- names$V1 %>% unique()
names <- names[!str_detect(names, " ")]
names <- str_remove_all(names, "L'")
ui <- navbarPage(title = "Code Name", id = "Dash",
                 # First Panel for access
                 tabPanel("Nouvelle partie", value = "Seed", 
                          useShinyjs(),
                          div(id = "title", h1("Entrez le code de la nouvelle partie"),
                              align = "center"),
                          hidden(div(id = "Chargement", h1("Chargement..."),
                                     align = "center")),
                          fluidRow(column(2, offset = 5,
                                          textInput("code", "", value = "NA"))),
                          fluidRow(column(2, offset = 5,
                                          fluidRow(column(6, offset = 3,
                              hidden(actionButton("Submit", label = "C'est parti"))))
                                   )
                          )
                 ),
                 
                 # Second Panel 
                 tabPanel("Plateau de Jeu", value = "Plateau", hidden = TRUE,
                  # Mouse Panel 
                   fluidRow(
                     column(3, br(), DTOutput("List")),
                     column(9, align = "center",
                            checkboxInput("Reveal", label = "Maitre du Jeu",
                                          value = FALSE),
                            br(), 
                            plotOutput("plateau", dblclick = "plot_dblclick"),
                            )
                   )
                )
)

server <- function(input, output, session) {
  # First panel ----
  observeEvent(input$code, {
    if (input$code != "NA") {
      shinyjs::show("Submit")
    } else{
      hideTab(session, inputId = "Dash", target = "Plateau")
    }
  })
  
  observeEvent(input$Submit,{
    hide(id = "title")
    hide(id = "code")
    hide(id = "Submit")
    shinyjs::show(id = "Chargement")
    Sys.sleep(2)
    hideTab(session, inputId = "Dash", target = "Seed")
    showTab(session, inputId = "Dash", target = "Plateau", select = T)
  })
  
  # Second Panel ----
  
  output$List <- renderDT({
    set.seed(as.numeric(input$code))
    ordre <- sample(0:24, 25)
    colors <- data.frame(place = ordre,
                         color = c(rep("Bleu", 9),
                                   rep("Rouge", 8),
                                   "Noir",
                                   rep("Gris", 7)),
                         stringsAsFactors = FALSE)
    set.seed(as.numeric(input$code))
    Noms <- sample(names, 25)
    Noms <- data.frame("Noms" = Noms, stringsAsFactors = FALSE) %>%
      mutate(place = 0:24) %>%
      mutate(x = place %% 5,
             y = (place - x) / 5) %>%
      full_join(colors) %>%
      mutate(displayed_colors = "Gris")
    Noms <- Noms %>% select(Noms)
    df <- datatable(Noms,
                    rownames = F,
                    options = list(paging = T, pageLength = 25,
                                   columnDefs = list(list(className = 'dt-center',
                                                          targets = 0))
                    ))
  }, server = T)
  
  List <- dataTableProxy('List')
  
  output$plateau <- renderPlot({
    set.seed(as.numeric(input$code))
    ordre <- sample(0:24, 25)
    colors <- data.frame(place = ordre,
                         color = c(rep("Bleu", 9),
                                   rep("Rouge", 8),
                                   "Noir",
                                   rep("Gris", 7)),
                         stringsAsFactors = FALSE)
    set.seed(as.numeric(input$code))
    Noms <- sample(names, 25)
    Noms <- data.frame("Noms" = Noms, stringsAsFactors = FALSE) %>%
      mutate(place = 0:24) %>%
      mutate(x = place %% 5,
             y = (place - x) / 5) %>%
      full_join(colors) %>%
      mutate(displayed_colors = "transparent")
    colors <- input$List_rows_selected
    Noms$displayed_colors[colors] <- Noms$color[colors]
    p <- ggplot(Noms) +
      theme_void()
    if (input$Reveal) {
      p <- p +
        geom_tile(aes(x = x, y = y, fill = color)
                  , height = .9, width = .9, col = "black",
                  alpha = .7, size = 2) +
        scale_fill_manual(values = c("Rouge" = "#E41A1C",
                                     "Bleu" = "#377EB8",
                                     "Noir" = "#525252",
                                     "Gris" = "#D9D9D9")) +
        guides(fill = FALSE)
    } else {
      p <-  p +
        geom_tile(aes(x = x, y = y, fill = displayed_colors),
                  height = .9, width = .9, col = "black", size = 2,
                  alpha = .7) +
        scale_fill_manual(values = c("Rouge" = "#E41A1C",
                                     "Bleu" = "#377EB8",
                                     "Noir" = "#525252",
                                     "Gris" = "#D9D9D9",
                                     "transparent" = "transparent")) +
        guides(fill = FALSE)
    }
    p <- p +
      geom_text(aes(x = x, y = y, label = Noms), size = 7) 
    return(p)
  })
}

shinyApp(ui = ui, server = server)