ui <- function(request) {
  fluidPage(
    textInput("txt", "Enter text"),
    checkboxInput("caps", "Capitalize"),
    verbatimTextOutput("out"),

    actionButton("Load", "Load"),
    selectInput("Select_settings", "Select which settings to use.", c("default_bookmark")),
    actionButton("Go", "Go"),
    textInput("book_name", "Enter the name of your bookmark"),
    bookmarkButton()
  )
}
server <- function(input, output, session) {
  output$out <- renderText({
    if (input$caps)
      toupper(input$txt)
    else
      input$txt
  })

  observeEvent(input$Load, {
    list <- list.files(here::here("Enhancement_Work", "Sample Shiny", "shiny_bookmark_names"))
    #list <- list.files(here::here("inst", "shiny", "shiny_bookmark_names"))
    list <- stringr::str_replace(list, pattern = ".rds", replacement = "")
    list <- unlist(list)


    updateSelectInput(session, "Select_settings",
                      choices = list)
  })

  observeEvent(input$Go, {
    choice <- paste(input$Select_settings, "rds", sep = ".")

    choice_list <- readRDS(file = here::here("Enhancement_Work", "Sample Shiny", "shiny_bookmark_names", choice))
    url <- choice_list[['url']]
    updateQueryString(url, mode = "push")

  })

  onBookmarked(function(url) {
    name <- paste(input$book_name, Sys.Date(), "bookmark.rds", sep = "_")
    x <- list("name" = input$book_name, "url" = url)

    saveRDS(x, file = here::here("Enhancement_Work", "Sample Shiny", "shiny_bookmark_names", name))
    # saveRDS(x, file = here::here("inst", "shiny", "shiny_bookmark_names", name))

    showNotification("The current state of QualtricsTools has been saved!", type = "message", duration = 7)

    # store <- getShinyOption("bookmarkStore", default = "")
    #
    # subtitle <- "The current state of QualtricsTools has been saved."
    #
    # showModal(urlModal(url, subtitle = subtitle))
  })
}

shinyApp(ui, server, enableBookmarking = "server")


#file <- readRDS(file = here::here("inst", "shiny", "shiny_bookmark_names", name))

# list <- list.files(here::here("inst", "shiny", "shiny_bookmark_names"))
#
#   list <- list.files(here::here("Enhancement_Work", "Sample Shiny", "shiny_bookmark_names"))
#   list <- stringr::str_replace(list, pattern = ".rds", replacement = "")
#   list <- rlist::list.clean(list)
#   list <- unlist(list)
#  list
#
#    right <- c("husdf", "sagd")
# #
# #  typeof(list)
# #  typeof(right)
#  right
#
# length(list)
# list[2]



