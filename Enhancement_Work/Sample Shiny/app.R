ui <- function(request) {
  fluidPage(
    textInput("txt", "Enter text"),
    checkboxInput("caps", "Capitalize"),
    verbatimTextOutput("out"),

    actionButton("Load", "Load"),
    selectInput("Select_settings", "Select which settings to use.", c("default_bookmark")),
    # actionButton("Go", "Go"),
    radioButtons("load", " ", c("Do Not Load", "Load")),
    textInput("file_name", "Enter the name of your file"),
    shinyDirButton('folder', 'Folder select', 'Please select a folder', FALSE),
    verbatimTextOutput("text"),
    actionButton("Save", "Save")

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
    path <- parseDirPath(roots = roots, input$folder)
    list <- list.files(paste(path, "shiny_bookmarks", sep = "//"))
    list <- stringr::str_replace(list, pattern = ".rds", replacement = "")
    list <- unlist(list)


    updateSelectInput(session, "Select_settings",
                      choices = list)
  })

  roots = c(wd='C:\\')
  shinyDirChoose(input, 'folder',  roots = roots, filetypes=c('', 'txt'))


  output$text <- renderPrint({
    parseDirPath(roots = roots, input$folder)
    #input$load
  })

  my_save_interface <- function(id, callback) {
    path <- parseDirPath(roots = roots, input$folder)
    stateDir <- file.path(path, "shiny_bookmarks", id)
    if (!shiny:::dirExists(stateDir))
      dir.create(stateDir, recursive = TRUE)
    callback(stateDir)
  }

  shinyOptions(save.interface = my_save_interface)


  observeEvent(input$Go, {
    choice <- paste(input$Select_settings, "rds", sep = ".")

    path <- parseDirPath(roots = roots, input$folder)
    choice_list <- readRDS(file = paste(path, "shiny_bookmarks", choice, sep = "/"))
    text <- choice_list[['text']]

    # loadInterface <- loadInterfaceLocal
    # loadInterface(id, loadFun)

    # attr(input, "Readonly") <- FALSE
    # #input$txt <- text
    # print(input)

    output$out <- renderText({
      text
    })

    # updateTextInput(session, "txt", value = text)

  })


  observeEvent(input$Save, {
    name <- paste(input$file_name, Sys.Date(), "bookmark.rds", sep = "_")
    path <- parseDirPath(roots = roots, input$folder)

    text <- input$txt

    x <- list("name" = input$file_name, 'path' = path, "text" = text)

    saveRDS(x, file = paste(path, "shiny_bookmarks", name, sep = "//"))


    # stateDir <- file.path(appDir, "shiny_bookmarks", id)
    # if (!dirExists(stateDir))
    #   dir.create(stateDir, recursive = TRUE)


    showNotification("The current state of QualtricsTools has been saved!", type = "message", duration = 7)
  })

}

shinyApp(ui, server, enableBookmarking = "server")

#
#
# hey <- readRDS(file = paste("C:/Users/Matth/Desktop/testing shiny", "shiny_bookmarks", "Dummy_Enhancement_Survey_2020-04-23_bookmark.rds", sep = "/"))
# hey
#
# text <- hey[['text']]
#
# as.character(text)
