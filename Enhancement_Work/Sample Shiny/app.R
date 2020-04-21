ui <- function(request) {
  fluidPage(
    textInput("txt", "Enter text"),
    checkboxInput("caps", "Capitalize"),
    verbatimTextOutput("out"),

    actionButton("Load", "Load"),
    selectInput("Select_settings", "Select which settings to use.", c("default_bookmark")),
    actionButton("Go", "Go"),
    textInput("book_name", "Enter the name of your bookmark"),
    shinyDirButton('folder', 'Folder select', 'Please select a folder', FALSE),
    verbatimTextOutput("text"),
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
    path <- parseDirPath(roots = roots, input$folder)
    list <- list.files(paste(path, "shiny_bookmarks", "shiny_bookmark_names", sep = "//"))
    #list <- list.files(here::here("inst", "shiny", "shiny_bookmark_names"))
    list <- stringr::str_replace(list, pattern = ".rds", replacement = "")
    list <- unlist(list)


    updateSelectInput(session, "Select_settings",
                      choices = list)
  })

  roots = c(wd='C:\\')
  shinyDirChoose(input, 'folder',  roots = roots, filetypes=c('', 'txt'))


  output$text <- renderPrint({
    parseDirPath(roots = roots, input$folder)
  })

  # reactive({
  #   filepath <- input$folder
  #   saveRDS(filepath, file = "C:/Users/Matth/Desktop/shiny_bookmarks/filepath.rds")
  #   strfilepath <- str(input$folder)
  #   saveRDS(strfilepath, file = "C:/Users/Matth/Desktop/shiny_bookmarks/strfilepath.rds")
  #
  # })
  # Changin where bookmarks are saved Sys.getenv("USERPROFILE"),"Desktop",fsep="/"

  my_save_interface <- function(id, callback) {
    # path <- switch(
    #   Sys.info()[['sysname']],
    #   Windows = gsub("\\\\", "/", file.path(str(input$folder))),
    #   Darwin = "~/Desktop",
    #   getShinyOption("appDir", default = getwd())
    # )
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
    path <- "C:/Users/Matth/Desktop/testing shiny"
    choice <- "Hi_2020-04-21_bookmark.rds"
    choice_list <- readRDS(file = paste(path, "shiny_bookmarks", "shiny_bookmark_names" ,choice, sep = "/"))
    rds <- readRDS(file = "C:/Users/Matth/Desktop/testing shiny/shiny_bookmarks/bc367f9be0e649fb/input.rds" )
    url <- choice_list[['url']]

    updateQueryString(url, mode = "replace")
    session$reload()
  })

  onBookmarked(function(url) {
    name <- paste(input$book_name, Sys.Date(), "bookmark.rds", sep = "_")
    path <- parseDirPath(roots = roots, input$folder)
    x <- list("name" = input$book_name, "url" = url, 'path' = path)

    saveRDS(x, file = paste(path, "shiny_bookmarks", "shiny_bookmark_names", name, sep = "//"))
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

# choice_list <- readRDS(file = here::here("Enhancement_Work", "Sample Shiny", "shiny_bookmark_names", choice))
# url <- choice_list[['url']]
#
# url
#
# locate <- stringr::str_locate(url, "state_id_=")
#
#
# querry <- stringr::str_sub(url, start = (locate[, 2] + 1), end = -1L)
#
# url
