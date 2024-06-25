ace_ui <- function(id, value, ..., debug = FALSE) {
  ns <- NS(id)
  hide <- if (debug) function(x) x else shinyjs::hidden

  tagList(
    hide(textAreaInput(ns("mirror"), NULL, value = NULL)),
    shinyAce::aceEditor(
      outputId = ns("editor"),
      value = value,
      theme = getOption("juicedownApp.light_theme"),
      ...
    )
  )
}


ace_server <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    # Toggle light/dark modes
    if (!is.null(isolate(session$userData$darkmode()))) {
      observe({
        theme <- if (session$userData$darkmode() == "light") {
          getOption("juicedownApp.light_theme")
        } else {
          getOption("juicedownApp.dark_theme")
        }
        shinyAce::updateAceEditor(
          session,
          editorId = "editor", theme = theme
        )
      }) |>
        bindEvent(session$userData$darkmode())
    }

    ## Options for shinyAce::aceEditor
    observe({
      shinyAce::updateAceEditor(
        session,
        editorId = "editor",
        fontSize = session$userData$aceOption$fontSize
      )
    })


    ## shinyAce::aceEditor -> textAreaInput
    ##   when indexeddb's data are already retrieved.
    ## This is necessary to backup input in because shinyAce::aceEditor because
    ##   shinyStorePlus does not support shinyAce::aceEditor
    observe({
      updateTextAreaInput(
        session,
        inputId = "mirror", value = input$editor
      )
    })

    ## textAreaInput -> shinyAce::aceEditor on start up
    ## We need to watch invalidation of input$mirror at most twice;
    ##   once when initialization of the textAreaInput and the other
    ##   when copied from indexeddb
    cnt <- function(cnt_id) {
      force(cnt_id)
      counter <- 0
      function() {
        counter <<- counter + 1
        # message(ns(cnt_id), ": ", counter)
        counter
      }
    }
    ta2ace <- cnt("ta2ace")

    o <- observe({
      if (ta2ace() > 1) o$destroy()

      if (!is.null(input$mirror)) {
        shinyAce::updateAceEditor(session, "editor", input$mirror)
      }
    }) |>
      bindEvent(
        input$mirror,
        ignoreInit = TRUE
      )

    session$userData[[id]] <- reactive(input$editor)
  })
}



ace_demo <- function(debug = FALSE) {
  ui <- fluidPage(
    shinyStorePlus::initStore(),
    shinyjs::useShinyjs(),
    ace_ui("ace", debug = debug)
  )

  server <- function(input, output, session) {
    ace_server("ace")

    shinyStorePlus::setupStorage(
      appId = "ace-demo",
      inputs = TRUE
    )
  }

  shinyApp(ui, server)
}
