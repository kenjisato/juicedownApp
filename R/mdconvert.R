# UI fragments ----

left_panel <- function(.ns) {
  panel_md <- nav_panel("Markdown",
                        class = "ta",
                        shinyAce::aceEditor(
                          outputId = .ns("md"), 
                          value = installed_doc("text", "default"),
                          mode = "markdown",
                          fontSize = 13,
                          height = "100%",
                          showLineNumbers = FALSE)
                        )
  panel_css <- nav_panel("CSS",
                         class = "ta",
                         shinyAce::aceEditor(
                           outputId = .ns("css"), 
                           value = installed_doc("css", "article"),
                           mode = "scss",
                           fontSize = 13,
                           height = "100%",
                           showLineNumbers = FALSE)
                         )
  
  panel_opts <- nav_panel(
    icon("gear"), 
    id = "options-container",
    card(
      fill = FALSE,
      h5("Document"),
      radioButtons(
        .ns("type_css"),
        "Type: ",
        c("Page" = "page", "Label" = "label"),
        width = "100%",
        inline = TRUE
      ),
      layout_column_wrap(
        actionLink(
          .ns("default_md"),
          "Reset to default document",
          icon = icon("broom")
        ),
        actionLink(
          .ns("default_css"), 
          "Reset to default CSS", 
          icon = icon("broom")
        ),
        width = 1/2
      ),
      class = "card-no-border"
    ),
    card(
      fill = FALSE,
      h5("Backup and Restore"),
      downloadButton(
        .ns("downloadData"), 
        "Download ZIP", 
        icon = icon("download")
      ),
      fileInput(
        .ns("uploadData"), 
        "Upload ZIP",
        width = "100%"
      ),
      actionButton(
        .ns("importData"),
        "Import ZIP",
        icon = icon("file-import")
      ),
      class = "card-no-border"
    ),
    card(
      fill = FALSE,
      h5("Tweaks"),
      checkboxInput(
        .ns("tweak_h2"),
        "Remove redundant heading (JS)",
        value = FALSE,
        width = NULL
      ),
      checkboxInput(
        .ns("tweak_fn"),
        "Fix footnote styling (JS)",
        value = FALSE,
        width = NULL
      ),
      class = "card-no-border"
    )
  )
  
  page_fillable(id = "left-panel",
                navset_card_underline(panel_md,
                                      panel_css,
                                      nav_spacer(),
                                      panel_opts))
}


right_panel <- function(.ns) {
  btn_convert <- nav_item(
    actionButton(
      .ns("convert"),
      "Convert",
      icon = icon("file-code"),
      class = "btn-sm"
    )
  )
  
  tagList(
    rclipboard::rclipboardSetup(),
    tags$head(
      tags$script(
        defer = "",
        src = .katex_url,
        crossorigin = "anonymous"
      )
    ),
    page_fillable(
      id = "right-panel",
      navset_card_underline(
        id = .ns("right_nav"),
        nav_panel("Preview", htmlOutput(.ns("preview"))),
        nav_panel(shiny::icon("markdown"), includeHTML(pkg_file("www", "info.html"))),
        nav_spacer(),
        btn_convert,
        nav_item(uiOutput(.ns("clip")))
      )
    )
  )
}


# Module UI ----

mdconvertUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyStorePlus::initStore(),
    layout_column_wrap(width = 1 / 2, left_panel(ns), right_panel(ns)),
    includeCSS(pkg_file("www", "css", "style.css"))
  )
}

# Server function ----

mdconvertServer <- function(id, allow_r = TRUE) {
  .tdir <- tempfile(pattern = "dir")
  dir.create(.tdir)
  
  # Paths for temporary files
  .md_file <- file.path(.tdir, "page.md")
  .css_file <- file.path(.tdir, "style.scss")
  .yml_file <- file.path(.tdir, "config.yml")
  .html_file <- file.path(.tdir, "page.html")
  .files <- c(.md_file, .css_file, .html_file, .yml_file)
  
  moduleServer(id, function(input, output, session) {
    r <- reactiveValues(html = "")
    
    # Initial Preview
    output$preview <-
      renderUI("Click on the convert button.")
    
    # Convert button
    observeEvent(input$convert, {
      
      md <- preprocess_md(input, !allow_r)
      writeLines(md, .md_file)
      
      css <- preprocess_css(input) # a workaround a bug of juicedown
      writeLines(css, .css_file)

      tag <- if (input$type_css == "page") "article" else "div"
      
      r$html <- juicedown::convert(.md_file,
                                   clip = FALSE,
                                   stylesheet = .css_file,
                                   tag = tag)
      
      output$preview <-
        renderUI(withMathJax(HTML(paste(
          r$html, collapse = "\n"
        ))))
      
      nav_select(id = "right_nav", selected = "Preview")
      unlink(.files)
    })
    
    # Download Button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("juicedown-pack-",
               format(Sys.time(), "%Y%m%d-%H%M%S"),
               ".zip")
      },
      content = function(file) {
        
        md <- preprocess_md(input, !allow_r, download = TRUE)
        writeLines(md, .md_file)
        
        css <- input$css
        writeLines(css, .css_file)
        
        cfg <- list(
          type_css = input$type_css,
          tweak_h2 = input$tweak_h2,
          tweak_fn = input$tweak_fn
        )
        yaml::write_yaml(cfg, .yml_file)
        
        writeLines(r$html, .html_file)
        utils::zip(file, c(.md_file, .html_file, .css_file, .yml_file), extras = '-j')
        unlink(.files)
      }
    )
    
    # Import Button
    observeEvent(input$importData, {
      
      callback <- function(x) {
        if (x == FALSE) return(invisible())
        
        zipfile <- input$uploadData$datapath[[1]]
        exdir <- file.path(.tdir, "archive")
        zip::unzip(zipfile, exdir = exdir)
        
        md <- readLines(list.files(exdir, pattern = "\\.R?md", full.names = TRUE)[[1]])
        md <- paste(md, collapse = "\n")
        #updateTextAreaInput("md", value = md, session = session)
        shinyAce::updateAceEditor(session = session, editorId = "md", value = md)
        
        css <- readLines(list.files(exdir, pattern = "\\.s?css", full.names = TRUE)[[1]])
        css <- paste(css, collapse = "\n")
        #updateTextAreaInput("css", value = css, session = session)
        shinyAce::updateAceEditor(session = session, editorId = "css", value = css)
        
        cfg <- list.files(exdir, pattern = "\\.yml", full.names = TRUE)
        if (length(cfg) > 0) {
          yml <- yaml::read_yaml(cfg[[1]])
          updateRadioButtons("type_css", selected = yml$type_css, session = session)
          updateCheckboxInput("tweak_h2", value = yml$tweak_h2, session = session)
          updateCheckboxInput("tweak_fn", value = yml$tweak_fn, session = session)
        }
        
        unlink(zipfile)
        unlink(exdir)
      }

      if (is.null(input$uploadData)) {
        shinyalert::shinyalert(
          "No file selected!",
          type = "error"
        )
      } else {
        shinyalert::shinyalert(
          title = "Caution!",
          text = "Any changes you made will be discarded! Do you wish to proceed?",
          size = "s", 
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Yes",
          confirmButtonCol = "#AEDEF4",
          cancelButtonText = "No",
          callbackR = callback
        )
      }
    })
    
    # Reset to default
    observeEvent(input$default_md, {
      shinyalert::shinyalert(
        title = "Caution!",
        text = "Any changes you made to the markdown document will be discarded.",
        size = "s", 
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "Cancel",
        callbackR = function(x) {
          if (x != FALSE) {
            #updateTextAreaInput(inputId = "md", value = installed_doc("text", "default"))
            shinyAce::updateAceEditor(session = session, editorId = "md",
                                      value = installed_doc("text", "default"))
          }
        }
      )
    })
    
    observeEvent(input$default_css, {
      default_css <- if (input$type_css == "page") {
        installed_doc("css", "article")
      } else {
        installed_doc("css", "div")
      }
      shinyalert::shinyalert(
        title = "Caution!",
        text = "Any changes you made to the custom CSS will be discarded.",
        size = "s", 
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "Cancel",
        callbackR = function(x) {
          if (x != FALSE) {
            #updateTextAreaInput(inputId = "css", value = default_css)
            shinyAce::updateAceEditor(session = session, editorId = "css",
                                      value = default_css)
          }
        }
      )
    })
    
    # Copy to clipboard
    output$clip <- renderUI({
        rclipboard::rclipButton(
          inputId = NS(id, "clipbtn"),
          label = "Copy to Clipboard",
          clipText = r$html,
          icon = icon("clipboard"),
          class = "btn-sm"
        )
    })
    
    
    observeEvent(input$clipbtn, {
      showNotification("Copied!", type = "message", duration = 2)
    })
  })
}

mdconvertApp <- function() {
  ui <- tags$body(fluidPage(title = "md2html",
                            fluidRow(mdconvertUI("test"))))
  server <- function(input, output, server) {
    mdconvertServer("test")
  }
  shinyApp(ui, server, options = list(launch.browser = TRUE))
}
