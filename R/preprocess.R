
preprocess_md <- function(input, allow_r) {
  md <- if (stringr::str_length(input$md) == 0) {
    .sample_md
  } else {
    input$md
  }
  
  # Escape R chunk
  if (!allow_r) {
    md <- md |>
      stringr::str_replace_all("```\\{r.*\\}", "```r") |>
      stringr::str_replace_all("`r(.*)`", "`\\1`")
  }
  
  # Special Commands
  md <- md |>
    stringr::str_replace('\\{%\\s*YouTube\\((.*)\\)\\s*%\\}', 
                         '\n```{r}\nincludeYT(\\1)\n```\n') |>
    stringr::str_replace('\\{%\\s*Audio\\((.*)\\)\\s*%\\}', 
                         '\n```{r}\nincludeAudio(\\1)\n```\n')
  
  # Options
  option_lines <- character(0)
  if (input$tweak_h2) {
    option_lines <- c(option_lines, "juicedown::tweak_moodle_heading()")
  }
  if (input$tweak_fn) {
    option_lines <-
      c(option_lines,
        "juicedown::tweak_footnote_highlight()")
  }
  if (length(option_lines) > 0) {
    option_lines <- c("```{r}", option_lines, "```")
  }
  md <- c(md, option_lines)
  md
}
