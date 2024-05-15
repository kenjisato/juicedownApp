### Global Variables

.packageName <- "juicedownApp"
.katex_url <- "https://cdn.jsdelivr.net/combine/npm/katex/dist/katex.min.js,npm/katex/dist/contrib/auto-render.min.js,npm/@xiee/utils/js/render-katex.js"


### Utility functions

pkg_file <- function(..., pkg = .packageName) {
  system.file(..., package = pkg, mustWork = TRUE)
}

read_pkg_file <- function(..., pkg = .packageName) {
  paste(readLines(pkg_file(..., pkg = pkg)), collapse = "\n")
}

