.onLoad <- function(libname, pkgname) {
  op <- options()
  op.juicedownApp <- list(
    juicedownApp.light_theme = "github",
    juicedownApp.dark_theme = "dracula"
  )
  toset <- !(names(op.juicedownApp) %in% names(op))
  if (any(toset)) options(op.juicedownApp[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {

}
