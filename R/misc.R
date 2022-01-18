
showError <- function(title, content) {
  showModal(modalDialog(title=title, content))
}

# This functions gives back as a string the output of 'dput',
# i.e an ASCII text representation of an R object
dput2 <- function (x, collapse ="\n") {
  paste(capture.output(dput(x)), collapse = collapse)
}

NOT_YET_IMPLEMENTED <- function() {
  showModal(modalDialog(title = "Not yet implemented",
                        "Sorry, not available yet. Will be there in the next versions of the App!", easyClose = TRUE))
}
