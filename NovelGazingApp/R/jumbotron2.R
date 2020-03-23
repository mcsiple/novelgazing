jumbotron2 <- function (header, content, button = TRUE) {
  if (button) {
    div(class = "jumbotron", 
        h2(header), 
        p(content), 
        p(a(class = "btn btn-primary btn-lg button",
            id = "tabBut",
            button_label)))
  }
  else {
    div(class = "jumbotron",
        h2(header),
        p(content))
  }
}