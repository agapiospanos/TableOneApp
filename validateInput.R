validateInput <- function(selector, value, type = NULL, required = TRUE) {
  if (required) {
    if (length(value) == 0) {
      removeUI(selector = ".error-message", multiple = TRUE)
      insertUI(
        selector = selector,
        where = "beforeEnd",
        ui = h6(class = "error-message", "This field cannot be empty. Please fill in a value")
      )
    } else {
       validateByType(type = type, value = value, selector = selector)
    }
  } else {
    if (length(value) > 0) {
      validateByType(type = type, value = value, selector = selector)
    }
  }
}

validateByType <- function(type, value, selector) {
  if (type == "numeric" & !is.numeric(value)) {
    removeUI(selector = ".error-message", multiple = TRUE)
    insertUI(
      selector = selector,
      where = "beforeEnd",
      ui = h6(class = "error-message", "Please use only numbers for this field")
    )
  } else if (type == "character") {
    removeUI(selector = ".error-message", multiple = TRUE)
    insertUI(
      selector = selector,
      where = "beforeEnd",
      ui = h6(class = "error-message", "Please use only numbers for this field")
    )
  }
}