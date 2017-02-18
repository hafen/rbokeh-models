#' @importFrom methods as
validate_scalar <- function(x, type = "character") {
  name <- as.character(substitute(x))
  if (length(x) > 1)
    stop("Attribute ", name, " must be a scalar.", call. = FALSE)
  # ensure NA gets populated as null (unbox wants a "character" NA for this)
  if (length(x) == 1 && is.na(x))
    type <- "character"
  jsonlite::unbox(methods::as(x, type))
}

validate_vector <- function(x, type = "character") {
  name <- as.character(substitute(x))
  # if it's NA, return null
  if (length(x) == 1 && is.na(x))
    return (jsonlite::unbox(as.character(NA)))
  if (!is.vector(x))
    stop("Attribute ", name, " must be a vector.", call. = FALSE)
  methods::as(x, type)
}

validate_enum <- function(x, values) {
  name <- as.character(substitute(x))
  if (!x %in% values)
    stop("Attribute ", name, " with value '", x, "' must be one of ",
      paste(values, collapse = ", "), call. = FALSE)
  jsonlite::unbox(x)
}

validate_list_class <- function(x, class, named = FALSE) {
  name <- as.character(substitute(x))
  if (!is.list(x) && !all(sapply(x, function(a) inherits(a, class))))
    stop("Attribute ", name, " must be a list with all elements of class ", class,
      call. = FALSE)
  if (named && is.null(names(x)))
    stop("Attribute ", name, " must be a named list", call. = FALSE)
  if (!named && !is.null(names(x)))
    names(x) <- NULL
  x
}
