validate <- function(x, type) {
  name <- as.character(substitute(x))

  # Completed:
  # "Bool"
  # "Float"
  # "Int"
  # "String"
  # "Seq ("
  # "Enum ("
  # "List ("
  # "Tuple ("
  # "Any"
  # "Instance ("
  # "Dict ( String"
  # "Color"
  # "Percent"
  # "Angle"
  # "JSON"
  # "StringSpec"
  # "ScreenDistanceSpec"
  # "NumberSpec"
  # "FontSizeSpec"
  # "DistanceSpec"
  # "AngleSpec"
  # "ColorSpec"
  # "TitleProp"

  # Need to do:
  # "RelativeDelta ( "
  # "MinMaxBounds"
  # "Either (" # ignore "Auto"
  # "DashPattern"
  # "ColumnData ( String , Seq ( Any  ) )"

  ttrns <- list(String = "character", Bool = "logical", Int = "numeric",
    Float = "numeric", Date = "numeric", Color = "character",
    Percent = "numeric", Angle = "numeric", JSON = "character")
  ttrnsnm <- names(ttrns)

  # if scalar
  if (type %in% ttrnsnm) {
    return (validate_scalar(x, ttrns[[type]], type, name))
  }

  if (grepl("^Tuple \\(", type)) {
    if (length(x) != 2)
      stop("Attribute ", name, " must be a vector of length 2.", call. = FALSE)
    subtype <- strip_white(gsub("^Tuple \\((.*)\\)$", "\\1", type))
    if (subtype %in% c("Date , Date", "Float , Float")) {
      if (!is.numeric(x))
        stop("Attribute ", name, " must be a numeric vector of length 2", call. = FALSE)
    }
  }

  if (grepl("^Seq ", type)) {
    type <- gsub("^Seq \\( ([A-Za-z]*)  \\)$", "\\1", type)
    if (type %in% ttrnsnm) {
      return (validate_vector(x, ttrns[[type]], type, name))
    }
  }

  if (grepl("List \\(", type)) {
    # List with scalar types and Seq with scalar types are the same
    subtype <- strip_white(gsub("^List \\((.*)\\)$", "\\1", type))
    if (subtype %in% ttrnsnm) {
      return (validate_vector(unname(x), ttrns[[subtype]], subtype, name))
    } else {
      x <- validate_list(x, named = FALSE)
      return (lapply(x, function(a) validate(a, subtype)))
    }
  }

  if (grepl("^Enum", type)) {
    type <- get_enum_type(type)
    return (validate_enum(x, type, name))
  }

  # Instance is either NULL or list(id = string, type = string)
  if (grepl("^Instance", type) || type == "TitleProp") {
    if (is.null(x) || is.na(x))
      return(jsonlite::unbox(as.character(NA)))
    nms <- names(x)
    if (is.list(x) && "id" %in% nms && "type" %in% nms &&
      length(x$id) == 1 && length(x$type == 1)) {

      return (list(
        id = jsonlite::unbox(as.character(x$id)),
        type = jsonlite::unbox(as.character(x$type))
      ))
    } else {
      stop("Attribute ", name, " must be NULL or a list with 'id' and 'type'", call. = FALSE)
    }
  }

  if (grepl("^Dict \\( String ,", type)) {
    subtype <- strip_white(gsub("^Dict \\( String , (.*) \\)", "\\1", type))
    x <- validate_list(x, named = TRUE, name)
    # validate subtypes
    return (lapply(x, function(a) validate(a, subtype)))
  }

  # Specs are lists of scalars
  if (grepl("Spec$", type)) {
    # At some point, we can check specific attributes of each of these
    #   StringSpec
    #   ScreenDistanceSpec
    #   NumberSpec
    #   FontSizeSpec
    #   DistanceSpec
    #   AngleSpec
    #   ColorSpec
    if (is.null(x) || is.na(x))
      return(jsonlite::unbox(as.character(NA)))

    x <- validate_list(x, named = TRUE, name)
    if (any(unlist(lapply(x, length)) > 1))
      stop("Attribute ", name, " must a named list of scalars", call. = FALSE)
    if (any(unlist(lapply(x, function(a) !inherits(a, c("character", "numeric")))) > 1))
      stop("Attribute ", name, " must a named list of scalars that are character or numeric",
        call. = FALSE)

    return (lapply(x, function(a) jsonlite::unbox(a)))
  }

  if (type == "Any")
    return(x)

  # all others
  message("Note: could not find validator for type '", type, "'... Returning as is.")
  return(x)

  # dflt_res$StringSpec[[1]]
  # dflt_res$ScreenDistanceSpec[[1]]
  # dflt_res$NumberSpec[[1]]
  # dflt_res$FontSizeSpec[[1]]
  # dflt_res$DistanceSpec[[1]]
  # dflt_res$AngleSpec[[1]]
  # dflt_res$ColorSpec[[1]]

  # dflt_res$MinMaxBounds[[1]]
  # dflt_res$TitleProp[[1]]
  # dflt_res$Angle[[1]]
  # dflt_res$Percent[[1]]
  # dflt_res$Color[[1]]
  # dflt_res$DashPattern[[1]]
  # dflt_res$Date[[1]]
  # dflt_res$JSON[[1]]
  # dflt_res[["ColumnData ( String , Seq ( Any  ) )"]][[1]]
}


strip_white <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

#' @importFrom methods as
validate_scalar <- function(x, type = "character", otype = "", name) {
  if (length(x) > 1)
    stop("Attribute ", name, " must be a scalar.", call. = FALSE)
  # ensure NA gets populated as null (unbox wants a "character" NA for this)
  if (length(x) == 1 && is.na(x))
    type <- "character"
  jsonlite::unbox(methods::as(x, type))
}

validate_vector <- function(x, type = "character", otype = "", name) {
  # if it's NA, return null
  if (length(x) == 1 && is.na(x))
    return (jsonlite::unbox(as.character(NA)))
  if (!is.vector(x))
    stop("Attribute ", name, " must be a vector.", call. = FALSE)
  methods::as(x, type)
}

# TODO: add additional validation for scalar and vector for otype Color and Percent

validate_enum <- function(x, type, name) {
  values <- enum_list[[type]]
  if (!x %in% values)
    stop("Attribute ", name, " with value '", x, "' must be one of ",
      paste(values, collapse = ", "), call. = FALSE)
  jsonlite::unbox(x)
}

validate_list <- function(x, named = FALSE, name) {
  if (!is.list(x))
    stop("Attribute ", name, " must be a list", call. = FALSE)
  if (named && is.null(names(x)))
    stop("Attribute ", name, " must be a named list", call. = FALSE)
  if (!named && !is.null(names(x)))
    names(x) <- NULL
  x
}

validate_list_class <- function(x, class, named = FALSE, name) {
  if (!is.list(x) && !all(sapply(x, function(a) inherits(a, class))))
    stop("Attribute ", name, " must be a list with all elements of class ", class,
      call. = FALSE)
  if (named && is.null(names(x)))
    stop("Attribute ", name, " must be a named list", call. = FALSE)
  if (!named && !is.null(names(x)))
    names(x) <- NULL
  x
}

get_enum_type <- function(x)
  gsub(" $", "", gsub("^Enum \\( (.*)\\ +)$", "\\1", x))

enum_list <- list(
  "ButtonType" =
    c("default", "primary", "success", "warning", "danger", "link"),
  "Enumeration(ascending, descending)" =
    c("ascending", "descending"),
  "Enumeration(check, check-circle, check-circle-o, check-square, check-square-o)" =
    c("check", "check-circle", "check-circle-o", "check-square", "check-square-o"),
  "Enumeration(horizontal, vertical, left, right, above, below)" =
    c("horizontal", "vertical", "left", "right", "above", "below"),
  "Enumeration(mouse, hline, vline)" =
    c("mouse", "hline", "vline"),
  "Enumeration(POST, GET)" =
    c("POST", "GET"),
  "Enumeration(prev, next, nearest, interp, none)" =
    c("prev", "next", "nearest", "interp", "none"),
  "Enumeration(replace, append)" =
    c("replace", "append"),
  "Enumeration(scroll, zoom)" =
    c("scroll", "zoom"),
  "Enumeration(select, inspect)" =
    c("select", "inspect"),
  "Enumeration(show, hide, change)" =
    c("show", "hide", "change"),
  "Enumeration(checkbox)" =
    c("checkbox"),
  "Enumeration(snap_to_data, follow_mouse, none)" =
    c("snap_to_data", "follow_mouse", "none"),
  "Enumeration(x, y)" =
    c("x", "y"),
  "JitterRandomDistribution" =
    c("uniform", "normal"),
  "MapType" =
    c("satellite", "roadmap", "terrain", "hybrid"),
  "RenderLevel" =
    c("image", "underlay", "glyph", "annotation", "overlay"),
  "SizingMode" =
    c("stretch_both", "scale_width", "scale_height", "scale_both", "fixed"),
  "StartEnd" =
    c("start", "end"),
  "StepMode" =
    c("before", "after", "center"),
  "AngleUnits" =
    c("deg", "rad"),
  "Dimension" =
    c("width", "height"),
  "Enumeration(normal, grey)" =
    c("normal", "grey"),
  "LegendLocation" =
    c("top_left", "top_center", "top_right", "center_left", "center", "center_right",
      "bottom_left", "bottom_center", "bottom_right"),
  "NumeralLanguage" =
    c("be-nl", "chs", "cs", "da-dk", "de-ch", "de", "en", "en-gb", "es-ES", "es", "et",
      "fi", "fr-CA", "fr-ch", "fr", "hu", "it", "ja", "nl-nl", "pl", "pt-br", "pt-pt",
      "ru", "ru-UA", "sk", "th", "tr", "uk-UA"),
  "Orientation" =
    c("horizontal", "vertical"),
  "RoundingFunction" =
    c("round", "nearest", "floor", "rounddown", "ceil", "roundup"),
  "SliderCallbackPolicy" =
    c("continuous", "throttle", "mouseup"),
  "Direction" =
    c("clock", "anticlock"),
  "Enumeration(horizontal, vertical)" =
    c("horizontal", "vertical"),
  "Location" =
    c("above", "below", "left", "right"),
  "RenderMode" =
    c("canvas", "css"),
  "Dimensions" =
    c("width", "height", "both"),
  "TextBaseline" =
    c("top", "middle", "bottom", "alphabetic", "hanging", "ideographic"),
  "FontStyle" =
    c("normal", "italic", "bold"),
  "TextAlign" =
    c("left", "right", "center"),
  "SpatialUnits" =
    c("screen", "data"),
  "LineCap" =
    c("butt", "round", "square"),
  "LineJoin" =
    c("miter", "round", "bevel"),
  "Enumeration(years, months, days, hours, minutes, seconds, microseconds)" =
    c("years", "months", "days", "hours", "minutes", "seconds", "microseconds")
)
