
# read each of the scraped files and parse the content
parse_scraped <- function() {
  files <- list.files("scripts/scraped_pages/", full.names = TRUE)
  fb <- basename(files)
  fb <- gsub("\\.json", "", fb)
  fb <- gsub("widgets\\.", "widgets_", fb)

  res <- list()

  glyphs <- c("AnnularWedge", "Annulus", "Arc", "Bezier", "Ellipse", "HBar", "Image",
    "ImageRGBA", "ImageURL", "Line", "MultiLine", "Oval", "Patch", "Patches", "Quad",
    "Quadratic", "Ray", "Rect", "Segment", "Text", "VBar", "Wedge")

  markers <- c("Asterisk", "Circle", "CircleCross", "CircleX", "Cross", "Diamond",
    "DiamondCross", "InvertedTriangle", "Square", "SquareCross", "SquareX", "Triangle", "X")

  for (ii in seq_along(files)) {
    message(fb[ii])
    a <- jsonlite::fromJSON(paste(readLines(files[ii], warn = FALSE), collapse = ""),
      simplifyVector = FALSE)

    for (jj in seq_along(a)) {
      js <- NULL
      if (a[[jj]]$json == "") {
        message("*** note: no json prototype for this model")
      } else {
        js <- try(jsonlite::fromJSON(a[[jj]]$json,
          simplifyVector = FALSE))
      }
      txt <- unlist(a[[jj]]$text)
      idx <- grepl("Bases: ", txt)
      basecls <- gsub("Bases: ", "", txt[idx])
      txt <- gsub("\n", " ", paste(txt[!idx], collapse = "\n"))
      a[[jj]]$base_class <- basecls
      if (a[[jj]]$name %in% glyphs)
        a[[jj]]$base_class <- "Glyph"
      if (a[[jj]]$name %in% markers)
        a[[jj]]$base_class <- "Marker"
      a[[jj]]$desc <- txt
      a[[jj]]$txt <- NULL
      a[[jj]]$dflt <- js
      for (kk in seq_along(a[[jj]]$attrs)) {
        att <- a[[jj]]$attrs[[kk]]
        dsc <- unlist(att$desc)
        idx <- grepl("property type:", dsc)
        type <- gsub("property type: ", "", dsc[idx])
        dsc <- gsub("\n", " ", paste(dsc[!idx], collapse = "\n"))
        val <- ""
        if (!is.null(js)) {
          val <- js[[att$name]]
          # if (is.list(val))
          #   val <- jsonlite::toJSON(val, auto_unbox = TRUE)
        }
        a[[jj]]$attrs[[kk]] <- list(
          name = att$name,
          type = type,
          desc = dsc,
          value = val
        )
      }
    }
    res[[fb[ii]]] <- a
  }

  # set the name of each list element to the model name
  for (ii in seq_along(res))
    names(res[[ii]]) <- sapply(res[[ii]], function(x) x$name)

  # make an unnested list of models
  res2 <- unlist(res, recursive = FALSE)
  names(res2) <- sapply(names(res2), function(y) tail(strsplit(y, "\\.")[[1]], 1))

  res2
}

mod_cleanup <- function(mods) {
  # there are unused attrs in the "Model" class - get rid of them...
  mods$attrs[4:5] <- NULL

  # add a class "Base" (hard-coded in R/classes.R) that everything inherits from...
  mods$Model$base_class <- "Base"
  mods$Base <- list(
    attrs = list(list(name = "id", type = "String", desc = "id", value = NULL))
  )

  # # look at multiple inheritance...
  # get_inherit(mods$ButtonGroup$base_class)
  # # AbstractGroup ButtonLike
  # get_inherit(mods$AbstractButton$base_class)
  # # Widget ButtonLike

  # # build ButtonLike into these two classes and get rid of ButtonLike...
  # mods$ButtonLike
  # cat(mods$AbstractButton$json)

  # has no type and isn't in prototype
  mods$ButtonLike$attrs[[2]] <- NULL

  mods$AbstractButton$attrs <- c(mods$AbstractButton$attrs, mods$ButtonLike$attrs)
  mods$AbstractButton$base_class <- "bokeh.models.widgets.widget.Widget"

  mods$ButtonGroup$attrs <- c(mods$ButtonGroup$attrs, mods$ButtonLike$attrs)
  mods$ButtonGroup$base_class <- "bokeh.models.widgets.groups.AbstractGroup"

  mods$ButtonLike <- NULL

  mods
}

# TODO: deal with multiple inheritance
get_inherit <- function(x) {
  if (is.null(x) || length(x) == 0)
    return(NULL)
  unname(sapply(strsplit(x, ",")[[1]], function(y) {
    tail(strsplit(y, "\\.")[[1]], 1)
  }))
}

# for each attr, need to get init_default and init_validator


clean_dput <- function(x) {
  tmp <- paste(capture.output(dput(x)), collapse = "")
  if (grepl("^structure\\(list", tmp) && !grepl("character\\(0\\)", tmp)) {
    tmp <- gsub("^structure\\(", "", tmp)
    tmp <- gsub("(.*), \\.Names.*", "\\1", tmp)
  }
  tmp
}

init_default <- function(val, type) {
  if (is.null(val))
    return("NULL")

  if (!is.null(type) && grepl("^Instance", type))
    return("NULL")

  return (clean_dput(val))
}

init_default_check <- function(val, type) {
  if (is.null(val))
    return("NULL")

  return (clean_dput(val))
}

find_with_type <- function(type) {
  which(unlist(lapply(res2, function(x) {
    any(unlist(lapply(x$attr, function(y) {
      y$type == type
    })))
  })))
}

init_validator <- function(type, name) {
  ttrns <- list(String = "character", Bool = "logical", Int = "numeric", Float = "Numeric")

  # if scalar
  if (type %in% c("String", "Bool", "Int", "Float")) {
    return (paste0("validate_scalar(", name, ", \"", ttrns[[type]], "\")"))
  }

  if (grepl("^Seq ", type)) {
    type <- gsub("^Seq \\( ([A-Za-z]*)  \\)$", "\\1", type)
    if (type %in% c("String", "Bool", "Int", "Float")) {
      return (paste0("validate_vector(", name, ", \"", ttrns[[type]], "\")"))
    }
    # also deal with color here...
  }

  if (grepl("^Enum", type)) {
    type <- get_enum_type(type)
    return (paste0("validate_enum(", name, ", \"", type, "\")"))
  }

  if (type == "List ( Any )")
    return (paste0("validate_list(", name, ", named = FALSE)"))
}

get_default_string <- function(attrs) {
  if (is.null(attrs))
    return("")

  dflts <- sapply(attrs, function(atr) {
    paste0(atr$name, "=", init_default(atr$value, atr$type))
  })
  paste(dflts, collapse = ", ")
}

get_super_string <- function(attrs) {
  if (is.null(attrs))
    return("")
  paste0("\nsuper$initialize(", paste0(names(attrs), "=", names(attrs), collapse = ", "), ")")
}

get_initialize_string2 <- function(attrs) {
  paste0(unlist(
    lapply(attrs, function(atr) {
      res <- paste0("      private$", atr$name, " <- ", init_validator(atr$type, atr$name))
    }
  )), collapse = "\n")
}

get_initialize_string <- function(attrs) {
  paste0(unlist(
    lapply(attrs, function(atr) {
      res <- paste0("      private$", atr$name, " <- validate(", atr$name, ", \"", atr$type, "\")")
    }
  )), collapse = "\n")
}

get_private_string <- function(attrs) {
  if (length(attrs) == 0)
    return ("")
  last_attr <- attrs[[length(attrs)]]$name
  res <- unlist(
    lapply(attrs, function(atr) {
      res <- paste_nice(atr$desc, simplify = FALSE)[[1]]
      res <- paste("    #", res, collapse = "\n")
      res <- c(res, paste0("    ", atr$name, " = NULL",
        ifelse(atr$name == last_attr, "", ","), " # ", atr$type))
      paste(res, collapse = "\n")
    }
  ))
  paste(res, collapse = "\n")
}

get_attrs <- function(obj, name, dflt, recurse = FALSE) {
  if (is.null(name))
    return(NULL)
  a <- obj[[name]]
  attrs <- a$attrs
  hastype <- unlist(lapply(attrs, function(x) length(x$type))) > 0
  attrs <- attrs[hastype]

  if (length(attrs) == 0) {
    attrs <- NULL
  } else {
    names(attrs) <- sapply(attrs, function(x) x$name)
    if (!is.null(a$dflt))
      attrs <- attrs[intersect(names(a$dflt), names(attrs))]
  }

  if (recurse && length(a$base_class) > 0) {
    tmp <- get_attrs(obj, get_inherit(a$base_class), dflt, recurse = TRUE)
    attrs <- c(attrs, tmp)
  }

  dfltnm <- names(dflt)
  for (ii in seq_along(attrs)) {
    if (attrs[[ii]]$name %in% dfltnm && attrs[[ii]]$name != "id")
      attrs[[ii]]$value <- dflt[[attrs[[ii]]$name]]
  }

  attrs
}

# sort(unique(allvals_orig))

paste_nice <- function(..., indent = 0, exdent = NULL, simplify = TRUE) {
  if (is.null(exdent))
    exdent <- indent
  wr <- strwrap(paste0(...), indent = indent, exdent = exdent, simplify = simplify, width = 72)
  if (!simplify)
    return (wr)
  res <- paste0(wr, collapse = "\n")
  # add spaces around equals
  gsub("([^ ])=([^ ])", "\\1 = \\2", res)
}

get_class_string <- function(a, obj) {
  cls <- a$name
  # get rid of attributes that don't have types...
  hastype <- unlist(lapply(a$attrs, function(x) length(x$type))) > 0
  a$attrs <- a$attrs[hastype]

  base <- get_inherit(a$base_class)
  base_attrs <- get_attrs(obj, base, dflt = a$dflt, recurse = TRUE)
  attrs <- get_attrs(obj, cls, dflt = a$dflt)
  desc <- paste_nice(tail(a$text, 1)[[1]], simplify = FALSE)[[1]]
  desc <- paste("#", desc, collapse = "\n")

  res <- paste0(desc, "\n", cls, ' <- R6::R6Class("', cls, '",',
  ifelse(is.null(base), "", paste0("\n  inherit = ", base, ",")), '
  public = list(
    initialize = function(\n',
      paste_nice(
        get_default_string(base_attrs),
          ifelse(is.null(base_attrs) || is.null(attrs), '', ', '),
        get_default_string(attrs),
        indent = 6
      ), '
    ) {\n',
      paste_nice(get_super_string(base_attrs), indent = 6, exdent = 8),
        ifelse(is.null(base_attrs), '', '\n'),
      get_initialize_string(attrs),
      '
    }
  ),
  private = list(\n', get_private_string(attrs), '
  )
)

')

  rfixup(res)
}

rfixup <- function(x) {
   x <- gsub("Click the question\n      mark to learn more about Bokeh plot tools.",
    "Click the question mark to learn more about Bokeh plot tools.", x)
  x
}

fixup <- function(x) {
  x <- gsub(": 1\\.0", ": 1", x)
  x <- gsub(": 2\\.0", ": 2", x)
  x <- gsub(": 0\\.0,", ": 0,", x)
  x <- gsub(": 10\\.0", ": 10", x)
  x <- gsub(": 1483920000000\\.0", ": 1483920000000", x)
  x <- gsub(": 156543\\.033928040972804097", ": 156543\\.0339", x)
  x <- gsub(": 156543\\.03392804097", ": 156543\\.0339", x)
  x
}

get_test_string <- function(a, obj) {
  if (a$json == "")
    return(paste0("\n\n### No test for model: ", a$name, " (no json prototype)\n\n"))

  # if there are any instance types, we need to specify them...
  all_attrs <- get_attrs(obj, a$name, dflt = a$dflt, recurse = TRUE)
  types <- unlist(lapply(all_attrs, function(x) x$type))
  idx <- which(grepl("^Instance", types))
  extra_args <- ""
  if (length(idx) > 0) {
    extra_args <- paste0(", ", paste(unlist(lapply(all_attrs[idx], function(x) {
      paste0(x$name, " = ", init_default_check(a$dflt[[x$name]], x$type))
    })), collapse = ", "))
  }

  paste0(
'test_that("\'', a$name, '\' model matches prototype", {
  obj <- ', a$name, '$new(id = "', a$dflt$id, '"', extra_args, ')
  jsn <- as.character(obj$to_json(pretty = TRUE))
  expect_identical(jsn,\n\'', fixup(strip_white(a$json)), '\')
})

')
}
