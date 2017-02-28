files <- list.files("_ignore/scrape_pages/", full.names = TRUE)
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
  a <- jsonlite::fromJSON(paste(readLines(files[ii]), collapse = ""),
    simplifyVector = FALSE)

  for (jj in seq_along(a)) {
    js <- NULL
    if (a[[jj]]$json == "") {
      message("*** note: no json prototype for this model")
    } else {
      js <- try(jsonlite::fromJSON(a[[jj]]$json))
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

res$model$Model$base_class <- NULL

# there are unused attrs in the "Model" class - get rid of them...
res$model$Model$attrs[4:5] <- NULL

# Model should have an "id" attributes
res$model$Model$attrs <- c(res$model$Model$attrs,
  list(list(name = "id", type = "String", desc = "id", value = NULL)))

# make an unnested list of models
res2 <- unlist(res, recursive = FALSE)
names(res2) <- sapply(names(res2), function(y) tail(strsplit(y, "\\.")[[1]], 1))

length(res2)
# 205 models!!!!
sum(sapply(res[!grepl("widgets", names(res))], length))
# 157 without widgets

allvals_orig <- unlist(lapply(res2, function(el) {
  lapply(el$attrs, function(x) x$type)
}))

allvals <- allvals_orig

# treat all enums as one and all instances as one just to get an idea
idx <- grepl("^Enum", allvals)
enums <- allvals[idx]
enums <- gsub(" $", "", gsub("^Enum \\( (.*)\\ +)$", "\\1", enums))
enumstbl <- sort(table(enums))
allvals[idx] <- "Enum"

# look at instances
idx <- grepl("^Instance", allvals)
inst <- allvals[idx]
allvals[idx] <- "Instance"

# look at dicts
idx <- grepl("^Dict", allvals)
dict <- allvals[idx]
# it's always dict ( String ,) which is great (named list)
allvals[idx] <- "Dict"

# look at lists
idx <- grepl("^List", allvals)
lst <- allvals[idx]
allvals[idx] <- "List"

# look at either
idx <- grepl("^Either", allvals)
eth <- allvals[idx]
# it's always dict ( String ,) which is great (named list)
allvals[idx] <- "Either"

# look at tuple
idx <- grepl("^Tuple", allvals)
tpl <- allvals[idx]
# Tuples are always two of the same data type
# RelativeDelta???
allvals[idx] <- "Tuple"

# look at tuple
idx <- grepl("^RelativeDelta", allvals)
rld <- allvals[idx]
allvals[idx] <- "RelativeDelta"

# look at columndata
idx <- grepl("^ColumnData", allvals)
cld <- allvals[idx]
allvals[idx] <- "ColumnData"

# look at seq
idx <- grepl("^Seq", allvals)
sq <- allvals[idx]
allvals[idx] <- "Seq"

# RelativeDelta???
# Any

valtbl <- sort(table(unname(allvals)))
valtbl

excl <- c("Tuple", "Dict", "Seq", "ColumnData", "RelativeDelta", "Either", "List",
  "Instance", "Enum")
tmp <- valtbl[setdiff(names(valtbl), excl)]

# these are the basic types we need to be able to deal with
cat(paste(names(tmp), collapse = "\n"))

# Bool
# Int
# Float
# String
# Color
# Date
# Angle
# Percent

# JSON
# MinMaxBounds
# DashPattern
# TitleProp

# StringSpec
# FontSizeSpec
# ScreenDistanceSpec
# DistanceSpec
# AngleSpec
# ColorSpec
# NumberSpec


## look at base classes
bases <- unlist(lapply(res2, function(el) el$base_class))

subs <- names(bases)
supers <- unname(bases)

get_bases <- function(x) {
  unname(sapply(strsplit(x, ",")[[1]], function(y) {
    tail(strsplit(y, "\\.")[[1]], 1)
  }))
}

supers <- lapply(supers, get_bases)
lns <- sapply(supers, length)
supers <- unlist(supers)
subs <- unlist(purrr::map2(subs, lns, function(x, y) rep(get_bases(x), y)))

allclasses <- sort(unique(c(subs, supers)))

allnames <- unname(unlist(lapply(res, names)))
# these don't have base classes
leftover <- setdiff(allnames, allclasses)
# "GMapOptions" "MapOptions"  "Viewable"

allclasses <- c(allclasses, leftover)

# are we missing entries for any of these?
setdiff(allclasses, unlist(lapply(res, names)))
# "CallbackManager" "HasProps"
# these are both base classes for "Model"

nodes <- data.frame(label = allclasses, id = seq_along(allclasses))
edges <- data.frame(from = match(subs, allclasses), to = match(supers, allclasses))

nodes$has_proto <- FALSE

# add an attribute if we don't have json prototype
for (i in seq_len(nrow(nodes))) {
  jsn <- res2[[as.character(nodes$label[i])]]$json
  if (!is.null(jsn) && nchar(jsn) > 0)
    nodes$has_proto[i] <- TRUE
}

nodes$has_proto[nodes$label == "Model"] <- TRUE
nodes$color <- ifelse(nodes$has_proto, "#97c2fc", "gray")
edges$arrows <- "to"
nodes$highlight <- "blue"

library(visNetwork)
p <- visNetwork(nodes, edges, width = "100%", height = 1000) %>%
  visIgraphLayout(layout = "layout_nicely", randomSeed = 5432345) %>%
  # visIgraphLayout(layout = "layout_with_sugiyama") %>%
  visLayout(improvedLayout = TRUE) %>%
  visEdges(arrows = list(to = list(scaleFactor = 0.5)),
    color = list(color = "#97c2fc", alpha = 0.5, highlight = "blue")) %>%
  visNodes(color = list(highlight = "blue")) %>%
  visInteraction(dragNodes = FALSE)
  # visNodes(shape = "dot", size = 10) %>%
  # visHierarchicalLayout(direction = "LR", treeSpacing = 5000)

visSave(p, file = "~/Desktop/bokehclasses.html", selfcontained = TRUE)


## which ones don't have type

notype <- sapply(res2, function(x) {
  if (length(x$attrs) == 0)
    return(0)
  sum(sapply(x$attrs, function(y) length(y$type) == 0))
})

notype <- notype[notype > 0]
dplyr::data_frame(class = names(notype), n_attr_without_type = notype) %>%
  dplyr::arrange(-n_attr_without_type)

##
##---------------------------------------------------------

# TODO: deal with multiple inheritance
get_inherit <- function(x) {
  if (is.null(x) || length(x) == 0)
    return(NULL)
  unname(sapply(strsplit(x, ",")[[1]], function(y) {
    tail(strsplit(y, "\\.")[[1]], 1)
  }))
}

# for each attr, need to get init_default and init_validator


init_default <- function(val, type) {
  if (is.null(val))
    return("NA")

  if (!is.null(type) && grepl("^Instance", type))
    return(NA)

  return (paste(capture.output(dput(val)), collapse = ""))
}

init_default_check <- function(val, type) {
  if (is.null(val))
    return("NA")

  return (paste(capture.output(dput(val)), collapse = ""))
}

## for init_default
# NULL value = "NA"
# "Bool|Int|Float" + val = val
# "String" + val = "val"
# "Enum" + val = "val"

dflt_res <- list()
for (aa in res2) {
  for (atr in aa$attrs) {
    nm <- atr$name
    if (nm %in% names(aa$dflt) && !is.null(atr$type)) {
      if (is.null(res[[atr$type]])) {
        dflt_res[[atr$type]] <- list(aa$dflt[[atr$name]])
      } else {
        dflt_res[[atr$type]] <- c(dflt_res[[atr$type]], list(aa$dflt[[atr$name]]))
      }
    }
  }
}
# every type has 1 unique default

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
  if (length(attrs) == 0)
    return(NULL)
  names(attrs) <- sapply(attrs, function(x) x$name)
  if (!is.null(a$dflt))
    attrs <- attrs[intersect(names(a$dflt), names(attrs))]

  if (recurse && length(a$base_class) > 0) {
    tmp <- get_attrs(obj, get_inherit(a$base_class), dflt, recurse = TRUE)
    attrs <- c(attrs, tmp)
  }

  dfltnm <- names(dflt)
  for (ii in seq_along(attrs)) {
    if (attrs[[ii]]$name %in% dfltnm)
      attrs[[ii]]$value <- dflt[[attrs[[ii]]$name]]
  }

  attrs
}

# sort(unique(allvals_orig))

paste_nice <- function(..., indent = 0, exdent = NULL, simplify = TRUE) {
  if (is.null(exdent))
    exdent <- indent
  wr <- strwrap(paste0(...), indent = indent, exdent = exdent, simplify = simplify)
  if (!simplify)
    return (wr)
  res <- paste0(wr, collapse = "\n")
  # add spaces around equals
  gsub("([^ ])=([^ ])", "\\1 = \\2", res)
}

get_class_string <- function(a, obj) {
  cls <- a$name
  base <- get_inherit(a$base_class)
  base_attrs <- get_attrs(obj, base, dflt = a$dflt, recurse = TRUE)
  attrs <- get_attrs(obj, a$name, dflt = a$dflt)

  paste0(cls, ' <- R6::R6Class("', cls, '",',
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
}

fixup <- function(x) {
  x <- gsub(": 1\\.0", ": 1", x)
  x <- gsub(": 2\\.0", ": 2", x)
  x <- gsub(": 0\\.0", ": 0", x)
  x <- gsub(": 10\\.0", ": 10", x)
  x <- gsub(": 1483920000000\\.0", ": 1483920000000", x)
  x
}

get_test_string <- function(a) {
  # if there are any instance types, we need to specify them...
  types <- unlist(lapply(a$attr, function(x) x$type))
  idx <- which(grepl("^Instance", types))
  extra_args <- ""
  if (length(idx) > 0) {
    extra_args <- paste0(", ", paste(unlist(lapply(a$attr[idx], function(x) {
      paste0(x$name, " = ", init_default_check(a$dflt[[x$name]], x$type))
    })), collapse = ", "))
  }

  paste0(
'test_that("', a$name, ' matches", {
  obj <- ', a$name, '$new(id = "', a$dflt$id, '"', extra_args, ')
  jsn <- as.character(obj$to_json(pretty = TRUE))
  expect_identical(jsn,\n\'', fixup(strip_white(a$json)), '\')
})

')
}


names(res$layouts)

base_length <- unlist(lapply(res2, function(x) {
  length(get_inherit(x$base_class))
}))

zero_length <- which(base_length == 0)
multi_length <- which(base_length > 1)

no_proto <- which(unlist(lapply(res2, function(x) x$json == "")))

idx <- unique(c(multi_length, no_proto))

res3 <- res2[-idx]

a <- res2[["Model"]]
a <- res2[["LayoutDOM"]]
a <- res2[["TableColumn"]]

a <- res2[["WidgetBox"]]
a <- res2[["Annotation"]]

cat(get_class_string(a, res2))
cat(get_test_string(a))

cat("", file = "R/classes_test.R")
cat("", file = "scripts/classes_test.R")

for (nm in names(res3)) {
  message(nm)
  a <- res3[[nm]]
  cat(get_class_string(a, res2), file = "R/classes_test.R", append = TRUE)
  cat(get_test_string(a), file = "scripts/classes_test.R", append = TRUE)
}


# init_default <- function(type, val) {
#   if (is.list(val) && length(val) == 1)
#     return(list())

#   if (type %in% c("Bool", "Int", "Float"))
#     return(val)

#   if (type == "String")
#     return(paste0("\"", val, "\""))

#   if (grepl("^Enum \\(", type))
#     return(paste0("\"", val, "\""))

#   # Dict ( String, ...) is always named list
#   # default is empty named list
#   if (grepl("^Dict \\(", type))
#     return("structure(list(), .Names = character(0))")

#   # capture.output(dput(res2[[192]]$dflt$js_callbacks))

#   # List ( ) is always unnamed list
#   # default is empty unnamed list
#   if (grepl("^List \\(", type))
#     return("list()")
# }



## try LayoutDOM
##---------------------------------------------------------






