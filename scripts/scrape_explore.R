source("scripts/process_fns.R")

mods <- parse_scraped()

length(mods)
# 207 models!

## investigate all attribute types
##---------------------------------------------------------

allvals_orig <- unlist(lapply(mods, function(el) {
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

valtbl <- sort(table(unname(allvals)))
valtbl

excl <- c("Tuple", "Dict", "Seq", "ColumnData", "RelativeDelta", "Either", "List",
  "Instance", "Enum")
tmp <- valtbl[setdiff(names(valtbl), excl)]

# these are the basic types we need to be able to deal with
cat(paste(names(tmp), collapse = "\n"))

## which ones don't have type
library(dplyr)

notype <- sapply(mods, function(x) {
  if (length(x$attrs) == 0)
    return(0)
  sum(sapply(x$attrs, function(y) length(y$type) == 0))
})

notype <- notype[notype > 0]
dplyr::data_frame(class = names(notype), n_attr_without_type = notype) %>%
  dplyr::arrange(-n_attr_without_type)

## see what defaults each attribute name has
##---------------------------------------------------------

dflt_res <- list()
for (aa in mods) {
  for (atr in aa$attrs) {
    nm <- atr$name
    if (nm %in% names(aa$dflt) && !is.null(atr$type)) {
      if (is.null(dflt_res[[atr$type]])) {
        dflt_res[[atr$type]] <- list(aa$dflt[[atr$name]])
      } else {
        dflt_res[[atr$type]] <- c(dflt_res[[atr$type]], list(aa$dflt[[atr$name]]))
      }
    }
  }
}

tmp <- unlist(lapply(dflt_res, length))

dflt_res$Date
dflt_res[[which.max(tmp)]]

## look at graph of class inheritance
##---------------------------------------------------------

bases <- unlist(lapply(mods, function(el) el$base_class))
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

allnames <- names(mods)
# these don't have base classes
leftover <- setdiff(allnames, allclasses)
# "GMapOptions" "MapOptions"  "Viewable"

allclasses <- c(allclasses, leftover)

# are we missing entries for any of these?
setdiff(allclasses, allnames)
# "CallbackManager" "HasProps"
# these are both base classes for "Model"

nodes <- data.frame(label = allclasses, id = seq_along(allclasses))
edges <- data.frame(from = match(subs, allclasses), to = match(supers, allclasses))

nodes$has_proto <- FALSE

# add an attribute if we don't have json prototype
for (i in seq_len(nrow(nodes))) {
  jsn <- mods[[as.character(nodes$label[i])]]$json
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
