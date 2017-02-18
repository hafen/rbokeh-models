files <- list.files("_ignore/scrape_pages/", full.names = TRUE)
fb <- basename(files)
fb <- gsub("\\.json", "", fb)
fb <- gsub("widgets\\.", "widgets_", fb)

res <- list()

for (ii in seq_along(files)) {
  message(fb[ii])
  a <- jsonlite::fromJSON(paste(readLines(files[ii]), collapse = ""),
    simplifyVector = FALSE)

  for (jj in seq_along(a)) {
    js <- try(jsonlite::fromJSON(a[[jj]]$json))
    if (inherits(js, "try-error"))
      js <- NULL
    txt <- unlist(a[[jj]]$text)
    idx <- grepl("Bases: ", txt)
    basecls <- gsub("Bases: ", "", txt[idx])
    txt <- gsub("\n", " ", paste(txt[!idx], collapse = "\n"))
    a[[jj]]$base_class <- basecls
    a[[jj]]$desc <- txt
    a[[jj]]$txt <- NULL
    a[[jj]]$json <- js
    for (kk in seq_along(a[[jj]]$attrs)) {
      att <- a[[jj]]$attrs[[kk]]
      dsc <- unlist(att$desc)
      idx <- grepl("property type:", dsc)
      type <- gsub("property type: ", "", dsc[idx])
      dsc <- gsub("\n", " ", paste(dsc[!idx], collapse = "\n"))
      val <- ""
      if (!is.null(js)) {
        val <- js[[att$name]]
        if (is.list(val))
          val <- jsonlite::toJSON(val, auto_unbox = TRUE)
      }
      a[[jj]]$attrs[[kk]] <- list(
        type = type,
        desc = dsc,
        value = val
      )
    }
  }
  res[[fb[ii]]] <- a
}

sapply(res, length)
sum(sapply(res, length))
# 193 models!!!!

allvals <- unlist(lapply(res, function(el) {
  lapply(el, function(x) {
    lapply(x$attrs, function(y) {
      y$type
    })
  })
}))

tail(sort(table(allvals)))
