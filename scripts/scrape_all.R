urlsb <- c("annotations.html", "arrow_heads.html", "axes.html", "callbacks.html", "formatters.html", "glyphs.html", "grids.html", "images.html", "layouts.html", "map_plots.html", "mappers.html", "markers.html", "plots.html", "ranges.html", "renderers.html", "sources.html", "tickers.html", "tiles.html", "tools.html", "transforms.html", "widgets.buttons.html", "widgets.groups.html", "widgets.icons.html", "widgets.inputs.html", "widgets.markups.html", "widgets.panels.html", "widgets.tables.html", "widgets.widget.html", "glyphs/annular_wedge.html", "glyphs/annulus.html", "glyphs/arc.html", "glyphs/bezier.html", "glyphs/ellipse.html", "glyphs/hbar.html", "glyphs/image.html", "glyphs/image_rgba.html", "glyphs/image_url.html", "glyphs/line.html", "glyphs/multi_line.html", "glyphs/oval.html", "glyphs/patch.html", "glyphs/patches.html", "glyphs/quad.html", "glyphs/quadratic.html", "glyphs/ray.html", "glyphs/rect.html", "glyphs/segment.html", "glyphs/text.html", "glyphs/vbar.html", "glyphs/wedge.html")

urls <- paste0("http://bokeh.pydata.org/en/latest/docs/reference/models/", urlsb)

outfiles <- gsub("html", "json", urlsb)
outfiles <- gsub("/", "_", outfiles)
outfiles <- paste0("_ignore/scrape_pages/", outfiles)

require(RSelenium)
pJS <- phantom()
Sys.sleep(5) # give the binary a moment
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()

script <- "
var items = $('dl.class');
var classes = items.map(function() {
  var className = $($($(this).children('dt').first()[0]).find('.descname')[0]).html()
  var dd = $($(this).children('dd'))
  var classText = $(dd[0]).children('p').map(function() {
    return $(this).text();
  }).get();
  var dl = $(dd[0]).children('dl.attribute')
  var attrs = dl.map(function() {
    return ({
      name: $(this).find('code.descname').text(),
      descs: $(this).find('dd > p').map(function() {
        return $(this).text()
      }).get()
    });
  }).get()
  var json = $($(this).next('div.expander')[0]).find('div.expander-content').text()
  return ({ name: className, text: classText, attrs: attrs, json: json });
}).get()
return JSON.stringify(classes);
"

for (ii in seq_along(outfiles)) {
  message(outfiles[ii])
  remDr$navigate(urls[ii])
  Sys.sleep(2)
  a <- remDr$executeScript(script)
  cat(a[[1]], file = outfiles[ii])
}
