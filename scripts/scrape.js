/////// to get all urls

// click the 'bokeh.models' link
$('li.toctree-l1.current.show').find('li > a').map(function() { return $(this).attr('href') }).get().join('\n')

// click on bokeh.models.glyphs
$('li.toctree-l2.current').find('li > a').map(function() { return $(this).attr('href') }).get().join('\n')

/////// scraping script

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

  return ({
    name: className,
    text: classText,
    attrs: attrs,
    json: json
  });
}).get()

JSON.stringify(classes);

// var b = JSON.stringify(classes);
// b.split('\\').join('\\\\');
