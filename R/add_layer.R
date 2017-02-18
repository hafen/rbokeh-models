# to add a layer:
# - create GlyphRenderer model
#   - create glyph model (e.g. "Circle")
#   - add glyph model to object
#   - add glyph reference as "glyph" attribute
#   - optionally create nonselection_glyph, selection_glyph attributes in similar fashion
#   - create ColumnDataSource model
#   - add ColumnDataSource model to object
#   - add ColumnDataSource reference as "data_source" attribute
# - add GlyphRenderer reference to "renderers"
# - add GlyphRenderer model to object
