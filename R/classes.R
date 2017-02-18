Ref <- function(type, id) {
  structure(list(type = type, id = id), class = "Ref")
}


#' @importFrom R6 R6Class
#' @importFrom jsonlite unbox toJSON
#' @importFrom digest digest

## class Model
##---------------------------------------------------------
# Base class for all objects stored in Bokeh Document instances.

Model <- R6::R6Class("Model",
  public = list(
    initialize = function(id = NA, name = NA, tags = character(0), js_callbacks = NULL) {
      if (is.na(id))
        id <- digest::digest(Sys.time())
      private$id <- validate_scalar(id, "character")
      # TODO: validate js_callbacks
      private$name <- validate_scalar(name, "character")
      private$tags <- validate_vector(tags, "character")
    },
    to_json = function(pretty = FALSE) {
      els <- as.list(private)
      jsonlite::toJSON(els[order(names(els))], pretty = pretty)
    }
  ),
  private = list(
    id = NULL,
    # A mapping of attribute names to lists of CustomJS callbacks,
    #   to be set up on BokehJS side when the document is created
    js_callbacks = NULL, # Dict ( String , List ( Instance ( CustomJS ) ) )
    # An arbitrary, user-supplied name for this model
    name = NULL,         # String
    # An optional list of arbitrary, user-supplied values to attach to this model
    tags = NULL         # List ( Any )
  )
)

# '{
#   "id": "ca214b1b-b6e5-4f1e-90d7-2588bcb83099",
#   "js_callbacks": {},
#   "name": null,
#   "tags": []
# }'

## class LayoutDOM
##   Bases: bokeh.model.Model
##     An abstract base class for layout components
##     LayoutDOM is not generally useful to instantiate on its own.
##---------------------------------------------------------

LayoutDOM <- R6::R6Class("LayoutDOM",
  inherit = Model,
  public = list(
    initialize = function(
      id = NA, name = NA, tags = character(0), js_callbacks = NULL,
      css_classes = NA, disabled = FALSE, height = NA, width = NA,
      sizing_mode = "fixed"
    ) {
      super$initialize(id, name, tags, js_callbacks)
      private$css_classes <- validate_vector(css_classes, "character")
      private$disabled <- validate_scalar(disabled, "logical")
      private$height <- validate_scalar(height, "numeric")
      private$width <- validate_scalar(width, "numeric")
      vals <- c("fixed", "scale_width", "scale_height", "scale_both")
      private$sizing_mode <- validate_enum(sizing_mode, vals)
    }
  ),
  private = list(
    # A list of css class names to add to this DOM element
    css_classes = NULL, # Seq ( String )
    # Whether the widget will be disabled when rendered
    # If True, the widget will be greyed-out, and not respond to UI events
    disabled = NULL,    # Bool
    # An optional height for the component (in pixels)
    height = NULL,      # Int
    # How the item being displayed should size itself
    # nolint see http://bokeh.pydata.org/en/latest/docs/reference/models/layouts.html#bokeh.models.layouts.LayoutDOM.sizing_mode
    sizing_mode = NULL, # Enum ( SizingMode )
    # An optional width for the component (in pixels)
    width = NULL        # Int
  )
)

## class Plot
##   Bases: bokeh.models.layouts.LayoutDOM
##     Model representing a plot, containing glyphs, guides, annotations.
##---------------------------------------------------------

Plot <- R6::R6Class("Plot",
  inherit = LayoutDOM,
  public = list(
    initialize = function(
      id = NA, name = NA, tags = character(0), js_callbacks = NULL,
      css_classes = NA, disabled = FALSE, height = NA, width = NA,
      sizing_mode = "fixed",
      above = list(), below = list(), left = list(), right = list()
    ) {
      super$initialize(id, name, tags, js_callbacks, css_classes, disabled,
        height, width, sizing_mode)
      private$above <- validate_list_class(above, "Ref")
      private$below <- validate_list_class(below, "Ref")
      private$left <- validate_list_class(left, "Ref")
      private$right <- validate_list_class(right, "Ref")
    }
  ),
  private = list(
    # A list of renderers to occupy the area above/below/left/right of the plot
    above = NULL, # List ( Instance ( Renderer ) )
    below = NULL, # List ( Instance ( Renderer ) )
    left = NULL,  # List ( Instance ( Renderer ) )
    right = NULL # List ( Instance ( Renderer ) )
  )
)

# obj <- Plot$new(id = "ca214b1b-b6e5-4f1e-90d7-2588bcb83099")
# obj$to_json(pretty = TRUE)


# # obj <- Plot$new(id = "5b5f2c41-3c1c-4965-aa25-73922f47892e")
# # jsn <- obj$to_json(pretty = TRUE)
# # jsn == '{
# #   "above": [],
# #   "background_fill_alpha": {
# #     "value": 1.0
# #   },
# #   "background_fill_color": {
# #     "value": "#ffffff"
# #   },
# #   "below": [],
# #   "border_fill_alpha": {
# #     "value": 1.0
# #   },
# #   "border_fill_color": {
# #     "value": "#ffffff"
# #   },
# #   "css_classes": null,
# #   "disabled": false,
# #   "extra_x_ranges": {},
# #   "extra_y_ranges": {},
# #   "h_symmetry": true,
# #   "height": null,
# #   "hidpi": true,
# #   "id": "5b5f2c41-3c1c-4965-aa25-73922f47892e",
# #   "js_callbacks": {},
# #   "left": [],
# #   "lod_factor": 10,
# #   "lod_interval": 300,
# #   "lod_threshold": 2000,
# #   "lod_timeout": 500,
# #   "min_border": 5,
# #   "min_border_bottom": null,
# #   "min_border_left": null,
# #   "min_border_right": null,
# #   "min_border_top": null,
# #   "name": null,
# #   "outline_line_alpha": {
# #     "value": 1.0
# #   },
# #   "outline_line_cap": "butt",
# #   "outline_line_color": {
# #     "value": "#e5e5e5"
# #   },
# #   "outline_line_dash": [],
# #   "outline_line_dash_offset": 0,
# #   "outline_line_join": "miter",
# #   "outline_line_width": {
# #     "value": 1
# #   },
# #   "plot_height": 600,
# #   "plot_width": 600,
# #   "renderers": [],
# #   "right": [],
# #   "sizing_mode": "fixed",
# #   "tags": [],
# #   "title": {
# #     "id": "e0831177-9fc9-4ab8-a385-fac5a1584399",
# #     "type": "Title"
# #   },
# #   "title_location": "above",
# #   "tool_events": {
# #     "id": "2db04914-27d3-4f31-8ef6-adccfa2b6e55",
# #     "type": "ToolEvents"
# #   },
# #   "toolbar": {
# #     "id": "a21d402b-cd34-424b-81c7-5d981d92b7c5",
# #     "type": "Toolbar"
# #   },
# #   "toolbar_location": "right",
# #   "toolbar_sticky": true,
# #   "v_symmetry": false,
# #   "webgl": false,
# #   "width": null,
# #   "x_mapper_type": "auto",
# #   "x_range": null,
# #   "y_mapper_type": "auto",
# #   "y_range": null
# # }'


# # NumberSpec
# # ColorSpec


# renderers
# # property type: List ( Instance ( Renderer ) )
# # A list of all renderers for this plot, including guides and annotations in addition to glyphs and markers.
# # This property can be manipulated by hand, but the add_glyph and add_layout methods are recommended to help make sure all necessary setup is performed.

# x_range
# # property type: Instance ( Range )
# # The (default) data range of the horizontal dimension of the plot.

# y_range
# # property type: Instance ( Range )
# # The (default) data range of the vertical dimension of the plot.

# extra_x_ranges
# # property type: Dict ( String , Instance ( Range ) )
# # Additional named ranges to make available for mapping x-coordinates.
# # This is useful for adding additional axes.

# extra_y_ranges
# # property type: Dict ( String , Instance ( Range ) )
# # Additional named ranges to make available for mapping y-coordinates.
# # This is useful for adding additional axes.


# background_fill_alpha
# # property type: NumberSpec
# # The fill alpha for the plot background style.

# background_fill_color
# # property type: ColorSpec
# # The fill color for the plot background style.

# border_fill_alpha
# # property type: NumberSpec
# # The fill alpha for the plot border style.

# border_fill_color
# # property type: ColorSpec
# # The fill color for the plot border style.

# hidpi
# # property type: Bool
# # Whether to use HiDPI mode when available.

# inner_height
# # property type: Int
# # This is the exact height of the plotting canvas, i.e. the height of the actual plot, without toolbars etc. Note this is computed in a web browser, so this property will work only in backends capable of bidirectional communication (server, notebook).
# # Note
# # This is an experimental feature and the API may change in near future.

# inner_width
# # property type: Int
# # This is the exact width of the plotting canvas, i.e. the width of the actual plot, without toolbars etc. Note this is computed in a web browser, so this property will work only in backends capable of bidirectional communication (server, notebook).
# # Note
# # This is an experimental feature and the API may change in near future.

# lod_factor
# # property type: Int
# # Decimation factor to use when applying level-of-detail decimation.

# lod_interval
# # property type: Int
# # Interval (in ms) during which an interactive tool event will enable level-of-detail downsampling.

# lod_threshold
# # property type: Int
# # A number of data points, above which level-of-detail downsampling may be performed by glyph renderers. Set to None to disable any level-of-detail downsampling.

# lod_timeout
# # property type: Int
# # Timeout (in ms) for checking whether interactive tool events are still occurring. Once level-of-detail mode is enabled, a check is made every lod_timeout ms. If no interactive tool events have happened, level-of-detail mode is disabled.

# min_border
# # property type: Int
# # A convenience property to set all all the min_border_X properties to the same value. If an individual border property is explicitly set, it will override min_border.

# min_border_bottom
# # property type: Int
# # Minimum size in pixels of the padding region below the bottom of the central plot region.
# # Note
# # This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.

# min_border_left
# # property type: Int
# # Minimum size in pixels of the padding region to the left of the central plot region.
# # Note
# # This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.

# min_border_right
# # property type: Int
# # Minimum size in pixels of the padding region to the right of the central plot region.
# # Note
# # This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.

# min_border_top
# # property type: Int
# # Minimum size in pixels of the padding region above the top of the central plot region.
# # Note
# # This is a minimum. The padding region may expand as needed to accommodate titles or axes, etc.

# outline_line_alpha
# # property type: NumberSpec
# # The line alpha for the plot border outline.

# outline_line_cap
# # property type: Enum ( LineCap )
# # The line cap for the plot border outline.

# outline_line_color
# # property type: ColorSpec
# # The line color for the plot border outline.

# outline_line_dash
# # property type: DashPattern
# # The line dash for the plot border outline.

# outline_line_dash_offset
# # property type: Int
# # The line dash offset for the plot border outline.

# outline_line_join
# # property type: Enum ( LineJoin )
# # The line join for the plot border outline.

# outline_line_width
# # property type: NumberSpec
# # The line width for the plot border outline.

# plot_height
# # property type: Int
# # Total height of the entire plot (including any axes, titles, border padding, etc.)
# # Note
# # This corresponds directly to the height of the HTML canvas that will be used.

# plot_width
# # property type: Int
# # Total width of the entire plot (including any axes, titles, border padding, etc.)
# # Note
# # This corresponds directly to the width of the HTML canvas that will be used.

# title
# # property type: TitleProp
# # A title for the plot. Can be a text string or a Title annotation. Default is Title(text=””).

# title_location
# # property type: Enum ( Location )
# # Where the title will be located. Titles on the left or right side will be rotated.

# tool_events
# # property type: Instance ( ToolEvents )
# # A ToolEvents object to share and report tool events.

# toolbar
# # property type: Instance ( Toolbar )
# # The toolbar associated with this plot which holds all the tools.
# # The toolbar is automatically created with the plot.

# toolbar_location
# # property type: Enum ( Location )
# # Where the toolbar will be located. If set to None, no toolbar will be attached to the plot.

# toolbar_sticky
# # property type: Bool
# # Stick the toolbar to the edge of the plot. Default: True. If False, the toolbar will be outside of the axes, titles etc.

# h_symmetry
# # property type: Bool
# # Whether the total horizontal padding on both sides of the plot will be made equal (the left or right padding amount, whichever is larger).

# v_symmetry
# # property type: Bool
# # Whether the total vertical padding on both sides of the plot will be made equal (the top or bottom padding amount, whichever is larger).

# webgl
# # property type: Bool
# # Whether WebGL is enabled for this plot. If True, the glyphs that support this will render via WebGL instead of the 2D canvas.

# x_mapper_type
# # property type: Either ( Auto , String )
# # What kind of mapper to use to convert x-coordinates in data space into x-coordinates in screen space.
# # Typically this can be determined automatically, but this property can be useful to, e.g., show datetime values as floating point “seconds since epoch” instead of formatted dates.

# y_mapper_type
# # property type: Either ( Auto , String )
# # What kind of mapper to use to convert y-coordinates in data space into y-coordinates in screen space.
# # Typically this can be determined automatically, but this property can be useful to, e.g., show datetime values as floating point “seconds since epoch” instead of formatted dates

# axis
# # Splattable list of Axis objects.

# background_fill

# border_fill

# grid
# # Splattable list of Grid objects.

# legend
# # Splattable list of Legend objects.


# logo

# responsive

# title_standoff

# title_text_align

# title_text_alpha

# title_text_baseline

# title_text_color

# title_text_font

# title_text_font_size

# title_text_font_style

# tools

# xaxis
# # Splattable list of Axis objects for the x dimension.

# xgrid
# # Splattable list of Grid objects for the x dimension.

# yaxis
# # Splattable list of Axis objects for the y dimension.

# ygrid
# # Splattable list of Grid objects for the y dimension.

# JSON Prototype





# ## Toolbar

# ## ToolEvents

# ## Toolbar

