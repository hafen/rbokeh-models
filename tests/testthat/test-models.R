context("models")

protos <- list()
protos$Model <- '{
  "id": "ca214b1b-b6e5-4f1e-90d7-2588bcb83099",
  "js_callbacks": {},
  "name": null,
  "tags": []
}'
protos$LayoutDOM <- '{
  "css_classes": null,
  "disabled": false,
  "height": null,
  "id": "373908d6-3b02-4dfd-a5e6-1e84d8a927fa",
  "js_callbacks": {},
  "name": null,
  "sizing_mode": "fixed",
  "tags": [],
  "width": null
}'
protos$Plot <- '{
  "above": [],
  "background_fill_alpha": {
    "value": 1.0
  },
  "background_fill_color": {
    "value": "#ffffff"
  },
  "below": [],
  "border_fill_alpha": {
    "value": 1.0
  },
  "border_fill_color": {
    "value": "#ffffff"
  },
  "css_classes": null,
  "disabled": false,
  "extra_x_ranges": {},
  "extra_y_ranges": {},
  "h_symmetry": true,
  "height": null,
  "hidpi": true,
  "id": "5b5f2c41-3c1c-4965-aa25-73922f47892e",
  "js_callbacks": {},
  "left": [],
  "lod_factor": 10,
  "lod_interval": 300,
  "lod_threshold": 2000,
  "lod_timeout": 500,
  "min_border": 5,
  "min_border_bottom": null,
  "min_border_left": null,
  "min_border_right": null,
  "min_border_top": null,
  "name": null,
  "outline_line_alpha": {
    "value": 1.0
  },
  "outline_line_cap": "butt",
  "outline_line_color": {
    "value": "#e5e5e5"
  },
  "outline_line_dash": [],
  "outline_line_dash_offset": 0,
  "outline_line_join": "miter",
  "outline_line_width": {
    "value": 1
  },
  "plot_height": 600,
  "plot_width": 600,
  "renderers": [],
  "right": [],
  "sizing_mode": "fixed",
  "tags": [],
  "title": {
    "id": "e0831177-9fc9-4ab8-a385-fac5a1584399",
    "type": "Title"
  },
  "title_location": "above",
  "tool_events": {
    "id": "2db04914-27d3-4f31-8ef6-adccfa2b6e55",
    "type": "ToolEvents"
  },
  "toolbar": {
    "id": "a21d402b-cd34-424b-81c7-5d981d92b7c5",
    "type": "Toolbar"
  },
  "toolbar_location": "right",
  "toolbar_sticky": true,
  "v_symmetry": false,
  "webgl": false,
  "width": null,
  "x_mapper_type": "auto",
  "x_range": null,
  "y_mapper_type": "auto",
  "y_range": null
}'

test_that("Model matches", {
  obj <- Model$new(id = "ca214b1b-b6e5-4f1e-90d7-2588bcb83099")
  jsn <- as.character(obj$to_json(pretty = TRUE))
  expect_identical(jsn, protos$Model)
})

test_that("LayoutDOM matches", {
  obj <- LayoutDOM$new(id = "373908d6-3b02-4dfd-a5e6-1e84d8a927fa")
  jsn <- as.character(obj$to_json(pretty = TRUE))
  expect_identical(jsn, protos$LayoutDOM)
})


test_that("LayoutDOM matches", {
  obj <- LayoutDOM$new(id = "373908d6-3b02-4dfd-a5e6-1e84d8a927fa")
  jsn <- as.character(obj$to_json(pretty = TRUE))
  expect_identical(jsn,
'{
  "css_classes": null,
  "disabled": false,
  "height": null,
  "id": "373908d6-3b02-4dfd-a5e6-1e84d8a927fa",
  "js_callbacks": {},
  "name": null,
  "sizing_mode": "fixed",
  "tags": [],
  "width": null
}')
})






