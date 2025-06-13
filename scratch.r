requireNamespace("jsonlite")
library(sf)
library(ggplot2)
library(ggspatial)

source("sdf_to_geojson.r")

sdf_file = "example.sdf"
outfile = "profiles.gpkg"
unlink(outfile)

# parse sdf file
all_xs = parse_sdf(sdf_file, what = "cross sections",
  geometry = "surface line")

all_reaches = parse_sdf(sdf_file, what = "stream network")

# convert to geojson string
xs_json = to_geojson(all_xs)
reach_json = to_geojson(all_reaches)

# convert to sf object
xs_polylines = st_read(xs_json, crs = 4430)
reach_polylines = st_read(reach_json, crs = 4430)

# convert z-polylines to XYZ points
xs_points = xs_polylines |>
  st_cast("MULTIPOINT") |>
  st_cast("POINT")
# add some attributes
xs_points["X"] = st_coordinates(points)[, 1]
xs_points["Y"] = st_coordinates(points)[, 2]
xs_points["Elev"] = st_coordinates(points)[, 3]
xs_points["Below_WSE"] = points$Elev < points$water_elevation

# write layers to geopackage
st_write(xs_points, outfile, "xs_points")
st_write(xs_polylines, outfile, "xs_lines")
st_write(reach_polylines, outfile, "reach_lines")
