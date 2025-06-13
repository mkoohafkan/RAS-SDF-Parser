requireNamespace("jsonlite")
library(sf)
library(ggplot2)
library(ggspatial)
source("sdf_to_geojson.r")

setwd("C:/Users/G2ECHMCK/Workspace/Scratch/Carpenter_Creek (113)")

outfile = "carpenter_creek_profiles.gpkg"
unlink(outfile)

sdf_files = list(
  "500yr" = "Simulations/carpenter_creek_ras.RASexport.500yr.sdf",
  "100yr" = "Simulations/carpenter_creek_ras.RASexport.100yr.sdf"
)

for (i in seq_along(sdf_files)) {
  sdf_file = sdf_files[[i]]
  layer_name = names(sdf_files)[i]

  # parse sdf file
  all_xs = parse_sdf(sdf_file, what = "cross sections",
    geometry = "surface line")

  # convert to geojson string
  xs_json = xs_to_geojson(all_xs)

  # convert to sf object
  rr = st_read(xs_json, crs = 4430)


  # convert z-polylines to XYZ points
  points = polylines |>
    st_cast("MULTIPOINT") |>
    st_cast("POINT")
  # add some attributes
  points["X"] = st_coordinates(points)[,1]
  points["Y"] = st_coordinates(points)[,2]
  points["Elev"] = st_coordinates(points)[,3]
  points["Below_WSE"] = points$Elev < points$water_elevation

  # write points layer to geopackage
  points |>
    st_write(outfile, layer_name)

  # plot with ggplot
  #ggplot(points) +
  #  annotation_map_tile(zoom = 15) +
  #  geom_sf()

}
