# Load required library
library(jsonlite)

# Set file paths
input_file <- "C:\\Users\\G2ECHMCK\\Workspace\\Scratch\\Carpenter_Creek (113)\\allxs.txt"
output_file <- "C:\\Users\\G2ECHMCK\\Workspace\\Scratch\\Carpenter_Creek (113)\\all_xs_now.json"

lines <- readLines(input_file)
features <- list()
i <- 1
while (i <= length(lines)) {
  line <- trimws(lines[i])

  if (line == "BEGIN HEADER:") {
    i <- i + 1
    # Parse metadata and find SURFACE LINE
    while (i <= length(lines)) {
      l <- trimws(lines[i])
      if (l == "END HEADER:") {
        break
      }
    }
  } else if (line == "BEGINSTREAMNETWORK:") {
    i <- i + 1
    # Parse metadata and find SURFACE LINE
    while (i <= length(lines)) {
      l <- trimws(lines[i])
      if (l == "ENDSTREAMNETWORK:") {
        break
      }
    }
  } else if (line == "CROSS-SECTION:") {
    # Initialize metadata
    stream_id <- NA
    reach_id <- NA
    station <- NA
    water_elevation <- NA
    coords <- list()
    i <- i + 1
    # Parse metadata and find SURFACE LINE
    while (i <= length(lines)) {
      l <- trimws(lines[i])
      if (grepl("^STREAM ID:", l)) {
        stream_id <- sub("^STREAM ID:\\s*", "", l)
      } else if (grepl("^REACH ID:", l)) {
        reach_id <- sub("^REACH ID:\\s*", "", l)
      } else if (grepl("^STATION:", l)) {
        station <- as.numeric(sub("^STATION:\\s*", "", l))
      } else if (grepl("^WATER ELEVATION:", l)) {
        water_elevation <- as.numeric(sub("^WATER ELEVATION:\\s*", "", l))
      } else if (grepl("^NODE NAME:", l)) {
        node_name = gsub("^NODE NAME:\\s*", "", l)
      } else if (grepl("^SURFACECUT LINE:", l)) {
        i <- i + 1
        # Parse coordinates until END:
        while (i <= length(lines) && trimws(lines[i]) != "END:") {
          coord_line <- trimws(lines[i])
          # Only process non-empty lines
          if (nchar(coord_line) > 0) {
            # Split by comma, trim whitespace, and convert to numeric
            vals <- trimws(unlist(strsplit(coord_line, ",")))
            # but don't do anything with it
          }
          i <- i + 1
        }
        break
      } else if (grepl("^SURFACE LINE:", l)) {
        i <- i + 1
        # Parse coordinates until END:
        while (i <= length(lines) && trimws(lines[i]) != "END:") {
          coord_line <- trimws(lines[i])
          # Only process non-empty lines
          if (nchar(coord_line) > 0) {
            # Split by comma, trim whitespace, and convert to numeric
            vals <- trimws(unlist(strsplit(coord_line, ",")))
            if (length(vals) == 3 && all(!is.na(as.numeric(vals)))) {
              coords[[length(coords)+1]] <- as.numeric(vals)
            }
          }
          i <- i + 1
        }
        break
      } else if (l == "END:") {
        break
      }
      i <- i + 1
    }
    # Only add feature if coordinates exist
    if (length(coords) > 0) {
      feature <- list(
        type = "Feature",
        geometry = list(
          type = "LineString",
          coordinates = coords
        ),
        properties = list(
          stream_id = stream_id,
          reach_id = reach_id,
          station = station,
          water_elevation = water_elevation
        )
      )
      features[[length(features)+1]] <- feature
    }
  }
  i <- i + 1
}

geojson <- list(
  type = "FeatureCollection",
  features = features
)

write(toJSON(geojson, auto_unbox = TRUE, digits = 8, pretty = TRUE), output_file)