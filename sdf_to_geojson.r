#' Parse an HEC-RAS SDF File
#'
#' @param sdf_file Path to the SDF file to parse.
#' @param what The data to extract from the SDF file.
#' @return A list of parsed data, depending on the `what` argument.
#'
#' @export
parse_sdf = function(sdf_file, what = "cross sections", ...) {

  what = match.arg(what, c("cross sections", "stream network"))

  all_lines = readLines(sdf_file)

  if (what == "cross sections") {
    search_term_start = "BEGIN CROSS-SECTIONS:"
    search_term_end = "END CROSS-SECTIONS:"
  } else if (what == "stream network") {
    search_term_start = "BEGINSTREAMNETWORK:"
    search_term_end = "ENDSTREAMNETWORK:"
  }

  start_line = grep(search_term_start, all_lines)
  end_line = grep(search_term_end, all_lines)

  if (length(start_line) == 0L) {
    stop("Could not find start of ", what, " block.")
  } else if (length(end_line) == 0) {
    stop("Could not find end of ", what, " block.")
  } else if (any(lengths(c(start_line, end_line)) > 1L)) {
    stop("Multiple start or end lines found for ", what, " block.")
  } else if (end_line < start_line) {
    stop("Detected end line for ", what, " block before start line.")
  }

  # Extract the lines between start and end
  block_lines = all_lines[seq(start_line + 1L, end_line - 1L)]

  if (what == "cross sections") {
    parse_xs_block(block_lines, ...)
  } else if (what == "stream network") {
    parse_network_block(block_lines, ...)
  }
}


#' Parse Cross Section Block
#'
#' @param block_lines A character vector of lines containing
#'   a cross section block.
#' @param ... Additional arguments passed to `parse_xs`.
#' @return A list of cross section list objects.
#'
#' @keywords internal
parse_xs_block = function(block_lines, ...) {

  block_starts = grep("CROSS-SECTION:", block_lines)
  block_ends = grep("END:", block_lines)

  if (length(block_starts) != length(block_ends)) {
    stop("Mismatched number of start and end lines in cross section block.")
  } else if (any(block_ends < block_starts)) {
    stop("Detected end line for cross section before start line.")
  }

  all_blocks = lapply(seq_along(block_starts), function(i) {
    start = block_starts[i] + 1L
    end = block_ends[i] - 1L
    block_lines[seq(start + 1L, end - 1L)]
  })

  lapply(all_blocks, parse_xs, ...)
}


#' Parse Cross Section Data
#'
#' @param xs_lines A character vector of lines containing a cross section.
#' @param geometry A character string indicating the type of geometry to parse.
#' @param ... Additional arguments passed to `parse_coordinates`.
#' @return A list of cross section properties.
#'
#' @keywords internal
parse_xs = function(xs_lines, geometry = c("SURFACE LINE", "CUT LINE"), ...) {

  geometry = match.arg(toupper(geometry), c("SURFACE LINE", "CUT LINE"))

  tags = grepl(":", xs_lines)

  tag_lines = xs_lines[tags]
  tag_labels = trimws(gsub("(.+):(.*)", "\\1", tag_lines))
  tag_values = trimws(gsub("(.+):(.*)", "\\2", tag_lines))

  properties = setNames(as.list(tag_values[nzchar(tag_values)]),
    tag_labels[nzchar(tag_values)])

  start_line = grep(geometry, xs_lines) + 1L
  next_tag = grep(geometry, tag_labels) + 1L
  if (next_tag > length(tag_labels)) {
    end_line = length(xs_lines)
  } else {
    end_line = grep(tag_labels[next_tag], xs_lines) - 1L
  }
  coordinate_lines = xs_lines[seq(start_line, end_line)]
  coordinate_df = parse_coordinates(coordinate_lines)

  properties[["TYPE"]] = geometry
  properties[["COORDINATES"]] = coordinate_df
  properties
}


#' Parse Coordinates
#'
#' @param coordinate_lines A character vector of lines containing coordinates.
#' @return A data frame with columns X, Y, and (possibly) Z coordinates.
#'
#' @keywords internal
parse_coordinates = function(coordinate_lines) {
  coords = lapply(coordinate_lines, function(x) {
    matrix(as.numeric(trimws(unlist(strsplit(x, ",")))), nrow = 1L)
  })
  coord_df = do.call(rbind.data.frame, coords)
  names(coord_df) = c("X", "Y", "Z")[seq_len(ncol(coord_df))]

  coord_df
}


#' Parse Stream Network Block
#'
#' @param block_lines A character vector of lines containing
#'   a stream network block.
#' @param ... Additional arguments passed to `parse_reach`.
#' @return A list of cross section list objects.
#'
#' @keywords internal
parse_network_block = function(block_lines, ...) {

  endpoint_lines = grep("ENDPOINT:", block_lines)
  endpoints = parse_endpoints(block_lines[endpoint_lines])

  block_starts = grep("REACH:", block_lines)
  block_ends = grep("END:", block_lines)

  if (length(block_starts) != length(block_ends)) {
    stop("Mismatched number of start and end lines in stream network block.")
  } else if (any(block_ends < block_starts)) {
    stop("Detected end line for stream network before start line.")
  }

  all_blocks = lapply(seq_along(block_starts), function(i) {
    start = block_starts[i] + 1L
    end = block_ends[i] - 1L
    block_lines[seq(start + 1L, end - 1L)]
  })

  lapply(all_blocks, parse_reach, endpoints)

}


#' Parse Endpoint Data
#'
#' @param endpoint_lines A character vector of lines containing endpoints.
#' @param ... Additional arguments passed to `parse_coordinates`.
#' @return A list of reach properties.
#'
#' @keywords internal
parse_endpoints = function(endpoint_lines) {

  endpoint_lines = trimws(gsub("ENDPOINT:", "", endpoint_lines))
  endpoint_coords = parse_coordinates(endpoint_lines)
  names(endpoint_coords)[ncol(endpoint_coords)] = "ID"
  endpoint_coords[, c("ID", "X", "Y", "Z")]
}


#' Parse Reach Data
#'
#' @param reach_lines A character vector of lines containing a reach.
#' @param ... Additional arguments passed to `parse_coordinates`.
#' @return A list of reach properties.
#'
#' @keywords internal
parse_reach = function(reach_lines, endpoints) {

  geometry = "CENTERLINE"

  tags = grepl(":", reach_lines)
  tag_lines = reach_lines[tags]
  tag_labels = trimws(gsub("(.+):(.*)", "\\1", tag_lines))
  tag_values = trimws(gsub("(.+):(.*)", "\\2", tag_lines))

  properties = setNames(as.list(tag_values[nzchar(tag_values)]),
    tag_labels[nzchar(tag_values)])

  # substitute end point ids for actual endpoints
  from_point = properties[["FROM POINT"]]
  if (!is.null(from_point)) {
    from_point = as.numeric(from_point)
    properties[["FROM POINT"]] = endpoints[endpoints$ID == from_point, ]
  }
  to_point = properties[["TO POINT"]]
  if (!is.null(to_point)) {
    to_point = as.numeric(to_point)
    properties[["TO POINT"]] = endpoints[endpoints$ID == to_point, ]
  }

  start_line = grep(geometry, reach_lines) + 1L
  next_tag = grep(geometry, tag_labels) + 1L
  if (next_tag > length(tag_labels)) {
    end_line = length(reach_lines)
  } else {
    end_line = grep(tag_labels[next_tag], reach_lines) - 1L
  }
  coordinate_lines = reach_lines[seq(start_line, end_line)]
  coordinate_df = parse_coordinates(coordinate_lines)

  properties[["TYPE"]] = geometry
  properties[["COORDINATES"]] = coordinate_df
  properties
}


#' Convert Cross Section Data to GeoJSON
#'
#' @param xs A list of cross sections.
#' @return A GeoJSON object representing the cross sections.
#'
#' @export
xs_to_geojson = function(xs) {

  features = lapply(xs, function(x) {
    # sanitize names
    coords = x[["COORDINATES"]]
    if (is.null(coords)) {
      coords = matrix(nrow = 0, ncol = 3)  # Empty matrix if no coordinates
    }
    colnames(coords) = NULL

    x[["COORDINATES"]] = NULL
    names(x) = gsub("\\s+", "_", tolower(names(x)))

    list(
      type = "Feature",
      geometry = list(
        type = "LineString",
        coordinates = coords
      ),
      properties = x
    )
  })

  jsonlite::toJSON(
    list(type = "FeatureCollection", features = features),
    auto_unbox = TRUE,
    digits = 8,
    pretty = TRUE
  )
}
