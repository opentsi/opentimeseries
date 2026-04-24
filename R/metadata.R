#' Get All Keys from Metadata
#'
#' Extracts all full time series keys from the metadata.
#' Keys are derived from the hierarchy if present, otherwise from
#' labels$dimnames (excluding dimensions listed in dim_order).
#'
#' @param meta Metadata as a list (from read_meta()).
#' @return Character vector of full keys.
#' @export
#' @examples
#' \dontrun{
#' meta <- read_meta("opentsi/kofethz")
#' get_keys(meta)
#' }
get_keys <- function(meta) {

  check_meta(meta)

  prefix <- paste(meta$country, meta$provider, meta$dataset, sep = ".")
  paths <- get_paths(meta)

  if (length(paths) == 0) {
    return(character())
  }

  vapply(paths, function(path) {
    paste(c(prefix, path), collapse = ".")
  }, character(1))
}


#' Resolve Metadata to Key-Label Mappings
#'
#' Maps all keys to their human-readable labels.
#'
#' @param meta Metadata as a list (from read_meta()).
#' @param lang Language code for labels (default "en").
#' @return A data.frame with columns: key, label_full, label_short.
#' @export
#' @examples
#' \dontrun{
#' meta <- read_meta("opentsi/kofethz")
#' resolve_meta(meta)
#' resolve_meta(meta, lang = "de")
#' }
resolve_meta <- function(meta, lang = "en") {

  check_meta(meta)

  prefix <- paste(meta$country, meta$provider, meta$dataset, sep = ".")
  title <- get_ml_label(meta$title, lang)
  source <- get_ml_label(meta$source_name, lang)
  freq <- meta$dataset_frequency %||% ""
  unit <- get_ml_label(meta$units$all, lang)

  # Get paths from hierarchy or dimnames
  paths <- get_paths(meta)

  if (length(paths) == 0) {
    return(data.frame(
      key = character(),
      label_full = character(),
      label_short = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(paths, function(path) {
    key <- paste(c(prefix, path), collapse = ".")

    # Get labels for each path segment
    seg_labels <- vapply(path, function(seg) {
      lbl <- lookup_label(seg, meta$labels, lang)
      if (is.null(lbl) || lbl == "") seg else lbl
    }, character(1))

    # Full: source, title, segments, unit
    full_parts <- c(source, title, seg_labels)
    if (unit != "") full_parts <- c(full_parts, unit)
    label_full <- paste(full_parts, collapse = ", ")

    # Short: title, freq, last segment
    label_short <- paste(c(title, freq, seg_labels[length(seg_labels)]), collapse = ", ")

    data.frame(key = key, label_full = label_full, label_short = label_short,
               stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}


#' Print Resolved Metadata
#'
#' Prints key-label mappings in a readable format.
#'
#' @param resolved Output from resolve_meta().
#' @param format Either "full" or "short".
#' @return Invisibly returns the input.
#' @export
print_resolved <- function(resolved, format = c("full", "short")) {

  format <- match.arg(format)

  for (i in seq_len(nrow(resolved))) {
    label <- if (format == "full") resolved$label_full[i] else resolved$label_short[i]
    cat(sprintf("%s\n  -> %s\n\n", resolved$key[i], label))
  }

  invisible(resolved)
}


# --- Internal helpers ---

get_paths <- function(meta) {
  # Try hierarchy first
  paths <- traverse_hierarchy(meta$hierarchy)

  if (length(paths) > 0) {
    return(paths)
  }

  # Fall back to labels$dimnames
  if (!is.null(meta$labels$dimnames)) {
    all_names <- names(meta$labels$dimnames)
    dim_order <- unlist(meta$dim_order)
    series_names <- setdiff(all_names, dim_order)

    if (length(series_names) > 0) {
      return(lapply(series_names, function(x) x))
    }
  }

  list()
}

check_meta <- function(meta) {
  if (!is.list(meta)) {
    stop("meta must be a list (from read_meta())")
  }
  for (f in c("country", "provider", "dataset")) {
    if (is.null(meta[[f]]) || meta[[f]] == "") {
      stop(sprintf("Missing required field: %s", f))
    }
  }
}

traverse_hierarchy <- function(h, path = character()) {
  if (!is.list(h) || length(h) == 0) {
    if (length(path) > 0) return(list(path))
    return(list())
  }

  # Unnamed list = JSON array of leaf values
  if (is.null(names(h))) {
    vals <- unlist(h)
    if (length(vals) > 0) return(lapply(vals, function(v) c(path, v)))
    if (length(path) > 0) return(list(path))
    return(list())
  }

  paths <- list()
  for (k in names(h)) {
    child <- h[[k]]
    if (is.list(child) && length(child) > 0) {
      paths <- c(paths, traverse_hierarchy(child, c(path, k)))
    } else if (is.character(child) && length(child) > 0) {
      # Scalar string: dimension k has leaf value(s) — use value as path element
      paths <- c(paths, lapply(child, function(v) c(path, v)))
    } else {
      paths <- c(paths, list(c(path, k)))
    }
  }
  paths
}

get_ml_label <- function(obj, lang = "en") {
  if (is.null(obj)) return("")
  if (!is.list(obj)) return(as.character(obj))
  if (!is.null(obj[[lang]]) && obj[[lang]] != "") return(obj[[lang]])
  if (!is.null(obj[["en"]]) && obj[["en"]] != "") return(obj[["en"]])
  vals <- unlist(obj)
  if (length(vals) > 0) return(vals[1])
  ""
}

lookup_label <- function(segment, labels, lang) {
  if (is.null(labels)) return(NULL)
  for (grp in names(labels)) {
    g <- labels[[grp]]
    if (is.list(g) && segment %in% names(g)) {
      return(get_ml_label(g[[segment]], lang))
    }
  }
  NULL
}

`%||%` <- function(a, b) if (is.null(a)) b else a


#' Update last_updated Timestamp in Metadata JSON
#'
#' Updates the last_updated field in metadata.json to current UTC time.
#' Designed for use in GHA workflows after data updates. Works directly
#' with JSON (no yaml dependency).
#'
#' @param json_path Path to the metadata JSON file.
#'   Defaults to "inst/metadata.json".
#' @return Invisibly returns the updated metadata as a list.
#' @importFrom jsonlite fromJSON toJSON
#' @export
#' @examples
#' \dontrun{
#' # Update timestamp in default location
#' update_last_updated()
#'
#' # Custom path
#' update_last_updated("path/to/metadata.json")
#' }
update_last_updated <- function(json_path = "inst/metadata.json") {

  if (!file.exists(json_path)) {
    stop(sprintf("Metadata file not found: %s", json_path))
  }

  meta <- fromJSON(json_path, simplifyVector = FALSE)

  meta$last_updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  json_content <- toJSON(meta, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, json_path)

  message(sprintf("Updated last_updated: %s", meta$last_updated))

  invisible(meta)
}


#' Read Metadata from Archive
#'
#' Reads and parses metadata.json from a local or remote OpenTSI archive.
#'
#' @param archive Either a remote repository path (e.g., "opentsi/kofethz")
#'   or a local path to an archive directory or metadata.json file.
#' @param ref Git ref (branch, tag, or commit) for remote archives. Defaults to "main".
#' @return Metadata as a list.
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' # Remote
#' meta <- read_meta("opentsi/kofethz")
#'
#' # Local archive directory
#' meta <- read_meta("path/to/my.dataset")
#'
#' # Local JSON file directly
#' meta <- read_meta("path/to/metadata.json")
#' }
read_meta <- function(archive, ref = "main") {

 # Check if local path
  if (file.exists(archive)) {
    # Direct JSON file
    if (grepl("\\.json$", archive)) {
      return(fromJSON(archive, simplifyVector = FALSE))
    }
    # Archive directory
    json_path <- file.path(archive, "inst", "metadata.json")
    if (file.exists(json_path)) {
      return(fromJSON(json_path, simplifyVector = FALSE))
    }
    stop(sprintf("metadata.json not found in: %s/inst/", archive))
  }

  # Remote GitHub archive
  url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/inst/metadata.json",
    archive,
    ref
  )

  fromJSON(url, simplifyVector = FALSE)
}
