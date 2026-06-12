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
    stop("meta must be a list (output of read_metadata())")
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


#' Read Metadata from Archive
#'
#' Reads and parses metadata.json from a local or remote OpenTSI archive.
#'
#' @param archive Either a remote repository path (e.g., "opentsi/kofethz")
#'   or a local path to an archive directory or metadata.json file.
#' @param ref Git ref (branch, tag, or commit) for remote archives. Defaults
#'   to \code{NULL}, which tries \code{"main"} then \code{"master"}.
#' @return Metadata as a list.
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_error req_perform resp_body_string
#' @export
#' @examples
#' \dontrun{
#' # Remote
#' meta <- read_metadata("opentsi/kofethz")
#'
#' # Local archive directory
#' meta <- read_metadata("path/to/my.dataset")
#'
#' # Local JSON file directly
#' meta <- read_metadata("path/to/metadata.json")
#' }
read_metadata <- function(archive, ref = NULL) {

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
  branches <- if (is.null(ref)) c("main", "master") else ref

  resp <- NULL
  for (branch in branches) {
    url <- sprintf(
      "https://raw.githubusercontent.com/%s/%s/inst/metadata.json",
      archive, branch
    )
    resp <- request(url) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()
    if (resp$status_code == 200) break
  }

  if (resp$status_code != 200) {
    stop(sprintf(
      "Could not fetch metadata.json from '%s' (tried: %s, last HTTP %d).",
      archive, paste(branches, collapse = ", "), resp$status_code
    ))
  }

  fromJSON(resp_body_string(resp), simplifyVector = FALSE)
}
