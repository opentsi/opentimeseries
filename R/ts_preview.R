#' Interactive time series preview
#'
#' Renders an ECharts line chart in the RStudio / Positron viewer.
#' Optionally overlay a second series (e.g. a vintage) for comparison.
#'
#' @param x A \code{data.table} returned by \code{read_open_ts()}, or a
#'   two-column \code{data.frame} with columns \code{date} and \code{value}.
#' @param compare Optional second series in the same format as \code{x}.
#' @param compare_date Character label for the vintage, e.g. \code{"Jan 2026"}.
#'   Inferred from \code{max(compare$date)} when \code{NULL}.
#' @param title Chart title. \code{NULL} for no title.
#' @param subtitle Chart subtitle. Auto-generated from frequency + vintage
#'   when \code{NULL}.
#' @param color Hex colour for the main series.
#' @param compare_color Hex colour for the comparison series.
#' @param height CSS height string.
#' @param theme \code{"dark"} (default) or \code{"light"}.
#' @return An \code{htmltools} tag list displayed in the viewer.
#' @export
ts_preview <- function(x,
                       compare       = NULL,
                       compare_date  = NULL,
                       title         = NULL,
                       subtitle      = NULL,
                       color         = "#6366f1",
                       compare_color = "#f59e0b",
                       height        = "320px",
                       theme         = c("dark", "light")) {
  theme <- match.arg(theme)
  main  <- .ts_to_df(x)

  freq_label <- .detect_freq(main$date)

  if (!is.null(compare)) {
    comp <- .ts_to_df(compare)

    if (is.null(compare_date))
      compare_date <- format(max(comp$date), "%b %Y")

    cmp_name <- paste0("Vintage (", compare_date, ")")

    if (is.null(subtitle))
      subtitle <- paste0(freq_label, " · Latest vs. ", compare_date, " vintage")

    merged <- .merge_series(main, comp)
    dates  <- format(merged$date, "%Y-%m-%d")
    series <- list(
      list(name = "Latest",   values = .nullify_na(merged$value_x),
           color = jsonlite::unbox(color),         dashed = jsonlite::unbox(FALSE)),
      list(name = cmp_name,   values = .nullify_na(merged$value_y),
           color = jsonlite::unbox(compare_color), dashed = jsonlite::unbox(TRUE))
    )
  } else {
    if (is.null(subtitle))
      subtitle <- freq_label
    dates  <- format(main$date, "%Y-%m-%d")
    series <- list(
      list(name = jsonlite::unbox(""), values = .nullify_na(main$value),
           color = jsonlite::unbox(color), dashed = jsonlite::unbox(FALSE))
    )
  }

  payload <- jsonlite::toJSON(
    list(dates = dates, series = series, title = title, subtitle = subtitle),
    auto_unbox = FALSE, null = "null"
  )

  id     <- paste0("tsp-", as.integer(Sys.time()), sample.int(1e5, 1))
  btn_id <- paste0("btn-", id)
  wrp_id <- paste0("wrp-", id)

  echarts_js <- paste(readLines(
    system.file("htmlwidgets/lib/echarts-line.min.js", package = "opentimeseries"),
    warn = FALSE
  ), collapse = "\n")

  css_reset <- paste0(
    "body{margin:0;padding:0;}",
    "#", wrp_id, " svg text{",
    "writing-mode:horizontal-tb!important;",
    "text-orientation:mixed!important;",
    "direction:ltr!important;}"
  )

  htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css_reset)),
      htmltools::tags$script(htmltools::HTML(echarts_js)),
      htmltools::tags$div(
        id = wrp_id,
        style = "border-radius:8px;overflow:hidden;position:relative;",
        htmltools::tags$button(
          id = btn_id,
          style = paste0(
            "position:absolute;top:6px;right:8px;z-index:10;",
            "padding:2px 8px;font-size:10px;border-radius:4px;cursor:pointer;",
            "border:1px solid rgba(148,163,184,0.3);background:transparent;",
            "color:rgba(148,163,184,0.7);"
          ),
          if (theme == "dark") "Light" else "Dark"
        ),
        htmltools::tags$div(id = id, style = paste0("width:100%;height:", height))
      ),
      htmltools::tags$script(htmltools::HTML(.preview_js(id, btn_id, wrp_id, payload, theme)))
    )
  )
}

.preview_js <- function(id, btn_id, wrp_id, payload, initial_theme) {
  paste0('(function(){
  var el  = document.getElementById("', id, '");
  var btn = document.getElementById("', btn_id, '");
  var wrp = document.getElementById("', wrp_id, '");
  var d   = ', payload, ';

  var THEMES = {
    dark: {
      bg:        "#0d1117",
      axisLine:  "#1d2b3a",
      label:     "#8b9eb8",
      splitLine: "rgba(26,37,53,0.8)",
      ttBg:      "#111b28",
      ttBd:      "#213046",
      ttTxt:     "#f8fafb"
    },
    light: {
      bg:        "#ffffff",
      axisLine:  "#e2e8f0",
      label:     "#64748b",
      splitLine: "#e2e8f0",
      ttBg:      "#ffffff",
      ttBd:      "#cbd5e1",
      ttTxt:     "#1e293b"
    }
  };

  var theme = "', initial_theme, '";

  function fmtDate(iso) {
    var dt = new Date(iso + "T00:00:00Z");
    return dt.toLocaleDateString("en-US", { month: "short", year: "numeric", timeZone: "UTC" });
  }

  var chart = tsEcharts.echarts.init(el, null, { renderer: "canvas" });
  var interval   = Math.max(1, Math.floor(d.dates.length / 7));
  var hasCompare = d.series.length > 1;
  var hasTitle   = !!(d.title || d.subtitle);
  var gridTop    = hasTitle && hasCompare ? 72 : hasTitle ? 56 : hasCompare ? 28 : 12;

  function buildOption(t) {
    return {
      backgroundColor: t.bg,
      animation: true,
      grid: { left: 44, right: 16, top: gridTop, bottom: 32 },
      title: hasTitle ? {
        text:    d.title    || "",
        subtext: d.subtitle || "",
        left: 8, top: 6,
        textStyle:    { fontSize: 13, fontWeight: "600", color: t.ttTxt },
        subtextStyle: { fontSize: 11, color: t.label }
      } : undefined,
      legend: hasCompare ? {
        show: true, top: hasTitle ? gridTop - 22 : 4, right: 40,
        itemWidth: 16, itemHeight: 8,
        textStyle: { fontSize: 10, color: t.label },
        data: d.series.map(function(s) { return { name: s.name, icon: "rect" }; })
      } : { show: false },
      xAxis: {
        type: "category", data: d.dates, boundaryGap: false,
        axisLine: { lineStyle: { color: t.axisLine } },
        axisTick: { show: false },
        axisLabel: { color: t.label, fontSize: 10, interval: interval,
          formatter: function(v) { return fmtDate(v); } }
      },
      yAxis: {
        scale: true,
        splitLine: { lineStyle: { color: t.splitLine, type: "dashed" } },
        axisLabel: { color: t.label, fontSize: 10 }
      },
      tooltip: {
        trigger: "axis",
        backgroundColor: t.ttBg,
        borderColor: t.ttBd,
        textStyle: { color: t.ttTxt, fontSize: 11 },
        formatter: function(params) {
          var out = \'<div style="font-weight:600;margin-bottom:4px">\' + fmtDate(params[0].axisValue) + \'</div>\';
          params.forEach(function(p) {
            if (p.value !== null && p.value !== undefined)
              out += \'<div>\' + p.marker + \' \' + p.seriesName +
                     \'&nbsp;<strong>\' + (+p.value).toFixed(2) + \'</strong></div>\';
          });
          return out;
        }
      },
      series: d.series.map(function(s) {
        var o = { name: s.name, type: "line", data: s.values,
          smooth: 0.4, showSymbol: false,
          lineStyle: { color: s.color, width: 2, type: s.dashed ? "dashed" : "solid" } };
        if (!s.dashed)
          o.areaStyle = { color: { type: "linear", x: 0, y: 0, x2: 0, y2: 1,
            colorStops: [{ offset: 0, color: s.color + "40" },
                         { offset: 1, color: s.color + "00" }] } };
        return o;
      })
    };
  }

  function applyTheme(name) {
    var t = THEMES[name];
    wrp.style.background  = t.bg;
    btn.style.color       = t.label;
    btn.style.borderColor = t.axisLine;
    btn.textContent       = name === "dark" ? "Light" : "Dark";
    chart.setOption(buildOption(t), true);
  }

  applyTheme(theme);
  new ResizeObserver(function() { chart.resize(); }).observe(el);
  btn.addEventListener("click", function() {
    theme = theme === "dark" ? "light" : "dark";
    applyTheme(theme);
  });
})();')
}

.detect_freq <- function(dates) {
  if (length(dates) < 2L) return(NULL)
  avg <- mean(as.numeric(diff(dates[seq_len(min(12L, length(dates)))])))
  if (avg < 45)       "Monthly"
  else if (avg < 120) "Quarterly"
  else                "Annual"
}

.ts_to_df <- function(x) {
  if (is.data.frame(x) && all(c("id", "date", "value") %in% names(x))) {
    ids <- unique(x$id)
    if (length(ids) > 1L) {
      warning("ts_preview: multiple series detected, showing only '", ids[1L], "'",
              call. = FALSE)
      x <- x[x$id == ids[1L], ]
    }
    return(data.frame(date = as.Date(x$date), value = as.numeric(x$value)))
  }

  if (is.data.frame(x) && ncol(x) >= 2L) {
    date_col  <- which(sapply(x, inherits, what = c("Date", "POSIXct")))[1L]
    value_col <- which(sapply(x, is.numeric))[1L]
    return(data.frame(date = as.Date(x[[date_col]]), value = x[[value_col]]))
  }

  if (inherits(x, "ts")) {
    tt   <- as.numeric(time(x))
    freq <- frequency(x)
    if (freq == 12L) {
      yr <- floor(tt); mo <- round((tt - yr) * 12) + 1L
      dates <- as.Date(paste(yr, mo, "01", sep = "-"))
    } else if (freq == 4L) {
      yr <- floor(tt); q <- round((tt - yr) * 4) + 1L
      dates <- as.Date(paste(yr, (q - 1L) * 3L + 1L, "01", sep = "-"))
    } else {
      dates <- as.Date(paste(floor(tt), "01", "01", sep = "-"))
    }
    return(data.frame(date = dates, value = as.numeric(x)))
  }

  if (inherits(x, c("xts", "zoo"))) {
    if (!requireNamespace("zoo", quietly = TRUE))
      stop("Install the 'zoo' package to use xts/zoo objects.", call. = FALSE)
    return(data.frame(date = as.Date(zoo::index(x)),
                      value = as.numeric(zoo::coredata(x)[, 1L])))
  }

  stop("ts_preview: unsupported class '", class(x)[1L], "'", call. = FALSE)
}

.merge_series <- function(a, b) {
  merge(a, b, by = "date", all = TRUE)
}

.nullify_na <- function(x) {
  lapply(x, function(v) if (is.na(v)) NULL else jsonlite::unbox(v))
}
