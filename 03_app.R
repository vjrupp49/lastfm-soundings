# =============================================================================
#  03_app.R  —  "Soundings"
#  An explorable chart of your listening space. Run with:
#      shiny::runApp("03_app.R")
#  Requires taste_space.rds from 01_build_taste_space.R in the same directory.
#
#  Three instruments:
#    CHART     the map, filterable by year, with per-artist soundings on click
#    BEARINGS  invent your own axes: pick two artists as poles and every other
#              artist gets a coordinate on the spectrum between them
#    CURRENTS  directed flow between regions, scored against chance
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(plotly); library(Matrix)
})

taste <- readRDS("taste_space.rds")
A   <- taste$artists
CL  <- taste$clusters
E   <- taste$E
MON <- taste$monthly
MONTHS <- taste$months
YEARS  <- as.integer(substr(MONTHS, 1, 4))
V   <- nrow(A)
K   <- nrow(CL)

CL$label <- ifelse(is.na(CL$tags), CL$name, CL$tags)
A$region <- CL$label[A$cluster]

PAL <- list(abyss = "#08171F", shelf = "#0F2C38", panel = "#122F3B",
            isoline = "#1F5566", sand = "#E9DFCB", faint = "#7C8F96",
            beacon = "#E4572E")
region_cols <- grDevices::colorRampPalette(
  c("#2E7D8F", "#4FA3A5", "#9DC3A4", "#D9C68B", "#C88F6A", "#9A6B7E", "#63698F"))(K)

pct <- function(v) round(100 * stats::ecdf(v)(v))
A$bridge_pct <- pct(A$bridge)
A$entropy_pct <- pct(A$ctx_entropy)

hulls <- lapply(seq_len(K), function(k) {
  g <- A[A$cluster == k, c("x", "y")]
  if (nrow(g) < 3) return(NULL)
  g[grDevices::chull(g), ]
})

# ------------------------------------------------------------------- css ----
css <- sprintf('
@import url("https://fonts.googleapis.com/css2?family=Archivo+Narrow:wght@400;600&family=EB+Garamond:ital@1&family=IBM+Plex+Mono:wght@400;500&display=swap");
:root { --abyss:%s; --shelf:%s; --panel:%s; --iso:%s; --sand:%s; --faint:%s; --beacon:%s; }
html,body { background:var(--abyss); color:var(--sand); font-family:"Archivo Narrow",system-ui,sans-serif; }
.wrap { max-width:1600px; margin:0 auto; padding:26px 30px 60px; }
.masthead { display:flex; align-items:baseline; gap:18px; border-bottom:1px solid var(--iso); padding-bottom:12px; margin-bottom:4px; }
.masthead h1 { font-size:30px; font-weight:600; letter-spacing:.14em; text-transform:uppercase; margin:0; }
.masthead .sub { font-family:"IBM Plex Mono",monospace; font-size:11px; color:var(--faint); letter-spacing:.04em; }
.tabbable > .nav-tabs { border-bottom:1px solid var(--iso); margin-bottom:18px; }
.nav-tabs > li > a { color:var(--faint) !important; background:transparent !important; border:0 !important;
  font-family:"IBM Plex Mono",monospace; font-size:11px; letter-spacing:.18em; text-transform:uppercase; padding:12px 18px; }
.nav-tabs > li.active > a { color:var(--sand) !important; box-shadow:inset 0 -2px 0 var(--beacon); }
.rail { background:var(--shelf); border:1px solid var(--iso); border-radius:2px; padding:16px 18px; }
.rail h4 { font-family:"IBM Plex Mono",monospace; font-size:10px; letter-spacing:.2em; text-transform:uppercase;
  color:var(--faint); margin:0 0 12px; border-bottom:1px solid var(--iso); padding-bottom:7px; }
.rail + .rail { margin-top:14px; }
label, .control-label { font-family:"IBM Plex Mono",monospace; font-size:10.5px; letter-spacing:.1em;
  text-transform:uppercase; color:var(--faint); font-weight:400; }
.sounding { font-family:"IBM Plex Mono",monospace; font-size:12px; line-height:1.9; }
.sounding .k { color:var(--faint); }
.sounding .v { color:var(--sand); }
.sounding .big { font-family:"Archivo Narrow",sans-serif; font-size:23px; font-weight:600;
  letter-spacing:.02em; display:block; line-height:1.2; margin-bottom:1px; }
.sounding .rgn { font-family:"EB Garamond",serif; font-style:italic; font-size:16px; color:var(--sand); opacity:.85; }
.bar { height:3px; background:var(--iso); margin:3px 0 9px; }
.bar > i { display:block; height:3px; background:var(--beacon); }
.note { font-family:"IBM Plex Mono",monospace; font-size:10.5px; color:var(--faint); line-height:1.65; }
.empty { font-family:"IBM Plex Mono",monospace; font-size:11.5px; color:var(--faint); line-height:1.8; }
table.nn { width:100%%; font-family:"IBM Plex Mono",monospace; font-size:11.5px; border-collapse:collapse; }
table.nn td { padding:3px 0; border-bottom:1px solid rgba(31,85,102,.4); }
table.nn td.s { text-align:right; color:var(--faint); }
.irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:var(--beacon); border-color:var(--beacon); }
.irs--shiny .irs-line { background:var(--iso); }
.irs--shiny .irs-handle { background:var(--sand); border:0; box-shadow:none; }
.irs--shiny .irs-min,.irs--shiny .irs-max,.irs--shiny .irs-grid-text { color:var(--faint); background:transparent; font-family:"IBM Plex Mono",monospace; }
.selectize-input, .selectize-dropdown { background:var(--panel) !important; color:var(--sand) !important;
  border-color:var(--iso) !important; font-family:"IBM Plex Mono",monospace; font-size:12px; border-radius:2px; }
.selectize-dropdown .active { background:var(--iso) !important; }
.checkbox label, .radio label { text-transform:none; letter-spacing:.02em; font-size:12px; color:var(--sand); }
', PAL$abyss, PAL$shelf, PAL$panel, PAL$isoline, PAL$sand, PAL$faint, PAL$beacon)

blank_ax <- list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "")
plotly_chrome <- function(p, h = NULL)
  layout(p, paper_bgcolor = PAL$abyss, plot_bgcolor = PAL$abyss,
         font = list(family = "IBM Plex Mono, monospace", color = PAL$sand, size = 11),
         margin = list(l = 10, r = 10, t = 10, b = 10), showlegend = FALSE,
         hoverlabel = list(bgcolor = PAL$shelf, bordercolor = PAL$isoline,
                           font = list(family = "IBM Plex Mono, monospace",
                                       color = PAL$sand, size = 11)))

# -------------------------------------------------------------------- ui ----
ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  div(class = "wrap",
      div(class = "masthead",
          h1("Soundings"),
          span(class = "sub", sprintf("%s artists · %s regions · %s scrobbles · %s",
                                      V, K, format(taste$fit$n_scrobbles, big.mark = ","),
                                      paste(format(taste$fit$span, "%Y"), collapse = "–")))),
      
      tabsetPanel(
        # ------------------------------------------------------------ chart ----
        tabPanel("Chart",
                 fluidRow(
                   column(3,
                          div(class = "rail",
                              h4("Period"),
                              sliderInput("yrs", NULL, min = min(YEARS), max = max(YEARS),
                                          value = range(YEARS), step = 1, sep = "", ticks = FALSE),
                              checkboxInput("dim_absent", "Fade artists absent in this period", TRUE)),
                          div(class = "rail",
                              h4("Overlays"),
                              checkboxInput("show_hulls", "Region boundaries", TRUE),
                              checkboxInput("show_bridges", "Bridge artists", FALSE),
                              checkboxInput("show_drift", "Yearly centre of gravity", FALSE),
                              checkboxInput("show_names", "Region names", TRUE)),
                          div(class = "rail",
                              h4("Find"),
                              selectizeInput("find", NULL, choices = NULL, options = list(
                                placeholder = "search an artist", maxOptions = 40)),
                              div(class = "note", "Selecting or clicking an artist draws lines to its twelve nearest neighbours in the full 128-dimensional space — not to whatever happens to look close on this flattened map."))),
                   column(6, plotlyOutput("chart", height = "760px")),
                   column(3, div(class = "rail", h4("Soundings"), uiOutput("detail")),
                          div(class = "rail", h4("Play history"), plotlyOutput("spark", height = "110px")))
                 ),
                 div(style = "margin-top:16px", class = "note",
                     "Position comes from a truncated SVD of the PPMI-weighted co-listening matrix, laid out with UMAP. Two artists sit together because they turn up near each other inside your sessions — no genre tags were used anywhere in the construction.")
        ),
        
        # --------------------------------------------------------- bearings ----
        tabPanel("Bearings",
                 fluidRow(
                   column(3,
                          div(class = "rail", h4("Horizontal axis"),
                              selectizeInput("xl", "West pole", choices = NULL),
                              selectizeInput("xr", "East pole", choices = NULL)),
                          div(class = "rail", h4("Vertical axis"),
                              selectizeInput("yb", "South pole", choices = NULL),
                              selectizeInput("yt", "North pole", choices = NULL)),
                          div(class = "rail", h4("Display"),
                              sliderInput("nlab", "Labels per pole", 3, 20, 8, 1, ticks = FALSE),
                              checkboxInput("colour_region", "Colour by region", TRUE)),
                          div(class = "rail", class = "note",
                              "Each axis is the difference vector between two artists, normalised. Every artist in the library is then projected onto it. The result is a spectrum that exists only in your listening — nobody else's axis runs from one of these artists to the other.")),
                   column(9, plotlyOutput("bearings", height = "760px"))
                 )
        ),
        
        # --------------------------------------------------------- currents ----
        tabPanel("Currents",
                 fluidRow(
                   column(7, plotlyOutput("flow", height = "620px")),
                   column(5, plotlyOutput("openers", height = "300px"),
                          div(style = "margin-top:18px", plotlyOutput("sticky", height = "300px")))
                 ),
                 div(style = "margin-top:16px", class = "note",
                     "Left: log2 of observed transitions over the number expected if the next track's region were drawn independently from your overall listening mix. Warm cells are moves you make more often than chance; cool cells are moves you avoid. The diagonal is excluded from the colour scale so it can't swamp everything else.")
        )
      )
  )
)

# ---------------------------------------------------------------- server ----
server <- function(input, output, session) {
  
  choices <- setNames(A$idx, A$artist)
  for (id in c("find", "xl", "xr", "yb", "yt"))
    updateSelectizeInput(session, id, choices = choices, server = TRUE,
                         selected = if (id == "find") "" else
                           A$idx[order(-A$plays)][match(id, c("xl","xr","yb","yt"))])
  
  # plays inside the selected period
  window_plays <- reactive({
    keep <- YEARS >= input$yrs[1] & YEARS <= input$yrs[2]
    as.numeric(rowSums(MON[, keep, drop = FALSE]))
  })
  
  selected <- reactiveVal(NULL)
  observeEvent(event_data("plotly_click", source = "chart"), {
    cd <- event_data("plotly_click", source = "chart")$customdata
    if (!is.null(cd) && !is.na(suppressWarnings(as.integer(cd)))) selected(as.integer(cd))
  })
  observeEvent(input$find, {
    if (!is.null(input$find) && nzchar(input$find)) selected(as.integer(input$find))
  })
  
  output$chart <- renderPlotly({
    wp <- window_plays()
    present <- wp > 0
    op <- if (input$dim_absent) ifelse(present, 0.85, 0.12) else 0.8
    sz <- 5 + 26 * sqrt(wp / max(wp, 1))
    
    p <- plot_ly(source = "chart") |> event_register("plotly_click")
    
    if (isTRUE(input$show_hulls)) {
      for (k in seq_len(K)) {
        h <- hulls[[k]]; if (is.null(h)) next
        p <- add_polygons(p, x = c(h$x, h$x[1]), y = c(h$y, h$y[1]),
                          fillcolor = paste0(region_cols[k], "1F"),
                          line = list(color = paste0(region_cols[k], "55"), width = 1),
                          hoverinfo = "skip", showlegend = FALSE)
      }
    }
    
    p <- add_trace(p, type = "scattergl", mode = "markers",
                   x = A$x, y = A$y, customdata = A$idx,
                   marker = list(size = sz, opacity = op, color = region_cols[A$cluster],
                                 line = list(width = 0)),
                   text = sprintf("<b>%s</b><br>%s<br>%s plays in period · %s all time",
                                  A$artist, A$region, format(round(wp), big.mark = ","),
                                  format(A$plays, big.mark = ",")),
                   hoverinfo = "text")
    
    if (isTRUE(input$show_bridges)) {
      b <- A[order(-A$bridge), ][1:18, ]
      p <- add_trace(p, type = "scattergl", mode = "markers", x = b$x, y = b$y,
                     customdata = b$idx,
                     marker = list(size = 15, color = "rgba(0,0,0,0)",
                                   line = list(color = PAL$beacon, width = 1.4)),
                     text = sprintf("<b>%s</b><br>bridge, %dth percentile", b$artist, b$bridge_pct),
                     hoverinfo = "text")
    }
    
    if (isTRUE(input$show_names)) {
      lb <- do.call(rbind, lapply(split(A, A$cluster), function(g)
        data.frame(x = mean(g$x, trim = .15), y = mean(g$y, trim = .15),
                   lab = g$region[1])))
      p <- add_trace(p, type = "scatter", mode = "text", x = lb$x, y = lb$y,
                     text = lb$lab, textfont = list(family = "EB Garamond, serif", size = 15,
                                                    color = PAL$sand),
                     opacity = 0.8, hoverinfo = "skip")
    }
    
    if (isTRUE(input$show_drift)) {
      D <- taste$drift
      p <- add_trace(p, type = "scatter", mode = "lines+markers+text", x = D$x, y = D$y,
                     text = D$year, textposition = "top center",
                     textfont = list(family = "IBM Plex Mono, monospace", size = 10, color = PAL$beacon),
                     line = list(color = PAL$beacon, width = 1.6, shape = "spline"),
                     marker = list(size = 8, color = PAL$beacon),
                     hovertext = sprintf("%d · %s scrobbles", D$year, format(D$scrobbles, big.mark = ",")),
                     hoverinfo = "text")
    }
    
    i <- selected()
    if (!is.null(i)) {
      nb <- taste$nn[i, ]
      p <- add_segments(p, x = A$x[i], y = A$y[i], xend = A$x[nb], yend = A$y[nb],
                        line = list(color = PAL$sand, width = 0.8), opacity = 0.45,
                        hoverinfo = "skip")
      p <- add_trace(p, type = "scatter", mode = "markers+text",
                     x = A$x[i], y = A$y[i], customdata = i, text = A$artist[i],
                     textposition = "top center",
                     textfont = list(family = "Archivo Narrow, sans-serif", size = 14, color = PAL$sand),
                     marker = list(size = 16, color = PAL$beacon,
                                   line = list(color = PAL$sand, width = 1.5)),
                     hoverinfo = "skip")
    }
    
    plotly_chrome(p) |>
      layout(xaxis = blank_ax, yaxis = c(blank_ax, list(scaleanchor = "x", scaleratio = 1)),
             dragmode = "pan") |>
      config(displayModeBar = FALSE, scrollZoom = TRUE)
  })
  
  output$detail <- renderUI({
    i <- selected()
    if (is.null(i)) return(div(class = "empty",
                               "Click any point on the chart, or search for an artist, to take a sounding.",
                               br(), br(), "You'll get its region, its nearest neighbours in the full space, and where it sits on the two structural measures: how much of a bridge it is, and how many different contexts you hear it in."))
    a <- A[i, ]
    nb <- taste$nn[i, ]; sm <- taste$nn_sim[i, ]
    wp <- window_plays()[i]
    tagList(
      div(class = "sounding",
          span(class = "big", a$artist),
          span(class = "rgn", a$region), br(), br(),
          span(class = "k", "plays "), span(class = "v", format(a$plays, big.mark = ",")),
          span(class = "k", "  ·  in period "), span(class = "v", format(round(wp), big.mark = ",")), br(),
          span(class = "k", "first "), span(class = "v", format(a$first_seen, "%b %Y")),
          span(class = "k", "  ·  last "), span(class = "v", format(a$last_seen, "%b %Y")), br(), br(),
          span(class = "k", sprintf("bridge · %dth pct", a$bridge_pct)),
          div(class = "bar", tags$i(style = sprintf("width:%d%%", a$bridge_pct))),
          span(class = "k", sprintf("context spread · %dth pct", a$entropy_pct)),
          div(class = "bar", tags$i(style = sprintf("width:%d%%", a$entropy_pct))),
          span(class = "k", "nearest neighbours"),
          tags$table(class = "nn", lapply(seq_along(nb), function(j)
            tags$tr(tags$td(A$artist[nb[j]]), tags$td(class = "s", sprintf("%.3f", sm[j])))))
      )
    )
  })
  
  output$spark <- renderPlotly({
    i <- selected()
    if (is.null(i)) return(plotly_empty(type = "scatter", mode = "lines") |> plotly_chrome())
    v <- as.numeric(MON[i, ])
    plot_ly(x = as.Date(paste0(MONTHS, "-01")), y = v, type = "scatter", mode = "lines",
            fill = "tozeroy", line = list(color = PAL$beacon, width = 1.2, shape = "spline"),
            fillcolor = "rgba(228,87,46,0.18)", hoverinfo = "x+y") |>
      plotly_chrome() |>
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, color = PAL$faint,
                          tickfont = list(size = 9)),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             margin = list(l = 4, r = 4, t = 6, b = 20)) |>
      config(displayModeBar = FALSE)
  })
  
  # ------------------------------------------------------------- bearings ----
  output$bearings <- renderPlotly({
    ids <- lapply(list(input$xl, input$xr, input$yb, input$yt), function(z)
      if (is.null(z) || !nzchar(z)) NA_integer_ else as.integer(z))
    if (any(is.na(unlist(ids)))) return(plotly_empty() |> plotly_chrome())
    xl <- ids[[1]]; xr <- ids[[2]]; yb <- ids[[3]]; yt <- ids[[4]]
    
    ax <- function(a, b) { v <- E[b, ] - E[a, ]; v / max(sqrt(sum(v^2)), 1e-9) }
    px <- as.numeric(E %*% ax(xl, xr))
    py <- as.numeric(E %*% ax(yb, yt))
    
    lab <- unique(c(order(px)[1:input$nlab], order(-px)[1:input$nlab],
                    order(py)[1:input$nlab], order(-py)[1:input$nlab], xl, xr, yb, yt))
    cols <- if (isTRUE(input$colour_region)) region_cols[A$cluster] else PAL$isoline
    
    plot_ly(source = "bearings") |>
      add_trace(type = "scattergl", mode = "markers", x = px, y = py,
                marker = list(size = 4 + 16 * sqrt(A$plays / max(A$plays)), opacity = 0.7,
                              color = cols, line = list(width = 0)),
                text = sprintf("<b>%s</b><br>%s<br>x %+.3f · y %+.3f", A$artist, A$region, px, py),
                hoverinfo = "text") |>
      add_trace(type = "scatter", mode = "text", x = px[lab], y = py[lab],
                text = A$artist[lab], textposition = "top center",
                textfont = list(family = "Archivo Narrow, sans-serif", size = 10.5, color = PAL$sand),
                opacity = 0.9, hoverinfo = "skip") |>
      add_trace(type = "scatter", mode = "markers", x = px[c(xl,xr,yb,yt)],
                y = py[c(xl,xr,yb,yt)],
                marker = list(size = 13, color = PAL$beacon, line = list(color = PAL$sand, width = 1.2)),
                hoverinfo = "skip") |>
      plotly_chrome() |>
      layout(
        shapes = list(
          list(type = "line", x0 = min(px), x1 = max(px), y0 = 0, y1 = 0,
               line = list(color = PAL$isoline, width = 1, dash = "dot")),
          list(type = "line", x0 = 0, x1 = 0, y0 = min(py), y1 = max(py),
               line = list(color = PAL$isoline, width = 1, dash = "dot"))),
        xaxis = list(title = list(text = sprintf("%s   ←   →   %s", A$artist[xl], A$artist[xr]),
                                  font = list(size = 11, color = PAL$faint)),
                     showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(title = list(text = sprintf("%s   ←   →   %s", A$artist[yb], A$artist[yt]),
                                  font = list(size = 11, color = PAL$faint)),
                     showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) |>
      config(displayModeBar = FALSE)
  })
  
  # ------------------------------------------------------------- currents ----
  output$flow <- renderPlotly({
    LR <- taste$flow_lr
    off <- LR; diag(off) <- NA
    lim <- max(abs(off), na.rm = TRUE)
    plot_ly(z = off, x = CL$label, y = CL$label, type = "heatmap",
            colorscale = list(c(0, "#2E7D8F"), c(0.5, PAL$abyss), c(1, PAL$beacon)),
            zmin = -lim, zmax = lim, xgap = 2, ygap = 2,
            hovertemplate = "from %{y}<br>to %{x}<br>log2 obs/exp %{z:.2f}<extra></extra>") |>
      plotly_chrome() |>
      layout(xaxis = list(title = "to", side = "top", tickangle = -35,
                          tickfont = list(size = 9.5), color = PAL$faint),
             yaxis = list(title = "from", autorange = "reversed",
                          tickfont = list(size = 9.5), color = PAL$faint),
             margin = list(l = 130, r = 20, t = 130, b = 20)) |>
      config(displayModeBar = FALSE)
  })
  
  output$openers <- renderPlotly({
    v <- 100 * CL$openers / sum(CL$openers)
    o <- order(v)
    plot_ly(x = v[o], y = factor(CL$label[o], levels = CL$label[o]), type = "bar",
            orientation = "h", marker = list(color = region_cols[o]),
            hovertemplate = "%{x:.1f}% of sessions open here<extra></extra>") |>
      plotly_chrome() |>
      layout(title = list(text = "Where sessions begin", x = 0,
                          font = list(size = 12, color = PAL$sand)),
             xaxis = list(title = "% of sessions", showgrid = FALSE, color = PAL$faint),
             yaxis = list(tickfont = list(size = 9.5), color = PAL$faint),
             margin = list(l = 150, r = 20, t = 40, b = 40)) |>
      config(displayModeBar = FALSE)
  })
  
  output$sticky <- renderPlotly({
    Fm <- taste$flow
    s <- 100 * diag(Fm) / pmax(rowSums(Fm), 1)
    o <- order(s)
    plot_ly(x = s[o], y = factor(CL$label[o], levels = CL$label[o]), type = "bar",
            orientation = "h", marker = list(color = region_cols[o]),
            hovertemplate = "%{x:.1f}% of next tracks stay in region<extra></extra>") |>
      plotly_chrome() |>
      layout(title = list(text = "Stickiness — chance the next track stays put", x = 0,
                          font = list(size = 12, color = PAL$sand)),
             xaxis = list(title = "%", showgrid = FALSE, color = PAL$faint),
             yaxis = list(tickfont = list(size = 9.5), color = PAL$faint),
             margin = list(l = 150, r = 20, t = 40, b = 40)) |>
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)

