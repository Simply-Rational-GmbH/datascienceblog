suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(ggplot2))

# Annahmen
jahre             <- 1:20
wachstum          <- 0.02   # jährliches Umsatzwachstum
anteil_verlust    <- 0.02   # entgangener Umsatz durch defensive Entscheidungen
entsch_mitarb     <- 3650   # Entscheidungen pro Mitarbeitende:r und Jahr
entsch_manager    <- 7300   # Entscheidungen pro Manager:in und Jahr
anteil_defensiv   <- 0.24   # Anteil defensiver Entscheidungen
p_innovation      <- 0.01   # 1 von 100 Entscheidungen führt zu einer Innovation

farben <- c(Mitarbeitende = "#1AA7B4", Manager = "#661AAE")

fmt <- function(x, digits = 0) {
  format(round(x, digits), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

theme_sr <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.background  = element_rect(fill = "#FFF5EB", colour = NA),
      panel.background = element_rect(fill = "#FFF5EB", colour = NA),
      panel.grid.minor = element_blank(),
      legend.position  = "top",
      legend.title     = element_blank(),
      plot.title       = element_text(face = "bold")
    )
}

ui <- page_fluid(
  theme = bs_theme(bg = "#FFF5EB", fg = "#020202", primary = "#661AAE"),
  card(
    layout_column_wrap(
      width = "220px",
      numericInput("umsatz", "Jahresumsatz (EUR)", value = 100e6, min = 0, step = 1e6),
      numericInput("mitarbeitende", "Mitarbeitende (gesamt)", value = 500, min = 1, step = 10),
      sliderInput("manager", "Anteil Manager:innen (%)", min = 0, max = 100, value = 15, step = 1)
    ),
    helpText(
      "Annahmen: Umsatz wächst 2 % p. a.; 2 % des Umsatzes entgehen durch defensive",
      "Entscheidungen. Mitarbeitende treffen 3.650, Manager:innen 7.300 Entscheidungen",
      "pro Jahr, davon sind 24 % defensiv. Jede 100. Entscheidung hätte eine",
      "Innovation ausgelöst. Manager:innen sind in der Gesamtzahl enthalten."
    )
  ),
  layout_column_wrap(
    width = "220px",
    value_box("Entgangener Umsatz (20 J.)", textOutput("vb_umsatz")),
    value_box("Defensive Entscheidungen (20 J.)", textOutput("vb_entsch")),
    value_box("Verpasste Innovationen (20 J.)", textOutput("vb_innov"))
  ),
  navset_card_underline(
    nav_panel("Entgangener Umsatz", plotOutput("plot_umsatz", height = "420px")),
    nav_panel("Defensive Entscheidungen", plotOutput("plot_entsch", height = "420px")),
    nav_panel("Verpasste Innovationen", plotOutput("plot_innov", height = "420px"))
  )
)

server <- function(input, output, session) {

  daten <- reactive({
    req(input$umsatz, input$mitarbeitende)
    n_manager <- input$mitarbeitende * input$manager / 100
    n_mitarb  <- input$mitarbeitende - n_manager

    umsatz  <- input$umsatz * (1 + wachstum)^(jahre - 1)
    verlust <- umsatz * anteil_verlust

    def_mitarb  <- n_mitarb  * entsch_mitarb  * anteil_defensiv
    def_manager <- n_manager * entsch_manager * anteil_defensiv

    entsch <- data.frame(
      jahr   = rep(jahre, 2),
      gruppe = factor(rep(c("Mitarbeitende", "Manager"), each = length(jahre)),
                      levels = names(farben)),
      defensiv_kum = c(cumsum(rep(def_mitarb, length(jahre))),
                       cumsum(rep(def_manager, length(jahre))))
    )
    entsch$innov_kum <- entsch$defensiv_kum * p_innovation

    list(
      umsatz = data.frame(jahr = jahre, verlust_kum = cumsum(verlust)),
      entsch = entsch
    )
  })

  summe_jahr20 <- function(df, spalte) sum(df[df$jahr == max(jahre), spalte])

  output$vb_umsatz <- renderText(
    paste(fmt(tail(daten()$umsatz$verlust_kum, 1) / 1e6, 1), "Mio. EUR")
  )
  output$vb_entsch <- renderText(fmt(summe_jahr20(daten()$entsch, "defensiv_kum")))
  output$vb_innov  <- renderText(fmt(summe_jahr20(daten()$entsch, "innov_kum")))

  output$plot_umsatz <- renderPlot({
    ggplot(daten()$umsatz, aes(jahr, verlust_kum / 1e6)) +
      geom_area(fill = "#EA5A0C", alpha = 0.85) +
      scale_x_continuous(breaks = seq(0, 20, 2)) +
      scale_y_continuous(labels = function(x) fmt(x, 1)) +
      labs(title = "Kumulierter entgangener Umsatz",
           x = "Jahr", y = "Mio. EUR") +
      theme_sr()
  }, bg = "#FFF5EB")

  output$plot_entsch <- renderPlot({
    ggplot(daten()$entsch, aes(jahr, defensiv_kum, fill = gruppe)) +
      geom_area(alpha = 0.85) +
      scale_fill_manual(values = farben) +
      scale_x_continuous(breaks = seq(0, 20, 2)) +
      scale_y_continuous(labels = fmt) +
      labs(title = "Kumulierte defensive Entscheidungen",
           x = "Jahr", y = "Anzahl Entscheidungen") +
      theme_sr()
  }, bg = "#FFF5EB")

  output$plot_innov <- renderPlot({
    ggplot(daten()$entsch, aes(jahr, innov_kum, fill = gruppe)) +
      geom_area(alpha = 0.85) +
      scale_fill_manual(values = farben) +
      scale_x_continuous(breaks = seq(0, 20, 2)) +
      scale_y_continuous(labels = fmt) +
      labs(title = "Kumulierte verpasste Innovationen (erwartet, p = 1 %)",
           x = "Jahr", y = "Anzahl Innovationen") +
      theme_sr()
  }, bg = "#FFF5EB")
}

shinyApp(ui, server)
