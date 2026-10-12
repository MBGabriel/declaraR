# ==============================================================================
# Aplicación R Shiny: declaraR: Evaluación de la Conformidad Metrológica Integrada
# Basada en JCGM 106:2012 e ILAC G8:09/2019
# Autor: G. Morales
# Versión: 2026.01
# Fecha: 2026-10-12
# Licencia: GNU General Public License (GPL v3.0)
# ==============================================================================

library(shiny)
library(ggplot2)

ui <- fluidPage(
  # Estilos CSS para el reporte imprimible y el pie de página
  tags$head(
    tags$style(HTML("
      @media print {
        body { font-family: Arial, sans-serif; background-color: white; }
        .sidebar-col, .no-print, .nav-tabs, footer, .btn { display: none !important; }
        .main-col { width: 100% !important; margin: 0 !important; padding: 0 !important; }
        .printable-report { display: block !important; border: none !important; box-shadow: none !important; }
      }
      footer {
        margin-top: 30px;
        padding: 15px;
        background-color: #f8f9fa;
        border-top: 1px solid #e9ecef;
        text-align: center;
        font-size: 13px;
        color: #6c757d;
      }
      .report-card {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 25px;
        background-color: #ffffff;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .report-table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 15px;
        margin-bottom: 15px;
      }
      .report-table th, .report-table td {
        border: 1px solid #dee2e6;
        padding: 8px 12px;
        text-align: left;
      }
      .report-table th {
        background-color: #f1f3f5;
      }
    "))
  ),
  
  titlePanel(
    title = div(
      "declaraR: Evaluación de la Conformidad Metrológica",
      div("Versión: 2026.01", style = "font-size: 10px; color: #666666; font-weight: normal; margin-top: 2px;")
    ),
    windowTitle = "Evaluación de la Conformidad Metrológica"
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-col",
      
      # 1. Inputs para Mensurando y Unidades
      h4("1. Identificación de la Medición"),
      textInput("measurand", "Mensurando (Propiedad / Magnitud):", value = "Diámetro Exterior"),
      textInput("units", "Unidades de Medida:", value = "mm"),
      
      hr(),
      # 2. Normatividad Aplicable
      h4("2. Normatividad Aplicable"),
      textInput("norm_ref", "Normatividad Aplicable:", value = "Ley / Decreto / Resolución / Norma Técnica"),
      
      hr(),
      # 3. Selección de Regla de Decisión
      h4("3. Regla de Decisión Metrológica"),
      selectInput("rule_type", "Seleccione la Regla de Decisión:",
                  choices = c(
                    "Regla Simple (Aceptación Simple / Shared Risk, w = 0)" = "simple",
                    "Banda de Guarda de Aceptación (Protección Consumidor)" = "guarded_accept",
                    "Banda de Guarda de Rechazo (Protección Productor)" = "guarded_reject"
                  ),
                  selected = "simple"),
      
      hr(),
      # 4. Datos de la Medición
      h4("4. Datos de la Medición"),
      numericInput("y_val", "Valor Medido (y):", value = 100.2, step = 0.1),
      numericInput("u_exp", "Incertidumbre Expandida (U, k=2):", value = 0.4, min = 0.001, step = 0.05),
      
      # Factor multiplicador de la banda de guarda (se muestra si es regla resguardada)
      conditionalPanel(
        condition = "input.rule_type != 'simple'",
        numericInput("guard_factor", "Multiplicador de Banda de Guarda (r, w = r * U):", 
                     value = 1.0, min = 0.1, max = 3.0, step = 0.1)
      ),
      
      hr(),
      # 5. Especificación de Tolerancias
      h4("5. Especificación de Tolerancias (TL)"),
      p(em("Seleccione al menos un límite. Puede desactivar TL_L o TL_U para especificaciones unilaterales.")),
      
      checkboxInput("use_tl_lower", "Habilitar Límite Inferior (TL_L)", value = TRUE),
      conditionalPanel(
        condition = "input.use_tl_lower == true",
        numericInput("tl_lower", "TL_L:", value = 98.0, step = 0.5)
      ),
      
      checkboxInput("use_tl_upper", "Habilitar Límite Superior (TL_U)", value = TRUE),
      conditionalPanel(
        condition = "input.use_tl_upper == true",
        numericInput("tl_upper", "TL_U:", value = 102.0, step = 0.5)
      ),
      
      hr(),
      helpText("Según JCGM 106:2012 e ILAC G8:09/2019.")
    ),
    
    mainPanel(
      class = "main-col",
      tabsetPanel(
        id = "main_tabs",
        
        # TAB 1: Resultados y Declaración
        tabPanel("Resultados y Declaración",
                 br(),
                 uiOutput("status_box"),
                 br(),
                 plotOutput("plot_conformidad", height = "400px"),
                 br(),
                 wellPanel(
                   h4("Análisis de Riesgos e Intervalos Metrológicos"),
                   htmlOutput("details_text")
                 )
        ),
        
        # TAB 2: Fundamento Metrológico
        tabPanel("Fundamento Metrológico",
                 br(),
                 wellPanel(
                   h4("Fundamentos de la Evaluación de la Conformidad:"),
                   tags$ul(
                     tags$li(strong("Regla Simple (w = 0):"), " Los límites de aceptación coinciden con los de tolerancia (AL = TL). Riesgo compartido entre cliente y laboratorio."),
                     tags$li(strong("Banda de Guarda de Aceptación (w = r * U):"), " Reduce el intervalo de aceptación (AL_L = TL_L + w, AL_U = TL_U - w) para mitigar el riesgo de falsa aceptación (consumidor) a <= 2.5% cuando r = 1."),
                     tags$li(strong("Banda de Guarda de Rechazo (w = r * U):"), " Amplía el intervalo de aceptación (AL_L = TL_L - w, AL_U = TL_U + w) para mitigar el riesgo de falso rechazo (productor) a <= 2.5% cuando r = 1."),
                     tags$li(strong("Especificaciones Unilaterales:"), " Cuando solo se define TL_L o TL_U, el cálculo de probabilidad e intervalo de aceptación se adapta automáticamente al límite existente.")
                   )
                 )
        ),
        
        # TAB 3: Reporte PDF / Imprimible
        tabPanel("Reporte",
                 br(),
                 div(class = "no-print", style = "margin-bottom: 20px;",
                     tags$button(
                       class = "btn btn-primary btn-lg",
                       onclick = "window.print();",
                       icon("print"), " Imprimir / Guardar en PDF"
                     ),
                     p(style = "color: #666; font-size: 13px; margin-top: 8px;",
                       "Nota: Al hacer clic se abrirá el cuadro de diálogo de su navegador. Seleccione 'Guardar como PDF' como impresora de destino.")
                 ),
                 uiOutput("pdf_report_view")
        ),
        
        # TAB 4: Referencias APA
        tabPanel("Referencias",
                 br(),
                 wellPanel(
                   h4("Referencias"),
                   hr(),
                   p(style = "padding-left: 2em; text-indent: -2em; margin-bottom: 12px;",
                     strong("Chang, W., Cheng, J., Allaire, J. J., Sievert, C., Schloerke, B., Xie, Y., Allen, J., McPherson, J., Dipert, A., & Borges, B."), 
                     " (2024). ", em("shiny: Web Application Framework for R"), 
                     " (R package version 1.8.1.1). ",
                     a(paste0("https://", "CRAN.R-project.org/package=shiny"), href = paste0("https://", "CRAN.R-project.org/package=shiny"), target = "_blank")
                   ),
                   p(style = "padding-left: 2em; text-indent: -2em; margin-bottom: 12px;",
                     strong("Cheng, J., Sievert, C., & Posit Software, PBC."), " (2024). ", em("shinylive: Run Shiny Applications in the Browser"), 
                     " [Software de computación]. Posit Software. ",
                     a(paste0("https://", "shiny.posit.co/r/shinylive/"), href = paste0("https://", "shiny.posit.co/r/shinylive/"), target = "_blank")
                   ),
                   p(style = "padding-left: 2em; text-indent: -2em; margin-bottom: 12px;",
                     strong("GitHub, Inc."), " (2026). ", em("GitHub Actions and GitHub Pages"), 
                     " [Plataforma de CI/CD y alojamiento web]. ",
                     a(paste0("https://", "pages.github.com/"), href = paste0("https://", "pages.github.com/"), target = "_blank")
                   ),
                   p(style = "padding-left: 2em; text-indent: -2em; margin-bottom: 12px;",
                     strong("Google."), " (2026). ", em("NotebookLM / Gemini Notebook"), 
                     " [Plataforma de inteligencia artificial para desarrollo y análisis de fuentes]. Google LLC. ",
                     a(paste0("https://", "notebooklm.google.com/"), href = paste0("https://", "notebooklm.google.com/"), target = "_blank")
                   ),
                   p(style = "padding-left: 2em; text-indent: -2em; margin-bottom: 12px;",
                     strong("ILAC."), " (2019). ", em("Guidelines on decision rules and statements of conformity"), 
                     " (ILAC-G8:09/2019). International Laboratory Accreditation Cooperation. ",
                     a(paste0("https://", "ilac.org/publications-and-resources/ilac-guidance-series/"), href = paste0("https://", "ilac.org/publications-and-resources/ilac-guidance-series/"), target = "_blank")
                   ),
                   p(style = "padding-left: 2em; text-indent: -2em; margin-bottom: 12px;",
                     strong("JCGM."), " (2012). ", em("Evaluation of measurement data – The role of measurement uncertainty in conformity assessment"), 
                     " (JCGM 106:2012). Joint Committee for Guides in Metrology (BIPM, IEC, IFCC, ILAC, ISO, IUPAC, IUPAP y OIML). ",
                     a(paste0("https://", "www.bipm.org/en/committees/jc/jcgm/publications"), href = paste0("https://", "www.bipm.org/en/committees/jc/jcgm/publications"), target = "_blank")
                   ),
                   p(style = "padding-left: 2em; text-indent: -2em; margin-bottom: 12px;",
                     strong("Morales, G."), " (2026). ", em("declaraR: Evaluación de la Conformidad Metrológica"), 
                     " (Versión 2026.01) [Código fuente en R / Shinylive]. GitHub. ",
                     a(paste0("https://", "github.com/MBGabriel/declaraR.git"), href = paste0("https://", "github.com/MBGabriel/declaraR.git"), target = "_blank")
                   ),
                   p(style = "padding-left: 2em; text-indent: -2em; margin-bottom: 12px;",
                     strong("R Core Team."), " (2024). ", em("R: A language and environment for statistical computing"), 
                     ". R Foundation for Statistical Computing, Vienna, Austria. ",
                     a(paste0("https://", "www.R-project.org/"), href = paste0("https://", "www.R-project.org/"), target = "_blank")
                   ),
                   p(style = "padding-left: 2em; text-indent: -2em; margin-bottom: 12px;",
                     strong("Wickham, H."), " (2016). ", em("ggplot2: Elegant Graphics for Data Analysis"), 
                     ". Springer-Verlag New York. ",
                     a(paste0("https://", "ggplot2.tidyverse.org"), href = paste0("https://", "ggplot2.tidyverse.org"), target = "_blank")
                   )
                 )
        ),
        
        # TAB 5: Repositorio GitHub
        tabPanel("GitHub",
                 br(),
                 wellPanel(
                   h4("Repositorio Oficial del Proyecto (declaraR)"),
                   hr(),
                   p("El código fuente completo de esta aplicación web, así como la configuración del flujo de despliegue automatizado con Shinylive y GitHub Actions, se encuentra alojado en GitHub como Software Libre."),
                   br(),
                   div(style = "background-color: #f8f9fa; border: 1px solid #e9ecef; border-radius: 8px; padding: 20px; text-align: center;",
                       h3(style = "color: #0366d6; margin-top: 0; font-weight: bold;", icon("github"), " MBGabriel / declaraR"),
                       p(style = "color: #586069; font-size: 14px;", "Evaluación de la Conformidad Metrológica basada en JCGM 106 e ILAC G8"),
                       br(),
                       a(class = "shiny-btn btn btn-success btn-lg", 
                         href = paste0("https://", "github.com/MBGabriel/declaraR.git"), 
                         target = "_blank", 
                         icon("external-linkalt"), " Visitar Repositorio en GitHub"),
                       a(class = "shiny-btn btn btn-outline-secondary btn-lg", style = "margin-left: 10px;",
                         href = paste0("https://", "github.com/MBGabriel/declaraR/issues"), 
                         target = "_blank", 
                         icon("bug"), " Reportar un Problema / Issue")
                   ),
                   br(),
                   h4("Información del Repositorio y Trazabilidad:"),
                   tags$ul(
                     tags$li(strong("URL del Repositorio: "), a(paste0("https://", "github.com/MBGabriel/declaraR.git"), href = paste0("https://", "github.com/MBGabriel/declaraR.git"), target = "_blank")),
                     tags$li(strong("Despliegue Web (Shinylive): "), a(paste0("https://", "mbgabriel.github.io/declaraR/"), href = paste0("https://", "mbgabriel.github.io/declaraR/"), target = "_blank")),
                     tags$li(strong("Licencia de Software: "), "GNU General Public License v3.0 (GPL-3.0)"),
                     tags$li(strong("Versión de la App: "), "2026.01"),
                     tags$li(strong("Autor: "), "G. Morales (ORCID: 0000-0002-3596-0373)")
                   )
                 )
        )
      )
    )
  ),
  
  # Pie de página con Autor, ORCID e hipervínculo a la licencia GNU
  tags$footer(
    div(
      p("G. Morales | ORCID: ",
        a(paste0("https://", "orcid.org/0000-0002-3596-0373"), href = paste0("https://", "orcid.org/0000-0002-3596-0373"), target = "_blank", rel = "noopener noreferrer")
      ),
      p("Aplicación de Evaluación de la Conformidad Metrológica desarrollada como Software Libre.", br(),
        "Código licenciado bajo la ",
        a("GNU General Public License (GPL v3.0)", href = paste0("https://", "www.gnu.org/licenses/gpl-3.0.html"), target = "_blank", rel = "noopener noreferrer"),
        " | Fundamentado en JCGM 106 e ILAC G8."
      )
    )
  )
)

server <- function(input, output, session) {
  
  calc <- reactive({
    measurand <- if (nchar(trimws(input$measurand)) > 0) input$measurand else "Mensurando sin especificar"
    units <- if (nchar(trimws(input$units)) > 0) input$units else "u"
    norm_ref <- if (nchar(trimws(input$norm_ref)) > 0) input$norm_ref else "Normatividad sin especificar"
    
    y <- input$y_val
    U <- input$u_exp
    u_std <- U / 2 # k = 2
    
    has_lower <- input$use_tl_lower
    has_upper <- input$use_tl_upper
    
    TL_L <- if (has_lower) input$tl_lower else -Inf
    TL_U <- if (has_upper) input$tl_upper else Inf
    
    # Validar al menos un límite
    validate(
      need(has_lower || has_upper, "Debe activar al menos un límite de tolerancia (TL_L o TL_U)."),
      need(!has_lower || !has_upper || (TL_U > TL_L), "El límite superior (TL_U) debe ser mayor que el inferior (TL_L).")
    )
    
    # Banda de guarda w
    r <- if (input$rule_type == "simple") 0 else input$guard_factor
    w <- r * U
    
    # Determinación de AL según la regla
    rule <- input$rule_type
    
    if (rule == "simple") {
      AL_L <- TL_L
      AL_U <- TL_U
    } else if (rule == "guarded_accept") {
      AL_L <- if (has_lower) TL_L + w else -Inf
      AL_U <- if (has_upper) TL_U - w else Inf
    } else if (rule == "guarded_reject") {
      AL_L <- if (has_lower) TL_L - w else -Inf
      AL_U <- if (has_upper) TL_U + w else Inf
    }
    
    # Decisión binaria
    is_pass <- (y >= AL_L) && (y <= AL_U)
    
    # Probabilidad de conformidad P_c (JCGM 106 Eq. 11)
    if (u_std > 0) {
      z_upper <- if (has_upper) (TL_U - y) / u_std else Inf
      z_lower <- if (has_lower) (TL_L - y) / u_std else -Inf
      p_conformance <- pnorm(z_upper) - pnorm(z_lower)
    } else {
      p_conformance <- if ((y >= TL_L) && (y <= TL_U)) 1.0 else 0.0
    }
    
    # Riesgos específicos
    risk_false_accept <- if (is_pass) (1 - p_conformance) * 100 else 0
    risk_false_reject <- if (!is_pass) p_conformance * 100 else 0
    
    # TUR (Test Uncertainty Ratio)
    tur <- if (has_lower && has_upper) {
      (TL_U - TL_L) / (2 * U)
    } else if (has_upper) {
      abs(TL_U - y) / U
    } else {
      abs(y - TL_L) / U
    }
    
    list(
      measurand = measurand, units = units, norm_ref = norm_ref,
      y = y, U = U, u_std = u_std, w = w, r = r,
      has_lower = has_lower, has_upper = has_upper,
      TL_L = TL_L, TL_U = TL_U,
      AL_L = AL_L, AL_U = AL_U,
      is_pass = is_pass, rule = rule,
      p_conformance = p_conformance * 100,
      risk_fa = risk_false_accept,
      risk_fr = risk_false_reject,
      tur = tur
    )
  })
  
  output$status_box <- renderUI({
    res <- calc()
    rule_label <- switch(res$rule,
                         "simple" = "Regla Simple (Shared Risk)",
                         "guarded_accept" = "Banda de Guarda de Aceptación (Aceptación Resguardada)",
                         "guarded_reject" = "Banda de Guarda de Rechazo (Rechazo Resguardado)"
    )
    
    if (res$is_pass) {
      div(
        style = "background-color: #d4edda; color: #155724; border: 2px solid #c3e6cb; padding: 15px; border-radius: 5px; font-size: 18px; text-align: center;",
        strong("DECLARACIÓN DE CONFORMIDAD: CONFORME (PASS)"),
        br(),
        span(style = "font-size: 14px;", paste0("Mensurando: ", res$measurand, " | Regla: ", rule_label))
      )
    } else {
      div(
        style = "background-color: #f8d7da; color: #721c24; border: 2px solid #f5c6cb; padding: 15px; border-radius: 5px; font-size: 18px; text-align: center;",
        strong("DECLARACIÓN DE CONFORMIDAD: NO CONFORME (FAIL)"),
        br(),
        span(style = "font-size: 14px;", paste0("Mensurando: ", res$measurand, " | Regla: ", rule_label))
      )
    }
  })
  
  output$plot_conformidad <- renderPlot({
    res <- calc()
    
    # Determinar rango x para el gráfico
    ref_min <- if (res$has_lower) res$TL_L else res$y - 4 * res$U
    ref_max <- if (res$has_upper) res$TL_U else res$y + 4 * res$U
    margin <- max(res$U * 4, (ref_max - ref_min) * 0.25)
    
    x_min <- min(ref_min - margin, res$y - res$U * 3)
    x_max <- max(ref_max + margin, res$y + res$U * 3)
    
    p <- ggplot()
    
    # Zona de Aceptación (AL)
    al_xmin <- if (res$has_lower) res$AL_L else x_min
    al_xmax <- if (res$has_upper) res$AL_U else x_max
    
    p <- p + annotate("rect", xmin = al_xmin, xmax = al_xmax, ymin = 0.5, ymax = 1.5,
                      fill = "#28a745", alpha = 0.25)
    
    # Líneas de Tolerancia TL
    if (res$has_lower) {
      p <- p + geom_vline(xintercept = res$TL_L, color = "#155724", linetype = "dashed", size = 1.1) +
        annotate("text", x = res$TL_L, y = 1.65, label = paste0("TL_L = ", res$TL_L, " ", res$units), color = "#155724", fontface = "bold")
    }
    if (res$has_upper) {
      p <- p + geom_vline(xintercept = res$TL_U, color = "#155724", linetype = "dashed", size = 1.1) +
        annotate("text", x = res$TL_U, y = 1.65, label = paste0("TL_U = ", res$TL_U, " ", res$units), color = "#155724", fontface = "bold")
    }
    
    # Líneas de Aceptación AL (si difieren de TL)
    if (res$rule != "simple") {
      if (res$has_lower) {
        p <- p + geom_vline(xintercept = res$AL_L, color = "#d9534f", linetype = "dotdash", size = 1.1) +
          annotate("text", x = res$AL_L, y = 0.35, label = paste0("AL_L = ", round(res$AL_L, 2), " ", res$units), color = "#d9534f", fontface = "bold")
      }
      if (res$has_upper) {
        p <- p + geom_vline(xintercept = res$AL_U, color = "#d9534f", linetype = "dotdash", size = 1.1) +
          annotate("text", x = res$AL_U, y = 0.35, label = paste0("AL_U = ", round(res$AL_U, 2), " ", res$units), color = "#d9534f", fontface = "bold")
      }
    }
    
    # Valor Medido con Incertidumbre
    p <- p + 
      geom_errorbarh(aes(xmin = res$y - res$U, xmax = res$y + res$U, y = 1), height = 0.2, color = "#0056b3", size = 1.2) +
      geom_point(aes(x = res$y, y = 1), color = "#0056b3", size = 4) +
      annotate("text", x = res$y, y = 1.15, label = paste0("y = ", res$y, " ± ", res$U, " ", res$units), color = "#0056b3", fontface = "bold") +
      scale_y_continuous(limits = c(0.1, 1.8)) +
      coord_cartesian(xlim = c(x_min, x_max)) +
      labs(
        title = paste0("Evaluación Visual de la Conformidad: ", res$measurand),
        subtitle = paste0("Incertidumbre Expandida U = ", res$U, " ", res$units, " | Banda de Guarda w = ", round(res$w, 3), " ", res$units),
        x = paste0("Valores en ", res$units),
        y = ""
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14)
      )
    
    p
  })
  
  output$details_text <- renderUI({
    res <- calc()
    
    tl_str <- if (res$has_lower && res$has_upper) {
      paste0("[", res$TL_L, ", ", res$TL_U, "] ", res$units)
    } else if (res$has_lower) {
      paste0("≥ ", res$TL_L, " ", res$units, " (Unilateral Inferior)")
    } else {
      paste0("≤ ", res$TL_U, " ", res$units, " (Unilateral Superior)")
    }
    
    al_str <- if (res$has_lower && res$has_upper) {
      paste0("[", round(res$AL_L, 3), ", ", round(res$AL_U, 3), "] ", res$units)
    } else if (res$has_lower) {
      paste0("≥ ", round(res$AL_L, 3), " ", res$units)
    } else {
      paste0("≤ ", round(res$AL_U, 3), " ", res$units)
    }
    
    tur_str <- if (res$has_lower && res$has_upper) {
      paste0(round(res$tur, 2), ":1 (Bilateral)")
    } else {
      paste0(round(res$tur, 2), " (Distancia relativa al límite en U)")
    }
    
    HTML(paste0(
      "<p><b>Mensurando:</b> ", res$measurand, " (", res$units, ")</p>",
      "<p><b>Normatividad Aplicable:</b> ", res$norm_ref, "</p>",
      "<p><b>Intervalo de Tolerancia (TL):</b> ", tl_str, "</p>",
      "<p><b>Intervalo de Aceptación (AL):</b> ", al_str, " <i>(Banda de Guarda w = ", round(res$w, 3), " ", res$units, ")</i></p>",
      "<p><b>Relación de Incertidumbre de Prueba (TUR):</b> ", tur_str, "</p>",
      "<p><b>Probabilidad de Conformidad (P<sub>c</sub>):</b> ", round(res$p_conformance, 2), "%</p>",
      if (res$is_pass) {
        paste0("<p><b>Riesgo Específico de Falsa Aceptación (R<sub>C</sub>*):</b> ", round(res$risk_fa, 2), "%</p>")
      } else {
        paste0("<p><b>Riesgo Específico de Falso Rechazo (R<sub>P</sub>*):</b> ", round(res$risk_fr, 2), "%</p>")
      }
    ))
  })
  
  # Vista del Reporte PDF / Imprimible
  output$pdf_report_view <- renderUI({
    res <- calc()
    rule_label <- switch(res$rule,
                         "simple" = "Regla Simple (Aceptación Simple / Shared Risk, w = 0)",
                         "guarded_accept" = "Banda de Guarda de Aceptación (Aceptación Resguardada)",
                         "guarded_reject" = "Banda de Guarda de Rechazo (Rechazo Resguardado)"
    )
    
    status_bg <- if (res$is_pass) "#d4edda" else "#f8d7da"
    status_color <- if (res$is_pass) "#155724" else "#721c24"
    status_text <- if (res$is_pass) "CONFORME (PASS)" else "NO CONFORME (FAIL)"
    
    tl_str <- if (res$has_lower && res$has_upper) {
      paste0("[", res$TL_L, ", ", res$TL_U, "] ", res$units)
    } else if (res$has_lower) {
      paste0("≥ ", res$TL_L, " ", res$units)
    } else {
      paste0("≤ ", res$TL_U, " ", res$units)
    }
    
    al_str <- if (res$has_lower && res$has_upper) {
      paste0("[", round(res$AL_L, 3), ", ", round(res$AL_U, 3), "] ", res$units)
    } else if (res$has_lower) {
      paste0("≥ ", round(res$AL_L, 3), " ", res$units)
    } else {
      paste0("≤ ", round(res$AL_U, 3), " ", res$units)
    }
    
    div(class = "report-card printable-report",
        div(style = "text-align: center; border-bottom: 2px solid #0056b3; padding-bottom: 10px; margin-bottom: 20px;",
            h2(style = "color: #0056b3; margin: 0; font-weight: bold;", "INFORME DE EVALUACIÓN DE LA CONFORMIDAD METROLÓGICA"),
            p(style = "color: #666; margin-top: 5px;", paste0("Fecha de emisión: ", Sys.Date(), " | Referencia: JCGM 106:2012 / ILAC G8:09/2019"))
        ),
        
        h4(style = "color: #333; font-weight: bold;", "1. Identificación del Ensayo"),
        tags$table(class = "report-table",
                   tags$tr(tags$th("Mensurando / Propiedad:"), tags$td(res$measurand), tags$th("Unidades de Medida:"), tags$td(res$units)),
                   tags$tr(tags$th("Norma de Referencia:"), tags$td(colspan = "3", res$norm_ref)),
                   tags$tr(tags$th("Regla de Decisión Aplicada:"), tags$td(colspan = "3", rule_label))
        ),
        
        h4(style = "color: #333; font-weight: bold; margin-top: 20px;", "2. Resultados de Medición e Intervalos Metrológicos"),
        tags$table(class = "report-table",
                   tags$tr(tags$th("Valor Medido (y):"), tags$td(paste0(res$y, " ", res$units)), tags$th("Incertidumbre Expandida (U, k=2):"), tags$td(paste0(res$U, " ", res$units))),
                   tags$tr(tags$th("Límites de Tolerancia (TL):"), tags$td(tl_str), tags$th("Banda de Guarda (w = r * U):"), tags$td(paste0(round(res$w, 3), " ", res$units, " (r = ", res$r, ")"))),
                   tags$tr(tags$th("Límites de Aceptación (AL):"), tags$td(al_str), tags$th("Relación TUR:"), tags$td(paste0(round(res$tur, 2), ":1")))
        ),
        
        h4(style = "color: #333; font-weight: bold; margin-top: 20px;", "3. Evaluación de Riesgos y Dictamen Final"),
        div(style = paste0("background-color: ", status_bg, "; color: ", status_color, "; padding: 15px; border-radius: 5px; text-align: center; font-size: 20px; font-weight: bold; margin-bottom: 15px;"),
            paste0("DICTAMEN: ", status_text)
        ),
        tags$table(class = "report-table",
                   tags$tr(tags$th("Probabilidad de Conformidad (Pc):"), tags$td(paste0(round(res$p_conformance, 2), " %"))),
                   if (res$is_pass) {
                     tags$tr(tags$th("Riesgo Específico del Consumidor (Falsa Aceptación R_C*):"), tags$td(paste0(round(res$risk_fa, 2), " %")))
                   } else {
                     tags$tr(tags$th("Riesgo Específico del Productor (Falso Rechazo R_P*):"), tags$td(paste0(round(res$risk_fr, 2), " %")))
                   }
        ),
        
        div(style = "margin-top: 25px; padding-top: 15px; border-top: 1px solid #dee2e6; font-size: 12px; color: #666;",
            p(strong("Declaración de Responsabilidad Metrológica:"), 
              " La declaración de conformidad aquí emitida toma en cuenta la incertidumbre de medición acorde con la regla de decisión especificada, cumpliendo con los requerimientos de la norma ISO/IEC 17025:2017.")
        )
    )
  })
}

shinyApp(ui = ui, server = server)
