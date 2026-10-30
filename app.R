library(shiny)
library(readxl)
library(DT)
library(ggplot2)

# -----------------------------
# Load dataset
# -----------------------------
data <- read_excel("crop_yield_two_way_anova_dataset.xlsx")

data$Irrigation <- factor(
  data$Irrigation,
  levels = c("Low", "Medium", "High")
)
data$Nitrogen_kg_ha <- factor(
  data$Nitrogen_kg_ha,
  levels = c(60, 120, 180)
)

# Two-way ANOVA with interaction
model <- aov(
  Yield_t_ha ~ Irrigation * Nitrogen_kg_ha,
  data = data
)

anova_df <- as.data.frame(summary(model)[[1]])
anova_df$Effect <- rownames(anova_df)
rownames(anova_df) <- NULL
names(anova_df) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)", "Effect")
anova_df <- anova_df[, c("Effect", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #f3f6fb;
        font-family: Arial, sans-serif;
      }

      .navbar {
        background: linear-gradient(90deg, #172554, #2563eb) !important;
        border: none;
      }

      .navbar-brand {
        color: white !important;
        font-weight: 700;
      }

      .nav-tabs > li > a {
        font-weight: 600;
      }

      .dashboard-title {
        background: linear-gradient(135deg, #172554, #2563eb);
        color: white;
        padding: 25px;
        border-radius: 14px;
        margin-bottom: 20px;
        box-shadow: 0 5px 18px rgba(37,99,235,0.20);
      }

      .dashboard-title h2 {
        margin-top: 0;
        font-weight: 700;
      }

      .metric-card {
        background: white;
        border-radius: 14px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 3px 14px rgba(15,23,42,0.08);
        border-left: 5px solid #2563eb;
        min-height: 110px;
      }

      .metric-label {
        color: #64748b;
        font-size: 14px;
        font-weight: 600;
        text-transform: uppercase;
      }

      .metric-value {
        color: #0f172a;
        font-size: 29px;
        font-weight: 800;
        margin-top: 8px;
      }

      .section-card {
        background: white;
        border-radius: 14px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 3px 14px rgba(15,23,42,0.08);
      }

      .section-card h3 {
        color: #172554;
        font-weight: 700;
        margin-top: 0;
      }

      .sidebar-panel {
        background: white;
        border-radius: 14px;
        padding: 18px;
        box-shadow: 0 3px 14px rgba(15,23,42,0.08);
      }

      .status-box {
        background: #ecfdf5;
        border-left: 5px solid #10b981;
        padding: 14px;
        border-radius: 9px;
        color: #065f46;
        font-weight: 600;
        margin-top: 15px;
      }

      .btn-primary {
        background: #2563eb;
        border-color: #2563eb;
      }

      table.dataTable thead th {
        background: #172554;
        color: white;
      }

      .footer {
        text-align: center;
        color: #64748b;
        padding: 20px;
      }
    "))
  ),
  navbarPage(
    title = "🌾 Crop Yield Analytics",
    tabPanel(
      "Dashboard",
      div(
        class = "dashboard-title",
        h2("Two-Way ANOVA Crop Yield Dashboard"),
        p("Interactive analysis of irrigation, nitrogen fertilizer and their interaction on crop yield.")
      ),
      fluidRow(
        column(
          3,
          div(
            class = "metric-card",
            div(class = "metric-label", "Observations"),
            div(class = "metric-value", nrow(data))
          )
        ),
        column(
          3,
          div(
            class = "metric-card",
            div(class = "metric-label", "Average Yield"),
            div(class = "metric-value", paste0(round(mean(data$Yield_t_ha), 2), " t/ha"))
          )
        ),
        column(
          3,
          div(
            class = "metric-card",
            div(class = "metric-label", "Maximum Yield"),
            div(class = "metric-value", paste0(max(data$Yield_t_ha), " t/ha"))
          )
        ),
        column(
          3,
          div(
            class = "metric-card",
            div(class = "metric-label", "Best Treatment"),
            div(class = "metric-value", "High + 180 kg/ha")
          )
        )
      ),
      fluidRow(
        column(
          7,
          div(
            class = "section-card",
            h3("📊 Mean Yield by Irrigation and Nitrogen"),
            plotOutput("mainPlot", height = "390px")
          )
        ),
        column(
          5,
          div(
            class = "section-card",
            h3("🌱 Yield Distribution"),
            plotOutput("boxPlot", height = "390px")
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "section-card",
            h3("📈 Interaction Effect"),
            p("Non-parallel lines indicate that the effect of nitrogen changes with irrigation level."),
            plotOutput("interactionPlot", height = "420px")
          )
        )
      )
    ),
    tabPanel(
      "Dataset",
      fluidRow(
        column(
          3,
          div(
            class = "sidebar-panel",
            h3("🔎 Filters"),
            selectInput(
              "irrigation",
              "Irrigation:",
              choices = c("All", levels(data$Irrigation)),
              selected = "All"
            ),
            selectInput(
              "nitrogen",
              "Nitrogen (kg/ha):",
              choices = c("All", levels(data$Nitrogen_kg_ha)),
              selected = "All"
            ),
            br(),
            downloadButton("downloadData", "Download Filtered CSV", class = "btn-primary"),
            div(
              class = "status-box",
              "Dataset contains 27 observations across 3 irrigation levels, 3 nitrogen levels and 3 replicates."
            )
          )
        ),
        column(
          9,
          div(
            class = "section-card",
            h3("📋 Complete Experimental Dataset"),
            DTOutput("dataTable")
          )
        )
      )
    ),
    tabPanel(
      "ANOVA Results",
      fluidRow(
        column(
          8,
          div(
            class = "section-card",
            h3("🧪 Two-Way ANOVA"),
            p("Model: Yield ~ Irrigation × Nitrogen"),
            DTOutput("anovaTable")
          )
        ),
        column(
          4,
          div(
            class = "section-card",
            h3("Interpretation"),
            uiOutput("anovaInterpretation")
          )
        )
      )
    ),
    tabPanel(
      "Treatment Comparison",
      fluidRow(
        column(
          12,
          div(
            class = "section-card",
            h3("🏆 Average Yield for Each Treatment"),
            plotOutput("treatmentPlot", height = "500px")
          )
        )
      )
    ),
    tabPanel(
      "Summary",
      fluidRow(
        column(
          6,
          div(
            class = "section-card",
            h3("📌 Statistical Summary"),
            verbatimTextOutput("summaryText")
          )
        ),
        column(
          6,
          div(
            class = "section-card",
            h3("💡 Key Findings"),
            uiOutput("keyFindings")
          )
        )
      )
    ),
    div(class = "footer", "Crop Yield Analytics • Two-Way ANOVA with Interaction Effects")
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  filteredData <- reactive({
    d <- data

    if (input$irrigation != "All") {
      d <- d[d$Irrigation == input$irrigation, ]
    }

    if (input$nitrogen != "All") {
      d <- d[d$Nitrogen_kg_ha == input$nitrogen, ]
    }

    d
  })

  output$dataTable <- renderDT({
    datatable(
      filteredData(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip"
      ),
      extensions = "Buttons",
      class = "display stripe hover"
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("filtered_crop_yield_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )

  output$mainPlot <- renderPlot({
    summary_data <- aggregate(
      Yield_t_ha ~ Irrigation + Nitrogen_kg_ha,
      data = data,
      FUN = mean
    )

    ggplot(
      summary_data,
      aes(
        x = Nitrogen_kg_ha,
        y = Yield_t_ha,
        fill = Irrigation
      )
    ) +
      geom_col(
        position = position_dodge(width = 0.75),
        width = 0.65
      ) +
      geom_text(
        aes(label = round(Yield_t_ha, 2)),
        position = position_dodge(width = 0.75),
        vjust = -0.4,
        size = 4
      ) +
      scale_fill_manual(
        values = c(
          "Low" = "#60a5fa",
          "Medium" = "#2563eb",
          "High" = "#172554"
        )
      ) +
      labs(
        x = "Nitrogen Fertilizer (kg/ha)",
        y = "Mean Yield (t/ha)",
        fill = "Irrigation"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "top"
      )
  })

  output$boxPlot <- renderPlot({
    ggplot(
      data,
      aes(
        x = Irrigation,
        y = Yield_t_ha,
        fill = Irrigation
      )
    ) +
      geom_boxplot(alpha = 0.85, width = 0.65) +
      geom_jitter(width = 0.08, size = 2.5, alpha = 0.75) +
      scale_fill_manual(
        values = c(
          "Low" = "#93c5fd",
          "Medium" = "#a2ff00",
          "High" = "#1f51d8"
        )
      ) +
      labs(
        x = "Irrigation Level",
        y = "Yield (t/ha)"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$interactionPlot <- renderPlot({
    interaction_data <- aggregate(
      Yield_t_ha ~ Irrigation + Nitrogen_kg_ha,
      data = data,
      FUN = mean
    )

    ggplot(
      interaction_data,
      aes(
        x = Nitrogen_kg_ha,
        y = Yield_t_ha,
        group = Irrigation,
        color = Irrigation
      )
    ) +
      geom_line(linewidth = 1.4) +
      geom_point(size = 4) +
      geom_text(
        aes(label = round(Yield_t_ha, 2)),
        vjust = -0.9,
        size = 4
      ) +
      scale_color_manual(
        values = c(
          "Low" = "#60a5fa",
          "Medium" = "#2563eb",
          "High" = "#ef4444"
        )
      ) +
      labs(
        x = "Nitrogen Fertilizer (kg/ha)",
        y = "Mean Yield (t/ha)",
        color = "Irrigation"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })

  output$treatmentPlot <- renderPlot({
    treatment_data <- aggregate(
      Yield_t_ha ~ Irrigation + Nitrogen_kg_ha,
      data = data,
      FUN = mean
    )

    treatment_data$Treatment <- paste(
      treatment_data$Irrigation,
      treatment_data$Nitrogen_kg_ha,
      "kg/ha"
    )

    ggplot(
      treatment_data,
      aes(
        x = reorder(Treatment, Yield_t_ha),
        y = Yield_t_ha,
        fill = Irrigation
      )
    ) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = round(Yield_t_ha, 2)),
        vjust = -0.4,
        fontface = "bold"
      ) +
      coord_flip() +
      scale_fill_manual(
        values = c(
          "Low" = "#60a5fa",
          "Medium" = "#2563eb",
          "High" = "#ef4444"
        )
      ) +
      labs(
        x = "Treatment",
        y = "Average Yield (t/ha)",
        fill = "Irrigation"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$anovaTable <- renderDT({
    display <- anova_df

    display$`Sum Sq` <- round(display$`Sum Sq`, 4)
    display$`Mean Sq` <- round(display$`Mean Sq`, 4)
    display$`F value` <- round(display$`F value`, 4)

    display$`Pr(>F)` <- ifelse(
      is.na(display$`Pr(>F)`),
      "",
      format.pval(display$`Pr(>F)`, digits = 4, eps = 0.0001)
    )

    datatable(
      display,
      rownames = FALSE,
      options = list(pageLength = 10, dom = "t"),
      class = "display stripe hover"
    )
  })

  output$anovaInterpretation <- renderUI({
    HTML(paste0(
      "<p><b>Irrigation:</b> Significant effect on crop yield (p &lt; 0.001).</p>",
      "<p><b>Nitrogen:</b> Significant effect on crop yield (p &lt; 0.001).</p>",
      "<p><b>Interaction:</b> Significant irrigation × nitrogen interaction (p &lt; 0.001).</p>",
      "<hr>",
      "<p><b>Conclusion:</b> Crop yield depends on both irrigation and nitrogen fertilizer, and the effect of nitrogen is different at different irrigation levels.</p>"
    ))
  })

  output$summaryText <- renderPrint({
    cat("CROP YIELD SUMMARY\n")
    cat("===================\n\n")
    cat("Observations:", nrow(data), "\n")
    cat("Irrigation levels:", paste(levels(data$Irrigation), collapse = ", "), "\n")
    cat("Nitrogen levels:", paste(levels(data$Nitrogen_kg_ha), collapse = ", "), "kg/ha\n\n")
    cat("Minimum yield:", min(data$Yield_t_ha), "t/ha\n")
    cat("Maximum yield:", max(data$Yield_t_ha), "t/ha\n")
    cat("Mean yield:", round(mean(data$Yield_t_ha), 2), "t/ha\n")
    cat("Standard deviation:", round(sd(data$Yield_t_ha), 3), "t/ha\n")
  })

  output$keyFindings <- renderUI({
    findings <- c(
      "Higher irrigation generally increases crop yield.",
      "Increasing nitrogen from 60 to 180 kg/ha increases yield.",
      "The highest average yield occurs under High irrigation + 180 kg/ha nitrogen.",
      "The significant interaction means nitrogen does not have the same effect at every irrigation level.",
      "The dataset contains 27 synthetic/sample observations."
    )

    tagList(lapply(findings, function(x) {
      div(
        style = "padding:10px; margin-bottom:8px; background:#eff6ff; border-left:4px solid #2563eb; border-radius:6px;",
        paste0("✓ ", x)
      )
    }))
  })
}

shinyApp(ui = ui, server = server)
