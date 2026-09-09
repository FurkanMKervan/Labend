library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(bslib)
library(readxl)
library(tools)
library(plotly)

# Define a chic and clean UI theme
my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2c3e50",
  secondary = "#18bc9c"
)

ui <- navbarPage(
  theme = my_theme,
  title = span(icon("flask"), "Labend", style = "font-size: 18px; font-weight: bold;"),
  header = tags$head(tags$style(HTML(".dt-buttons { float: right !important; margin-top: 10px; }"))),
  
  tabPanel("Molarity Calculator", icon = icon("calculator"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4(icon("cogs"), "Global Settings"),
        numericInput("mol_mw", "Molecular Weight:", value = 50000, min = 0),
        selectInput("mol_mw_unit", "MW Unit:", choices = c("Da (g/mol)" = "1", "kDa" = "1000"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Find Mass", icon = icon("weight"),
            br(),
            fluidRow(
              column(4, numericInput("mol_mass_vol", "Volume:", value = 1, min = 0)),
              column(4, selectInput("mol_mass_vol_unit", "Volume Unit:", choices = c("L" = "1", "mL" = "1e-3", "uL" = "1e-6")))
            ),
            fluidRow(
              column(4, numericInput("mol_mass_conc", "Concentration:", value = 1, min = 0)),
              column(4, selectInput("mol_mass_conc_unit", "Conc Unit:", choices = c("M" = "1", "mM" = "1e-3", "uM" = "1e-6", "nM" = "1e-9")))
            ),
            hr(),
            uiOutput("mol_result_mass")
          ),
          tabPanel("Find Volume", icon = icon("flask"),
            br(),
            fluidRow(
              column(4, numericInput("mol_vol_mass", "Mass:", value = 1, min = 0)),
              column(4, selectInput("mol_vol_mass_unit", "Mass Unit:", choices = c("g" = "1", "mg" = "1e-3", "ug" = "1e-6", "ng" = "1e-9")))
            ),
            fluidRow(
              column(4, numericInput("mol_vol_conc", "Concentration:", value = 1, min = 0)),
              column(4, selectInput("mol_vol_conc_unit", "Conc Unit:", choices = c("M" = "1", "mM" = "1e-3", "uM" = "1e-6", "nM" = "1e-9")))
            ),
            hr(),
            uiOutput("mol_result_vol")
          ),
          tabPanel("Find Concentration", icon = icon("vial"),
            br(),
            fluidRow(
              column(4, numericInput("mol_conc_mass", "Mass:", value = 1, min = 0)),
              column(4, selectInput("mol_conc_mass_unit", "Mass Unit:", choices = c("g" = "1", "mg" = "1e-3", "ug" = "1e-6", "ng" = "1e-9")))
            ),
            fluidRow(
              column(4, numericInput("mol_conc_vol", "Volume:", value = 1, min = 0)),
              column(4, selectInput("mol_conc_vol_unit", "Volume Unit:", choices = c("L" = "1", "mL" = "1e-3", "uL" = "1e-6")))
            ),
            hr(),
            uiOutput("mol_result_conc")
          )
        )
      )
    )
  ),
  
  tabPanel("ProtParam & A280", icon = icon("dna"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4(icon("keyboard"), "Sequence Input"),
        textAreaInput("prot_seq", "Amino Acid Sequence (1-letter code):", rows = 6, placeholder = "e.g., MVLSPADKTN..."),
        radioButtons("prot_cys", "Cysteines:", choices = c("All reduced" = "reduced", "All form cystines" = "cystine"), selected = "cystine"),
        hr(),
        h4(icon("lightbulb"), "A280 Measurement"),
        numericInput("prot_a280", "A280 Absorbance:", value = 0.5, min = 0, step = 0.01),
        numericInput("prot_path", "Path length (cm):", value = 1.0, min = 0, step = 0.1),
        hr(),
        radioButtons("eps_source", "Extinction Coefficient (\U03B5):", choices = c("Calculate from Sequence" = "seq", "Enter Manually" = "manual"), inline = TRUE),
        conditionalPanel(
          condition = "input.eps_source == 'manual'",
          numericInput("manual_eps", "Manual \U03B5 (M⁻¹ cm⁻¹):", value = 30000, min = 0)
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Protein Parameters", icon = icon("list-alt"),
            br(),
            h3("Sequence Parameters"),
            tableOutput("prot_params_table")
          ),
          tabPanel("Concentration (A280)", icon = icon("vial"),
            br(),
            h3("Calculated Concentration"),
            uiOutput("prot_conc_ui")
          )
        )
      )
    )
  ),
  
  tabPanel("Growth Curve", icon = icon("chart-line"),
    br(),
    br(),
    h2("Coming Soon...", align = "center", style = "color: #7f8c8d;"),
    p("This module is currently under development.", align = "center")
  ),
  
  tabPanel("Protein Quantification", icon = icon("calculator"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h4(icon("upload"), "Data Input"),
        fileInput("upload_file", "Upload Plate Data (.xlsx, .csv):", 
                  accept = c(".csv", ".xlsx", ".xls")),
        checkboxInput("subtract_blank", "Subtract Blank (if present in file)", value = TRUE),
        helpText("Upload the raw export file from your microplate reader (which has 'Well', 'Type', 'Abs' columns)."),
        hr(),
        
        h4(icon("sliders-h"), "WB Parameters"),
        numericInput("dilution_factor", "Pre-BCA Dilution Factor:", value = 10, min = 1),
        numericInput("target_mass", "Target Protein Mass (ug):", value = 30, min = 1),
        numericInput("total_vol", "Total Well Volume (uL):", value = 20, min = 1),
        numericInput("dye_x", "Loading Dye Concentration (X):", value = 4, min = 1),
        numericInput("norm_vol", "Norm. Stock Target Vol (uL):", value = 100, min = 1),
        hr(),
        h4(icon("calculator"), "Calculation"),
        p(style = "color: #7f8c8d; font-size: 0.85em;", 
          "Rename groups or enter standard concentrations in the Data Entry tab, then click calculate."),
        actionButton("run_calc", "Calculate & Plot", class = "btn-primary", style = "width: 100%; font-weight: bold;")
      ),
      
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Data Entry & Standards", icon = icon("table"),
                   br(),
                   p(strong(textOutput("blank_status_text")), style = "color: #e74c3c;"),
                   p("Groups are pulled directly from the 'Sample' column in your file. ", 
                     strong("Double-click on the 'Group_Name' or 'Conc_mg_mL' cells to edit them before calculating.")),
                   DTOutput("group_table")
          ),
          tabPanel("Standard Curve", icon = icon("chart-line"),
                   br(),
                   plotOutput("std_curve_plot", height = "600px"),
                   br(),
                   div(style = "text-align: right;", 
                       downloadButton("download_plot", "Download High-Res Plot", class = "btn-success")
                   ),
                   br(),
                   verbatimTextOutput("std_eq")
          ),
          tabPanel("Concentrations", icon = icon("vial"),
                   br(),
                   h4("Sample Results"),
                   DTOutput("conc_results")
          ),
          tabPanel("WB Prep Recipes", icon = icon("list-alt"),
                   br(),
                   h4("Phase 1: Normalization (Equalizing to C_min)"),
                   uiOutput("norm_desc_text"),
                   DTOutput("wb_norm"),
                   hr(),
                   h4("Alternative: Direct Single-Tube Mix"),
                   p("Direct pipetting volumes to reach the target well volume. (Cells highlighted in red indicate that loading the target mass is physically impossible for that sample):"),
                   DTOutput("wb_direct")
          )
        )
      )
    )
  ),
  
  tabPanel("WB Densitometry", icon = icon("image"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4(icon("upload"), "Densitometry Data"),
        radioButtons("wb_data_source", "Data Source:", 
                     choices = c("Use BCA Samples & Paste" = "bca", "Upload File" = "file")),
        
        conditionalPanel(
          condition = "input.wb_data_source == 'file'",
          fileInput("upload_wb", "Upload Data (.csv, .xlsx):", accept = c(".csv", ".xlsx", ".xls")),
          helpText("Upload densitometry results (e.g., from ImageJ). The file must contain 'Sample' and 'Area' columns.")
        ),
        
        conditionalPanel(
          condition = "input.wb_data_source == 'bca'",
          helpText("Samples below are pulled directly from your BCA results."),
          textAreaInput("bulk_paste_target", "Paste Target Areas (e.g. Protein X):", rows = 2, placeholder = "Ctrl+V here"),
          textAreaInput("bulk_paste_lc", "Paste Loading Control Areas (e.g. GAPDH):", rows = 2, placeholder = "Ctrl+V here"),
          DTOutput("bca_paste_table"),
          br()
        ),
        
        uiOutput("control_group_ui"),
        hr(),
        radioButtons("wb_plot_type", "Plot Type:", choices = c("Bar Plot" = "bar", "Line Plot" = "line"), inline = TRUE),
        br(),
        actionButton("run_wb_calc", "Calculate & Plot", class = "btn-primary", style = "width: 100%; font-weight: bold;")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Target Plot (Pre-Norm)", icon = icon("chart-bar"),
                   br(),
                   div(style = "text-align: right;", downloadButton("download_wb_plot_target", "Download High-Res Plot", class = "btn-success")),
                   br(),
                   plotOutput("wb_plot_target", height = "600px")
          ),
          tabPanel("Normalized Plot (w/ LC)", icon = icon("balance-scale"),
                   br(),
                   div(style = "text-align: right;", downloadButton("download_wb_plot_norm", "Download High-Res Plot", class = "btn-success")),
                   br(),
                   plotOutput("wb_plot_norm", height = "600px")
          ),
          tabPanel("Results Table", icon = icon("table"),
                   br(),
                   DTOutput("wb_results_table")
          )
        )
      )
    )
  ),
  
  tabPanel("DSF Analysis", icon = icon("fire"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4(icon("upload"), "Upload DSF Data"),
        fileInput("dsf_upload", "Upload Data (.txt, .csv):", accept = c(".txt", ".csv")),
        helpText("File should contain alternating Temperature and Fluorescence columns (e.g., from qPCR machine export)."),
        hr(),
        h4(icon("sliders-h"), "Analysis Parameters"),
        checkboxInput("dsf_invert_deriv", "Invert Derivative (-1x)", value = TRUE),
        helpText("Check this if your raw fluorescence decreases (peaks go down) so they point upwards."),
        sliderInput("dsf_smooth", "Smoothing Factor (spar):", min = 0.1, max = 1.0, value = 0.4, step = 0.05),
        helpText("Increase smoothing if the derivative has multiple noisy peaks. Decrease if you miss sharp peaks."),
        hr(),
        actionButton("run_dsf", "Calculate Melting Peaks", class = "btn-primary", style = "width: 100%; font-weight: bold;")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Raw Melt Curves", icon = icon("chart-line"),
            br(),
            plotlyOutput("dsf_raw_plot", height = "500px"),
            br()
          ),
          tabPanel("-Derivative (-dF/dT)", icon = icon("chart-area"),
            br(),
            plotlyOutput("dsf_deriv_plot", height = "500px"),
            br()
          ),
          tabPanel("Tm Results", icon = icon("table"),
            br(),
            DTOutput("dsf_tm_table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Read uploaded file
  raw_data <- reactive({
    req(input$upload_file)
    file_path <- input$upload_file$datapath
    ext <- tools::file_ext(input$upload_file$name)
    
    tryCatch({
      if (tolower(ext) %in% c("xls", "xlsx")) {
        raw <- readxl::read_excel(file_path, col_names = FALSE, .name_repair = "minimal")
        header_row <- which(raw[[1]] == "Well")[1]
        if (is.na(header_row)) stop("Could not find a row starting with 'Well'")
        df <- raw[(header_row + 1):nrow(raw), ]
        colnames(df) <- as.character(raw[header_row, ])
        df <- df[!is.na(df$Well) & df$Well != "", ]
      } else {
        lines <- readLines(file_path)
        header_idx <- grep("^Well", lines)[1]
        if (is.na(header_idx)) stop("Could not find a row starting with 'Well'")
        df <- read.csv(text = lines[header_idx:length(lines)], stringsAsFactors = FALSE)
      }
      
      # Clean column names
      colnames(df)[grep("Abs", colnames(df), ignore.case = TRUE)] <- "Abs"
      
      # Ensure Abs is numeric
      df$Abs <- as.numeric(df$Abs)
      
      # Filter out trailing garbage rows (e.g. Autoloading range text)
      df <- df[grepl("^[A-Za-z][0-9]{1,2}$", trimws(df$Well)), ]
      
      return(df)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Group replicates automatically
  grouped_data <- reactive({
    req(raw_data())
    df <- raw_data()
    
    df <- df %>%
      mutate(Auto_Group = Sample)
      
    blank_mean <- 0
    blank_msg <- "No blank subtracted."
    
    if (input$subtract_blank && "Blank" %in% df$Type) {
      blank_mean <- mean(df$Abs[df$Type == "Blank"], na.rm = TRUE)
      df$Abs <- df$Abs - blank_mean
      blank_msg <- sprintf("Blank Subtracted: %.4f (Blank row is hidden from table)", blank_mean)
      df <- df %>% filter(Type != "Blank")
    } else if (!input$subtract_blank && "Blank" %in% df$Type) {
      blank_msg <- "Blank detected in file but NOT subtracted (Checkbox is unchecked)."
    }
      
    summary_table <- df %>%
      group_by(Auto_Group) %>%
      summarise(
        Wells = paste(Well, collapse = ", "),
        Mean_Abs = mean(Abs, na.rm = TRUE),
        Type = first(Type),
        Replicates = n(),
        .groups = "drop"
      ) %>%
      arrange(Type != "Standard", Auto_Group) %>%
      mutate(
        Group_Name = Auto_Group,
        Conc_mg_mL = NA_real_
      )
      
    std_indices <- which(summary_table$Type == "Standard")
    default_concs <- c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1.0)
    
    if (length(std_indices) > 0) {
      for (i in seq_along(std_indices)) {
        if (i <= length(default_concs)) {
          summary_table$Conc_mg_mL[std_indices[i]] <- default_concs[i]
        } else {
          summary_table$Conc_mg_mL[std_indices[i]] <- 0
        }
      }
    }
      
    summary_table <- summary_table %>%
      select(Auto_Group, Group_Name, Wells, Mean_Abs, Conc_mg_mL, Replicates)
      
    list(df = df, summary = summary_table, blank_msg = blank_msg, blank_mean = blank_mean)
  })
  
  # Render editable table
  output$group_table <- renderDT({
    req(grouped_data())
    dt_data <- grouped_data()$summary
    
    # Columns: Auto_Group(0), Group_Name(1), Wells(2), Mean_Abs(3), Conc_mg_mL(4), Replicates(5)
    # Editable: Group_Name (1) and Conc_mg_mL (4)
    datatable(dt_data %>% mutate(Mean_Abs = round(Mean_Abs, 4)), 
              editable = list(target = 'cell', disable = list(columns = c(0, 2, 3, 5))), 
              rownames = FALSE, 
              options = list(
                dom = 't', 
                paging = FALSE, 
                ordering = FALSE,
                columnDefs = list(list(visible = FALSE, targets = 0)) # Hide Auto_Group
              )) %>%
      formatStyle('Group_Name', backgroundColor = '#f9f9f9', cursor = 'pointer') %>%
      formatStyle('Conc_mg_mL', backgroundColor = '#f9f9f9', cursor = 'pointer')
  })
  
  output$blank_status_text <- renderText({
    req(grouped_data())
    grouped_data()$blank_msg
  })
  
  edited_groups <- reactiveVal()
  observe({ 
    req(grouped_data())
    edited_groups(grouped_data()$summary) 
  })
  
  observeEvent(input$group_table_cell_edit, {
    info <- input$group_table_cell_edit
    tmp <- edited_groups()
    
    # info$col is 0-indexed.
    if (info$col == 1) tmp[info$row, info$col + 1] <- as.character(info$value)
    if (info$col == 4) tmp[info$row, info$col + 1] <- as.numeric(info$value)
    
    edited_groups(tmp)
  })
  
  # Calculation Engine
  results <- eventReactive(input$run_calc, {
    req(grouped_data())
    df <- grouped_data()$df
    grps <- edited_groups()
    blank_mean <- grouped_data()$blank_mean
    
    # Map Auto_Group to the edited Group_Name and Conc_mg_mL
    df <- df %>%
      left_join(grps %>% select(Auto_Group, Group_Name, Conc_mg_mL), by = "Auto_Group")
      
    std_data <- df %>% filter(Type == "Standard")
    unk_data <- df %>% filter(Type == "Unknown")
    
    # Regression Model
    std_summary <- std_data %>%
      group_by(Conc_mg_mL) %>%
      summarise(Mean_Abs = mean(Abs, na.rm=TRUE), SD_Abs = sd(Abs, na.rm=TRUE)) %>%
      rename(Concentration = Conc_mg_mL)
    
    model <- lm(Mean_Abs ~ Concentration, data = std_summary)
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    r_sq <- summary(model)$r.squared
    
    # Sample Concentration Calculation
    unk_data <- unk_data %>%
      mutate(
        Calculated_Conc = (Abs - intercept) / slope * input$dilution_factor,
        Calculated_Conc = ifelse(Calculated_Conc < 0, 0, Calculated_Conc)
      )
    
    unk_summary <- unk_data %>%
      mutate(Group_Name = factor(Group_Name, levels = unique(grps$Group_Name))) %>%
      group_by(Group_Name) %>%
      summarise(
        Mean_Abs = mean(Abs, na.rm=TRUE),
        Mean_Conc_mg_mL = mean(Calculated_Conc, na.rm=TRUE),
        SD_Conc = sd(Calculated_Conc, na.rm=TRUE),
        .groups = "drop"
      ) %>%
      rename(Sample = Group_Name) %>%
      arrange(Sample)
    
    # WB Calculations
    valid_concs <- unk_summary$Mean_Conc_mg_mL[unk_summary$Mean_Conc_mg_mL > 0]
    min_conc <- if(length(valid_concs) > 0) min(valid_concs, na.rm = TRUE) else NA
    
    dye_vol <- input$total_vol / input$dye_x
    norm_vol <- input$norm_vol
    
    # 1. Normalization
    norm_df <- unk_summary %>%
      select(Sample, Mean_Conc_mg_mL) %>%
      mutate(
        Target_Final_Conc = min_conc,
        Stock_uL = ifelse(!is.na(min_conc) & Mean_Conc_mg_mL > 0, norm_vol * (min_conc / Mean_Conc_mg_mL), NA),
        Water_uL = ifelse(!is.na(Stock_uL), norm_vol - Stock_uL, NA)
      )
      
    # 2. Direct WB Prep
    direct_df <- unk_summary %>%
      select(Sample, Mean_Conc_mg_mL) %>%
      mutate(
        Sample_uL = ifelse(Mean_Conc_mg_mL > 0, input$target_mass / Mean_Conc_mg_mL, NA),
        Dye_4X_uL = dye_vol,
        Water_uL = input$total_vol - Sample_uL - Dye_4X_uL
      )
      
    list(
      blank_mean = blank_mean,
      std_summary = std_summary, slope = slope, intercept = intercept, r_sq = r_sq,
      unk_summary = unk_summary, norm_df = norm_df, direct_df = direct_df, min_conc = min_conc
    )
  })
  
  std_plot_obj <- reactive({
    req(results())
    res <- results()
    
    eq_label <- sprintf("y = %.4fx + %.4f\nR² = %.4f", res$slope, res$intercept, res$r_sq)
    min_x <- min(res$std_summary$Concentration, na.rm = TRUE)
    max_y <- max(res$std_summary$Mean_Abs, na.rm = TRUE)
    
    ggplot(res$std_summary, aes(x = Concentration, y = Mean_Abs)) +
      geom_point(color = "#2c3e50", size = 4) +
      geom_errorbar(aes(ymin = Mean_Abs - SD_Abs, ymax = Mean_Abs + SD_Abs), width = 0.03, color = "#e74c3c", linewidth = 1) +
      geom_smooth(method = "lm", se = FALSE, color = "#18bc9c", linetype = "dashed", linewidth = 1.2) +
      annotate("text", x = min_x, y = max_y, label = eq_label, hjust = 0, vjust = 1, size = 6, fontface = "bold", color = "#2c3e50") +
      theme_minimal(base_size = 18) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#2c3e50", size = 22)
      ) +
      labs(title = "Standard Curve", 
           subtitle = if(res$blank_mean > 0) sprintf("(Blank value %.4f was subtracted from all absorbances)", res$blank_mean) else "",
           x = "Concentration (mg/mL)", y = "Mean Absorbance")
  })
  
  output$std_curve_plot <- renderPlot({
    std_plot_obj()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("Standard_Curve_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = std_plot_obj(), width = 10, height = 8, dpi = 300, bg = "white")
    }
  )
  
  output$std_eq <- renderText({
    req(results())
    res <- results()
    sprintf("Linear Equation: y = %.4fx + %.4f\nR-Squared: %.4f\nLowest Sample Concentration (C_min): %.3f mg/mL", 
            res$slope, res$intercept, res$r_sq, res$min_conc)
  })
  
  output$conc_results <- renderDT({
    req(results())
    datatable(results()$unk_summary %>% mutate(across(where(is.numeric), ~round(., 3))), 
              rownames = FALSE, 
              extensions = 'Buttons',
              options = list(
                dom = 'tB', 
                buttons = list(
                  'copy', 
                  list(extend = 'csv', filename = 'LabDash_Concentrations'),
                  list(extend = 'excel', filename = 'LabDash_Concentrations', title = 'Protein Concentrations', sheetName = 'Concentrations')
                ), 
                paging = FALSE
              ))
  })
  
  output$norm_desc_text <- renderUI({
    req(results())
    res <- results()
    
    if (is.na(res$min_conc)) return(HTML("<p>No valid sample concentrations calculated.</p>"))
    
    load_vol <- input$target_mass / res$min_conc
    dye_vol <- input$total_vol / input$dye_x
    water_vol <- input$total_vol - load_vol - dye_vol
    
    water_str <- if (water_vol < 0) {
      sprintf("<b style='color: red;'>IMPOSSIBLE (Volume Exceeded: %.2f &mu;L)</b>", water_vol)
    } else {
      sprintf("<b>%.2f &mu;L</b>", water_vol)
    }
    
    HTML(sprintf(
      "<p style='font-size: 1.1em;'>Volumes required to prepare a <b>%s &mu;L</b> intermediate stock at the lowest concentration (C_min = <b>%.3f mg/mL</b>):</p>
       <div style='background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #2980b9;'>
         <h5 style='margin-top: 0; color: #2980b9;'><i class='fa fa-flask'></i> After Preparing These Normalized Stocks</h5>
         <p style='margin-bottom: 0;'>To load <b>%s &mu;g</b> of protein per well in the gel, take exactly <b style='color: #c0392b; font-size: 1.2em;'>%.2f &mu;L</b> from <u>each</u> normalized stock, and mix it with <b>%.2f &mu;L</b> of %sX Loading Dye and %s of Water.</p>
       </div>",
      input$norm_vol, res$min_conc, input$target_mass, load_vol, dye_vol, input$dye_x, water_str
    ))
  })

  output$wb_norm <- renderDT({
    req(results())
    datatable(results()$norm_df %>% mutate(across(where(is.numeric), ~round(., 2))), 
              rownames = FALSE, 
              extensions = 'Buttons',
              options = list(
                dom = 'tB', 
                buttons = list(
                  'copy', 
                  list(extend = 'csv', filename = 'LabDash_WB_Normalization'),
                  list(extend = 'excel', filename = 'LabDash_WB_Normalization', title = 'WB Normalization Volumes', sheetName = 'WB_Norm')
                ), 
                paging = FALSE
              ))
  })
  
  output$wb_direct <- renderDT({
    req(results())
    df <- results()$direct_df %>% mutate(across(where(is.numeric), ~round(., 2)))
    datatable(df, rownames = FALSE, 
              extensions = 'Buttons',
              options = list(
                dom = 'tB', 
                buttons = list(
                  'copy', 
                  list(extend = 'csv', filename = 'LabDash_WB_Direct_Recipe'),
                  list(extend = 'excel', filename = 'LabDash_WB_Direct_Recipe', title = 'WB Direct Pipetting Recipe', sheetName = 'WB_Direct')
                ), 
                paging = FALSE
              )) %>% 
      formatStyle('Water_uL', backgroundColor = styleInterval(c(0), c('#ff9999', 'white')))
  })
  
  # ==========================================
  # WB DENSITOMETRY PLUGIN SERVER LOGIC
  # ==========================================
  
  # BCA Sample Linking for WB Densitometry
  bca_samples <- reactiveVal(NULL)
  
  observe({
    req(results())
    samps <- as.character(unique(results()$unk_summary$Sample))
    if(length(samps) > 0) {
      df <- data.frame(Sample = samps, Target_Area = NA_real_, LC_Area = NA_real_, stringsAsFactors = FALSE)
      bca_samples(df)
    }
  })
  
  output$bca_paste_table <- renderDT({
    req(bca_samples())
    datatable(bca_samples(), 
              editable = list(target = 'cell', disable = list(columns = 0)), 
              rownames = FALSE,
              options = list(dom = 't', paging = FALSE, bSort = FALSE))
  })
  
  observeEvent(input$bca_paste_table_cell_edit, {
    info <- input$bca_paste_table_cell_edit
    tmp <- bca_samples()
    if (info$col == 1) {
      tmp[info$row, 2] <- as.numeric(info$value)
    } else if (info$col == 2) {
      tmp[info$row, 3] <- as.numeric(info$value)
    }
    bca_samples(tmp)
  })
  
  # Helper to parse text area pastes
  parse_paste <- function(txt) {
    lines <- strsplit(txt, "[\n\r]+")[[1]]
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    vals <- sapply(lines, function(x) {
      parts <- strsplit(x, "\\s+")[[1]]
      last_part <- gsub(",", ".", parts[length(parts)])
      suppressWarnings(as.numeric(last_part))
    }, USE.NAMES = FALSE)
    vals[!is.na(vals)]
  }
  
  observeEvent(input$bulk_paste_target, {
    req(input$bulk_paste_target != "", bca_samples())
    vals <- parse_paste(input$bulk_paste_target)
    tmp <- bca_samples()
    n <- min(length(vals), nrow(tmp))
    if (n > 0) { tmp$Target_Area[1:n] <- vals[1:n] }
    bca_samples(tmp)
    updateTextAreaInput(session, "bulk_paste_target", value = "")
  })
  
  observeEvent(input$bulk_paste_lc, {
    req(input$bulk_paste_lc != "", bca_samples())
    vals <- parse_paste(input$bulk_paste_lc)
    tmp <- bca_samples()
    n <- min(length(vals), nrow(tmp))
    if (n > 0) { tmp$LC_Area[1:n] <- vals[1:n] }
    bca_samples(tmp)
    updateTextAreaInput(session, "bulk_paste_lc", value = "")
  })
  
  wb_raw_data <- reactive({
    if (input$wb_data_source == "file") {
      req(input$upload_wb)
      inFile <- input$upload_wb
      ext <- tools::file_ext(inFile$name)
      
      if (ext == "csv") {
        df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(inFile$datapath)
      } else {
        return(NULL)
      }
      
      colnames_lower <- tolower(colnames(df))
      sample_col <- grep("sample|group|name|...1", colnames_lower, value = TRUE)[1]
      lc_col <- grep("lc|loading|gapdh|actin|tubulin", colnames_lower, value = TRUE)[1]
      
      # If LC is identified, remove it from the search for target area
      search_cols <- colnames_lower
      if (!is.na(lc_col)) { search_cols[which(colnames_lower == lc_col)] <- "" }
      target_col <- grep("target|area|surface|volume|intensity|sphk", search_cols, value = TRUE)[1]
      
      if (is.na(sample_col)) sample_col <- colnames(df)[1]
      if (is.na(target_col)) target_col <- colnames(df)[2]
      
      orig_sample <- colnames(df)[which(colnames_lower == sample_col)[1]]
      orig_target <- colnames(df)[which(colnames_lower == target_col)[1]]
      
      out_df <- df %>% select(Sample = !!sym(orig_sample), Target_Area = !!sym(orig_target))
      
      if (!is.na(lc_col)) {
        orig_lc <- colnames(df)[which(colnames_lower == lc_col)[1]]
        out_df$LC_Area <- df[[orig_lc]]
      } else {
        out_df$LC_Area <- NA_real_
      }
      
      out_df <- out_df %>%
        filter(!is.na(Sample) & Sample != "") %>%
        mutate(Target_Area = as.numeric(Target_Area), LC_Area = as.numeric(LC_Area)) %>%
        filter(!is.na(Target_Area)) %>%
        mutate(Sample = factor(Sample, levels = unique(Sample)))
        
      return(out_df)
      
    } else {
      req(bca_samples())
      df <- bca_samples() %>% filter(!is.na(Target_Area))
      if (nrow(df) == 0) return(NULL)
      df <- df %>% mutate(Sample = factor(Sample, levels = unique(Sample)))
      return(df)
    }
  })
  
  output$control_group_ui <- renderUI({
    req(wb_raw_data())
    samples <- levels(wb_raw_data()$Sample)
    default_ctrl <- grep("control|ctrl", samples, ignore.case = TRUE, value = TRUE)[1]
    if (is.na(default_ctrl)) default_ctrl <- samples[1]
    
    selectInput("wb_control", "Select Control Group (Baseline = 1):", choices = samples, selected = default_ctrl)
  })
  
  wb_results <- eventReactive(input$run_wb_calc, {
    req(wb_raw_data(), input$wb_control)
    df <- wb_raw_data()
    ctrl_group <- input$wb_control
    
    # Calculate row-level LC normalization
    df <- df %>% mutate(Row_Norm = ifelse(!is.na(LC_Area) & LC_Area > 0, Target_Area / LC_Area, NA_real_))
    
    # Group and summarize
    summary_df <- df %>%
      group_by(Sample) %>%
      summarise(
        Mean_Target = mean(Target_Area, na.rm = TRUE),
        SD_Target = sd(Target_Area, na.rm = TRUE),
        Mean_LC_Norm = mean(Row_Norm, na.rm = TRUE),
        SD_LC_Norm = sd(Row_Norm, na.rm = TRUE),
        Replicates = n(),
        .groups = "drop"
      )
    
    # Get control means
    ctrl_target <- summary_df$Mean_Target[summary_df$Sample == ctrl_group]
    ctrl_lc_norm <- summary_df$Mean_LC_Norm[summary_df$Sample == ctrl_group]
    
    if (length(ctrl_target) == 0 || ctrl_target == 0) {
      showNotification("Invalid Control Group Target Area.", type = "error")
      return(NULL)
    }
    
    # Calculate Relative Densities (Fold Changes)
    summary_df <- summary_df %>%
      mutate(
        Relative_Target = Mean_Target / ctrl_target,
        SD_Relative_Target = ifelse(Replicates > 1, SD_Target / ctrl_target, 0),
        Relative_Norm = ifelse(length(ctrl_lc_norm)>0 && !is.na(ctrl_lc_norm) && ctrl_lc_norm > 0, Mean_LC_Norm / ctrl_lc_norm, NA_real_),
        SD_Relative_Norm = ifelse(Replicates > 1 & length(ctrl_lc_norm)>0 && !is.na(ctrl_lc_norm) && ctrl_lc_norm > 0, SD_LC_Norm / ctrl_lc_norm, 0)
      )
    
    list(df = summary_df, ctrl = ctrl_group)
  })
  
  # Helper function to generate standardized plots
  generate_wb_plot <- function(res, ctrl_name, y_col, sd_col, title, subtitle, plot_type) {
    if (all(is.na(res[[y_col]]))) {
      return(ggplot() + annotate("text", x=1, y=1, label="No data provided for this metric.", size=6) + theme_void())
    }
    
    npg_colors <- rep(c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", 
                        "#F39B7F", "#8491B4", "#91D1C2", "#DC0000", 
                        "#7E6148", "#B09C85"), 5)
                        
    p <- ggplot(res, aes(x = Sample, y = .data[[y_col]]))
    
    if (plot_type == "bar") {
      p <- p + 
        geom_col(aes(fill = Sample), color = "black", alpha = 0.85) +
        geom_errorbar(aes(ymin = ifelse(.data[[y_col]] - .data[[sd_col]] < 0, 0, .data[[y_col]] - .data[[sd_col]]), 
                          ymax = .data[[y_col]] + .data[[sd_col]]), 
                      width = 0.2, linewidth = 0.8, color = "black") +
        scale_fill_manual(values = npg_colors) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
    } else {
      p <- p + 
        geom_line(aes(group = 1), color = "grey40", linewidth = 1) +
        geom_point(aes(color = Sample), size = 5) +
        geom_errorbar(aes(ymin = ifelse(.data[[y_col]] - .data[[sd_col]] < 0, 0, .data[[y_col]] - .data[[sd_col]]), 
                          ymax = .data[[y_col]] + .data[[sd_col]]), 
                      width = 0.15, linewidth = 0.8, color = "black") +
        scale_color_manual(values = npg_colors) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
    }
    
    p + theme_classic(base_size = 16) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.line = element_line(color = "black", linewidth = 1),
        axis.ticks = element_line(color = "black", linewidth = 1),
        legend.position = "none"
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        y = "Relative Density (Fold Change)",
        x = "Groups"
      )
  }
  
  wb_plot_target_obj <- reactive({
    req(wb_results())
    generate_wb_plot(
      wb_results()$df, wb_results()$ctrl, 
      "Relative_Target", "SD_Relative_Target", 
      "Target Densitometry", 
      paste("Relative to:", wb_results()$ctrl, "(No Loading Control)"), 
      input$wb_plot_type
    )
  })
  
  wb_plot_norm_obj <- reactive({
    req(wb_results())
    generate_wb_plot(
      wb_results()$df, wb_results()$ctrl, 
      "Relative_Norm", "SD_Relative_Norm", 
      "Normalized Densitometry", 
      paste("Relative to:", wb_results()$ctrl, "(LC Normalized)"), 
      input$wb_plot_type
    )
  })
  
  output$wb_plot_target <- renderPlot({ wb_plot_target_obj() })
  output$wb_plot_norm <- renderPlot({ wb_plot_norm_obj() })
  
  output$download_wb_plot_target <- downloadHandler(
    filename = function() { paste("WB_Target_Only_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = wb_plot_target_obj(), width = 10, height = 7, dpi = 300, bg = "white") }
  )
  
  output$download_wb_plot_norm <- downloadHandler(
    filename = function() { paste("WB_LC_Normalized_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = wb_plot_norm_obj(), width = 10, height = 7, dpi = 300, bg = "white") }
  )
  
  output$wb_results_table <- renderDT({
    req(wb_results())
    datatable(wb_results()$df %>% mutate(across(where(is.numeric), ~round(., 4))), 
              rownames = FALSE, 
              extensions = 'Buttons',
              options = list(
                dom = 'tB', 
                buttons = list(
                  'copy', 
                  list(extend = 'csv', filename = 'LabDash_Densitometry'),
                  list(extend = 'excel', filename = 'LabDash_Densitometry', title = 'WB Densitometry Results', sheetName = 'Densitometry')
                ), 
                paging = FALSE
              ))
  })
  
  # ==========================================
  # MOLARITY CALCULATOR SERVER LOGIC
  # ==========================================
  
  # Helper to format numbers cleanly without scientific notation
  fmt_num <- function(x) {
    # Drop trailing zeros, but avoid scientific notation
    sub("\\.$", "", sub("0+$", "", format(x, scientific = FALSE, nsmall = 6, trim = TRUE)))
  }
  
  output$mol_result_mass <- renderUI({
    req(input$mol_mw, input$mol_mass_vol, input$mol_mass_conc)
    mw <- as.numeric(input$mol_mw) * as.numeric(input$mol_mw_unit)
    if (is.na(mw) || mw <= 0) return(h4("Please enter a valid Molecular Weight.", style = "color: red;"))
    
    vol_L <- as.numeric(input$mol_mass_vol) * as.numeric(input$mol_mass_vol_unit)
    conc_M <- as.numeric(input$mol_mass_conc) * as.numeric(input$mol_mass_conc_unit)
    mass_g <- conc_M * vol_L * mw
    
    HTML(sprintf("<div style='padding: 20px; background-color: #f8f9fa; border-left: 5px solid #18bc9c;'>
                   <h4 style='color: #7f8c8d;'>Required Mass:</h4>
                   <h2 style='color: #2c3e50; margin-top: 5px;'><b>%s g</b></h2>
                   <h4 style='color: #34495e;'>= %s mg</h4>
                   <h4 style='color: #34495e;'>= %s &mu;g</h4>
                 </div>", fmt_num(mass_g), fmt_num(mass_g * 1e3), fmt_num(mass_g * 1e6)))
  })
  
  output$mol_result_vol <- renderUI({
    req(input$mol_mw, input$mol_vol_mass, input$mol_vol_conc)
    mw <- as.numeric(input$mol_mw) * as.numeric(input$mol_mw_unit)
    if (is.na(mw) || mw <= 0) return(h4("Please enter a valid Molecular Weight.", style = "color: red;"))
    
    mass_g <- as.numeric(input$mol_vol_mass) * as.numeric(input$mol_vol_mass_unit)
    conc_M <- as.numeric(input$mol_vol_conc) * as.numeric(input$mol_vol_conc_unit)
    if (conc_M == 0) return(h4("Concentration cannot be 0.", style = "color: red;"))
    vol_L <- mass_g / (conc_M * mw)
    
    HTML(sprintf("<div style='padding: 20px; background-color: #f8f9fa; border-left: 5px solid #3498db;'>
                   <h4 style='color: #7f8c8d;'>Required Volume:</h4>
                   <h2 style='color: #2c3e50; margin-top: 5px;'><b>%s L</b></h2>
                   <h4 style='color: #34495e;'>= %s mL</h4>
                   <h4 style='color: #34495e;'>= %s &mu;L</h4>
                 </div>", fmt_num(vol_L), fmt_num(vol_L * 1e3), fmt_num(vol_L * 1e6)))
  })
  
  output$mol_result_conc <- renderUI({
    req(input$mol_mw, input$mol_conc_mass, input$mol_conc_vol)
    mw <- as.numeric(input$mol_mw) * as.numeric(input$mol_mw_unit)
    if (is.na(mw) || mw <= 0) return(h4("Please enter a valid Molecular Weight.", style = "color: red;"))
    
    mass_g <- as.numeric(input$mol_conc_mass) * as.numeric(input$mol_conc_mass_unit)
    vol_L <- as.numeric(input$mol_conc_vol) * as.numeric(input$mol_conc_vol_unit)
    if (vol_L == 0) return(h4("Volume cannot be 0.", style = "color: red;"))
    conc_M <- mass_g / (mw * vol_L)
    
    HTML(sprintf("<div style='padding: 20px; background-color: #f8f9fa; border-left: 5px solid #9b59b6;'>
                   <h4 style='color: #7f8c8d;'>Resulting Concentration:</h4>
                   <h2 style='color: #2c3e50; margin-top: 5px;'><b>%s M</b></h2>
                   <h4 style='color: #34495e;'>= %s mM</h4>
                   <h4 style='color: #34495e;'>= %s &mu;M</h4>
                 </div>", fmt_num(conc_M), fmt_num(conc_M * 1e3), fmt_num(conc_M * 1e6)))
  })
  
  # ==========================================
  # PROTPARAM & A280 SERVER LOGIC
  # ==========================================
  prot_data <- reactive({
    seq <- toupper(gsub("[^A-Za-z]", "", input$prot_seq))
    if (nchar(seq) == 0) return(NULL)
    
    # Safely load Peptides for advanced parameters
    has_peptides <- requireNamespace("Peptides", quietly = TRUE)
    
    # Average isotopic masses of amino acid residues
    aa_masses <- c(A=71.0788, R=156.1875, N=114.1038, D=115.0886, 
                   C=103.1388, E=129.1155, Q=128.1307, G=57.0519, 
                   H=137.1411, I=113.1594, L=113.1594, K=128.1741, 
                   M=131.1926, F=147.1766, P=97.1167, S=87.0782, 
                   T=101.1051, W=186.2132, Y=163.1760, V=99.1326)
                   
    seq_chars <- strsplit(seq, "")[[1]]
    valid_chars <- seq_chars[seq_chars %in% names(aa_masses)]
    if (length(valid_chars) == 0) return(NULL)
    len <- length(valid_chars)
    
    mw <- sum(aa_masses[valid_chars]) + 18.01524 # Add water for terminal groups
    
    # Atomic Composition (Atoms per residue in peptide bond)
    atom_C <- c(A=3, R=6, N=4, D=4, C=3, E=5, Q=5, G=2, H=6, I=6, L=6, K=6, M=5, F=9, P=5, S=3, T=4, W=11, Y=9, V=5)
    atom_H <- c(A=5, R=12, N=6, D=5, C=5, E=7, Q=8, G=3, H=7, I=11, L=11, K=12, M=9, F=9, P=7, S=5, T=7, W=10, Y=9, V=9)
    atom_N <- c(A=1, R=4, N=2, D=1, C=1, E=1, Q=2, G=1, H=3, I=1, L=1, K=2, M=1, F=1, P=1, S=1, T=1, W=2, Y=1, V=1)
    atom_O <- c(A=1, R=1, N=2, D=3, C=1, E=3, Q=2, G=1, H=1, I=1, L=1, K=1, M=1, F=1, P=1, S=2, T=2, W=1, Y=2, V=1)
    atom_S <- c(A=0, R=0, N=0, D=0, C=1, E=0, Q=0, G=0, H=0, I=0, L=0, K=0, M=1, F=0, P=0, S=0, T=0, W=0, Y=0, V=0)
    
    counts <- table(factor(valid_chars, levels = names(aa_masses)))
    
    tot_C <- sum(counts * atom_C)
    tot_H <- sum(counts * atom_H) + 2 # +2 for terminal ends
    tot_N <- sum(counts * atom_N)
    tot_O <- sum(counts * atom_O) + 1 # +1 for terminal end
    tot_S <- sum(counts * atom_S)
    tot_Atoms <- tot_C + tot_H + tot_N + tot_O + tot_S
    
    formula <- sprintf("C<sub>%d</sub>H<sub>%d</sub>N<sub>%d</sub>O<sub>%d</sub>S<sub>%d</sub>", tot_C, tot_H, tot_N, tot_O, tot_S)
    
    # Charged residues
    n_neg <- sum(valid_chars %in% c("D", "E"))
    n_pos <- sum(valid_chars %in% c("R", "K"))
    
    nW <- sum(valid_chars == "W")
    nY <- sum(valid_chars == "Y")
    nC <- sum(valid_chars == "C")
    
    eps_reduced <- (nW * 5500) + (nY * 1490)
    eps_cystine <- (nW * 5500) + (nY * 1490) + (floor(nC / 2) * 125)
    
    eps <- if (input$prot_cys == "cystine") eps_cystine else eps_reduced
    
    # Advanced parameters
    aliphatic <- NA; gravy <- NA; instability <- NA; is_unstable <- NA
    if (has_peptides) {
      seq_clean <- paste(valid_chars, collapse="")
      aliphatic <- Peptides::aIndex(seq_clean)
      instability <- Peptides::instaIndex(seq_clean)
      is_unstable <- if (instability > 40) "unstable" else "stable"
      gravy <- Peptides::hydrophobicity(seq_clean, scale="KyteDoolittle")
    }
    
    list(mw = mw, eps = eps, eps_reduced = eps_reduced, eps_cystine = eps_cystine, 
         len = len, n_neg = n_neg, n_pos = n_pos, formula = formula, tot_Atoms = tot_Atoms,
         aliphatic = aliphatic, gravy = gravy, instability = instability, is_unstable = is_unstable)
  })
  
  output$prot_params_table <- renderTable({
    res <- prot_data()
    if (is.null(res)) return(data.frame(Message = "Waiting for valid amino acid sequence..."))
    
    df <- data.frame(Parameter = character(), Value = character(), stringsAsFactors = FALSE)
    add_row <- function(p, v) { df <<- rbind(df, data.frame(Parameter = p, Value = as.character(v), stringsAsFactors = FALSE)) }
    
    add_row("<b>Length (Amino Acids)</b>", res$len)
    add_row("<b>Molecular Weight</b>", sprintf("%.2f Da", res$mw))
    add_row("<b>Total negatively charged residues (Asp + Glu)</b>", res$n_neg)
    add_row("<b>Total positively charged residues (Arg + Lys)</b>", res$n_pos)
    add_row("<b>Formula</b>", res$formula)
    add_row("<b>Total number of atoms</b>", res$tot_Atoms)
    add_row("<b>Ext. coefficient (all Cys reduced)</b>", sprintf("%d M⁻¹ cm⁻¹ <span style='color:grey; font-size:0.9em;'>(Abs 0.1%% = %.3f)</span>", res$eps_reduced, ifelse(res$eps_reduced == 0, 0, res$eps_reduced/res$mw)))
    add_row("<b>Ext. coefficient (all pairs form cystines)</b>", sprintf("%d M⁻¹ cm⁻¹ <span style='color:grey; font-size:0.9em;'>(Abs 0.1%% = %.3f)</span>", res$eps_cystine, ifelse(res$eps_cystine == 0, 0, res$eps_cystine/res$mw)))
    
    if (!is.na(res$instability)) {
      add_row("<b>Instability Index (II)</b>", sprintf("%.2f <i>(Classifies protein as %s)</i>", res$instability, res$is_unstable))
      add_row("<b>Aliphatic Index</b>", sprintf("%.2f", res$aliphatic))
      add_row("<b>Grand average of hydropathicity (GRAVY)</b>", sprintf("%.3f", res$gravy))
    }
    
    df
  }, colnames = FALSE, sanitize.text.function = function(x) x)
  
  output$prot_conc_ui <- renderUI({
    req(input$prot_a280, input$prot_path, input$eps_source)
    
    a280 <- as.numeric(input$prot_a280)
    l <- as.numeric(input$prot_path)
    
    if (is.na(a280) || is.na(l)) return(NULL)
    if (l <= 0) return(h4("Path length must be greater than 0.", style="color:red;"))
    
    res <- prot_data()
    eps_val <- 0
    mw_val <- NA
    
    if (input$eps_source == "manual") {
      req(input$manual_eps)
      eps_val <- as.numeric(input$manual_eps)
      if (!is.null(res)) mw_val <- res$mw
    } else {
      if (is.null(res)) return(h4("Please enter a protein sequence or select 'Enter Manually' for Extinction Coefficient.", style="color:#7f8c8d;"))
      eps_val <- res$eps
      mw_val <- res$mw
    }
    
    if (eps_val <= 0) return(h4("Extinction coefficient must be greater than 0.", style="color:red;"))
    
    conc_M <- a280 / (eps_val * l)
    conc_uM <- conc_M * 1e6
    
    if (!is.na(mw_val)) {
      conc_mg_ml <- conc_M * mw_val
      mg_ml_html <- sprintf("<h3 style='color: #e67e22;'>%.4f mg/mL</h3>", conc_mg_ml)
    } else {
      mg_ml_html <- "<h4 style='color: #95a5a6; font-style: italic;'>(mg/mL requires Molecular Weight from Sequence)</h4>"
    }
    
    HTML(sprintf("<div style='padding: 20px; background-color: #f8f9fa; border-left: 5px solid #f39c12;'>
                   <h2 style='color: #2c3e50; margin-top: 0;'><b>%.2f &mu;M</b></h2>
                   %s
                   <hr style='border-top: 1px solid #bdc3c7;'>
                   <p style='font-size: 0.9em; margin-bottom: 0;'>Calculated using Beer-Lambert Law: <i>c = A / (&epsilon; &times; l)</i> <br/> (Using &epsilon; = %s M⁻¹ cm⁻¹)</p>
                 </div>", conc_uM, mg_ml_html, format(eps_val, scientific=FALSE)))
  })
  
  # ==========================================
  # DSF ANALYSIS SERVER LOGIC
  # ==========================================
  
  dsf_data <- eventReactive(input$run_dsf, {
    req(input$dsf_upload)
    
    file_path <- input$dsf_upload$datapath
    ext <- tools::file_ext(input$dsf_upload$name)
    
    # Read the data (handles both comma and tab separated)
    delim <- if(tolower(ext) == "csv") "," else "\t"
    raw <- read.delim(file_path, header = TRUE, sep = delim, check.names = FALSE, stringsAsFactors = FALSE)
    
    # Process column pairs (Temp, Fluor)
    # The assumption is Col 1 = Temp, Col 2 = Fluor, Col 3 = Temp, Col 4 = Fluor ...
    
    long_raw <- data.frame()
    long_deriv <- data.frame()
    tm_results <- data.frame()
    
    for (i in seq(2, ncol(raw), by = 2)) {
      temp <- as.numeric(raw[[i-1]])
      fluor <- as.numeric(raw[[i]])
      sample_name <- colnames(raw)[i]
      
      # Clean NAs and remove non-numeric rows (e.g. metadata text at top of some exports)
      valid <- !is.na(temp) & !is.na(fluor)
      temp <- temp[valid]
      fluor <- fluor[valid]
      
      if (length(temp) < 5) next
      
      # Sort by temperature to avoid spline errors if data isn't perfectly ordered
      ord <- order(temp)
      temp <- temp[ord]
      fluor <- fluor[ord]
      
      # Spline smoothing for derivative
      spl <- smooth.spline(temp, fluor, spar = input$dsf_smooth)
      fluor_smooth <- predict(spl, temp)$y
      deriv_raw <- predict(spl, temp, deriv = 1)$y
      
      # Multiply by -1 as requested by user to make the peaks point up (if checked)
      deriv_final <- if (input$dsf_invert_deriv) -deriv_raw else deriv_raw
      
      # Find peak (Tm) using the absolute maximum to ensure it works for both valleys and peaks
      tm_idx <- which.max(abs(deriv_final))
      tm_val <- temp[tm_idx]
      
      # Build data frames
      df_r <- data.frame(Temperature = temp, Fluorescence = fluor, Sample = sample_name)
      df_d <- data.frame(Temperature = temp, Derivative = deriv_final, Sample = sample_name)
      
      long_raw <- rbind(long_raw, df_r)
      long_deriv <- rbind(long_deriv, df_d)
      
      tm_results <- rbind(tm_results, data.frame(Sample = sample_name, `Tm (C)` = round(tm_val, 2), check.names = FALSE))
    }
    
    list(raw = long_raw, deriv = long_deriv, tm = tm_results)
  })
  
  dsf_plot_raw_obj <- reactive({
    req(dsf_data())
    df <- dsf_data()$raw
    ggplot(df, aes(x = Temperature, y = Fluorescence, color = Sample)) +
      geom_line(size = 1) +
      theme_classic() +
      labs(title = "Raw Melt Curves", x = "Temperature (°C)", y = "Fluorescence") +
      theme(text = element_text(size = 14), legend.position = "right")
  })
  
  dsf_plot_deriv_obj <- reactive({
    req(dsf_data())
    df <- dsf_data()$deriv
    
    y_label <- if (input$dsf_invert_deriv) "-dF/dT" else "dF/dT"
    title_label <- if (input$dsf_invert_deriv) "-Derivative (-dF/dT)" else "Derivative (dF/dT)"
    
    ggplot(df, aes(x = Temperature, y = Derivative, color = Sample)) +
      geom_line(size = 1) +
      theme_classic() +
      labs(title = title_label, x = "Temperature (°C)", y = y_label) +
      theme(text = element_text(size = 14), legend.position = "right")
  })
  
  output$dsf_raw_plot <- renderPlotly({
    p <- dsf_plot_raw_obj()
    ggplotly(p, tooltip = c("x", "y", "color")) %>% style(visible = "legendonly") %>% layout(legend = list(title = list(text = 'Sample')))
  })
  
  output$dsf_deriv_plot <- renderPlotly({
    p <- dsf_plot_deriv_obj()
    ggplotly(p, tooltip = c("x", "y", "color")) %>% style(visible = "legendonly") %>% layout(legend = list(title = list(text = 'Sample')))
  })
  
  output$download_dsf_raw <- downloadHandler(
    filename = function() { paste("DSF_Raw_", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = dsf_plot_raw_obj(), width = 10, height = 7, dpi = 300) }
  )
  
  output$download_dsf_deriv <- downloadHandler(
    filename = function() { paste("DSF_Derivative_", Sys.Date(), ".png", sep="") },
    content = function(file) { ggsave(file, plot = dsf_plot_deriv_obj(), width = 10, height = 7, dpi = 300) }
  )
  
  output$dsf_tm_table <- renderDT({
    req(dsf_data())
    datatable(dsf_data()$tm, 
              rownames = FALSE, 
              extensions = 'Buttons',
              options = list(dom = 'tB', buttons = list('copy', 'csv', 'excel'), paging = FALSE))
  })
}

shinyApp(ui = ui, server = server)
