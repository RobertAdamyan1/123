library(shiny)
library(readxl)
library(haven)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tools)

ui <- fluidPage(
  titlePanel("Clinical Data Analyzer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data File", 
                accept = c(".xlsx", ".xls", ".csv", ".sav", ".sas7bdat")),
      
      selectizeInput("vars", "Variables for Summary", choices = NULL, multiple = TRUE),
      checkboxInput("select_all", "Select All"),
      selectInput("group", "Group By:", choices = NULL),
      
      hr(),
      selectInput("y_var", "Dependent Variable (Y):", choices = NULL), 
      selectizeInput("x_var", "Predictors (X):", choices = NULL, multiple = TRUE),
      actionButton("run", "Run Analysis", class = "btn-primary btn-block"),
      
      hr(),
      h4("Settings Management"),
      downloadButton("save_config_csv", "Save Settings (CSV)", class = "btn-info btn-block"),
      br(), br(),
      fileInput("load_config_csv", "Load Settings (CSV)", accept = ".csv")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DTOutput("table")),
        tabPanel("Summary", DTOutput("summary")),
        tabPanel("Plots", plotlyOutput("plot"), plotlyOutput("pie_plot")),
        tabPanel("Model", DTOutput("model_table"), plotlyOutput("model_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 50 * 1024^2)
  
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- switch(ext,
                 "csv" = read.csv(input$file$datapath, stringsAsFactors = FALSE),
                 "xls" = readxl::read_excel(input$file$datapath),
                 "xlsx" = readxl::read_excel(input$file$datapath),
                 "sav" = haven::read_spss(input$file$datapath),
                 "sas7bdat" = haven::read_sas(input$file$datapath))
    
    for (col in colnames(df)) {
      if (is.character(df[[col]]) || is.factor(df[[col]])) {
        df[[col]] <- as.character(df[[col]])
        df[[col]][is.na(df[[col]]) | trimws(df[[col]]) == ""] <- "Missing"
        df[[col]] <- as.factor(df[[col]])
      }
    }
    return(df)
  })
  
  observe({
    req(data())
    cols <- names(data())
    updateSelectizeInput(session, "vars", choices = cols)
    updateSelectInput(session, "group", choices = c("None" = "", cols))
    updateSelectInput(session, "y_var", choices = cols)
    updateSelectizeInput(session, "x_var", choices = cols)
  })
  
  observeEvent(input$select_all, {
    req(data())
    updateSelectizeInput(session, "vars", 
                         selected = if (input$select_all) names(data()) else character(0))
  })
  
  output$save_config_csv <- downloadHandler(
    filename = function() { paste0("settings_", Sys.Date(), ".csv") },
    content = function(file) {
      settings_df <- data.frame(
        parameter = c("vars", "group", "y_var", "x_var"),
        value = c(paste(input$vars, collapse = "|"), input$group, 
                  input$y_var, paste(input$x_var, collapse = "|")),
        stringsAsFactors = FALSE
      )
      write.csv(settings_df, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$load_config_csv, {
    req(input$load_config_csv, data())
    conf <- read.csv(input$load_config_csv$datapath, stringsAsFactors = FALSE)
    get_val <- function(p) conf$value[conf$parameter == p]
    decode <- function(val) if (is.na(val) || val == "") character(0) else unlist(strsplit(val, "\\|"))
    
    updateSelectizeInput(session, "vars", selected = decode(get_val("vars")))
    updateSelectInput(session, "group", selected = get_val("group"))
    updateSelectInput(session, "y_var", selected = get_val("y_var"))
    updateSelectizeInput(session, "x_var", selected = decode(get_val("x_var")))
    showNotification("Настройки применены!", type = "message")
  })
  
  output$table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
    output$summary <- renderDT({
    req(data(), input$vars)
    df <- data()
    group_var <- input$group
    
    res <- lapply(input$vars, function(v) {
      if (is.numeric(df[[v]])) {
        if (group_var != "" && group_var != "None") {
          df %>% group_by(Group = as.character(.data[[group_var]])) %>%
            summarise(Variable = v, 
                      Mean = round(mean(.data[[v]], na.rm=T), 2),
                      Min = min(.data[[v]], na.rm=T), 
                      Max = max(.data[[v]], na.rm=T),
                      Percentage = round(n() / nrow(df) * 100, 2),
                      n = n(), 
                      .groups = "drop")
        } else {
          data.frame(Variable = v, 
                     Group = "All", 
                     Mean = round(mean(df[[v]], na.rm=T), 2),
                     Min = min(df[[v]], na.rm=T), 
                     Max = max(df[[v]], na.rm=T), 
                     n = nrow(df),
                     Percentage = 100.00)
        }
      } else {
        if (group_var != "" && group_var != "None") {
          df %>% group_by(Group = as.character(.data[[group_var]]), Level = as.character(.data[[v]])) %>%
            summarise(n = n(), .groups = "drop_last") %>%
            mutate(Variable = v, Percentage = round(n/sum(n)*100, 2)) %>% ungroup()
        } else {
          df %>% group_by(Level = as.character(.data[[v]])) %>%
            summarise(n = n(), .groups = "drop") %>%
            mutate(Variable = v, Group = "All", Percentage = round(n/sum(n)*100, 2))
        }
      }
    }) %>% bind_rows()
    
    if ("Level" %in% names(res)) {
      res <- res %>% select(Variable, Level, Group, n, Percentage, everything())
    } else {
      res <- res %>% select(Variable, Group, n, Percentage, everything())
    }
    
    datatable(res, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$plot <- renderPlotly({
    req(data(), input$vars)
    df <- data()
    var <- input$vars[1]
    if (input$group != "" && input$group != "None") {
      p <- ggplot(df, aes_string(x = var, fill = input$group)) + geom_bar(position = "dodge")
    } else {
      p <- ggplot(df, aes_string(x = var)) + geom_bar(fill = "steelblue")
    }
    ggplotly(p + theme_minimal())
  })
  
  output$pie_plot <- renderPlotly({
    req(data())
    df <- data()
    
    target_var <- if (input$group != "" && input$group != "None") input$group else input$vars[1]
    req(target_var)
    
    pie_data <- df %>% 
      group_by(value = as.character(.data[[target_var]])) %>% 
      summarise(n = n(), .groups = "drop")
    
    plot_ly(pie_data, labels = ~value, values = ~n, type = "pie",
            textinfo = 'label+percent',
            insidetextorientation = 'radial') %>%
      layout(title = paste("Pie Chart:", target_var),
             showlegend = TRUE)
  })
  
  model <- eventReactive(input$run, {
    req(data(), input$x_var, input$y_var)
    df <- data()
    
    y_val <- df[[input$y_var]]
    x_formula <- paste(input$x_var, collapse = " + ")
    
    if (is.numeric(y_val)) {
      formula_str <- paste(input$y_var, "~", x_formula)
      lm(as.formula(formula_str), data = df)
    } else {
      formula_str <- paste("as.factor(", input$y_var, ") ~", x_formula)
      glm(as.formula(formula_str), data = df, family = binomial)
    }
  })
  
  output$model_table <- renderDT({
    req(model())
    datatable(round(as.data.frame(summary(model())$coefficients), 4), rownames = TRUE)
  })
  
  output$model_plot <- renderPlotly({
    req(model(), data(), input$x_var)
    df <- data()
    first_x <- input$x_var[1]
    p <- ggplot(df, aes_string(x = first_x, y = input$y_var))
    
    if (is.numeric(df[[input$y_var]])) {
      p <- p + geom_point() + geom_smooth(method = "lm", formula = y ~ x)
    } else {
      p <- p + geom_jitter(width = 0.1, height = 0.1)
    }
    ggplotly(p + theme_minimal() + ggtitle(paste("Y:", input$y_var, "vs X:", first_x)))
  })
}

shinyApp(ui, server)
