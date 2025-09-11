# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(colourpicker)

# UI
ui <- fluidPage(
  titlePanel("Patient Data Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      
      uiOutput("varSelectUI"),
      
      # Grouping and Faceting selectors
      uiOutput("groupSelectUI"),
      uiOutput("facetSelectUI"),
      
      hr(),
      
      # Plot Type Selection (multiple options can be selected)
      checkboxGroupInput("plot_type", "Select Plot Type(s):",
                         choices = c("Boxplot" = "boxplot",
                                     "Violin Plot" = "violin",
                                     "Individual Points" = "points",
                                     "Histogram" = "histogram")),
      
      # Color options
      colourInput("fill_color", "Default Fill Color", value = "skyblue"),
      colourInput("outline_color", "Outline Color", value = "black"),
      
      # Color pickers for each group level (to be rendered dynamically)
      uiOutput("group_color_ui"),
      
      # Theme Selection
      selectInput("ggtheme", "Select Theme:",
                  choices = c("Minimal" = "theme_minimal",
                              "Grey" = "theme_grey",
                              "Classic" = "theme_classic",
                              "Light" = "theme_light",
                              "Dark" = "theme_dark",
                              "BW" = "theme_bw"),
                  selected = "theme_minimal")
    ),
    
    mainPanel(
      plotOutput("boxPlot"),
      h4("ggplot2 Code:"),
      verbatimTextOutput("ggplotCode")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded CSV
  uploaded_data <- reactive({
    req(input$file)
    tryCatch({
      read.csv(input$file$datapath)
    }, error = function(e) {
      showNotification("Error reading file. Please upload a valid CSV.", type = "error")
      NULL
    })
  })
  
  # Variable selection UI
  output$varSelectUI <- renderUI({
    data <- uploaded_data()
    req(data)
    
    num_vars <- names(data)[sapply(data, is.numeric)]
    selectInput("selected_var", "Select Numeric Variable:", choices = num_vars)
  })
  
  # Grouping variable selection
  output$groupSelectUI <- renderUI({
    data <- uploaded_data()
    req(data)
    
    cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || (is.character(x) && n_distinct(x) < 20))]
    
    if (length(cat_vars) > 0) {
      selectInput("group_var", "Group by Variable (optional):",
                  choices = c("None", cat_vars), selected = "None")
    }
  })
  
  # Faceting variable selection
  output$facetSelectUI <- renderUI({
    data <- uploaded_data()
    req(data)
    
    cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || (is.character(x) && n_distinct(x) < 20))]
    
    if (length(cat_vars) > 0) {
      selectInput("facet_var", "Facet by Variable (optional):",
                  choices = c("None", cat_vars), selected = "None")
    }
  })
  
  # Render color pickers for each group level if group_var is selected
  output$group_color_ui <- renderUI({
    data <- uploaded_data()
    req(data)
    
    # Only show color pickers if a grouping variable is selected
    if (input$group_var != "None") {
      group_levels <- unique(data[[input$group_var]])
      
      colorPickers <- lapply(group_levels, function(level) {
        colourInput(paste0("color_", level), 
                    label = paste("Color for", level), 
                    value = sample(colors(), 1))  # Generate a random color for each level
      })
      
      do.call(tagList, colorPickers)  # Return all color pickers as a list
    }
  })
  
  # Generate color map based on the group variable and user selections
  generate_color_map <- reactive({
    data <- uploaded_data()
    req(data)
    
    if (input$group_var != "None") {
      group_levels <- unique(data[[input$group_var]])
      color_map <- sapply(group_levels, function(level) {
        color_value <- input[[paste0("color_", level)]]
        
        # Validate the color value, default to black if invalid
        if (!is.character(color_value) || !grepl("^#[0-9A-Fa-f]{6}$|^[a-zA-Z]+$", color_value)) {
          color_value <- "black"  # Fallback to black if invalid
        }
        
        return(color_value)
      })
      names(color_map) <- group_levels
      return(color_map)
    } else {
      return(rep(input$fill_color, length(unique(data[[input$group_var]]))))
    }
  })
  
  # Render the plot with overlaid plot types
  output$boxPlot <- renderPlot({
    data <- uploaded_data()
    req(data, input$selected_var)
    
    # Remove rows with missing values in selected variables
    data <- data %>%
      filter(!is.na(.data[[input$selected_var]]))  # Remove NA from selected_var
    
    # Ensure group_var is a factor
    if (input$group_var != "None") {
      data[[input$group_var]] <- as.factor(data[[input$group_var]])
    }
    
    # Create the base ggplot object
    p <- ggplot(data, aes_string(x = input$group_var, y = input$selected_var, fill = input$group_var))
    
    # Get color map from the reactive expression
    color_map <- generate_color_map()
    
    # Add selected plot types to the plot
    if ("boxplot" %in% input$plot_type) {
      p <- p + geom_boxplot(color = input$outline_color, alpha = 0.5, position = position_dodge(0.8))
    }
    
    if ("violin" %in% input$plot_type) {
      p <- p + geom_violin(color = input$outline_color, alpha = 0.5, position = position_dodge(0.8))
    }
    
    if ("points" %in% input$plot_type) {
      p <- p + geom_jitter(width = 0.2, color = input$outline_color, alpha = 0.5)
    }
    
    if ("histogram" %in% input$plot_type) {
      p <- p + geom_histogram(binwidth = 1, color = input$outline_color, alpha = 0.5, position = "identity")
    }
    
    # Apply custom colors based on group levels (only if group_var is selected)
    if (input$group_var != "None") {
      p <- p + scale_fill_manual(values = color_map)
    }
    
    # Apply Faceting (if selected)
    if (input$facet_var != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
    }
    
    # Apply labels and theme
    p <- p + labs(
      y = input$selected_var,
      title = paste("Overlayed Plots of", input$selected_var),
      x = input$group_var
    ) + match.fun(input$ggtheme)()
    
    p
  })
  
  # Show ggplot2 code
  output$ggplotCode <- renderText({
    req(input$selected_var)
    
    # Generate color map from the reactive expression
    color_map <- generate_color_map()
    
    # Generate code dynamically based on selected plot types
    plot_types_code <- lapply(input$plot_type, function(type) {
      switch(type,
             "boxplot" = paste0("  geom_boxplot(color = \"", input$outline_color, "\", alpha = 0.5, position = position_dodge(0.8))"),
             "violin" = paste0("  geom_violin(color = \"", input$outline_color, "\", alpha = 0.5, position = position_dodge(0.8))"),
             "points" = paste0("  geom_jitter(width = 0.2, color = \"", input$outline_color, "\", alpha = 0.5)"),
             "histogram" = paste0("  geom_histogram(binwidth = 1, color = \"", input$outline_color, "\", alpha = 0.5, position = \"identity\")")
      )
    })
    
    plot_types_code <- paste(plot_types_code, collapse = " +\n")
    
    facet_code <- if (input$facet_var != "None") {
      paste0("  + facet_wrap(~ ", input$facet_var, ")")
    } else {
      ""
    }
    
    color_map_code <- paste("  scale_fill_manual(values = c(", paste(names(color_map), "=", color_map, collapse = ", "), "))", sep = "")
    
    code <- paste0(
      "ggplot(data, aes(y = ", input$selected_var, ", x = ", input$group_var, ")) +\n",
      plot_types_code, facet_code, "\n",
      color_map_code, " +\n",
      "  labs(y = \"", input$selected_var, "\", title = \"Overlayed Plots of ", input$selected_var, "\") +\n",
      "  ", input$ggtheme, "()"
    )
    
    code
  })
}

# Run the app
shinyApp(ui = ui, server = server)
