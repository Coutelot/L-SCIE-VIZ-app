# Load required packages
if (!require("shiny")) install.packages("shiny", repos = "https://cran.rstudio.com/")
if (!require("shinydashboard")) install.packages("shinydashboard", repos = "https://cran.rstudio.com/")
if (!require("DT")) install.packages("DT", repos = "https://cran.rstudio.com/")
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cran.rstudio.com/")
if (!require("plotly")) install.packages("plotly", repos = "https://cran.rstudio.com/")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cran.rstudio.com/")
if (!require("reshape2")) install.packages("reshape2", repos = "https://cran.rstudio.com/")
if (!require("factoextra")) install.packages("factoextra", repos = "https://cran.rstudio.com/")
if (!require("viridis")) install.packages("viridis", repos = "https://cran.rstudio.com/")
if (!require("shinythemes")) install.packages("shinythemes")
# Load libraries
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(reshape2)
library(factoextra)
library(viridis)
library(shinythemes)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(reshape2)
library(factoextra)
library(viridis)

# Load the data
data <- read.csv("C:/Users/fanny/OneDrive - Clemson University/NEUP/Database/Shiny-app/L-VIZ/VIZ-APP/VIZ-APP/sc.dataset_added-column.csv")
#data %>%
#  select(A, B, E)
######################################################################################################################################################
######################################################################################################################################################
server <- function(input, output, session) {
  # Filter data based on selections #######################################################################################################################################
  ######################################################################################################################################################
 
    # Reactive function to filter data
   output$table <- DT::renderDataTable({
    filtered_data <- data
    if (input$Mineral != "All") {
      filtered_data <- filtered_data[filtered_data$Mineral == input$Mineral, ]
    }
    if (input$Sorbate != "All") {
      filtered_data <- filtered_data[filtered_data$Sorbate == input$Sorbate, ]
    }
    if (input$Reference != "All") {
      filtered_data <- filtered_data[filtered_data$Reference == input$Reference, ]
    }
    DT::datatable(filtered_data)
  })
   
   # Boxplot #################################################################################################################################
   ###########################################################################################################################################
   
   # Prepare mineral data summary using dplyr::filter on a local copy of data
   mineral_summary <- reactive({
     local_data <- data
     dplyr::filter(local_data, !is.na(Mineral_Class) & Mineral_Class != '') %>%
       group_by(Mineral_Class) %>%
       summarise(Count = n()) %>%
       arrange(desc(Count))
   })
   
   # Prepare sorbate data summary
   sorbate_summary <- reactive({
     local_data <- data
     dplyr::filter(local_data, !is.na(Sorbate) & Sorbate != '') %>%
       group_by(Sorbate) %>%
       summarise(Count = n()) %>%
       arrange(desc(Count))
   })
   
   
   # Bar plot for Mineral_Class
   output$barMineral <- renderPlot({
ggplot(mineral_summary(), aes(x = Count, y = reorder(Mineral_Class, Count))) + 
       geom_bar(stat = 'identity', fill = 'steelblue') +
       labs(x = 'Number of Data Points', y = 'Mineral Class') +
       theme_minimal()
   })
   
   # Bar plot for Sorbate
   output$barSorbate <- renderPlot({
ggplot(sorbate_summary(), aes(x = Count, y = reorder(Sorbate, Count))) + 
       geom_bar(stat = 'identity', fill = 'darkorange') +
       labs(x = 'Number of Data Points', y = 'Sorbate') +
       theme_minimal()
   })
   
   
   # Pie chart for Mineral_Class
   output$pieMineral <- renderPlotly({
     data_pie <- mineral_summary()
     p1 <- plot_ly(data_pie, labels = ~Mineral_Class, values = ~Count, type = 'pie') %>%
       layout(title = 'Distribution of Mineral Classes')
     p1
   })
   
   # Pie chart for Sorbate
   output$pieSorbate <- renderPlotly({
     data_pie <- sorbate_summary()
     p2 <- plot_ly(data_pie, labels = ~Sorbate, values = ~Count, type = 'pie') %>%
       layout(title = 'Distribution of Sorbate')
     p2
   })
   
   # Summary Table (using explicit subsetting on data)
   output$summaryTable <- renderTable({
     total_references <- length(unique(data$Reference))
     total_datapoints <- nrow(data)
     total_datasets <- length(unique(data$Set))
     total_sorbate <- length(unique(data$Sorbate[data$Sorbate != '' & !is.na(data$Sorbate)]))
     total_minerals <- length(unique(data$Mineral[data$Mineral != '' & !is.na(data$Mineral)]))
     
     summary_data <- data.frame(
       Metric = c('Number of References', 'Number of Data Points', 'Number of Datasets', 'Number of Sorbate', 'Number of Minerals'),
       Count = c(total_references, total_datapoints, total_datasets, total_sorbate, total_minerals),
       stringsAsFactors = FALSE
     )
     summary_data
   })
   
  # Scatter plot/ view data tab  #######################################################################################################################################
  ######################################################################################################################################################
  
  # Reactive data based on user input
  filteredData <- reactive({
    data_subset <- data

    if (input$mineral != "All") {
      data_subset <- data_subset[data_subset$Mineral_Class == input$mineral, ]
    }
    if (input$sorbate != "All") {
      data_subset <- data_subset[data_subset$Sorbate == input$sorbate, ]
    }
    # Check required columns
    required_cols <- c("pH", "Aq_val", "Mineral", "Sorbate", "percent_sorbed", "Kd")
    if (!all(required_cols %in% colnames(data_subset))) {
      stop("Missing required columns for scatter plot: ", paste(setdiff(required_cols, colnames(data_subset)), collapse = ", "))
    }
    # Remove rows with NA values in relevant columns
    data_subset <- na.omit(data_subset[, required_cols])
    data_subset
  })
  
  # Render the scatter plot with the aqueaous concentration vs pH
  output$scatterPlot_aq <- renderPlot({
    req(filteredData())  # Ensure data is available
    ggplot(filteredData(), aes(x = pH, y = Aq_val, color = interaction(Mineral, Sorbate))) +
      geom_point(size = 3, alpha = 0.7) +
       scale_y_log10() +
      #scale_y_continuous(limits=c(0, 12))+
      # ylim(0, 20000)+
      scale_color_viridis(discrete = TRUE) + 
      labs(title = "pH vs Aq_val",
           x = "pH",
           y = "Aq aqueous concentration (mol/L)",
           color = "Mineral & Sorbate") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "right"
      )
  })
  
  # Render the scatter plot with the aqueaous concentration vs pH
  output$scatterPlot_percent <- renderPlot({
    req(filteredData())  # Ensure data is p
    ggplot(filteredData(), aes(x = pH, y = percent_sorbed, color = interaction(Mineral, Sorbate))) +
      geom_point(size = 3, alpha = 0.7) +
      # scale_x_log10() +
      #scale_y_continuous(limits=c(0, 12))+
      ylim(0, 100)+
      scale_color_viridis(discrete = TRUE) + 
      labs(title = "pH vs sorbed fraction",
           x = "pH",
           y = "Sorbed (%)",
           color = "Mineral & Sorbate") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "right"
      )
  })
  
 
  # Kd Analysis ################################################################################################################################
  ##############################################################################################################################################
  # Clean the dataset by removing infinite and NA values, and extreme outliers
  # Load and clean the dataset
  data_clean <- reactive({
    data <- read.csv("sc.dataset_added-column.csv")
    data %>%
      filter(!is.na(Kd)) %>%
      filter(is.finite(Kd)) %>%
      filter(Kd > -1e6 & Kd < 1e6)
  })
  
  # Create dynamic UI for mineral class selection
  output$mineral_class_input <- renderUI({
    req(data_clean())
    mineral_classes <- unique(data_clean()$Mineral_Class)
    checkboxGroupInput("mineral_class_select",
                       "Select Mineral Classes:",
                       choices = mineral_classes,
                       selected = mineral_classes)
  })
  
  # Reactive function to filter data
  filtered_data <- reactive({
    req(data_clean(), input$mineral_class_select)
    data_clean() %>%
      filter(Mineral_Class %in% input$mineral_class_select)
  })
  
  # Reactive function for count data
  count_data <- reactive({
    req(filtered_data(), input$mineral_class_select)
    data.frame(
      Mineral_Class = input$mineral_class_select,
      total_sorbates = sapply(input$mineral_class_select, function(x) {
        n_distinct(filtered_data()$Sorbate[filtered_data()$Mineral_Class == x])
      })
    )
  })
  
  # Main plot
  output$box_bar_plot <- renderPlot({
    req(filtered_data(), count_data())
    validate(need(nrow(filtered_data()) > 0, "No data available for selected criteria"))
    
    max_count <- max(count_data()$total_sorbates)
    
    ggplot() +
      # Bar plot with one color
      geom_bar(data = count_data(), 
               aes(x = Mineral_Class, y = total_sorbates),
               stat = "identity", fill = "steelblue", alpha = 0.6) +
      # Box plot with a notch for the mean
      geom_boxplot(data = filtered_data(), 
                   aes(x = Mineral_Class, 
                       y = log10(Kd + 1) * max_count/10,
                       group = Mineral_Class),
                   color = "black", fill = "gray", notch = TRUE, alpha = 0.5) +
      scale_y_continuous(
        name = "Number of Unique Sorbates",
        sec.axis = sec_axis(~. * 10/max_count,
                            name = "log10(Kd)")
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1), # Vertical orientation
        axis.title.y.left = element_text(color = "black"),
        axis.title.y.right = element_text(color = "black"),
        legend.position = "right", # Add legend to the right
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      ) +
      labs(x = "Mineral Class",
           title = "Distribution of Unique Sorbates and Kd Values by Mineral Class")
  })
  # Data summary output
  output$data_summary <- renderPrint({
    req(filtered_data())
    summary(filtered_data())
  })
  
  # Mineral class statistics
  output$mineral_stats <- renderTable({
    req(filtered_data())
    filtered_data() %>%
      group_by(Mineral_Class) %>%
      summarise(
        n_observations = n(),
        mean_Kd = mean(Kd),
        median_Kd = median(Kd),
        n_unique_sorbates = n_distinct(Sorbate)
      )
  })
  
  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      "mineral_class_analysis.png"
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 15, height = 8)
    }
  )
  
  
  # Kd explorer #######################################################################################################
  ######################################################################################################################
  
 
  
  # Initialize the choices
  observe({
    req(data_clean())
    # Update Mineral Classes
    updateSelectInput(session, "mineral_class_select",
                      choices = sort(unique(data_clean()$Mineral_Class)),
                      selected = unique(data_clean()$Mineral_Class)[1])
    
    # Update Minerals
    updateSelectizeInput(session, "mineral_select",
                         choices = sort(unique(data_clean()$Mineral)))
    
    # Update Sorbates
    updateSelectizeInput(session, "sorbate_select",
                         choices = sort(unique(data_clean()$Sorbate)))
  })
  
  # Reactive function to filter data
  filtered_data <- reactive({
    req(input$mineral_class_select)
    filtered <- data_clean() %>%
      filter(Mineral_Class %in% input$mineral_class_select) %>%
      filter(log10(Kd) >= input$kd_range[1],
             log10(Kd) <= input$kd_range[2])
    
    if (!is.null(input$mineral_select) && length(input$mineral_select) > 0) {
      filtered <- filtered %>%
        filter(Mineral %in% input$mineral_select)
    }
    
    if (!is.null(input$sorbate_select) && length(input$sorbate_select) > 0) {
      filtered <- filtered %>%
        filter(Sorbate %in% input$sorbate_select)
    }
    
    filtered
  })
  
  # Create the boxplot
  output$boxplot <- renderPlot({
    req(filtered_data())
    validate(need(nrow(filtered_data()) > 0, "No data available for selected criteria"))
    
    ggplot(filtered_data(), aes(x = log10(Kd), y = Mineral, fill = Sorbate)) +
      geom_boxplot(outlier.shape = if(input$show_outliers) 16 else NA) +
      facet_wrap(~Mineral_Class, scales = "free_y") +
      scale_fill_viridis(discrete = TRUE) +
      theme_bw() +
      theme(
        axis.text.y = element_text(angle = 0, hjust = 1, size = 8),
        legend.position = "right", # Add legend to the right
        strip.text = element_text(size = 10, face = "bold"),
        panel.spacing = unit(0.5, "lines"),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      ) +
      labs(x = "log10(Kd)", y = "Mineral",
           title = "Distribution of Kd Values by Mineral Class",
           fill = "Sorbate") # Add legend title for Sorbate
  })
  
  # Data summary output
  output$data_summary <- renderPrint({
    req(filtered_data())
    summary_data <- filtered_data() %>%
      group_by(Mineral_Class) %>%
      summarise(
        n_observations = n(),
        mean_Kd = mean(log10(Kd), na.rm = TRUE),
        median_Kd = median(log10(Kd), na.rm = TRUE),
        n_minerals = n_distinct(Mineral)
      )
    print(summary_data)
  })
  
  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      "kd_distribution_analysis.png"
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 15, height = 12)
    }
  )
  
  
  # PCA Analysis ################################################################################################################################
  ##############################################################################################################################################
  
  # Load and clean the dataset
  
  # Load and clean the dataset for PCA
  data_clean_pca <- reactive({
    data_pca <- read.csv("sc.dataset_added-column.csv")
    
    # Select columns for PCA
    selected_cols <- c("Temp", "pH", "Sorbate_val", "Aq_val", 
                       "Sorbed_val", "Mineral_val", "MineralSA", "Kd")
    
    # Include Sorbate and Mineral columns for grouping and coloring
    data_cleaned_pca <- data_pca %>%
      select(all_of(c(selected_cols, "Sorbate", "Mineral"))) %>%
      # Remove rows with NA values
      na.omit() %>%
      # Remove rows with infinite values
      filter_all(all_vars(is.finite(.)))
    
    # Print number of rows after cleaning
    print(paste("Number of rows after cleaning:", nrow(data_cleaned_pca)))
    
    return(data_cleaned_pca)
  })
  
  # Initialize Sorbate choices
  observe({
    req(data_clean_pca())
    sorbate_choices <- unique(data_clean_pca()$Sorbate)
    print("Available Sorbate choices:")
    print(sorbate_choices)
    
    updateSelectizeInput(session, "sorbate_select",
                         choices = sort(sorbate_choices),
                         selected = NULL)
  })
  
  # Perform PCA
  pca_results <- reactive({
    req(data_clean_pca(), input$sorbate_select)
    
    # Filter data based on selected sorbates
    filtered_data <- data_clean_pca() %>%
      filter(Sorbate %in% input$sorbate_select)
    
    # Extract numeric columns for PCA
    numeric_cols <- c("pH", "Sorbate_val", "Aq_val", 
                      "Sorbed_val", "Mineral_val", "MineralSA", "Kd", 
                      "Electrolyte1_val", "Electrolyte2_val")
    
    # Perform PCA
    pca_data <- filtered_data[numeric_cols]
    prcomp(pca_data, scale. = input$scale_data)
  })
  
  # Create PCA plot
  output$pca_plot <- renderPlot({
    req(pca_results())
    
    # Get filtered data for coloring
    filtered_data <- data_clean_pca() %>%
      filter(Sorbate %in% input$sorbate_select)
    
    # Create PCA plot using factoextra
    fviz_pca_ind(pca_results(),
                 col.ind = filtered_data$Mineral,
                 palette = "viridis",
                 addEllipses = TRUE,
                 ellipse.type = "confidence",
                 legend.title = "Mineral",
                 pointsize = 3) +
      theme_minimal() +
      theme(legend.position = "right",
            text = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  # Create scree plot
  output$scree_plot <- renderPlot({
    req(pca_results())
    fviz_eig(pca_results(),
             addlabels = TRUE,
             ylim = c(0, 100)) +
      theme_minimal() +
      theme(text = element_text(size = 12))
  })
  
  # Create variable contributions plot
  output$var_contrib_plot <- renderPlot({
    req(pca_results())
    fviz_pca_var(pca_results(),
                 col.var = "contrib",
                 gradient.cols = viridis(256),
                 repel = TRUE) +
      theme_minimal() +
      theme(text = element_text(size = 12))
  })
  
  # Create PCA summary
  output$pca_summary <- renderPrint({
    req(pca_results())
    summary(pca_results())
  })
  
  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      "pca_analysis.png"
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 12, height = 8)
    }
  )
}


