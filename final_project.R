library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(DT)
library(forecast)
library(cluster)
library(ggthemes)
library(leaflet.extras)

# Load Dataset
data <- read.csv("superstore_sales.csv")

# Data Preprocessing
data$Order.Date <- as.Date(data$Order.Date, "%Y-%m-%d")
data$Year <- format(data$Order.Date, "%Y")
data$Month <- format(data$Order.Date, "%Y-%m")

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Global Superstore Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Insights", tabName = "insights", icon = icon("chart-line")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("calendar-alt")),
      menuItem("Geographic Analysis", tabName = "maps", icon = icon("globe")),
      menuItem("Data View", tabName = "data", icon = icon("table")),
      menuItem("Advanced Analysis", tabName = "advanced", icon = icon("tools"))
    ),
    hr(),
    sliderInput("salesRange", "Filter by Sales Range:",
                min = min(data$Sales), max = max(data$Sales),
                value = c(min(data$Sales), max(data$Sales))),
    selectInput("regionSelect", "Select Region:",
                choices = c("All", unique(data$Region)), selected = "All"),
    selectInput("categorySelect", "Select Category:",
                choices = c("All", unique(data$Category)), selected = "All")
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Key Metrics",
            width = 12,
            valueBoxOutput("totalSales"),
            valueBoxOutput("totalProfit"),
            valueBoxOutput("topCategory")
          )
        ),
        fluidRow(
          box(
            status = "primary",
            width = 6,
            plotlyOutput("profitVsSales")
          ),
          box(
            status = "primary",
            width = 6,
            plotlyOutput("salesByCategory")
          )
        )
      ),
      
      # Insights Tab
      tabItem(
        tabName = "insights",
        fluidRow(
          box(
            title = "Profit by Region",
            status = "primary",
            width = 6,
            plotlyOutput("profitRegion")
          ),
          box(
            title = "Category Performance",
            status = "primary",
            width = 6,
            plotlyOutput("categoryPerformance")
          )
        ),
        fluidRow(
          box(
            title = "Correlation Matrix",
            status = "primary",
            width = 12,
            plotlyOutput("correlationMatrix")
          )
        )
      ),
      
      # Forecasting Tab
      tabItem(
        tabName = "forecasting",
        fluidRow(
          box(
            title = "Sales Forecast",
            status = "primary",
            width = 12,
            plotlyOutput("salesForecast")
          )
        )
      ),
      
      # Maps Tab
      tabItem(
        tabName = "maps",
        fluidRow(
          box(
            title = "Sales by Region",
            status = "primary",
            width = 12,
            leafletOutput("salesMap")
          ),
          box(
            title = "Sales Heatmap",
            status = "primary",
            width = 12,
            leafletOutput("salesHeatmap")
          )
        )
      ),
      
      # Data Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Interactive Data Table",
            status = "primary",
            width = 12,
            DTOutput("dataTable")
          )
        )
      ),
      
      # Advanced Analysis Tab
      tabItem(
        tabName = "advanced",
        fluidRow(
          box(
            title = "Clustering Analysis",
            status = "primary",
            width = 12,
            plotlyOutput("clusteringAnalysis")
          )
        ),
        fluidRow(
          box(
            title = "Profit Prediction",
            status = "primary",
            width = 12,
            plotlyOutput("profitPrediction")
          )
        ),
        fluidRow(
          box(
            title = "Sub-Category Sales Forecast",
            status = "primary",
            width = 12,
            plotlyOutput("subCategoryForecast")
          )
        ),
        fluidRow(
          box(
            title = "Actionable Insights",
            status = "primary",
            width = 12,
            uiOutput("actionableInsights")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Filtered Data
  filteredData <- reactive({
    data %>%
      filter(Sales >= input$salesRange[1],
             Sales <= input$salesRange[2],
             if (input$regionSelect != "All") Region == input$regionSelect else TRUE,
             if (input$categorySelect != "All") Category == input$categorySelect else TRUE)
  })
  
  # Key Metrics
  output$totalSales <- renderValueBox({
    valueBox(
      formatC(sum(filteredData()$Sales), format = "d", big.mark = ","),
      subtitle = "Total Sales",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$totalProfit <- renderValueBox({
    valueBox(
      formatC(sum(filteredData()$Profit), format = "d", big.mark = ","),
      subtitle = "Total Profit",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$topCategory <- renderValueBox({
    topCat <- filteredData() %>% group_by(Category) %>% 
      summarise(TotalSales = sum(Sales)) %>% 
      arrange(desc(TotalSales)) %>% 
      slice(1)
    valueBox(
      topCat$Category,
      subtitle = "Top Performing Category",
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  # Profit vs. Sales
  output$profitVsSales <- renderPlotly({
    gg <- ggplot(filteredData(), aes(x = Sales, y = Profit)) +
      geom_point(alpha = 0.6, color = "darkorange") +
      theme_minimal() +
      labs(
        title = "Profit vs. Sales",
        x = "Sales",
        y = "Profit"
      )
    
    ggplotly(gg)
  })
  
  # Category Contribution to Sales
  output$salesByCategory <- renderPlotly({
    categoryData <- filteredData() %>%
      group_by(Category) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE))
    
    plot_ly(
      categoryData,
      labels = ~Category,
      values = ~TotalSales,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent"
    ) %>%
      layout(title = "Category Contribution to Total Sales")
  })
  
  # Profit by Region
  output$profitRegion <- renderPlotly({
    region <- filteredData() %>% group_by(Region) %>% 
      summarise(TotalProfit = sum(Profit))
    gg <- ggplot(region, aes(x = reorder(Region, TotalProfit), y = TotalProfit, fill = Region)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Profit by Region", x = "Region", y = "Total Profit")
    ggplotly(gg)
  })
  
  # Category Performance
  output$categoryPerformance <- renderPlotly({
    category <- filteredData() %>% group_by(Category) %>% 
      summarise(TotalSales = sum(Sales))
    gg <- ggplot(category, aes(x = Category, y = TotalSales, fill = Category)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Category Performance", x = "Category", y = "Total Sales")
    ggplotly(gg)
  })
  
  # Correlation Matrix
  output$correlationMatrix <- renderPlotly({
    corrData <- filteredData() %>% select(Sales, Profit, Quantity)
    corr <- cor(corrData, use = "complete.obs")
    plot_ly(z = corr, type = "heatmap", x = colnames(corr), y = rownames(corr))
  })
  
  # Sales Forecast
  output$salesForecast <- renderPlotly({
    monthlySales <- filteredData() %>% 
      group_by(Month) %>% 
      summarise(TotalSales = sum(Sales, na.rm = TRUE)) %>% 
      mutate(Month = as.Date(paste0(Month, "-01")))
    
    if (nrow(monthlySales) < 12) {
      plot_ly() %>%
        layout(title = "Insufficient Data for Forecasting")
    } else {
      tsSales <- ts(
        monthlySales$TotalSales, 
        frequency = 12, 
        start = c(as.numeric(format(min(monthlySales$Month), "%Y")), 
                  as.numeric(format(min(monthlySales$Month), "%m")))
      )
      fit <- auto.arima(tsSales)
      forecasted <- forecast(fit, h = 12)
      forecastDf <- data.frame(
        Date = seq(max(monthlySales$Month) + 1, by = "month", length.out = 12),
        Sales = as.numeric(forecasted$mean)
      )
      historical <- data.frame(Date = monthlySales$Month, Sales = monthlySales$TotalSales)
      combined <- rbind(historical, forecastDf)
      
      plot_ly(combined, x = ~Date, y = ~Sales, type = "scatter", mode = "lines+markers",
              name = "Forecast")
    }
  })
  
  # Sales Map
  output$salesMap <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, radius = ~Sales / 1000, color = "blue", fill = TRUE)
  })
  
  # Sales Heatmap
  output$salesHeatmap <- renderLeaflet({
    # Aggregating data
    heatmapData <- filteredData() %>%
      group_by(Latitude, Longitude) %>%
      summarise(TotalSales = sum(Sales), .groups = "drop")
    
    # Remove NAs and zero sales values
    heatmapData <- heatmapData %>%
      filter(!is.na(TotalSales) & TotalSales > 0)
    
    # Check max sales value
    max_sales <- max(heatmapData$TotalSales, na.rm = TRUE)
    
    leaflet(heatmapData) %>%
      addTiles() %>%
      setView(lng = mean(heatmapData$Longitude), lat = mean(heatmapData$Latitude), zoom = 5) %>%
      addHeatmap(
        lng = ~Longitude, lat = ~Latitude, intensity = ~TotalSales,
        blur = 25, radius = 20, max = max_sales,
        gradient = c("blue", "green", "yellow", "red")
      )
  })
  
  # Advanced Analysis
  
  # Clustering Analysis
  output$clusteringAnalysis <- renderPlotly({
    clusterData <- filteredData() %>%
      select(Sales, Profit) %>%
      na.omit()
    clusterDataScaled <- scale(clusterData)
    kmeansResult <- kmeans(clusterDataScaled, centers = 3)
    clusterData$Cluster <- as.factor(kmeansResult$cluster)
    
    gg <- ggplot(clusterData, aes(x = Sales, y = Profit, color = Cluster)) +
      geom_point(size = 3, alpha = 0.8) +
      theme_minimal() +
      labs(title = "Clustering Analysis (K-Means)", x = "Sales", y = "Profit")
    ggplotly(gg)
  })
  
  # Profit Prediction
  output$profitPrediction <- renderPlotly({
    lmModel <- lm(Profit ~ Sales + Quantity, data = filteredData())
    predictionData <- filteredData()
    predictionData$PredictedProfit <- predict(lmModel, newdata = filteredData())
    
    gg <- ggplot(predictionData, aes(x = Sales, y = Profit)) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_line(aes(y = PredictedProfit), color = "red") +
      theme_minimal() +
      labs(title = "Profit Prediction", x = "Sales", y = "Profit")
    ggplotly(gg)
  })
  
  # Sub-Category Sales Forecast
  output$subCategoryForecast <- renderPlotly({
    subCategorySales <- filteredData() %>%
      group_by(Sub.Category) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(TotalSales)) %>%
      slice(1)
    forecastData <- filteredData() %>%
      filter(Sub.Category == subCategorySales$Sub.Category) %>%
      group_by(Order.Date) %>%
      summarise(DailySales = sum(Sales, na.rm = TRUE))
    
    tsData <- ts(forecastData$DailySales, frequency = 365)
    forecastResult <- forecast(auto.arima(tsData), h = 30)
    plot_ly() %>%
      add_lines(x = time(forecastResult$fitted), y = forecastResult$fitted, name = "Fitted") %>%
      add_lines(x = time(forecastResult$mean), y = forecastResult$mean, name = "Forecast") %>%
      layout(title = paste("Sales Forecast for Sub-Category:", subCategorySales$Sub.Category))
  })
  
  #Data View
  
  # Filtered Data
  filteredData <- reactive({
    data %>%
      filter(Sales >= input$salesRange[1],
             Sales <= input$salesRange[2],
             if (input$regionSelect != "All") Region == input$regionSelect else TRUE,
             if (input$categorySelect != "All") Category == input$categorySelect else TRUE)
  })
  
  # Interactive Data Table
  output$dataTable <- renderDT({
    df <- filteredData()
    
    # Check if there's any data to show
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No data to display"), options = list(scrollX = TRUE)))
    }
    
    datatable(df, options = list(scrollX = TRUE))
  })
  
  # Actionable Insights
  output$actionableInsights <- renderUI({
    insights <- paste(
      "1. The top-performing region is ", filteredData() %>%
        group_by(Region) %>%
        summarise(TotalSales = sum(Sales)) %>%
        arrange(desc(TotalSales)) %>%
        slice(1) %>%
        pull(Region), ".",
      "2. The most profitable sub-category is ", filteredData() %>%
        group_by(Sub.Category) %>%
        summarise(TotalProfit = sum(Profit)) %>%
        arrange(desc(TotalProfit)) %>%
        slice(1) %>%
        pull(Sub.Category), ".",
      "3. Discounts significantly affect sales volume.",
      sep = "\n"
    )
    HTML(insights)
  })
}

# Run the app
shinyApp(ui, server)
