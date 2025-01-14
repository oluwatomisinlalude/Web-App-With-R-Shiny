# R Shiny Dashboard - Global Superstore Sales Analysis

## Project Overview
This project showcases an interactive dashboard built using R Shiny. The dashboard provides insights into sales, profits, and customer data, supporting business decision-making with advanced visualizations and interactivity.

## Features
1. **Comprehensive Data Analysis**:
   - Sales trends, profit analysis, and customer segmentation.

2. **Interactive Visualizations**:
   - Time-series and categorical plots using `ggplot2` and `plotly`.

3. **Geographic Insights**:
   - Interactive maps using `leaflet`.

4. **Widgets and Controls**:
   - Dropdowns, sliders, and data filters.

## Prerequisites
Ensure the following software and packages are installed on your system:
- R version 4.0 or higher
- RStudio
- Packages: `shiny`, `shinydashboard`, `ggplot2`, `dplyr`, `plotly`, `leaflet`, `DT`,`forecast`, `cluster`, `ggthemes`, `leaflet.extras`

Install the required packages using the following R command:
```R
install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "plotly", "leaflet", "readr"))
```
## How to Run the Project
1. Clone this repository:

```
git clone https://github.com/oluwatomisinlalude/Web-App-With-R-Shiny
```

2. Open the R project in RStudio.

3. Ensure the dataset (superstore_sales.csv) is placed in the project directory.

4. Run the Shiny app:

- Open the app.R file.
- Click the "Run App" button in RStudio.
- Access the dashboard in your browser. The app will display an interactive interface with tabs for data exploration and insights.

## Dataset
The dataset (superstore_sales.csv) contains the following columns:

- Order.ID: Unique identifier for each order.
- Order.Date: Date of order placement.
- Region: Sales region.
- Category: Product category.
- Sub.Category: Product sub-category.
- Sales: Total sales value.
- Profit: Profit earned from the sale.
- Quantity: Quantity of items sold.
- Latitude: Latitude of the location.
- Longitude: Longitude of the location.

