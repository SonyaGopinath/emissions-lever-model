library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)


ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {
      font-family: 'Segoe UI', 'Helvetica Neue', sans-serif;
    }
    .container-fluid {
      margin: 30px;
    }
    .tabbable > .nav > li > a {
      font-weight: 500;
      color: #2C3E50;
    }
    h2, h4 {
      color: #2C3E50;
    }
  "))),
  
  titlePanel(
    div("SCOPE: Sustainability Cost & Output Projection Engine", 
        style = "color: #2C3E50; font-size: 26px; font-weight: 600;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("initial_output", "Initial Output (in tonnes)", value = 1000000, min = 1),
      numericInput("initial_revenue", "Initial Revenue (£)", value = 5000000, min = 1),
      numericInput("initial_emissions", "Initial Emissions (tonnes CO2e)", value = 500000, min = 1),
      selectInput("firm_size", "Firm Size", choices = c("Small" = 1000, "Mid-size" = 10000, "Large" = 35000), selected = 10000),
      numericInput("capital_productivity_rate", "Capital Productivity Contribution to Output Growth (%)", value = 2, min = 0, max = 10),
      numericInput("carbon_price", "Carbon Price (£/tonne)", value = 50, min = 0),
      numericInput("productivity_drop_year", "Year Labour Productivity Drops Due to Emissions", value = 4, min = 1, max = 10),
      numericInput("labour_productivity_drop_rate", "Annual Labour Productivity Decline (%)", value = 1.5, min = 0, max = 10),
      
      checkboxInput("intensity_degrades", "Emissions Intensity Worsens Annually?", FALSE),
      conditionalPanel(
        condition = "input.intensity_degrades == true",
        numericInput("intensity_degradation_rate", "Emissions Intensity Increase Rate (%)", value = 1, min = 0, max = 10)
      ),
      sliderInput("re_share", "Planned Renewable Energy Share (%)", min = 0, max = 100, value = 5, step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 fluidRow(
                   column(12,
                          HTML('
  <div style="padding: 20px;">
    <h2>📘 About This Tool</h2>
    
    <h4>🔎 What This Is</h4>
    <p>This simulation model helps analysts and decision-makers explore how emissions trajectories and profit dynamics change under different operational strategies. It is designed to unpack the hidden consequences of emissions intensity and sustainability levers at a firm level.</p>
    
    <h4>⚙️ How to Use It</h4>
    <ul>
      <li>Set a baseline by entering the company’s current output, emissions, and revenue.</li>
      <li>Use the <strong>two levers</strong>: Carbon Price and Renewable Energy Share to model interventions.</li>
      <li>Tweak other realistic parameters to simulate labour productivity shocks or operational growth.</li>
      <li>View how emissions, revenue, and profits change over a 10-year horizon.</li>
    </ul>
    
    <h4>📈 What You’ll See</h4>
    <ul>
      <li>Total emissions and intensity over time</li>
      <li>Baseline vs. adjusted output and revenue trajectories</li>
      <li>Profit comparisons with and without carbon pricing effects</li>
      <li>Impacts of worsening emissions profiles on firm performance</li>
    </ul>
    
    <h4>🎯 Where to Focus</h4>
    <p><strong>To reduce emissions:</strong> increase the Renewable Energy Share and counteract intensity degradation.</p>
    <p><strong>To protect profits:</strong> model high Carbon Prices with a strong RE strategy and output efficiency gains.</p>
    <p><strong>To understand trade-offs:</strong> stress test productivity declines and emissions pricing across 10 years to see financial strain or resilience patterns.</p>

    <p>The model is designed for experimentation — no assumptions are hidden. Every variable plays out visibly over time.</p>
  </div>
  ')
                   )
                 )
        ),
        tabPanel("Emissions Graph", plotOutput("emissions_plot", height = "500px")),
        tabPanel("Output Graph", plotOutput("output_plot", height = "500px")),
        tabPanel("Revenue Graph", plotOutput("revenue_plot", height = "500px")),
        tabPanel("Profit Graph", plotOutput("profit_plot", height = "500px")),
        tabPanel("Key Metrics",
                 tableOutput("key_metrics_table"))
        )
    )
  ),   
  tags$footer(HTML("
  <hr>
  <p style='text-align: center; font-size: 13px; color: #888;'>Built by Sonya Gopinath | <a href='https://github.com/SonyaGopinath/emissions-lever-model/tree/main' target='_blank'>GitHub</a></p>
"))
)


server <- function(input, output) {
  simulation <- reactive({
    years <- 1:10
    n_years <- length(years)
    
    output_vec <- numeric(n_years)
    revenue_vec <- numeric(n_years)
    emissions_vec <- numeric(n_years)
    emissions_intensity_vec <- numeric(n_years)
    output_bau <- numeric(n_years)
    revenue_bau <- numeric(n_years)
    profit_naive <- numeric(n_years)
    profit_adjusted <- numeric(n_years)
    
    workers <- as.numeric(input$firm_size)
    capital_rate <- input$capital_productivity_rate / 100
    labour_drop <- input$labour_productivity_drop_rate / 100
    drop_year <- input$productivity_drop_year
    carbon_price <- input$carbon_price
    degradation_rate <- ifelse(input$intensity_degrades, input$intensity_degradation_rate / 100, 0)
    re_share <- input$re_share / 100
    re_reduction_factor <- 0.8
    
    output_vec[1] <- input$initial_output
    revenue_vec[1] <- input$initial_revenue
    emissions_vec[1] <- input$initial_emissions
    emissions_intensity_vec[1] <- emissions_vec[1] / output_vec[1]
    
    output_bau[1] <- output_vec[1]
    revenue_bau[1] <- revenue_vec[1]
    
    for (t in 2:n_years) {
      labour_productivity_change <- ifelse(t >= drop_year, (1 - labour_drop)^(t - drop_year + 1), 1)
      output_growth = capital_rate * labour_productivity_change
      
      output_vec[t] <- output_vec[t - 1] * (1 + output_growth)
      output_bau[t] <- output_bau[t - 1] * (1 + capital_rate)
      
      revenue_vec[t] <- revenue_vec[t - 1] * (output_vec[t] / output_vec[t - 1])
      revenue_bau[t] <- revenue_bau[t - 1] * (output_bau[t] / output_bau[t - 1])
      
      intensity_degraded <- emissions_intensity_vec[t - 1] * (1 + degradation_rate)
      re_adjustment <- 1 - (re_share * re_reduction_factor)
      emissions_intensity_vec[t] <- intensity_degraded * re_adjustment
      emissions_vec[t] <- emissions_intensity_vec[t] * output_vec[t]
      
      intensity_threshold <- 0.25  # Set your benchmark here (e.g., 0.25 tCO2/tonne)
      
      cost_base <- 0.6 * revenue_bau[t]
      
      if (emissions_intensity_vec[t] > intensity_threshold) {
        excess_intensity <- emissions_intensity_vec[t] - intensity_threshold
        carbon_cost <- excess_intensity * output_vec[t] * carbon_price
      } else {
        carbon_cost <- 0
      }
      
      profit_naive[t] <- revenue_bau[t] - cost_base
      profit_adjusted[t] <- revenue_vec[t] - (cost_base + carbon_cost)
    }
    
    data.frame(
      Year = years,
      Output = output_vec,
      Output_BAU = output_bau,
      Revenue = revenue_vec,
      Revenue_BAU = revenue_bau,
      Emissions = emissions_vec,
      Emissions_Intensity = emissions_intensity_vec,
      Profit_Naive = profit_naive,
      Profit_Adjusted = profit_adjusted
    )
  })
  
  output$key_metrics_table <- renderTable({
    df <- simulation()
    
    df_filtered <- df %>%
      filter(Year %in% c(1, 10)) %>%
      transmute(
        Year = Year,
        `Emissions (tCO2e)` = round(Emissions, 0),
        `Output (tonnes)` = round(Output, 0),
        `Revenue (£)` = round(Revenue, 0),
        `Profit after Emissions Costs (£)` = round(Profit_Adjusted, 0),
        `Emissions Intensity (tCO2e per tonne)` = round(Emissions_Intensity, 3)
      )
    
    df_filtered
  })
  
  
  output$emissions_plot <- renderPlot({
    df <- simulation()
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Emissions, color = "Total Emissions")) +
      labs(title = "Emissions Over Time", y = "Emissions (tonnes CO2e)", color = "") +
      theme_minimal()
  })
  
  output$output_plot <- renderPlot({
    df <- simulation()
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Output_BAU, color = "BAU Output")) +
      geom_line(aes(y = Output, color = "Adjusted Output")) +
      labs(title = "Output Over Time", y = "Output (tonnes)", color = "") +
      theme_minimal()
  })
  
  output$revenue_plot <- renderPlot({
    df <- simulation()
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Revenue_BAU, color = "BAU Revenue")) +
      geom_line(aes(y = Revenue, color = "Adjusted Revenue")) +
      labs(title = "Revenue Over Time", y = "Revenue (£)", color = "") +
      theme_minimal()
  })
  
  output$profit_plot <- renderPlot({
    df <- simulation()
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Profit_Naive, color = "Regular Profit Estimation")) +
      geom_line(aes(y = Profit_Adjusted, color = "Profits After Emissions Costs")) +
      labs(title = "Profit Over Time", y = "Profit (£)", color = "") +
      scale_y_continuous(labels = label_comma()) + 
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
