library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Emissions, Output, and Profit Simulation Model"),
  
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
      )
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
                  <p>This model simulates how different sustainability strategies affect a company’s emissions and long-term viability. It’s designed for people who want to move beyond generic ESG claims and actually see the numbers behind emissions reduction choices.</p>
                  <p>You can simulate real or fictional firms by adjusting variables like renewable energy share, energy efficiency, production levels, and emissions profiles. The goal? See how different actions (or inaction) shape your carbon trajectory — and what that means for your business.</p>
                  
                  <h4>⚙️ How to Use It</h4>
                  <ul>
                    <li><strong>Start with the baseline scenario</strong> — this reflects current operations with no interventions.</li>
                    <li><strong>Adjust levers</strong> like production volume, energy mix, or RE share using the sliders or input boxes.</li>
                    <li><strong>Compare scenarios</strong> side-by-side using charts that show carbon intensity and emissions trends.</li>
                    <li>The model updates in real time, so you can experiment and iterate freely.</li>
                  </ul>

                  <h4>📈 What You’ll See</h4>
                  <p>The output reflects:</p>
                  <ul>
                    <li>Total emissions across different scenarios</li>
                    <li>Carbon intensity per unit of output</li>
                    <li>Visual comparisons over time</li>
                    <li>Early-stage logic linking emissions to profit impact (more to come)</li>
                  </ul>
                  <p>The current version uses simplified assumptions, but future releases will include dynamic lag effects, sector-specific multipliers, and cost of inaction projections.</p>
                </div>
              ')
                   )
                 )
        ),
        tabPanel("Emissions Graph", plotOutput("emissions_plot")),
        tabPanel("Output Graph", plotOutput("output_plot")),
        tabPanel("Revenue Graph", plotOutput("revenue_plot")),
        tabPanel("Profit Graph", plotOutput("profit_plot")),
      )
    )
  )
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
      
      emissions_intensity_vec[t] <- emissions_intensity_vec[t - 1] * (1 + degradation_rate)
      emissions_vec[t] <- emissions_intensity_vec[t] * output_vec[t]
      
      cost_base <- 0.6 * revenue_bau[t]
      carbon_cost <- emissions_vec[t] * carbon_price
      
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
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
