library(shiny)
library(plotly)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Interactive Statistical Inference Demo"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Sample Size (n):", min = 10, max = 200, value = 50),
      sliderInput("sims", "Number of Bootstrap Simulations:", min = 100, max = 2000, value = 1000, step = 100),
      sliderInput("conf", "Confidence Level:", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      actionButton("resample", "Draw New Sample")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("1. Bootstrap Distribution", plotlyOutput("boot_plot")),
        tabPanel("2. CI Coverage (Simulations)", plotlyOutput("coverage_plot")),
        tabPanel("3. Hypothesis Testing", plotlyOutput("test_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  true_mean <- 100
  true_sd <- 15
  
  sample_data <- eventReactive(input$resample, {
    rnorm(input$n, mean = true_mean, sd = true_sd)
  }, ignoreNULL = FALSE)
  
  # ----- 图 1：Bootstrap -----
  output$boot_plot <- renderPlotly({
    data <- sample_data()
    boot_means <- replicate(input$sims, mean(sample(data, replace = TRUE)))
    
    alpha <- (1 - input$conf) / 2
    ci <- quantile(boot_means, probs = c(alpha, 1 - alpha))
    
    df <- data.frame(boot_means = boot_means)
    
    p <- ggplot(df, aes(x = boot_means)) +
      geom_histogram(bins = 40, fill = "skyblue", color = "white") +
      geom_vline(xintercept = ci[1], color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = ci[2], color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = true_mean, color = "black", size = 1.5) +
      labs(title = "Bootstrap Distribution of the Mean",
           subtitle = paste("True Mean vs ", input$conf*100, "% CI"),
           x = "Bootstrap Sample Means", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ----- 图 2：覆盖率 -----
  output$coverage_plot <- renderPlotly({
    trials <- 100
    ci_df <- data.frame(trial = 1:trials, lower = numeric(trials), upper = numeric(trials), covers = logical(trials))
    
    alpha <- (1 - input$conf) / 2
    
    for(i in 1:trials) {
      x <- rnorm(input$n, mean = true_mean, sd = true_sd)
      m <- mean(x)
      se <- sd(x) / sqrt(input$n)
      z <- qnorm(1 - alpha)
      
      ci_df$lower[i] <- m - z * se
      ci_df$upper[i] <- m + z * se
      ci_df$covers[i] <- (true_mean >= ci_df$lower[i]) & (true_mean <= ci_df$upper[i])
    }
    
    p <- ggplot(ci_df, aes(x = trial, ymin = lower, ymax = upper, color = covers)) +
      geom_linerange(size = 1) +
      geom_hline(yintercept = true_mean, linetype = "dashed", color = "black", size = 1) +
      scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none") +
      labs(title = "Coverage of Confidence Intervals",
           x = "Simulation Trial", y = "Confidence Interval") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ----- 图 3：假设检验 -----
  output$test_plot <- renderPlotly({
    x <- sample_data()
    obs_t <- (mean(x) - true_mean) / (sd(x) / sqrt(input$n))
    
    null_t <- replicate(input$sims, {
      x_null <- rnorm(input$n, mean = true_mean, sd = true_sd)
      (mean(x_null) - true_mean) / (sd(x_null) / sqrt(input$n))
    })
    
    df <- data.frame(null_t = null_t)
    
    p <- ggplot(df, aes(x = null_t)) +
      geom_histogram(bins = 40, fill = "lightgray", color = "white", aes(y = ..density..)) +
      stat_function(fun = dnorm, color = "blue", size = 1) +
      geom_vline(xintercept = obs_t, color = "red", size = 1.5) +
      labs(title = "Null Distribution and P-value",
           x = "T-statistic", y = "Density") +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui, server)
