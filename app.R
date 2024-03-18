library(shiny)
library(ggplot2)
library(htmltools);
library(future); library(future.apply);

#library(parallel);plan(multisession, workers = detectCores() - 1)

# Simulation function with parallel computation
simulate_game <- function(flip_count = 100, sims = 10000) {
  future_sapply(1:sims, function(i) {
    flips <- rbinom(flip_count, 1, 0.5)
    alice_seq <- which(flips[-length(flips)] == 1 & flips[-1] == 1)
    bob_seq <- which(flips[-length(flips)] == 1 & flips[-1] == 0)
    c(length(alice_seq), length(bob_seq))
  }, future.seed = TRUE)
}
# Define UI
ui <- fluidPage(
  includeCSS('style.css'),
  titlePanel('Coin Flip Game Simulation with Advanced Graphics'),
  sidebarLayout(
    sidebarPanel(
      htmltools::HTML("<blockquote class='twitter-tweet'><p lang='en' dir='ltr'>Flip a fair coin 100 times—it gives a sequence of heads (H) and tails (T). For each HH in the sequence of flips, Alice gets a point; for each HT, Bob does, so e.g. for the sequence THHHT Alice gets 2 points and Bob gets 1 point. Who is most likely to win?</p>&mdash; Daniel Litt (@littmath) <a href='https://twitter.com/littmath/status/1769044719034647001?ref_src=twsrc%5Etfw'>March 16, 2024</a></blockquote> <script async src='https://platform.twitter.com/widgets.js' charset='utf-8'></script>"),
      wellPanel(
      numericInput(
        inputId = 'numFlips', 
        label = 'Number of Coin Flips:', 
        value = 2, min = 0, max = 200, 
        step = 1, 
        width = '100%'
      ),
      numericInput(
        inputId = 'numSims', 
        label = 'Number of Simulations:', 
        value = 5000, min = 0, max = 20000, 
        step = 100, 
        width = '100%'
      ),
      actionButton('simButton', 'Run Simulation')
      ),
      tableOutput("table")
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput('distributionHexPlot')),
        column(6, plotOutput('densityPlot'))
      ),
      fluidRow(
        column(6, plotOutput('winRatePlot')),
        column(6, plotOutput('variancePlot'))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  results <- eventReactive(input$simButton, {
    results_matrix <- simulate_game(input$numFlips, input$numSims)
    data.frame(Alice = results_matrix[1, ], Bob = results_matrix[2, ])
  })
  
  output$distributionHexPlot <- renderPlot({
    req(results())
    ggplot(results(), aes(x = Alice, y = Bob)) +
      geom_hex() +
      scale_fill_viridis_c() +
      labs(title = 'Hexagonal Distribution of Scores',
           x = "Alice's Score", y = "Bob's Score") +
      theme_minimal()
  })
  
  output$winRatePlot <- renderPlot({
    req(results())
    results_df <- results()
    results_df$Winner <- ifelse(results_df$Alice > results_df$Bob, 'Alice', ifelse(results_df$Alice < results_df$Bob, 'Bob', 'Tie'))
    ggplot(results_df, aes(x = Winner)) +
      geom_bar(fill = 'steelblue') +
      labs(title = 'Win Rates for Alice vs. Bob', x = '', y = 'Number of Wins') +
      theme_minimal()
  })
  
  
  output$densityPlot <- renderPlot({
    req(results())
    results_df <- results()
    
    # Calculate means and standard deviations for Alice and Bob
    alice_mean <- mean(results_df$Alice)
    bob_mean <- mean(results_df$Bob)
    alice_sd <- sd(results_df$Alice)
    bob_sd <- sd(results_df$Bob)
    
    # Calculate variances for Alice and Bob
    alice_var <- var(results_df$Alice)
    bob_var <- var(results_df$Bob)
    
    ggplot(results_df, aes(x = Alice)) +
      geom_density(aes(x = Alice), color = 'blue', fill = 'blue', alpha = 0.5) +
      geom_density(aes(x = Bob), color = 'red', fill = 'red', alpha = 0.5) +
      geom_vline(xintercept = alice_mean, color = 'blue', linetype = 'solid', size = 1) +
      geom_vline(xintercept = bob_mean, color = 'red', linetype = 'solid', size = 1) +
      geom_vline(xintercept = alice_mean + alice_sd, color = 'blue', linetype = 'dashed', size = 0.5) +
      geom_vline(xintercept = alice_mean - alice_sd, color = 'blue', linetype = 'dashed', size = 0.5) +
      geom_vline(xintercept = bob_mean + bob_sd, color = 'red', linetype = 'dashed', size = 0.5) +
      geom_vline(xintercept = bob_mean - bob_sd, color = 'red', linetype = 'dashed', size = 0.5) +
      annotate('text', x = Inf, y = Inf, label = sprintf('Alice Mean: %.2f\nAlice SD: %.2f\nAlice Var: %.2f\nBob Mean: %.2f\nBob SD: %.2f\nBob Var: %.2f', alice_mean, alice_sd, alice_var, bob_mean, bob_sd, bob_var),
               hjust = 1.1, vjust = 1.1, size = 3, color = 'black') +
      labs(title = 'Density Plot of Scores', x = 'Score', y = 'Density') +
      theme_minimal() +
      scale_color_manual(values = c('Alice' = 'blue', 'Bob' = 'red')) +
      scale_fill_manual(values = c('Alice' = 'blue', 'Bob' = 'red'))
  })
  
  output$variancePlot <- renderPlot({
    req(results())
    results_df <- results()
    
    # Calculate variances for Alice and Bob
    alice_var <- var(results_df$Alice)
    bob_var <- var(results_df$Bob)
    
    # Data frame for plotting
    variance_df <- data.frame(
      Participant = c('Alice', 'Bob'),
      Variance = c(alice_var, bob_var)
    )
    
    ggplot(variance_df, aes(x = Participant, y = Variance, fill = Participant)) +
      geom_bar(stat = 'identity') +
      labs(title = 'Variance of Scores', x = '', y = 'Variance') +
      theme_minimal() +
      scale_fill_manual(values = c('Alice' = 'blue', 'Bob' = 'red'))
  })
  
  # Render the results table
  output$table <- renderTable({
    req(results())  # Ensure that results are available before proceeding
    results_df <- results()
    
    # Calculate summary statistics
    alice_wins <- sum(results_df$Alice > results_df$Bob)
    bob_wins <- sum(results_df$Bob > results_df$Alice)
    ties <- sum(results_df$Alice == results_df$Bob)
    
    # Calculate means and standard deviations for Alice and Bob
    alice_mean <- mean(results_df$Alice)
    bob_mean <- mean(results_df$Bob)
    alice_sd <- sd(results_df$Alice)
    bob_sd <- sd(results_df$Bob)
    
    # Calculate variances for Alice and Bob
    alice_var <- var(results_df$Alice)
    bob_var <- var(results_df$Bob)
    
    # Prepare a summary table
    summary_table <- data.frame(
      Alice=c(alice_wins, alice_mean, alice_sd, alice_var),
      Bob=c(bob_wins, bob_mean, bob_sd, bob_var))
    rownames(summary_table) <- c("Wins","Mean","SD","Var")
    
    summary_table  # Return the table for rendering
  },rownames = T ,align = 'c')  # Center align the table columns
  
}

# Run the application 
shinyApp(ui = ui, server = server)