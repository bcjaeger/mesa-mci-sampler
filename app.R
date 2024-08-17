library(shiny)
library(bayesrules)
library(tidyverse)

ui <- fluidPage(

  titlePanel("Bayesian Inference with Beta-Binomial Distribution"),

  sidebarLayout(
    sidebarPanel(
      numericInput("alpha_prior", "Alpha Prior (α):", value = 1, min = 0.1, step = 0.1),
      numericInput("beta_prior", "Beta Prior (β):", value = 10, min = 0.1, step = 0.1),
      numericInput("n_mci", "Number of MCI cases observed (y):", value = 1, min = 0, step = 1),
      numericInput("n_sampled", "Sample Size (n):", value = 50, min = 1, step = 1),
      numericInput("min_prob_thresh", "Minimum Probability Threshold:", value = 0.05, min = 0.01, step = 0.01),
      numericInput("min_mci_remain", "Minimum MCI count:", value = 5, min = 1, step = 1),
    ),

    mainPanel(
      plotOutput("priorPlot"),
      plotOutput("posteriorPlot"),
      tableOutput("summaryTable"),
      verbatimTextOutput("posteriorProb")
    )
  )
)

server <- function(input, output) {

  output$priorPlot <- renderPlot({
    plot_beta(input$alpha_prior, input$beta_prior)
  })

  output$posteriorPlot <- renderPlot({
    plot_beta_binomial(alpha = input$alpha_prior,
                       beta = input$beta_prior,
                       y = input$n_mci,
                       n = input$n_sampled)
  })

  output$summaryTable <- renderTable({
    summarize_beta_binomial(alpha = input$alpha_prior,
                            beta = input$beta_prior,
                            y = input$n_mci,
                            n = input$n_sampled)
  })

  output$posteriorProb <- renderText({
    set.seed(84735)

    sim <- tibble(pi = rbeta(100000, input$alpha_prior, input$beta_prior)) %>%
      mutate(y = rbinom(100000, size = input$n_sampled, prob = pi)) %>%
      filter(y == input$n_mci)

    # big text output: probability of
    prob_final <- pbinom(q = input$min_mci_remain,
                         size = 250 - input$n_sampled,
                         prob = mean(sim$pi))

    paste0("Posterior Probability that π <", input$min_prob_thresh, ": ",
          mean(sim$pi < input$min_prob_thresh), "\n",
          "Probability of at most ", input$min_mci_remain,
          " MCI cases in the remaining ", 250 - input$n_sampled,
          " participants: ", round(prob_final, 2))
  })

}

shinyApp(ui = ui, server = server)
