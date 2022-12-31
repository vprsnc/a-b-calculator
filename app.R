library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)


ui <- fluidPage(

  theme = shinytheme("flatly"),

  titlePanel("Bayesian A/B test calculator"),

  fluidRow(
    sidebarPanel(
      h4('Input your data according to ->'),
      h6('Or use the dummy data, just click "Iterate"'),

      textInput(inputId = "prior_alpha" , label = "Prior alpha: ","3"),
      textInput(inputId = "prior_beta"  , label = "Prior beta: ", "7"),

      textInput(inputId = "a_alpha"     , label = "A Posterior alpha: ", "36"),
      textInput(inputId = "a_beta"      , label = "A Posterior beta: ", "114"),
      textInput(inputId = "b_alpha"     , label = "B Posterior alpha: ", "50"),
      textInput(inputId = "b_beta"      , label = "B Posterior beta: ", "100"),

      actionButton(inputId = "iterate_but", label = "Iterate")

      ), # sidebarPanel

    mainPanel(
      h3("How it works"),
      textOutput("prior_txt"),
      h3("Beta's comparison"),
      plotlyOutput("beta_fig"),
      textOutput("beta_text")

    ) # mainPanel
  ), # fluidRow 1

  fluidRow(

    h2('Monte-Carlo simulation over 100k iterations'),

    column(width = 6,
      textOutput("monte_placeholder")
      ),
    column(width = 3,
           h4("Number of samples to reach 95% confidence:"),
           textOutput("no_samples")
      ), # col

    column(width = 3,
      tableOutput("monte_table")
      ) # col
    ), # fludRow

  fluidRow(
    column(
      h3("B times better than A hist"),
      width = 4,
      plotlyOutput("monte_fig")

    ), # col
    column(
      h3("B times better than A empirical Cummulative Distribution"),
      width = 4,
      plotlyOutput("ecdf")

    ), # col

    column(
      h3("Quantile Function"),
      width = 4,
      plotlyOutput("quantile")

    ) # col
  ) # fluidRow 2
) # fluidPage


server <- function(input, output, session){

  xs <- seq(0, 1, by=0.0001)

  output$prior_txt <- renderText(
    "Prior represents the historical data + your beliefs.
     More certain you're about B will now have effect, stronger should be prior.
     E.g. you have historical conversion of 0.3, very basic prior would be (3, 7).
     If you set up stronger prior. e.g. (30, 70), more data it will take to prove you should update your beliefs.
     Posterior is the actual data you get + prior.
     Remember for the next iterations you should use prior+posterior from this iteration as prior!"
    )
  output$beta_text <- renderText(
    "This graph shows the difference between Prior, A & B Probability Densities.
     More superior one than another, further their tails will be, and less cross area they'll have.
     Stronger prior will make it harder to differentiate, you will have to do more iterations to collect more data."
  )

  output$monte_placeholder <- renderText({
    paste("First two graphs below represent the data collected over 100k iterations of Monte Carlo simulation.",
           "It allows to correctly evaluate the chances that A is superior than B or vice/versa.",
          "Graph on the right allows us to visually assess the confidence intervals.",
           sep = "\n"
    )})

  observeEvent(
    input$iterate_but,
    {
      prior.alpha <- input$prior_alpha%>% as.numeric()
      prior.beta  <- input$prior_beta%>% as.numeric()

      beta.xs <- dbeta( xs, prior.alpha, prior.beta )

      data.a <- list(alpha = as.numeric(input$a_alpha),
                     beta = as.numeric(input$a_beta))
      data.b <- list(alpha = as.numeric(input$b_alpha),
                     beta = as.numeric(input$b_beta))

      beta.a <- dbeta(xs, prior.alpha + data.a$alpha, prior.beta + data.a$beta)
      beta.b <- dbeta(xs, prior.alpha + data.b$alpha, prior.beta + data.b$beta)

      mydata <- data.frame(xs, beta.xs, beta.a, beta.b)


      ### Beta PDF's

      output$beta_fig <- renderPlotly({

        plot_ly(mydata, x = xs)%>%
          add_trace(y = ~beta.xs, name = "Prior Beta", type = "scatter",
                 fill= "tozeroy", mode="none")%>%
          add_trace(y = ~beta.a, name = "Beta A", type = "scatter",
                  fill= "tozeroy", mode="none")%>%
          add_trace(y = ~beta.b, name = "Beta B", type = "scatter",
                fill = "tozeroy", mode="none")
      }) # renderPlotly 1

      ### Monte-Carlo Hist

      n.trials <- 100000

      a.samples <- rbeta(n.trials,
                         data.a$alpha + prior.alpha,
                         data.a$beta + prior.beta)
      b.samples <- rbeta(n.trials,
                         data.b$alpha + prior.alpha,
                         data.b$beta + prior.beta)

      monte_data <- data.frame(iter = seq(1, 100000), ratio = b.samples/a.samples)

      b_superior <- sum(b.samples > a.samples) / n.trials
      b_septxt <- paste0(b_superior * 100, "% samples")
      b_sup.alpha <- b_superior * 100000
      b_sup.beta <- (1 - b_superior) * 100000
      l_conf <- qbeta(0.025, b_sup.alpha, b_sup.beta)
      u_conf <- qbeta(0.975, b_sup.alpha, b_sup.beta)

      monte_table <- data.frame(
        metrics = c("B superior in", "2.5% conf value",
                    "97.5% conf value", "B superior times mean"),
        values = c(b_septxt, l_conf, u_conf, mean(b.samples/a.samples))
      )

      output$monte_table <- renderTable(monte_table)

      ### How much samples?
      p.b_superior <-  -1
      no.samples <- 0

      while(p.b_superior < 0.95) {
        no.samples <- no.samples + 100
        a.results <- runif(no.samples / 2) <= data.a$alpha / (data.a$alpha + data.a$beta)
        b.results <- runif(no.samples / 2) <= data.b$alpha / (data.b$alpha + data.b$beta)
        a.samples <- rbeta(n.trials,
                   sum(a.results == T) + prior.alpha,
                   sum(a.results == F) + prior.beta)
        b.samples <- rbeta(n.trials,
                   sum(b.results == T) + prior.alpha,
                   sum(b.results == F) + prior.beta)
        p.b_superior <- sum(b.samples > a.samples) / n.trials
      }


      output$no_samples <- renderText(no.samples)

      output$monte_fig <- renderPlotly({

        plot_ly(x = monte_data$ratio, type = "histogram", alpha = 0.6,
                xaxis = 'B times better than A', yaxis = 'Density')

      }) # renderPlotly 2

      output$ecdf <- renderPlotly({

        ggplotly(ggplot(monte_data, aes(ratio)) +
          stat_ecdf(geom = "line", col = "steelblue") +
          xlab('B times better than A') +
          ylab('Probability density') +
          theme_light())

      }) # renderPlotly 3

      xs_q <- seq(0.01, 0.99, by=0.001)
      output$quantile <- renderPlotly({
        plot_ly(monte_data, x = xs_q,
                y = qbeta(xs_q, b_superior * 100000,
                                (1-b_superior) * 100000),
                type = "scatter", mode = "line")

      }) # renderPlotly 4

    }) # observeEvent

} # server

shinyApp(ui, server)
