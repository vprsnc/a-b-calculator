library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)

ui <- fluidPage(

  theme = shinytheme("flatly"),

  fluidRow(
    sidebarPanel(
      title = "Input",

      textInput(inputId = "prior_alpha" , label = "Prior alpha: "),
      textInput(inputId = "prior_beta"  , label = "Prior beta: "),
      textInput(inputId = "a_alpha"     , label = "A Posterior alpha: "),
      textInput(inputId = "a_beta"      , label = "A Posterior beta: "),
      textInput(inputId = "b_alpha"     , label = "B Posterior alpha: "),
      textInput(inputId = "b_beta"      , label = "B Posterior beta: "),

      actionButton(inputId = "iterate_but", label = "Iterate"),

      ), # sidebarPanel

    mainPanel(
      h1("A/B test results"),
      h3("Beta's comparison"),
      plotlyOutput("beta_fig"),

    ) # mainPanel
  ), # fluidRow 1

  fluidRow(

    h2('Monte-Carlo simulation over 100k iterations'),
    verbatimTextOutput("monte_text"),
    column(
      h3("B times better than A hist"),
      width = 6,
      plotlyOutput("monte_fig"),

    ), # col
    column(
      h3("B times better than A empirical Cummulative Distribution"),
      width = 6,
      plotlyOutput("ecdf"),

    ) # col
  ) # fluidRow 2
) # fluidPage


options(browser = "brave")

server <- function(input, output, session){

  xs <- seq(0, 1, by=0.0001)

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

      monte_data <- data.frame(iter = seq(1, 1000000), ratio = b.samples/a.samples)

      b_superior <- sum(b.samples > a.samples) / n.trials

      output$monte_text <- paste0("In ", b_superior * 100,
                                  "% of iterations B was better than A.",
                                  "We can see the comparison in the ECDF graph.")%>%
        renderText()

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

      output$quantile <- renderPlotly({

        plot_ly(monte_data, x = ~xs, y = qbeta(xs, data.a$alha))

      }) # renderPlotly 4

    }) # observeEvent

} # server

shinyApp(ui, server)
