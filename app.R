# app.R
# One-sample population proportion hypothesis test (Shiny)
# Sidebar: population proportion (p0), sample proportion (phat),
# sample size (n), significance level (alpha), and alternative hypothesis.

library(shiny)
library(bslib)
library(ggplot2)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  layout_sidebar(
    sidebar = sidebar(
      title = "Inputs",
      numericInput("p0", "Null proportion (p0)", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("phat", "Sample proportion (p̂)", value = 0.55, min = 0, max = 1, step = 0.001),
      numericInput("n", "Sample size (n)", value = 100, min = 1, step = 1),
      numericInput("alpha", "Significance level (α)", value = 0.05, min = 0.0001, max = 0.5, step = 0.001),
      selectInput("alt", "Alternative hypothesis",
                  choices = c("Two-sided (p ≠ p0)" = "two.sided",
                              "Greater (p > p0)" = "greater",
                              "Less (p < p0)" = "less"),
                  selected = "two.sided"),
      helpText("Rule of thumb for normal approx: n*p0 ≥ 10 and n*(1-p0) ≥ 10")
    ),
    
    # Main content: three tabbed cards
    navset_card_pill(
      nav_panel(
        "Test summary",
        card_body(
          tableOutput("summary")
        )
      ),
      nav_panel(
        "Conclusion",
        card_body(
          h5(textOutput("hypotheses")),
          h5(textOutput("decision")),
          p(textOutput("conclusion"))
        )
      ),
      nav_panel(
        "Visualize p-value",
        card_body(
          plotOutput("pplot", height = "380px"),
          div(class = "text-muted", style = "font-size: 0.9rem;",
              "Shaded area = p-value under standard normal curve; vertical line = test statistic (z)")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Core reactive calculations
  calc <- reactive({
    p0 <- input$p0
    ph <- input$phat
    n  <- input$n
    alpha <- input$alpha
    alt <- input$alt

    se <- sqrt(p0 * (1 - p0) / n)
    z  <- (ph - p0) / se

    if (alt == "two.sided") {
      pval <- 2 * pnorm(-abs(z))
      zcrit <- qnorm(1 - alpha/2)
      reject <- abs(z) > zcrit
      crit_desc <- paste0("±", round(zcrit, 3))
    } else if (alt == "greater") {
      pval <- 1 - pnorm(z)
      zcrit <- qnorm(1 - alpha)
      reject <- z > zcrit
      crit_desc <- paste0(round(zcrit, 3))
    } else { # less
      pval <- pnorm(z)
      zcrit <- qnorm(alpha)
      reject <- z < zcrit
      crit_desc <- paste0(round(zcrit, 3))
    }

    list(p0 = p0, ph = ph, n = n, alpha = alpha, alt = alt,
         se = se, z = z, pval = pval, zcrit = zcrit, reject = reject,
         crit_desc = crit_desc)
  })

  fmt <- function(x, d = 4) formatC(x, format = "f", digits = d)

  # Hypotheses text
  output$hypotheses <- renderText({
    p0 <- calc()$p0
    alt <- calc()$alt
    h1 <- paste0("H0: p = ", fmt(p0, 3))
    h2 <- switch(alt,
      two.sided = paste0("H1: p ≠ ", fmt(p0, 3)),
      greater   = paste0("H1: p > ", fmt(p0, 3)),
      less      = paste0("H1: p < ", fmt(p0, 3))
    )
    paste(h1, " | ", h2)
  })

  # Summary table
  output$summary <- renderTable({
    cdat <- calc()
    data.frame(
      Quantity = c("Null proportion (p0)", "Sample proportion (p̂)", "n",
                   "Standard error (SE)", "Test statistic (z)", "p-value",
                   "Significance (α)", "Critical value(s)"),
      Value = c(fmt(cdat$p0, 4), fmt(cdat$ph, 4), cdat$n,
                fmt(cdat$se, 6), fmt(cdat$z, 4), fmt(cdat$pval, 6),
                fmt(cdat$alpha, 4), cdat$crit_desc),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, digits = 4)

  # Decision & conclusion
  output$decision <- renderText({
    if (calc()$reject) {
      paste0("Decision: Reject H0 at α = ", fmt(calc()$alpha, 4))
    } else {
      paste0("Decision: Fail to reject H0 at α = ", fmt(calc()$alpha, 4))
    }
  })

  output$conclusion <- renderText({
    cdat <- calc()
    dir_text <- switch(cdat$alt,
      two.sided = "that p ≠ p0",
      greater   = "that p > p0",
      less      = "that p < p0"
    )
    if (cdat$reject) {
      paste0(
        "There is sufficient evidence to conclude ", dir_text,
        " (z = ", fmt(cdat$z, 4), ", p = ", fmt(cdat$pval, 6), ")."
      )
    } else {
      paste0(
        "There is not sufficient evidence to conclude ", dir_text,
        " (z = ", fmt(cdat$z, 4), ", p = ", fmt(cdat$pval, 6), ")."
      )
    }
  })

  # P-value plot
  output$pplot <- renderPlot({
    cdat <- calc()
    z <- cdat$z
    alt <- cdat$alt

    x <- seq(-4, 4, length.out = 2000)
    dens <- dnorm(x)

    shade <- switch(alt,
      two.sided = abs(x) >= abs(z),
      greater   = x >= z,
      less      = x <= z
    )

    df <- data.frame(x = x, y = dens, shade = shade)

    ggplot(df, aes(x, y)) +
      geom_line(linewidth = 0.9) +
      geom_area(data = subset(df, shade), aes(y = y), alpha = 0.3) +
      geom_vline(xintercept = z, linetype = "dashed", linewidth = 0.9) +
      annotate("text", x = z, y = dnorm(z) * 0.95, label = paste0("z = ", round(z, 3)), vjust = -0.6) +
      labs(x = "z (standard normal)", y = "Density",
           title = "P-value area under the standard normal curve") +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)