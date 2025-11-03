library(shiny)

library(bslib)

library(ggplot2)

ui <- page_fillable(
  title = "Hypothesis Test for Population Proportions",
  theme = bs_theme(primary = "#A90533"),
  withMathJax(),
  div(
    style = "text-align: left; margin-bottom: 20px; background-color: #A90533; padding: 20px; border-radius: 5px;",
    h1("Hypothesis Test for Population Proportions",
       style = "color: white; font-weight: bold; margin: 0;")
  ),
  layout_sidebar(
    sidebar = sidebar(
      selectInput(
        "test_type", "Test Type:",
        list("One-Sample Prop" = "one", "Two-Sample Prop" = "two")
      ),

      # One-sample inputs
      conditionalPanel(
        condition = "input.test_type == 'one'",
        numericInput("p0",    "Null Proportion: p\u2080", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("p_hat", "Sample Proportion: p\u0302", value = 0.55, min = 0, max = 1, step = 0.001),
        numericInput("n",     "Sample Size: n", value = 100, min = 1, step = 1)
      ),

      # Two-sample inputs
      conditionalPanel(
        condition = "input.test_type == 'two'",
        numericInput("p1hat", "Group 1 Proportion: p\u0302\u2081", value = 0.60, min = 0, max = 1, step = 0.001),
        numericInput("n1",    "Group 1 Size: n\u2081", value = 120, min = 1, step = 1),
        numericInput("p2hat", "Group 2 Proportion: p\u0302\u2082", value = 0.50, min = 0, max = 1, step = 0.001),
        numericInput("n2",    "Group 2 Size: n\u2082", value = 100, min = 1, step = 1)
      ),

      # Shared controls
      numericInput("alpha", "Significance Level: \u03B1", value = 0.05, min = 0.0001, max = 0.5, step = 0.001),
      selectInput(
        "alt", "Alternative Hypothesis",
        choices = c("Two-Sided: ≠" = "two.sided", "Right-Sided: >" = "greater", "Left-Sided: <" = "less"),
        selected = "two.sided"
      )
    ),

    # Main content
    div(class = "app-fill main-fill",
      navset_card_pill(
        nav_panel(
          "Test Summary",
          card_body(tableOutput("summary"))
        ),
        nav_panel(
          "Visualize p-Value",
          card_body(
            # container for overlay
            div(
              style = "position: relative;",
              plotOutput("pplot", height = "420px"),
              # overlay card
              div(
                class = "card shadow-sm",
                style = paste(
                  "position: absolute; top: 16px; left: 100px;",
                  "background-color: white; opacity: 0.94;",
                  "border-radius: 12px; padding: 10px 14px;",
                  "border: 1px solid #e5e7eb; font-size: 0.95rem; color: #333;"
                ),
                htmlOutput("zpvals")
              )
            )
          )
        ),
        nav_panel(
          "Conclusion",
          card_body(
            h5(textOutput("hypotheses")),
            h5(textOutput("decision")),
            p(textOutput("conclusion"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  fmt  <- function(x, d = 4) formatC(x, format = "f", digits = d)
  fmtp <- function(p) ifelse(p < 1e-4, formatC(p, format = "e", digits = 2),
                             formatC(p, format = "f", digits = 6))

  # Core reactive calculations (branches by test type)
  calc <- reactive({
    alpha <- input$alpha
    alt   <- input$alt
    ttype <- input$test_type

    if (ttype == "one") {
      p0 <- input$p0; ph <- input$p_hat; n <- input$n
      se <- sqrt(p0 * (1 - p0) / n)
      z  <- (ph - p0) / se

      if (alt == "two.sided") {
        pval <- 2 * pnorm(-abs(z)); zcrit <- qnorm(1 - alpha/2); reject <- abs(z) > zcrit; crit_desc <- paste0("±", round(zcrit, 3))
      } else if (alt == "greater") {
        pval <- 1 - pnorm(z);       zcrit <- qnorm(1 - alpha);    reject <- z > zcrit;       crit_desc <- paste0(round(zcrit, 3))
      } else {
        pval <- pnorm(z);           zcrit <- qnorm(alpha);        reject <- z < zcrit;       crit_desc <- paste0(round(zcrit, 3))
      }

      list(type="one", alpha=alpha, alt=alt, p0=p0, ph=ph, n=n, se=se, z=z, pval=pval, zcrit=zcrit, reject=reject, crit_desc=crit_desc)

    } else {
      p1 <- input$p1hat; n1 <- input$n1; p2 <- input$p2hat; n2 <- input$n2
      p_pool <- (p1*n1 + p2*n2) / (n1 + n2)
      se <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
      z  <- (p1 - p2) / se

      if (alt == "two.sided") {
        pval <- 2 * pnorm(-abs(z)); zcrit <- qnorm(1 - alpha/2); reject <- abs(z) > zcrit; crit_desc <- paste0("±", round(zcrit, 3))
      } else if (alt == "greater") {
        pval <- 1 - pnorm(z);       zcrit <- qnorm(1 - alpha);    reject <- z > zcrit;       crit_desc <- paste0(round(zcrit, 3))
      } else {
        pval <- pnorm(z);           zcrit <- qnorm(alpha);        reject <- z < zcrit;       crit_desc <- paste0(round(zcrit, 3))
      }

      list(type="two", alpha=alpha, alt=alt, p1=p1, n1=n1, p2=p2, n2=n2, p_pool=p_pool, se=se, z=z, pval=pval, zcrit=zcrit, reject=reject, crit_desc=crit_desc)
    }
  })

  # Hypotheses text
  output$hypotheses <- renderText({
    cdat <- calc()
    if (cdat$type == "one") {
      h1 <- paste0("H0: p = ", fmt(cdat$p0, 3))
      h2 <- switch(cdat$alt,
        two.sided = paste0("H1: p ≠ ", fmt(cdat$p0, 3)),
        greater   = paste0("H1: p > ", fmt(cdat$p0, 3)),
        less      = paste0("H1: p < ", fmt(cdat$p0, 3))
      )
    } else {
      h1 <- "H0: p1 = p2"
      h2 <- switch(cdat$alt,
        two.sided = "H1: p1 ≠ p2",
        greater   = "H1: p1 > p2",
        less      = "H1: p1 < p2"
      )
    }
    paste(h1, " | ", h2)
  })

  # Summary table
  output$summary <- renderTable({
    cdat <- calc()
    if (cdat$type == "one") {
      data.frame(
        Quantity = c("Null proportion (p0)", "Sample proportion (p̂)", "n",
                     "Standard error (SE)", "Test statistic (z)", "p-value",
                     "Significance (α)", "Critical value(s)"),
        Value = c(fmt(cdat$p0, 4), fmt(cdat$ph, 4), cdat$n,
                  fmt(cdat$se, 6), fmt(cdat$z, 4), fmt(cdat$pval, 6),
                  fmt(cdat$alpha, 4), cdat$crit_desc),
        check.names = FALSE
      )
    } else {
      data.frame(
        Quantity = c("p̂1", "n1", "p̂2", "n2",
                     "Pooled proportion (p̂_pool)", "Standard error (SE)",
                     "Test statistic (z)", "p-value",
                     "Significance (α)", "Critical value(s)"),
        Value = c(fmt(cdat$p1, 4), cdat$n1, fmt(cdat$p2, 4), cdat$n2,
                  fmt(cdat$p_pool, 6), fmt(cdat$se, 6),
                  fmt(cdat$z, 4), fmt(cdat$pval, 6),
                  fmt(cdat$alpha, 4), cdat$crit_desc),
        check.names = FALSE
      )
    }
  }, striped = TRUE, bordered = TRUE, digits = 4)

  # Decision & conclusion
  output$decision <- renderText({
    if (calc()$reject) paste0("Decision: Reject H0 at α = ", fmt(calc()$alpha, 4))
    else               paste0("Decision: Fail to reject H0 at α = ", fmt(calc()$alpha, 4))
  })

  output$conclusion <- renderText({
    cdat <- calc()
    dir_text <- if (cdat$type == "one") {
      switch(cdat$alt,
             two.sided = "that p ≠ p0",
             greater   = "that p > p0",
             less      = "that p < p0")
    } else {
      switch(cdat$alt,
             two.sided = "that p1 ≠ p2",
             greater   = "that p1 > p2",
             less      = "that p1 < p2")
    }

    paste0(
      if (cdat$reject) "There is sufficient evidence to conclude " else
        "There is not sufficient evidence to conclude ",
      dir_text,
      " (z = ", fmt(cdat$z, 4), ", p = ", fmtp(cdat$pval), ")."
    )
  })

  # Overlay content (z & p) for the plot tab
  output$zpvals <- renderUI({
    cdat <- calc()
    tags$div(
      style = "line-height: 1.25;",
      tags$div(tags$b("z-statistic: "), sprintf("%.4f", cdat$z)),
      tags$div(tags$b("p-value: "),     fmtp(cdat$pval))
    )
  })

  # P-value plot
  output$pplot <- renderPlot({
  cdat <- calc()
  z   <- cdat$z
  alt <- cdat$alt

  x <- seq(-4, 4, length.out = 4000)
  df <- data.frame(x = x, y = dnorm(x))

  # Prepare tail data explicitly
  z_abs <- abs(z)
  left_tail  <- NULL
  right_tail <- NULL
  one_tail   <- NULL

  if (alt == "two.sided") {
    left_tail  <- subset(df, x <= -z_abs)
    right_tail <- subset(df, x >=  z_abs)
  } else if (alt == "greater") {
    one_tail <- subset(df, x >= z)
  } else { # alt == "less"
    one_tail <- subset(df, x <= z)
  }

  g <- ggplot(df, aes(x, y)) +
    geom_line(linewidth = 0.9) +
    # --- fill tails only ---
    {
      if (alt == "two.sided") {
        list(
          geom_area(data = left_tail,  aes(x = x, y = y), fill = "#A90533", alpha = 0.30, inherit.aes = FALSE),
          geom_area(data = right_tail, aes(x = x, y = y), fill = "#A90533", alpha = 0.30, inherit.aes = FALSE)
        )
      } else {
        geom_area(data = one_tail, aes(x = x, y = y), fill = "#A90533", alpha = 0.30, inherit.aes = FALSE)
      }
    } +
    # --- vertical line(s) ---
    {
      if (alt == "two.sided") {
        list(
          geom_vline(xintercept = -z_abs, linetype = "dashed", linewidth = 0.9, color = "#A90533"),
          geom_vline(xintercept =  z_abs, linetype = "dashed", linewidth = 0.9, color = "#A90533")
        )
      } else {
        geom_vline(xintercept = z, linetype = "dashed", linewidth = 0.9, color = "#A90533")
      }
    } +
    annotate("text", x = z, y = 0, label = paste0("z = ", round(z, 3)),
             vjust = 3, color = "#A90533") +
    labs(y = "Density") +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 13)

    g
  })
}

shinyApp(ui, server)