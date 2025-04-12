# Shiny app for calculating no-vig fair odds and expected value (EV) based on sportsbook odds.
# Betting odds are entered in American format, and the app computes no-vig probabilities, expected value, 
# and optimal bet sizes using the Kelly criterion.

# Adam Wickwire - Bettor Analysis
# 04/12/2025


library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)
library(ggplot2)

#--------------------#
# Utility Functions  #
#--------------------#

american_to_decimal <- function(american_odds) {
  if (is.na(american_odds)) return(NA)
  if (american_odds > 0) {
    1 + (american_odds / 100)
  } else {
    1 + (100 / abs(american_odds))
  }
}

decimal_to_american <- function(decimal_odds) {
  if (is.na(decimal_odds)) return(NA)
  if (decimal_odds >= 2) {
    as.integer(round((decimal_odds - 1) * 100))
  } else {
    as.integer(round(-100 / (decimal_odds - 1)))
  }
}

calculate_kelly_fraction <- function(prob, decimal_odds) {
  if (is.na(prob) || is.na(decimal_odds) || decimal_odds <= 1) return(NA)
  b <- decimal_odds - 1
  (prob * (b + 1) - 1) / b
}

apply_devig_method <- function(p1, p2, method) {
  if (method == "Multiplicative") {
    total <- p1 + p2
    p_away <- p1 / total
    p_home <- p2 / total
  } else if (method == "Additive") {
    overround <- (p1 + p2) - 1
    p_away <- p1 - overround / 2
    p_home <- p2 - overround / 2
    if (p_away < 0 || p_home < 0) {
      p_away <- p1 / (p1 + p2)
      p_home <- p2 / (p1 + p2)
    }
  } else {
    stop("Unknown method")
  }
  return(c(p_away, p_home))
}

#---------------------------#
#         UI Layout         #
#---------------------------#
ui <- dashboardPage(
  dashboardHeader(title = "No-Vig EV Calculator"),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; background-color: #f9f9f9; }
        .content-wrapper { margin: 20px; }
        .box { box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
        h3, h4 { color: #333; }
        .margin-bottom { margin-bottom: 20px; }
      "))
    ),
    
    fluidRow(
      box(width = 12, title = "Welcome", status = "info", solidHeader = TRUE, collapsible = TRUE,
          p("Use this app to compute no-vig odds, expected value, and optimal bet sizes based on sportsbook odds. Note: This app is for educational purposes. Always gamble responsibly.")
      )
    ),
    
    fluidRow(
      column(width = 6,
             box(title = "Odds Input", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                 sliderInput("bookCount", "Number of Books:", 
                             min = 1, max = 10, value = 1, step = 1),
                 uiOutput("booksInput")
             )
      ),
      column(width = 6,
             box(title = "Bet Preferences", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                 selectInput("betSide", "Which side are you betting on?",
                             choices = c("Away/Over", "Home/Under")),
                 numericInput("userBetOdds", "Your Betting Line (American Odds):", value = NA),
                 bsTooltip("userBetOdds", "Enter the American odds for your bet (e.g. +150 or -200).", placement = "auto", trigger = "hover"),
                 numericInput("bankroll", "Bankroll:", value = 500, min = 0, step = 1),
                 bsTooltip("bankroll", "Enter your total bankroll available for betting.", placement = "auto", trigger = "hover"),
                 numericInput("kellyFactor", "Kelly Multiplier (0-1, e.g. 0.5 for half Kelly):", 
                              value = 1, min = 0, max = 1, step = 0.1),
                 bsTooltip("kellyFactor", "Adjust the Kelly bet size (1 = full Kelly, 0.5 = half Kelly).", placement = "auto", trigger = "hover"),
                 actionButton("calculate", "Calculate", icon = icon("calculator")),
                 actionButton("reset", "Reset", icon = icon("refresh"))
             )
      )
    ),
    
    fluidRow(
      box(title = "No-Vig Market Results", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
          DTOutput("noVigTable"),
          downloadButton("downloadData", "Download Results"),
          bsTooltip("noVigTable", "No-vig probabilities and odds used in calculations.", placement = "bottom", trigger = "hover")
      )
    ),
    fluidRow(
      box(title = "Bet Sizing and EV Details", status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE,
          verbatimTextOutput("resultsText")
      )
    ),
    fluidRow(
      box(title = "EV Visualization", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
          plotOutput("evPlot")
      )
    )
  )
)

#---------------------------#
#         SERVER            #
#---------------------------#
server <- function(input, output, session) {
  
  output$booksInput <- renderUI({
    count <- input$bookCount
    inputList <- lapply(1:count, function(i) {
      fluidRow(
        column(6,
               numericInput(inputId = paste0("awayOdds_", i),
                            label = paste("Book", i, "Away/Over Odds:"), 
                            value = NA)
        ),
        column(6,
               numericInput(inputId = paste0("homeOdds_", i),
                            label = paste("Book", i, "Home/Under Odds:"), 
                            value = NA)
        )
      )
    })
    do.call(tagList, inputList)
  })
  
  awayOddsVec <- reactive({
    count <- input$bookCount
    sapply(1:count, function(i) input[[paste0("awayOdds_", i)]])
  })
  
  homeOddsVec <- reactive({
    count <- input$bookCount
    sapply(1:count, function(i) input[[paste0("homeOdds_", i)]])
  })
  
  calcData <- eventReactive(input$calculate, {
    awayOdds <- awayOddsVec()
    homeOdds <- homeOddsVec()
    
    validate(
      need(length(awayOdds[!is.na(awayOdds)]) > 0, "Please provide at least one valid Away/Over odds."),
      need(length(homeOdds[!is.na(homeOdds)]) > 0, "Please provide at least one valid Home/Under odds."),
      need(!is.na(input$userBetOdds), "Please enter your betting odds."),
      need(input$bankroll > 0, "Bankroll must be greater than 0."),
      need(input$kellyFactor >= 0 && input$kellyFactor <= 1, "Kelly Multiplier must be between 0 and 1.")
    )
    
    awayOdds <- awayOdds[!is.na(awayOdds)]
    homeOdds <- homeOdds[!is.na(homeOdds)]
    
    awayDec <- sapply(awayOdds, american_to_decimal)
    homeDec <- sapply(homeOdds, american_to_decimal)
    awayImp <- 1 / awayDec
    homeImp <- 1 / homeDec
    
    # Convert implied probabilities to log-probabilities
    awayLogImp <- log(awayImp)
    homeLogImp <- log(homeImp)
    
    # Average the log-probabilities
    awayLogImpMean <- mean(awayLogImp)
    homeLogImpMean <- mean(homeLogImp)
    
    # Convert back to implied probabilities
    awayImpMean <- exp(awayLogImpMean)
    homeImpMean <- exp(homeLogImpMean)
    
    # Compute Multiplicative and Additive probabilities
    mult_probs <- apply_devig_method(awayImpMean, homeImpMean, "Multiplicative")
    add_probs <- apply_devig_method(awayImpMean, homeImpMean, "Additive")
    
    # Compute blend (average of Multiplicative and Additive)
    blend_awayProb <- mean(c(mult_probs[1], add_probs[1]))
    blend_homeProb <- mean(c(mult_probs[2], add_probs[2]))
    
    # Ensure probabilities sum to 1
    total_prob <- blend_awayProb + blend_homeProb
    finalAwayProb <- blend_awayProb / total_prob
    finalHomeProb <- blend_homeProb / total_prob
    
    # Compute odds and EV
    awayDecimal <- 1 / finalAwayProb
    homeDecimal <- 1 / finalHomeProb
    awayAmerican <- decimal_to_american(awayDecimal)
    homeAmerican <- decimal_to_american(homeDecimal)
    
    betSide <- input$betSide
    userBetOdds <- input$userBetOdds
    userDecimal <- american_to_decimal(userBetOdds)
    betProbFinal <- if (betSide == "Away/Over") finalAwayProb else finalHomeProb
    ev_final <- betProbFinal * (userDecimal - 1) - (1 - betProbFinal)
    
    kellyFrac <- calculate_kelly_fraction(betProbFinal, userDecimal)
    kellyStake <- if (is.na(kellyFrac) || kellyFrac <= 0) {
      0
    } else {
      kellyFrac * input$kellyFactor * input$bankroll
    }
    totalExpectedProfit <- if (kellyStake > 0) kellyStake * ev_final else 0
    
    list(
      finalAwayProb = finalAwayProb,
      finalHomeProb = finalHomeProb,
      awayAmerican = awayAmerican,
      homeAmerican = homeAmerican,
      userDecimal = userDecimal,
      betProbFinal = betProbFinal,
      ev_final = ev_final,
      kellyFrac = kellyFrac,
      kellyStake = kellyStake,
      totalExpectedProfit = totalExpectedProfit
    )
  })
  
  output$noVigTable <- renderDT({
    data <- calcData()
    if (is.null(data)) return(NULL)
    df <- data.frame(
      Side = c("Away/Over", "Home/Under"),
      `No-Vig Probability` = c(paste0(formatC(round(100 * data$finalAwayProb, 1), format = "f", digits = 1), "%"),
                               paste0(formatC(round(100 * data$finalHomeProb, 1), format = "f", digits = 1), "%")),
      `No-Vig Decimal Odds` = c(formatC(round(1 / data$finalAwayProb, 2), format = "f", digits = 2),
                                formatC(round(1 / data$finalHomeProb, 2), format = "f", digits = 2)),
      `No-Vig American Odds` = c(paste0(ifelse(data$awayAmerican > 0, "+", ""), data$awayAmerican),
                                 paste0(ifelse(data$homeAmerican > 0, "+", ""), data$homeAmerican))
    )
    datatable(df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  output$resultsText <- renderText({
    data <- calcData()
    if (is.null(data)) return("")
    
    if (data$kellyStake == 0) {
      paste0(
        "Selected bet side no-vig probability: ", formatC(round(100 * data$betProbFinal, 1), format = "f", digits = 1), "%\n",
        "Your betting odds (decimal conversion): ", formatC(round(data$userDecimal, 2), format = "f", digits = 2), "\n\n",
        "Expected Value (EV) per $1 wager: $", formatC(round(data$ev_final, 2), format = "f", digits = 2),
        " (", formatC(round(100 * data$ev_final, 1), format = "f", digits = 1), "%)\n\n",
        "Since the Kelly fraction is not positive, no bet is recommended."
      )
    } else {
      paste0(
        "Selected bet side no-vig probability: ", formatC(round(100 * data$betProbFinal, 1), format = "f", digits = 1), "%\n",
        "Your betting odds (decimal conversion): ", formatC(round(data$userDecimal, 2), format = "f", digits = 2), "\n\n",
        "Expected Value (EV) per $1 wager: $", formatC(round(data$ev_final, 2), format = "f", digits = 2),
        " (", formatC(round(100 * data$ev_final, 1), format = "f", digits = 1), "%)\n\n",
        "Full Kelly fraction: ", formatC(round(100 * data$kellyFrac, 1), format = "f", digits = 1), "%\n",
        "Kelly multiplier (user input): ", input$kellyFactor, "\n",
        "Recommended bet size: $", formatC(round(data$kellyStake, 2), format = "f", digits = 2), "\n\n",
        "Total Expected Profit on Recommended Bet: $", formatC(round(data$totalExpectedProfit, 2), format = "f", digits = 2),
        " (", formatC(round(100 * data$ev_final, 1), format = "f", digits = 1), "% return)"
      )
    }
  })
  
  output$evPlot <- renderPlot({
    data <- calcData()
    if (is.null(data) || is.na(data$ev_final)) return(NULL)
    
    df <- data.frame(
      Type = c("EV per $1", "Total EV on Kelly Bet"),
      EV = c(data$ev_final, data$totalExpectedProfit)
    )
    
    ggplot(df, aes(x = Type, y = EV, fill = Type)) +
      geom_bar(stat = "identity", width = 0.5) +
      labs(title = "Expected Value Comparison", x = "", y = "EV ($)") +
      theme_minimal() +
      theme(legend.position = "none", text = element_text(size = 14))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("no-vig-results-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      data <- calcData()
      df <- data.frame(
        Side = c("Away/Over", "Home/Under"),
        NoVig_Probability = c(data$finalAwayProb, data$finalHomeProb),
        NoVig_American_Odds = c(data$awayAmerican, data$homeAmerican)
      )
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$reset, {
    updateSliderInput(session, "bookCount", value = 1)
    updateSelectInput(session, "betSide", selected = "Away/Over")
    updateNumericInput(session, "userBetOdds", value = NA)
    updateNumericInput(session, "bankroll", value = 500)
    updateNumericInput(session, "kellyFactor", value = 1)
    for (i in 1:10) {
      updateNumericInput(session, paste0("awayOdds_", i), value = NA)
      updateNumericInput(session, paste0("homeOdds_", i), value = NA)
    }
  })
}

shinyApp(ui = ui, server = server)