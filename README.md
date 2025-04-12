# No-Vig Expected Value (EV) and Kelly Criterion Calculator

## Overview

This Shiny application empowers bettors to calculate **no-vig (fair) odds**, **expected value (EV)**, and **optimal bet sizes** based on sportsbook odds. By leveraging the **Kelly Criterion**, the app suggests stake sizes to help users identify profitable betting opportunities and maximize long-term bankroll growth.

## Features

- **Odds Conversion**: Input American odds to automatically calculate decimal odds and implied probabilities.
- **No-Vig Odds Calculation**: Removes bookmaker's margin (vig) using multiplicative and additive methods, delivering blended no-vig odds for precise market analysis.
- **Expected Value (EV) Calculation**: Computes EV based on your selected bet and no-vig probabilities.
- **Kelly Criterion Bet Sizing**: Recommends optimal bet sizes tailored to your bankroll, adjustable via a Kelly multiplier.
- **Interactive Visualization**: Displays graphs comparing EV per wager versus total EV using recommended Kelly stakes.
- **Dynamic Interface**: Adjust the number of sportsbooks and betting preferences through an intuitive, user-friendly interface.

## Application Usage

### Inputs
- **Number of Books**: Select how many sportsbooks’ odds to input (1–10).
- **Sportsbook Odds**: Enter American odds for both sides of the bet from different sportsbooks.
- **Bet Preferences**:
  - **Side Selection**: Choose "Away/Over" or "Home/Under".
  - **Your Betting Line**: Input your own odds in American format.
  - **Bankroll**: Specify your total available betting funds.
  - **Kelly Multiplier**: Adjust staking aggressiveness (1 = Full Kelly, <1 = fractional Kelly).

### Outputs
- **No-Vig Market Table**: Shows no-vig probabilities, decimal odds, and American odds.
- **EV and Stake Recommendations**: Details EV calculations, Kelly fraction, optimal stake sizes, and total expected profit.
- **EV Visualization**: Graphically compares expected value metrics for better decision-making.

## Technical Information

### Dependencies
- `shiny`
- `shinydashboard`
- `DT`
- `shinyBS`
- `ggplot2`

### Functions Used
- **Odds Conversion**:
  - `american_to_decimal(american_odds)`
  - `decimal_to_american(decimal_odds)`
- **No-Vig Methods**:
  - `apply_devig_method(p1, p2, method)` [Multiplicative & Additive]
- **Bet Sizing**:
  - `calculate_kelly_fraction(prob, decimal_odds)`

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/bettor-analysis/no-vig-ev-kelly-calculator.git
2. Ensure R is installed, then istall required packages
   `install.packages(c("shiny", "shinydashboard", "DT", "shinyBS", "ggplot2"))`
3. Run the Shiny app:
   `library(shiny)
   runApp("path_to_app_directory")`


## Disclaimer
- This tool is for educational purposes. Always gamble responsibly and within your financial limits.


## License
- This project is licensed under the MIT License.

   
