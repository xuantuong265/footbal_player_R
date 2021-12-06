library(plotly)

players <- read_csv("FullData.csv")

count_numbers = count(players, "Preffered_Foot")

View(players)

fig <- plot_ly(
  type='histogram',
  x=~rnorm(100, 5),
  bingroup=1)

fig <- fig %>% add_trace(
  type='histogram',
  x=~rnorm(20, 5),
  bingroup=1)

fig <- fig %>% layout(
  barmode="overlay",
  bargap=0.1)

fig