radar_df <- function(df, data) {
  max_row <- tibble(PTS = max(data$PTS), TRB = max(data$TRB), AST = max(data$AST), STL = max(data$STL), BLK = max(data$BLK))
  min_row <- tibble(PTS = 0, TRB = 0, AST = 0, STL = 0, BLK = 0)

  df %>%
    add_row(max_row, .before = 1) %>%
    add_row(min_row, .before = 2)
}
