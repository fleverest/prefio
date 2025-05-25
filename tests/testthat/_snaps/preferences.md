# wide_preferences handles NA values with drop_preferences option

    Code
      result <- wide_preferences(wide_data, ranking_cols = c(Apple, Banana, Carrot),
      na_action = "drop_preferences")
    Message
      Found rows containing `NA`. Any such preferences will be dropped.
      Removing 2 row(s) containing NA values in ranking columns.

