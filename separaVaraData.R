writexl::write_xlsx(df2, "data/carrefour.xlsx")

df2 <- tidyr::separate(df, datavara, into = c("data", "vara"),
                      sep = "\\s?-\\s?",
                      extra = "merge")


