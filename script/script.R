library(rbcb)

ipca <- get_series(433,
                   start_date = "2000-01-01")

selic <- get_series(432,
                    start_date = "2000-01-01")

dolar <- get_series(1,
                    start_date = "2000-01-01")