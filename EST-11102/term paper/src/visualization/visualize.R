# Título    : visualize.R
# Autor(es) : carlos, paulina

source("src/data/explore.R")

plots <- list()

plots[[1]] <- food_hist(food_hist_t)
plots[[2]] <- food_hist(food_hist_1)
plots[[3]] <- food_hist(food_hist_2)
plots[[4]] <- food_hist(food_hist_3)
plots[[5]] <- food_hist(food_hist_4)
plots[[6]] <- tickets_hist(tickets_hist_t)
plots[[7]] <- tickets_hist(tickets_hist_r1)
plots[[8]] <- tickets_hist(tickets_hist_r2)
plots[[9]] <- tickets_hist(tickets_hist_r3)
plots[[10]] <- tickets_hist(tickets_hist_r4)
plots[[11]] <- region_boxplot(food_boxplot)
plots[[12]] <- region_boxplot(tickets_boxplot)
plots[[13]] <- total_boxplot(food_boxplot_t)
plots[[14]] <- total_boxplot(tickets_boxplot_t)
plots[[15]] <- time_series(
  food_time_series,
  "Ventas en dulcería de 2017",
  "2016-12-29", "2018-01-01"
)
plots[[16]] <- time_series(
  food_time_series,
  "Ventas en dulcería de 2018",
  "2017-12-31", "2019-01-01"
)
plots[[17]] <- time_series(
  food_time_series,
  "Ventas en dulcería de 2019",
  "2018-12-31", "2019-08-01"
)
plots[[18]] <- time_series(
  tickets_time_series,
  "Venta de boletos en 2017",
  "2016-12-29", "2018-01-01"
)
plots[[19]] <- time_series(
  tickets_time_series,
  "Venta de boletos en 2018",
  "2017-12-31", "2019-01-01"
)
plots[[20]] <- time_series(
  tickets_time_series,
  "Venta de boletos en 2019",
  "2018-12-31", "2019-08-01"
)
plots[[21]] <- ggplot() +
  geom_line(
    data = food_sales,
    aes(
      x = as.Date(week),
      y = sales
    )
  ) +
  labs(title = "Ventas en dulcería") +
  scale_x_date(
    date_labels = "%b/%Y",
    date_breaks = "2 months"
  ) +
  scale_y_continuous(
    label = dollar_format(),
    breaks = seq(0, 200000000, 25000000)
  ) +
  theme_fivethirtyeight() +
  theme(
    text = element_text(
      family = "Linux Libertine",
      size = 14
    ),
    plot.title = element_text(
      family = "Linux Libertine",
      size = 20,
      face = "bold.italic"
    )
  )
plots[[22]] <- tickets_per_week %>%
  ggplot() +
  geom_line(aes(
    x = as.Date(week),
    y = sum
  )) +
  labs(title = "Venta de boletos") +
  scale_x_date(
    date_labels = "%b/%Y",
    date_breaks = "2 months"
  ) +
  scale_y_continuous(
    label = comma,
    breaks = seq(500000, 8000000, 1000000)
  ) +
  theme_fivethirtyeight() +
  theme(
    text = element_text(
      family = "Linux Libertine",
      size = 14
    ),
    plot.title = element_text(
      family = "Linux Libertine",
      size = 20,
      face = "bold.italic"
    )
  )
plots[[23]] <- scatter_plot(
  tickets_v_bottled_drinks,
  tickets_v_bottled_drinks$sales,
  "Asistentes semanales promedio v.
                           Venta de bebidas embotelladas",
  "Ventas en dulcería",
  2500000,
  250000
)
plots[[24]] <- scatter_plot(
  tickets_v_coffee_drinks,
  tickets_v_coffee_drinks$sales,
  "Asistentes semanales promedio v.
                           Venta de bebidas de cafetería",
  "Ventas en dulcería",
  2000000,
  200000
)
plots[[25]] <- scatter_plot(
  tickets_v_combos,
  tickets_v_combos$sales,
  "Asistentes semanales promedio v.
                           Venta de combos",
  "Ventas en dulcería",
  120000000,
  10000000
)
plots[[26]] <- scatter_plot(
  tickets_v_other,
  tickets_v_other$sales,
  "Asistentes semanales promedio v.
                           Venta de postres, nachos, nuggets, etc.",
  "Ventas en dulcería",
  60000000,
  5000000
)
plots[[27]] <- scatter_plot(
  tickets_v_popcorn,
  tickets_v_popcorn$sales,
  "Asistentes semanales promedio v.
                           Venta de palomitas",
  "Ventas en dulcería",
  20000000,
  2500000
)
plots[[28]] <- scatter_plot(
  tickets_v_bottled_drinks,
  tickets_v_bottled_drinks$price,
  "Asistentes semanales promedio v.
                            precios de bebidas embotelladas",
  "Precio",
  40,
  2.5
)
plots[[29]] <- scatter_plot(
  tickets_v_coffee_drinks,
  tickets_v_coffee_drinks$price,
  "Asistentes semanales promedio v.
                            precios de bebidas de cafetería",
  "Precio",
  70,
  2.5
)
plots[[30]] <- scatter_plot(
  tickets_v_combos,
  tickets_v_combos$price,
  "Asistentes semanales promedio v.
                            precios de combos",
  "Precio",
  160,
  5
)
plots[[31]] <- scatter_plot(
  tickets_v_other,
  tickets_v_other$price,
  "Asistentes semanales promedio v.
                            precios de postres, nachos, nuggets, etc.",
  "Precio",
  70,
  5
)
plots[[32]] <- scatter_plot(
  tickets_v_popcorn,
  tickets_v_popcorn$price,
  "Asistentes semanales promedio v.
                            precios de palomitas",
  "Precio",
  80,
  2.5
)

# Exportar ====
# export_svg(1, 14, 12, 6.75)
# export_svg(15, 22, 15, 5)
# export_svg(23, 32, 12, 6.75)