# Título    : explore.R
# Autor(es) : carlos, paulina

source("src/data/import.R")

# Exploración de datos ====
# Columna de precios ----
food_prices <- food_and_drinks_sales %>% mutate(price = sales / quantity)

# Tickets por semana ----
tickets_per_week <- ticket_office %>%
  group_by(week) %>%
  summarise(
    sum = sum(tickets),
    mean = mean(tickets),
    med = median(tickets),
    q_1 = quantile(tickets, 0.25),
    q_3 = quantile(tickets, 0.75),
    sd = sd(tickets),
    cv = sd / mean
  )

# Ventas en dulcería por semana ----
food_and_drinks_sales_per_week <- food_and_drinks_sales %>%
  group_by(week) %>%
  summarise(
    sum = sum(sales),
    mean = mean(sales),
    med = median(sales),
    q_1 = quantile(sales, 0.25),
    q_3 = quantile(sales, 0.75),
    sd = sd(sales),
    cv = sd / mean
  )

# Ventas en dulcería por tipo de producto ----
food_bd <- food_prices %>% filter(family == "Bottled drinks")
food_cd <- food_prices %>% filter(family == "Coffee drinks")
food_combos <- food_prices %>% filter(family == "Combos")
food_other <- food_prices %>% filter(family == "Other families")
food_popcorn <- food_prices %>% filter(family == "Popcorn")

# Ventas en dulcería por zona ----
food_r1 <- food_prices %>% filter(region == 1)
food_r2 <- food_prices %>% filter(region == 2)
food_r3 <- food_prices %>% filter(region == 3)
food_r4 <- food_prices %>% filter(region == 4)

# Venta de boletos por zona ----
tickets_r1 <- ticket_office %>% filter(region == 1)
tickets_r2 <- ticket_office %>% filter(region == 2)
tickets_r3 <- ticket_office %>% filter(region == 3)
tickets_r4 <- ticket_office %>% filter(region == 4)

# Ventas semanales en dulcería ----
food_sales <- food_and_drinks_sales %>%
  group_by(week) %>%
  summarise(sales = sum(sales))
food_sales_r1 <- food_and_drinks_sales %>%
  filter(region == 1) %>%
  group_by(week) %>%
  summarise(sales = sum(sales))
food_sales_r2 <- food_and_drinks_sales %>%
  filter(region == 2) %>%
  group_by(week) %>%
  summarise(sales = sum(sales))
food_sales_r3 <- food_and_drinks_sales %>%
  filter(region == 3) %>%
  group_by(week) %>%
  summarise(sales = sum(sales))
food_sales_r4 <- food_and_drinks_sales %>%
  filter(region == 4) %>%
  group_by(week) %>%
  summarise(sales = sum(sales))

# Datos para gráficas ====
colours <- c("#f2efea", "#fc7753", "#66d7d1", "#403d58", "#dbd56e")
regions <- c("Nueva Inglaterra", "Atlántico Central", "Sureste", "Florida")

# Ancho de caja, histogramas
bw_food_t <- food_binwidth(food_prices)
bw_food_r1 <- food_binwidth(food_r1)
bw_food_r2 <- food_binwidth(food_r2)
bw_food_r3 <- food_binwidth(food_r3)
bw_food_r4 <- food_binwidth(food_r4)
bw_tickets_t <- tickets_binwidth(ticket_office)
bw_tickets_r1 <- tickets_binwidth(tickets_r1)
bw_tickets_r2 <- tickets_binwidth(tickets_r2)
bw_tickets_r3 <- tickets_binwidth(tickets_r3)
bw_tickets_r4 <- tickets_binwidth(tickets_r4)

# Info histogramas
food_hist_t <- list(
  food_prices, bw_food_t, colours[[1]],
  "Precios de dulcería"
)
food_hist_1 <- list(
  food_r1, bw_food_r1, colours[[2]],
  "Precios de dulcería en Nueva Inglaterra"
)
food_hist_2 <- list(
  food_r2, bw_food_r2, colours[[3]],
  "Precios de dulcería en el Atlántico Central"
)
food_hist_3 <- list(
  food_r3, bw_food_r3, colours[[4]],
  "Precios de dulcería en el Sureste"
)
food_hist_4 <- list(
  food_r4, bw_food_r4, colours[[5]],
  "Precios de dulcería en Florida"
)
tickets_hist_t <- list(
  ticket_office, bw_tickets_t, colours[[1]],
  "Venta de boletos"
)
tickets_hist_r1 <- list(
  tickets_r1, bw_tickets_r1, colours[[2]],
  "Venta de boletos en Nueva Inglaterra"
)
tickets_hist_r2 <- list(
  tickets_r2, bw_tickets_r2, colours[[3]],
  "Venta de boletos en el Atlántico Central"
)
tickets_hist_r3 <- list(
  tickets_r3, bw_tickets_r3, colours[[4]],
  "Venta de boletos en el Sureste"
)
tickets_hist_r4 <- list(
  tickets_r4, bw_tickets_r4, colours[[5]],
  "Venta de boletos en Florida"
)

# Info diagramas de caja y brazos
food_boxplot <- list(
  food_prices, food_prices$price,
  "Precios de dulcería por zona", dollar_format()
)
tickets_boxplot <- list(
  ticket_office, ticket_office$tickets,
  "Venta de boletos por zona", comma
)
food_boxplot_t <- list(
  food_prices, food_prices$price,
  "Precios de dulcería", dollar_format()
)
tickets_boxplot_t <- list(
  ticket_office, ticket_office$tickets,
  "Venta de boletos", comma
)

# Info series de tiempo
food_time_series <- list(
  food_sales_r1, food_sales_r1$sales,
  food_sales_r2, food_sales_r2$sales,
  food_sales_r3, food_sales_r3$sales,
  food_sales_r4, food_sales_r4$sales,
  dollar_format(),
  seq(0, 60000000, 10000000)
)
tickets_time_series <- list(
  tickets_r1, tickets_r1$tickets,
  tickets_r2, tickets_r2$tickets,
  tickets_r3, tickets_r3$tickets,
  tickets_r4, tickets_r4$tickets,
  comma,
  seq(0, 2500000, 500000)
)

# Info diagramas de dispersión
food_prices %>%
  filter(family == "Bottled drinks") %>%
  group_by(week) %>%
  summarise(
    sales = sum(sales),
    price = mean(price)
  ) -> tickets_v_bottled_drinks
food_prices %>%
  filter(family == "Coffee drinks") %>%
  group_by(week) %>%
  summarise(
    sales = sum(sales),
    price = mean(price)
  ) -> tickets_v_coffee_drinks
food_prices %>%
  filter(family == "Combos") %>%
  group_by(week) %>%
  summarise(
    sales = sum(sales),
    price = mean(price)
  ) -> tickets_v_combos
food_prices %>%
  filter(family == "Other families") %>%
  group_by(week) %>%
  summarise(
    sales = sum(sales),
    price = mean(price)
  ) -> tickets_v_other
food_prices %>%
  filter(family == "Popcorn") %>%
  group_by(week) %>%
  summarise(
    sales = sum(sales),
    price = mean(price)
  ) -> tickets_v_popcorn

tickets_v_bottled_drinks$tickets <- tickets_per_week$mean
tickets_v_coffee_drinks$tickets <- tickets_per_week$mean
tickets_v_combos$tickets <- tickets_per_week$mean
tickets_v_other$tickets <- tickets_per_week$mean
tickets_v_popcorn$tickets <- tickets_per_week$mean