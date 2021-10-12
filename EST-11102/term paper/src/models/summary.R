# Título    : summary.R
# Autor(es) : carlos, paulina

source("src/data/explore.R")

csv <- list()

# 2 ====
# Precios promedio por tipo de producto ----
average_prices <- food_and_drinks_sales %>%
  group_by(family) %>%
  summarise(price = mean(sales) / mean(quantity))

# Visitantes promedio (global) ----
weekly_visitors_average <-
  (sum(tickets_per_week$sum)) / (nrow(tickets_per_week))

# 3 ====
basic_stat_food <- basic_stat_food_f(food_prices)
basic_stat_food_r1 <- basic_stat_food_f(food_r1)
basic_stat_food_r2 <- basic_stat_food_f(food_r2)
basic_stat_food_r3 <- basic_stat_food_f(food_r3)
basic_stat_food_r4 <- basic_stat_food_f(food_r4)

basic_stat_tickets <- basic_stat_tickets_f(ticket_office)
basic_stat_tickets_r1 <- basic_stat_tickets_f(tickets_r1)
basic_stat_tickets_r2 <- basic_stat_tickets_f(tickets_r2)
basic_stat_tickets_r3 <- basic_stat_tickets_f(tickets_r3)
basic_stat_tickets_r4 <- basic_stat_tickets_f(tickets_r4)

cor_bd_sales_v_tickets <- cor(tickets_v_bottled_drinks$sales,
  tickets_v_bottled_drinks$tickets,
  method = "pearson"
)
cor_cd_sales_v_tickets <- cor(tickets_v_coffee_drinks$sales,
  tickets_v_coffee_drinks$tickets,
  method = "pearson"
)
cor_combos_sales_v_tickets <- cor(tickets_v_combos$sales,
  tickets_v_combos$tickets,
  method = "pearson"
)
cor_other_sales_v_tickets <- cor(tickets_v_other$sales,
  tickets_v_other$tickets,
  method = "pearson"
)
cor_popcorn_sales_v_tickets <- cor(tickets_v_popcorn$sales,
  tickets_v_popcorn$tickets,
  method = "pearson"
)
cor_bd_price_v_tickets <- cor(tickets_v_bottled_drinks$price,
  tickets_v_bottled_drinks$tickets,
  method = "pearson"
)
cor_cd_price_v_tickets <- cor(tickets_v_coffee_drinks$price,
  tickets_v_coffee_drinks$tickets,
  method = "pearson"
)
cor_combos_price_v_tickets <- cor(tickets_v_combos$price,
  tickets_v_combos$tickets,
  method = "pearson"
)
cor_other_price_v_tickets <- cor(tickets_v_other$price,
  tickets_v_other$tickets,
  method = "pearson"
)
cor_popcorn_price_v_tickets <- cor(tickets_v_popcorn$price,
  tickets_v_popcorn$tickets,
  method = "pearson"
)

# 4 ====
food_combos$month <- floor_date(food_combos$week, "month")
fc <- subset(food_combos, month > "2016-12-01") %>%
  summarise(month = month, region = region, sales = sales)

fc_r1 <- fc %>%
  filter(region == 1) %>%
  group_by(month) %>%
  summarise(sales = sum(sales))
fc_r2 <- fc %>%
  filter(region == 2) %>%
  group_by(month) %>%
  summarise(sales = sum(sales))
fc_r3 <- fc %>%
  filter(region == 3) %>%
  group_by(month) %>%
  summarise(sales = sum(sales))
fc_r4 <- fc %>%
  filter(region == 4) %>%
  group_by(month) %>%
  summarise(sales = sum(sales))

z_r1 <- normalize(fc_r1, fc_r1$sales)
z_r2 <- normalize(fc_r2, fc_r2$sales)
z_r3 <- normalize(fc_r3, fc_r3$sales)
z_r4 <- normalize(fc_r4, fc_r4$sales)

p_r1 <- pnorm(z_r1, 0, 1)
p_r2 <- pnorm(z_r2, 0, 1)
p_r3 <- pnorm(z_r3, 0, 1)
p_r4 <- pnorm(z_r4, 0, 1)

# 5 ====
tickets_r1_ci <- confidence_interval(
  mean(tickets_r1$tickets),
  sd(tickets_r1$tickets),
  nrow(tickets_r1)
)
tickets_r2_ci <- confidence_interval(
  mean(tickets_r2$tickets),
  sd(tickets_r2$tickets),
  nrow(tickets_r2)
)
tickets_r3_ci <- confidence_interval(
  mean(tickets_r3$tickets),
  sd(tickets_r3$tickets),
  nrow(tickets_r3)
)
tickets_r4_ci <- confidence_interval(
  mean(tickets_r4$tickets),
  sd(tickets_r4$tickets),
  nrow(tickets_r4)
)

# 6 ====
cor_total_tickets_v_total_sales <- cor(tickets_per_week$sum,
  food_and_drinks_sales_per_week$sum,
  method = "pearson"
)
cor_total_tickets_v_mean_sales <- cor(tickets_per_week$sum,
  food_and_drinks_sales_per_week$mean,
  method = "pearson"
)

# 7 ====
food_and_drinks_sales_per_week$blockbuster <- blockbusters$blockbuster
blockbusters_only <- food_and_drinks_sales_per_week %>%
  filter(blockbuster == 1)
no_blockbusters <- food_and_drinks_sales_per_week %>%
  filter(blockbuster == 0)

blockbusters_only_ci <- confidence_interval(
  mean(blockbusters_only$sum),
  sd(blockbusters_only$sum),
  nrow(blockbusters_only)
)
no_blockbusters_ci <- confidence_interval(
  mean(no_blockbusters$sum),
  sd(no_blockbusters$sum),
  nrow(no_blockbusters)
)

# 8 ====
tickets_per_week$outstanding <- ifelse(tickets_per_week$sum > 5000000,
  1, 0
)
EMV <- mean(tickets_per_week$outstanding) /
  (1 - mean(tickets_per_week$outstanding))

# CSV ====
csv[[1]] <- average_prices
csv[[2]] <- basic_stat_food
csv[[3]] <- basic_stat_food_r1
csv[[4]] <- basic_stat_food_r2
csv[[5]] <- basic_stat_food_r3
csv[[6]] <- basic_stat_food_r4
csv[[7]] <- basic_stat_tickets
csv[[8]] <- basic_stat_tickets_r1
csv[[9]] <- basic_stat_tickets_r2
csv[[10]] <- basic_stat_tickets_r3
csv[[11]] <- basic_stat_tickets_r4
csv[[12]] <- tickets_r1_ci
csv[[13]] <- tickets_r2_ci
csv[[14]] <- tickets_r3_ci
csv[[15]] <- tickets_r4_ci
csv[[16]] <- blockbusters_only_ci
csv[[17]] <- no_blockbusters_ci

# Exportar datos ====
export_csv(1, 17)