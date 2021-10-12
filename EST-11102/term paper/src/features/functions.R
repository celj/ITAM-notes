# Título    : functions.R
# Autor(es) : carlos, paulina

source("src/features/packages.R")

# Coeficiente de variación ----
cv <- function(x) {
  sd(x) / mean(x)
}

# Estadísticos básicos ----
basic_stat_food_f <- function(data) {
  df <- data %>%
    summarise(
      sales_mean = mean(sales),
      sales_median = median(sales),
      sales_q1 = quantile(sales, 0.25),
      sales_q3 = quantile(sales, 0.75),
      sales_sd = sd(sales),
      sales_cv = cv(sales),
      price_mean = mean(price),
      price_median = median(price),
      price_q1 = quantile(price, 0.25),
      price_q3 = quantile(price, 0.75),
      price_sd = sd(price),
      price_cv = cv(price)
    )
}

basic_stat_tickets_f <- function(data) {
  df <- data %>%
    summarise(
      mean = mean(tickets),
      median = median(tickets),
      q1 = quantile(tickets, 0.25),
      q3 = quantile(tickets, 0.75),
      sd = sd(tickets),
      cv = cv(tickets)
    )
}

# Normalizar ----
normalize <- function(dataframe, parameter) {
  r <- ((sqrt(nrow(dataframe))) * (mean(parameter) - 45000000)) / sd(parameter)
  return(r)
}

# Intervalos de confianza
confidence_interval <- function(mean, sd, sample) {
  Ln <- c(
    mean - (abs(qnorm(0.1 / 2, 0, 1)) * (sd / sqrt(sample))),
    mean - (abs(qnorm(0.05 / 2, 0, 1)) * (sd / sqrt(sample))),
    mean - (abs(qnorm(0.01 / 2, 0, 1)) * (sd / sqrt(sample)))
  )
  Un <- c(
    mean + (abs(qnorm(0.1 / 2, 0, 1)) * (sd / sqrt(sample))),
    mean + (abs(qnorm(0.05 / 2, 0, 1)) * (sd / sqrt(sample))),
    mean + (abs(qnorm(0.01 / 2, 0, 1)) * (sd / sqrt(sample)))
  )
  confidence_level <- c("90%", "95%", "99%")
  length <- Un - Ln
  df <- data.frame(confidence_level, Ln, Un, length)
  return(df)
}

# Ancho de caja para histogramas por regla Freedman–Diaconis ----
food_binwidth <- function(x) {
  f_d <- 2 * IQR(x$price) * (nrow(x) ^ (-1 / 3))
  return(f_d)
}

tickets_binwidth <- function(x) {
  f_d <- 2 * IQR(x$tickets) * (nrow(x) ^ (-1 / 3))
  return(f_d)
}

# Histogramas ====
food_hist <- function(x) {
  a <- x[[1]]
  b <- x[[2]]
  c <- x[[3]]
  d <- x[[4]]
  plot <- a %>%
    ggplot(aes(price)) +
    geom_histogram(
      binwidth = b,
      fill = c,
      color = "#000000",
      alpha = 0.6
    ) +
    labs(
      title = d,
      x = "Precio",
      y = "Frecuencia"
    ) +
    scale_x_continuous(
      label = dollar_format(),
      breaks = seq(0, 180, 20)
    ) +
    scale_y_continuous(breaks = seq(0, 300, 25)) +
    theme_minimal() +
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
}

tickets_hist <- function(x) {
  a <- x[[1]]
  b <- x[[2]]
  c <- x[[3]]
  d <- x[[4]]
  plot <- a %>%
    ggplot(aes(tickets)) +
    geom_histogram(
      binwidth = b,
      fill = c,
      color = "#000000",
      alpha = 0.6
    ) +
    labs(
      title = d,
      x = "Boletos vendidos",
      y = "Frecuencia"
    ) +
    scale_x_continuous(label = comma) +
    coord_flip() +
    theme_minimal() +
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
}

# Diagramas de caja y brazos ====
region_boxplot <- function(x) {
  a <- x[[1]]
  b <- x[[2]]
  c <- x[[3]]
  d <- x[[4]]
  plot <- a %>%
    ggplot(aes(
      x = as.factor(region),
      y = b
    )) +
    geom_boxplot(
      alpha = 0.6,
      fill = colours[2:5],
      outlier.size = 0.5,
      outlier.alpha = 0.2
    ) +
    labs(title = c) +
    scale_x_discrete(labels = regions) +
    scale_y_continuous(label = d) +
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
}

total_boxplot <- function(x) {
  a <- x[[1]]
  b <- x[[2]]
  c <- x[[3]]
  d <- x[[4]]
  plot <- a %>%
    ggplot(aes(
      x = "",
      y = b
    )) +
    geom_boxplot(
      alpha = 0.6,
      fill = colours[1],
      outlier.size = 0.5,
      outlier.alpha = 0.5
    ) +
    geom_jitter(
      color = "black",
      alpha = 0.2,
      size = 0.2
    ) +
    labs(title = c) +
    scale_x_discrete(labels = "Total") +
    scale_y_continuous(label = d) +
    coord_flip() +
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
}

# Series de tiempo ====
time_series <- function(w, x, y, z) {
  a <- w[[1]]
  b <- w[[2]]
  c <- w[[3]]
  d <- w[[4]]
  e <- w[[5]]
  f <- w[[6]]
  g <- w[[7]]
  h <- w[[8]]
  i <- w[[9]]
  j <- w[[10]]
  plot <- ggplot() +
    geom_line(
      data = a, aes(
        x = as.Date(week),
        y = b,
        color = "Nueva Inglaterra"
      ),
      alpha = 0.8
    ) +
    geom_line(
      data = c, aes(
        x = as.Date(week),
        y = d,
        color = "Atlántico Central"
      ),
      alpha = 0.8
    ) +
    geom_line(
      data = e, aes(
        x = as.Date(week),
        y = f,
        color = "Sureste"
      ),
      alpha = 0.8
    ) +
    geom_line(
      data = g, aes(
        x = as.Date(week),
        y = h,
        color = "Florida"
      ),
      alpha = 0.8
    ) +
    labs(
      title = x,
      color = "Zona"
    ) +
    scale_x_date(
      date_labels = "%b/%Y",
      date_breaks = "months",
      limits = as.Date(c(y, z))
    ) +
    scale_y_continuous(
      label = i,
      breaks = j
    ) +
    scale_color_manual(values = c(
      "Nueva Inglaterra" = colours[[2]],
      "Atlántico Central" = colours[[3]],
      "Sureste" = colours[[4]],
      "Florida" = colours[[5]]
    )) +
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
}

# Gráficos de dispersión ====
scatter_plot <- function(a, b, c, d, e, f) {
  plot <- a %>%
    ggplot(aes(
      x = tickets,
      y = b
    )) +
    geom_point(alpha = 0.6) +
    geom_smooth(
      method = loess,
      color = colours[[2]],
      fill = colours[[5]],
      se = TRUE
    ) +
    labs(
      title = c,
      x = "Boletos vendidos",
      y = d
    ) +
    scale_x_continuous(
      label = comma,
      breaks = seq(0, 2000000, 250000)
    ) +
    scale_y_continuous(
      label = dollar_format(),
      breaks = seq(0, e, f)
    ) +
    theme_minimal() +
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
}

# Exportar gráficas ====
export_svg <- function(a, b, c, d) {
  outdir <- "../term paper/out/figures/"
  for (i in a:b) {
    outfile <- sprintf("fig_%02d.svg", i)
    ggsave(
      filename = file.path(outdir, outfile),
      width = c,
      height = d,
      dpi = 500,
      plots[[i]]
    )
  }
}

# export_csv <- function(a, b) {
#   for (i in a:b) {
#     outfile <- sprintf("../term paper/data/processed/data_%02d.csv", i)
#     write.csv(csv[[i]], outfile)
#   }
# }