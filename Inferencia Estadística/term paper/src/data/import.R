# Título    : import.R
# Autor(es) : carlos, paulina

source("src/features/functions.R")

# Importar datos ====
blockbusters <- read_excel("data/raw/data.xlsx",
    sheet = "Blockbusters"
)

food_and_drinks_sales <- read_excel("data/raw/data.xlsx",
    sheet = "Food and drinks sales"
)

ticket_office <- read_excel("data/raw/data.xlsx",
    sheet = "Ticket office"
)