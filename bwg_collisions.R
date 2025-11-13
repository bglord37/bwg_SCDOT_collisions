
library(DBI)
library(RPostgres)
library(ggplot2)
library(dplyr)
library(lubridate)

home_dir <- 'C:/Repos/bwg_SCDOT_collisions/'
out_dir <- './Out/'

setwd(home_dir)

# ---- 1️⃣ Connect to your PostgreSQL database ----
con <- dbConnect(
  RPostgres::Postgres(),
  host     = "mrsm.io",   # e.g. "localhost" or "db.yourdomain.com"
  port     = 5433,                     # default Postgres port
  dbname   = "bwg",
  user     = "bwg",
  password = "replica-2024"
)
# ---- 2️⃣ Example query: all Greenville rows ----
query <- r"(SELECT * FROM "SCDPS"."LOCATION" WHERE county = 'Greenville' and first_harmful_event in('Pedestrian','Pedalcycle');)"

df <- dbGetQuery(con, query)

# ---- 4️⃣ Always disconnect when done ----
dbDisconnect(con)



#Sort top 20 streets
top_streets <- df %>%
  count(base_route_street_name, sort = TRUE) %>%
  filter(!is.na(base_route_street_name)) %>%
  head(20)
  
  
  write.csv(top_streets, paste0(out_dir,'Top20Streets.csv'),
            quote = FALSE,
            row.names = FALSE)
#


# Colors
night_color <- "#ECECEC"     # light cool gray
day_color   <- "#FFF9C4"     # pale warm yellow
bar_color   <- "#1565C0"     # deep contrasting blue


# ---- 1️⃣ Clean and prepare data ----
df_clean <- df %>%
  mutate(
    # Convert time like "1830" → 18.5 hours
    hour = as.numeric(substr(time, 1, 2)) + as.numeric(substr(time, 3, 4)) / 60,
    # Make month a properly ordered factor
    month = factor(month, 
                   levels = as.character(1:12), 
                   labels = month.name, 
                   ordered = TRUE),
    year = as.numeric(year)
  ) %>%
  filter(!is.na(hour), !is.na(month))

# ---- 2️⃣ Approximate daylight hours per month (Greenville, SC) ----
daylight_hours <- data.frame(
  month = factor(month.name, levels = month.name, ordered = TRUE),
  sunrise = c(7.3, 7.0, 6.5, 6.1, 6.0, 6.0, 6.3, 6.7, 7.1, 7.3, 6.9, 7.2),
  sunset  = c(17.4, 18.0, 18.8, 19.6, 20.3, 20.8, 20.6, 20.0, 19.1, 18.2, 17.3, 17.1)
)

# ---- 3️⃣ Merge daylight data ----
df_plot <- df_clean %>%
  left_join(daylight_hours, by = "month")

# ---- 4️⃣ Plot ----
ggplot(df_plot, aes(x = hour)) +
  # Nighttime (midnight → sunrise)
  geom_rect(aes(xmin = 0, xmax = sunrise, ymin = -Inf, ymax = Inf),
            fill = "gray15", alpha = 0.6) +
  # Daytime (sunrise → sunset)
  geom_rect(aes(xmin = sunrise, xmax = sunset, ymin = -Inf, ymax = Inf),
            fill = "lightgoldenrod1", alpha = 0.6) +
  # Nighttime (sunset → midnight)
  geom_rect(aes(xmin = sunset, xmax = 24, ymin = -Inf, ymax = Inf),
            fill = "gray15", alpha = 0.6) +
  # Histogram of crash counts
  geom_histogram(binwidth = 1, fill = "#2E86AB", color = "black") +
  facet_wrap(~month, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 24, by = 6), limits = c(0, 24)) +
  labs(
    title = "Crashes by Time of Day (Shaded by Day/Night Cycle)",
    x = "Hour of Day",
    y = "Crash Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

