library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(purrr)

install.packages("httpgd")
library(httpgd)
httpgd::hgd()



# === Deine korrekten VIX-Futures-Verfallstermine ===
expiration_dates <- as.Date(c(
  "2006-01-18", "2006-02-15", "2006-03-22", "2006-04-19", "2006-05-17", "2006-06-21",
  "2006-07-19", "2006-08-16", "2006-09-20", "2006-10-18", "2006-11-15", "2006-12-20",
  "2007-01-17", "2007-02-14", "2007-03-21", "2007-04-18", "2007-05-16", "2007-06-20",
  "2007-07-18", "2007-08-22", "2007-09-19", "2007-10-17", "2007-11-21", "2007-12-19",
  "2008-01-16", "2008-02-19", "2008-03-19", "2008-04-16", "2008-05-21", "2008-06-18",
  "2008-07-16", "2008-08-20", "2008-09-17", "2008-10-22", "2008-11-19", "2008-12-17",
  "2009-01-21", "2009-02-18", "2009-03-18", "2009-04-15", "2009-05-20", "2009-06-17",
  "2009-07-22", "2009-08-19", "2009-09-16", "2009-10-21", "2009-11-18", "2009-12-16",
  "2010-01-20", "2010-02-17", "2010-03-17", "2010-04-21", "2010-05-19", "2010-06-16",
  "2010-07-21", "2010-08-18", "2010-09-15", "2010-10-20", "2010-11-17", "2010-12-22",
  "2011-01-19", "2011-02-16", "2011-03-16", "2011-04-20", "2011-05-18", "2011-06-15",
  "2011-07-20", "2011-08-17", "2011-09-21", "2011-10-19", "2011-11-16", "2011-12-21",
  "2012-01-18", "2012-02-15", "2012-03-21", "2012-04-18", "2012-05-16", "2012-06-20",
  "2012-07-18", "2012-08-22", "2012-09-19", "2012-10-17", "2012-11-21", "2012-12-19",
  "2013-01-16", "2013-02-13", "2013-03-20", "2013-04-17", "2013-05-22", "2013-06-19",
  "2013-07-17", "2013-08-21", "2013-09-18", "2013-10-16", "2013-11-20", "2013-12-18",
  "2014-01-22", "2014-02-19", "2014-03-18", "2014-04-16", "2014-05-21", "2014-06-18",
  "2014-07-16", "2014-08-20", "2014-09-17", "2014-10-22", "2014-11-19", "2014-12-17",
  "2015-01-21", "2015-02-18", "2015-03-18", "2015-04-15", "2015-05-20", "2015-06-17",
  "2015-07-22", "2015-08-19", "2015-09-16", "2015-10-21", "2015-11-18", "2015-12-16",
  "2016-01-20", "2016-02-17", "2016-03-16", "2016-04-20", "2016-05-18", "2016-06-15",
  "2016-07-20", "2016-08-17", "2016-09-21", "2016-10-19", "2016-11-16", "2016-12-21",
  "2017-01-18", "2017-02-15", "2017-03-22", "2017-04-19", "2017-05-17", "2017-06-21",
  "2017-07-19", "2017-08-16", "2017-09-20", "2017-10-18", "2017-11-15", "2017-12-20",
  "2018-01-17", "2018-02-14", "2018-03-21", "2018-04-18", "2018-05-16", "2018-06-20",
  "2018-07-18", "2018-08-22", "2018-09-19", "2018-10-17", "2018-11-21", "2018-12-19",
  "2019-01-16", "2019-02-13", "2019-03-19", "2019-04-17", "2019-05-22", "2019-06-19",
  "2019-07-17", "2019-08-21", "2019-09-18", "2019-10-16", "2019-11-20", "2019-12-18",
  "2020-01-22", "2020-02-19", "2020-03-18", "2020-04-15", "2020-05-20", "2020-06-17",
  "2020-07-22", "2020-08-19", "2020-09-16", "2020-10-21", "2020-11-18", "2020-12-16",
  "2021-01-20", "2021-02-17", "2021-03-17", "2021-04-21", "2021-05-19", "2021-06-16",
  "2021-07-21", "2021-08-18", "2021-09-15", "2021-10-20", "2021-11-17", "2021-12-22",
  "2022-01-19", "2022-02-16", "2022-03-15", "2022-04-20", "2022-05-18", "2022-06-15",
  "2022-07-20", "2022-08-17", "2022-09-21", "2022-10-19", "2022-11-16", "2022-12-21",
  "2023-01-18", "2023-02-15", "2023-03-22", "2023-04-19", "2023-05-17", "2023-06-21",
  "2023-07-19", "2023-08-16", "2023-09-20", "2023-10-18", "2023-11-15", "2023-12-20",
  "2024-01-17", "2024-02-14", "2024-03-20", "2024-04-17", "2024-05-22", "2024-06-18",
  "2024-07-17", "2024-08-21", "2024-09-18", "2024-10-16", "2024-11-20", "2024-12-18",
  "2025-01-22", "2025-02-19", "2025-03-18", "2025-04-16", "2025-05-21", "2025-06-18",
  "2025-07-16", "2025-08-20", "2025-09-17", "2025-10-22", "2025-11-19", "2025-12-17",
  "2026-01-21", "2026-02-18", "2026-03-18", "2026-04-15", "2026-05-19", "2026-06-17",
  "2026-07-22", "2026-08-19", "2026-09-16", "2026-10-21", "2026-11-18", "2026-12-16"
))


# Zuordnungstabelle für Kontraktjahr+monat → korrekter Verfall
expiration_df <- tibble(
  year = year(expiration_dates),
  month = month(expiration_dates),
  settlement_date = expiration_dates
)

# Konvertiert Monatscode (F, G, ...) zu numerischem Monat
month_code_to_num <- function(code) {
  month_codes <- c("F"=1, "G"=2, "H"=3, "J"=4, "K"=5, "M"=6,
                   "N"=7, "Q"=8, "U"=9, "V"=10, "X"=11, "Z"=12)
  return(month_codes[[code]])
}

# Datumsparser: robust
parse_date_robust <- function(x) {
  possible_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%b %d %Y", "%Y/%m/%d")
  parsed <- suppressWarnings(parse_date_time(x, orders = possible_formats))
  as.Date(parsed)
}

# Ladefunktion für einzelne CSV-Datei
load_vix_csv <- function(file) {
  tryCatch({
    filename <- basename(file)

    is_old_format <- str_detect(filename, "^CFE_")

    # Kontraktinformationen aus Dateinamen extrahieren
    if (is_old_format) {
      parts <- str_match(filename, "CFE_([A-Z])([0-9]{2})_VX\\.csv")[, 2:3]
      month_code <- parts[1]
      year_suffix <- as.integer(parts[2])
      year <- ifelse(year_suffix > 90, 1900 + year_suffix, 2000 + year_suffix)
      month <- month_code_to_num(month_code)
    } else {
      contract_date <- ymd(str_match(filename, "VX_([0-9]{4}-[0-9]{2}-[0-9]{2})")[,2])
      month <- month(contract_date)
      year <- year(contract_date)
    }

    df <- read_csv(file, show_col_types = FALSE)

    date_col <- names(df)[str_detect(names(df), "Date|TradeDate|date")][1]
    price_col <- names(df)[str_detect(names(df), "Settle|Settlement|Price|Close")][1]

    if (is.null(date_col) || is.null(price_col)) stop("Datum oder Preis nicht gefunden.")

    df <- df %>%
      rename(date_raw = !!sym(date_col), price_raw = !!sym(price_col)) %>%
      mutate(
        date = parse_date_robust(date_raw),
        price_raw = as.numeric(price_raw),
        price = if (is_old_format) if_else(price_raw > 100, price_raw / 10, price_raw) else price_raw,
        year = year,
        month = month
      ) %>%
      left_join(expiration_df, by = c("year", "month")) %>%
      mutate(
        days_to_settlement = as.numeric(settlement_date - date),
        contract = sprintf("%04d-%02d", year, month),
        source_file = filename
      ) %>%
      select(date, contract, price, days_to_settlement, settlement_date, source_file) %>%
      filter(!is.na(date), !is.na(price), !is.na(settlement_date))

    return(df)

  }, error = function(e) {
    message("❌ Fehler in Datei: ", file, "\n", e$message)
    return(NULL)
  })
}

# Alle Dateien laden
folder <- "data/vix_futures"
files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
vix_futures_all <- map_dfr(files, load_vix_csv)

# Optionale Filterung
vix_futures_clean <- vix_futures_all %>%
  filter(price > 0)

# Neu berechnen:
vix1_series <- vix_futures_clean %>%
  group_by(date) %>%
  arrange(days_to_settlement) %>%
  filter(n() >= 2) %>%                         # mindestens zwei Kontrakte
  slice_head(n = 2) %>%
  summarise(
    t1 = first(days_to_settlement),
    t2 = last(days_to_settlement),
    F1 = first(price),
    F2 = last(price),
    w = (t2 - 30) / (t2 - t1),
    vix1 = w * F1 + (1 - w) * F2,
    .groups = "drop"
  ) %>%
  filter(!is.na(vix1), t1 < t2, between(t1, 10, 60), between(t2, 15, 90)) %>%
  distinct(date, .keep_all = TRUE)




library(ggplot2)

ggplot(vix1_series, aes(x = date, y = vix1)) +
  geom_line(color = "darkblue") +
  labs(title = "VIX1 (30-Tage gewichtete VIX-Futures)", y = "VIX1", x = NULL) +
  theme_minimal()


library(quantmod)

getSymbols("^VIX", from = "2006-01-01")
vix_spot <- Cl(VIX) %>%
  fortify.zoo() %>%
  rename(date = Index, vix_spot = VIX.Close)
vix1_vs_spot <- vix1_series %>%
  left_join(vix_spot, by = "date")

ggplot(vix1_vs_spot, aes(x = date)) +
  geom_line(aes(y = vix_spot, color = "VIX (Spot)"), size= 0.8) +
  geom_line(aes(y = vix1, color = "VIX1 (Futures)"), size= 0.8) +
  labs(title = "VIX1 vs VIX Spot", y = "Vola", color = NULL) +
  theme_minimal()

#--------------------------------------------------------------------

library(quantmod)

getSymbols("^GSPC", from = "2006-01-01")

sp500 <- Cl(GSPC) %>%
  fortify.zoo() %>%
  rename(date = Index, close = GSPC.Close)


library(zoo)
library(dplyr)

sp500 <- sp500 %>%
  arrange(date) %>%
  mutate(return = log(close / lag(close)))

sp500 <- sp500 %>%
  mutate(
    hvol10 = 100 * sqrt(252) * rollapply(return, width = 10, FUN = sd, fill = NA, align = "right"),
    hvol10s = rollmean(hvol10, k = 10, fill = NA, align = "right")
  )


library(ggplot2)

ggplot(sp500, aes(x = date)) +
  geom_line(aes(y = hvol10, color = "HVOL10")) +
  geom_line(aes(y = hvol10s, color = "HVOL10S")) +
  labs(title = "HVOL10 und HVOL10S – S&P 500", y = "Volatilität", color = "") +
  theme_minimal()

vrp_data <- vix1_series %>%
  left_join(sp500 %>% select(date, hvol10, hvol10s), by = "date") %>%
  mutate(vrp = vix1 - hvol10s)

ggplot(vrp_data, aes(x = date)) +
  geom_line(aes(y = vix1, color = "Implied Volatility (VIX1)")) +
  geom_line(aes(y = hvol10s, color = "Realized Volatility (HVOL10S)")) +
  labs(title = "VIX1 vs. Realized Volatility", y = "Volatility", color = NULL) +
  theme_minimal()

#-------------------------------

etfs <- c("VIXY", "VIXM", "SVXY")

getSymbols(etfs, from = "2011-01-01")

etf_prices <- bind_rows(
  lapply(etfs, function(ticker) {
    Cl(get(ticker)) %>%
      fortify.zoo() %>%
      rename(date = Index, price = 2) %>%
      mutate(ticker = ticker)
  })
)


library(dplyr)
library(quantmod)
library(ggplot2)

# Berechne tägliche Renditen je ETF
etf_returns <- etf_prices %>%
  group_by(ticker) %>%
  arrange(date) %>%
  mutate(ret = price / lag(price) - 1) %>%
  ungroup()


# Nur nötige Spalten aus vrp_data
vrp_signal <- vrp_data %>%
  select(date, vrp) %>%
  filter(!is.na(vrp)) %>%
  mutate(signal = if_else(vrp > 0, "SVXY", "VIXY"))

# Füge Signal pro Tag zu ETF-Renditen hinzu
strategy_data <- etf_returns %>%
  inner_join(vrp_signal, by = "date") %>%
  filter(ticker == signal)  # nur den jeweils aktiven ETF pro Tag behalten

strategy_returns <- strategy_data %>%
  arrange(date) %>%
  mutate(strat_return = replace_na(ret, 0),
         strat_value = cumprod(1 + strat_return))  # Startwert = 1

getSymbols("SPY", from = "2011-01-01")
spy <- Cl(SPY) %>%
  fortify.zoo() %>%
  rename(date = Index, price = SPY.Close) %>%
  arrange(date) %>%
  mutate(spy_return = price / lag(price) - 1,
         spy_value = cumprod(1 + replace_na(spy_return, 0)))

ggplot() +
  geom_line(data = strategy_returns, aes(x = date, y = strat_value, color = "VRP Strategy")) +
  geom_line(data = spy, aes(x = date, y = spy_value, color = "SPY Benchmark")) +
  labs(title = "VRP-Strategie (SVXY/VIXY) vs. SPY",
       y = "Wertentwicklung (Start = 1)", x = NULL, color = NULL) +
  theme_minimal()



  View(strategy_returns)

  # ETF-Renditen
  etf_returns <- etf_prices %>%
    group_by(ticker) %>%
    arrange(date) %>%
    mutate(ret = price / lag(price) - 1) %>%
    ungroup()

  # Strategie nur wenn Signal = Ticker
  strategy_data <- etf_returns %>%
    inner_join(vrp_signal, by = "date") %>%
    filter(ticker == signal)

  strategy_returns <- strategy_data %>%
    arrange(date) %>%
    mutate(strat_return = replace_na(ret, 0),
           strat_value = cumprod(1 + strat_return))
