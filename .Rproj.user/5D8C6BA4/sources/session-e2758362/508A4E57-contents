# Для удобной выгрузки с Rustata.

rst_get <- function(
    indicator,
    class = class,
    geo = 'country',
    start = '1990-01',
    labels = 0
){
  # Windows: если консоль печатает кириллицу как <U+0411><U+0435>…, дело в локали сессии,
  # а не в данных - сами строки целы. Лечится одной строкой:
  if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "Russian_Russia.utf8")
  
  token <- "rst_gO2NyfKZ-C_97Dhpkp6w6GkAyTpACWtk6GkQUcK8t9I"
  resp <- GET(
    "https://rustata.ru/api/v1/data",
    query = list(dataset = indicator, class = class,
                 geo = geo, labels = labels, start = start, format = "csv"),
    add_headers(Authorization = paste("Bearer", token))
  )
  stop_for_status(resp)
  
  # content(resp, "raw") + locale(): readr сам разбирает UTF-8, без промежуточной строки
  df <- read_delim(
    content(resp, "raw"),
    delim = ";",
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE) %>%
    #select(geo_label, period, value) %>%
    arrange(geo_label, period)
}

# История индекса с ISS Мосбиржи.
# CSV: имя блока + пустая строка, разделитель ";", charset windows-1251.
# По умолчанию страницы по 100 строк; полный объём — в блоке history.cursor.
download_index <- function(index_code = "IMOEX") {
  
  url <- paste0(
    "https://iss.moex.com/iss/history/engines/stock/markets/index/",
    "boards/SNDX/securities/",
    index_code,
    ".csv"
  )
  
  iss_csv <- function(block, start = 0) {
    GET(
      url,
      query = list(start = start, `iss.only` = block),
      timeout(60)
    ) %>%
      stop_for_status() %>%
      content(as = "raw") %>%
      read_delim(
        delim = ";",
        skip = 2,
        locale = locale(encoding = "Windows-1251"),
        show_col_types = FALSE
      )
  }
  
  cursor <- iss_csv("history.cursor")
  
  seq(0, cursor$TOTAL - 1, by = cursor$PAGESIZE) %>%
    map(~ iss_csv("history", start = .x)) %>%
    bind_rows() %>%
    mutate(TRADEDATE = as.Date(TRADEDATE)) %>%
    arrange(TRADEDATE)
}

# ИБК Банка России из mp_survey_data.xlsx (мониторинг предприятий).
# Два столбца с одним названием — исходный ряд (nsa) и сезонно скорректированный (sa).
# ibk_current / ibk_expected — субкомпоненты (факт и ожидания на 3 месяца).
download_ibk <- function(
    url = "https://www.cbr.ru/Content/Document/File/135603/mp_survey_data.xlsx"
) {
  
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  
  GET(
    url,
    user_agent("Mozilla/5.0"),
    write_disk(tmp, overwrite = TRUE),
    timeout(120)
  ) %>%
    stop_for_status()
  
  magic <- readBin(tmp, what = "raw", n = 2)
  if (!identical(magic, charToRaw("PK"))) {
    stop("CBR вернул не xlsx (возможно, защита DDoS-Guard).")
  }
  
  raw <- suppressMessages(
    readxl::read_excel(tmp, sheet = "Экономика всего", col_names = FALSE)
  )
  
  header_row <- which(raw[[1]] == "Отчетный период")[1]
  if (is.na(header_row) || header_row < 2) {
    stop("Не нашёл строку заголовков на листе «Экономика всего».")
  }
  
  dates <- as.Date(
    suppressWarnings(as.numeric(raw[[1]])),
    origin = "1899-12-30"
  )
  
  tibble(
    col   = seq_len(ncol(raw)),
    group = unlist(raw[header_row - 1, ], use.names = FALSE) %>% as.character(),
    name  = unlist(raw[header_row, ], use.names = FALSE) %>% as.character()
  ) %>%
    fill(group) %>%
    filter(str_detect(name, "Индикатор бизнес-климата")) %>%
    mutate(
      date  = list(dates),
      value = map(col, ~ suppressWarnings(as.numeric(raw[[.x]])))
    ) %>%
    unnest(c(date, value)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    mutate(
      series = case_when(
        str_detect(name, "факт") ~ "ibk_current",
        str_detect(name, "ожид") ~ "ibk_expected",
        TRUE ~ "ibk"
      ),
      seas = if_else(str_detect(group, regex("сезонно", ignore_case = TRUE)),
                     "sa", "nsa")
    ) %>%
    select(date, series, seas, value) %>%
    arrange(series, seas, date)
}

# Проверка на полноту

check_continuity <- function(df, freq = 'month'){
  
  tibble_per <- tibble(
    period =  seq(min(df$period), max(df$period), by = freq)
  )
  
  df %>%
    right_join(tibble_per) %>%
    filter(if_any(everything(), is.na))
}

# Функция для сезонной корректировки временных рядов

seas_adj <- function(x, freq){
  library(seasonal)
  
  # Работает корректно только с NA на конце или в начале.
  
  na_ids <- x %>% is.na()
  x_nona <- x[!na_ids] %>%
    ts(frequency = freq, start = c(2010, 1)) # с данным методом не принципиально, как выбирать начальный период
  
  x_seas <- 
    x_nona %>%
    seas(x11 = '',
         #transform.function = "log") %>%
         #regression.architect = #'td',
         force.type = 'regress') %>%
    final() %>%
    c() %>%
    as.numeric()
  
  x_seas_na <- rep(NA_real_, length(x))
  x_seas_na[!na_ids] <- x_seas
  x_seas_na
}
