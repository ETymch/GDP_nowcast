## Заголовок ====================================================================
##
## Название: Квартальный DFM в MARSS
##
## Проект: Наукаст ВВП
##
## Автор: Евгений Тымченко
##
## Почта: tymchenko.e@hse.ru
##
## Дата: 2026-09-11
##
## Примечания: 
## Наукаст ВВП на основе:
## а) численности рабочей силы
## б) объёмов розничной торговли
## в) индекса МБ
## г) Индикатора бизнес климата Банка России
## д) Индекса промышленного производства
##
# Библиотеки ===================================================================

library(tidyverse)
library(MARSS)
library(arrow)

# Данные =======================================================================

df <- read_parquet("data_input/data_q.parquet") %>%
  arrange(date)

d_tbl <- df %>%
  mutate(
    across(
      c(gdp, wf, retail, imoex, ipp),
      ~ .x - lag(.x)
      )
    )

gdp <- read_parquet('data_input/gdp_raw.parquet') %>%
  #filter(date >= (d_tbl$date %>% min)) %>%
  mutate(
    value_sa = seas_adj(value, freq = 4)
  )

# Для восстановления после стандартизации созраняем mu и sigma

mu <- d_tbl %>% summarise(across(-date, ~ mean(.x, na.rm = TRUE)))
sg <- d_tbl %>% summarise(across(-date, ~ sd(.x, na.rm = TRUE)))

# Финальная подготовка данных

y <- d_tbl %>%
  mutate(
    across(
      -date,
      ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
      )
    ) %>%
  select(-date) %>%
  as.matrix() %>%
  t()

# Модель =======================================================================

marss_fk <- function(y, k = 1, maxit = 1000) {

  m <- nrow(y)
  Z <- matrix(
    as.list(
      outer(seq_len(m),
            seq_len(k),
            function(i, j) paste0("z", i, j))),
    m, k
  )
  
  if (k >= 2) {
    for (i in 2:k) {
      for (j in 1:(i - 1)) Z[[i, j]] <- 0
    }
  }

  MARSS(
    y,
    model = list(
      Z  = Z,
      A  = "zero",
      B  = "diagonal and unequal",
      U  = "zero",
      Q  = "identity",
      R  = "diagonal and unequal",
      x0 = "zero",
      V0 = "identity"
    ),
    silent = TRUE,
    control = list(maxit = maxit)
  )
}

# Оценка =======================================================================

mod_1 <- marss_fk(y, k = 1)
mod_k <- marss_fk(y, k = 2)

mod_1$AIC # модель с 1 фактором
mod_k$AIC # модель с 2 факторами

# модель с 2 факторами лучше по AIC, но незначитально и в тестах однофакторная себя лучше показывает,
# так что дальше это бадовая модель

# определяем h 

h <- 4 - (df %>% pull(date) %>% max) %>% quarter()

# Прогноз до конца года

fc_1 <- forecast(mod_1, h = h)

fc_1_pred <- 
  fc_1$pred %>% 
  filter(.rownames == 'gdp',
         t > nrow(df)) %>%
  pull(estimate)

# Наукаст

p_nc <-
  d_tbl %>%
  tail() %>%
  pull(gdp) %>%
  is.na() %>%
  sum()

nc_1_pred <- tail(t(mod_1$ytT), p_nc)[1]

# Общее

fc_fin <- c(nc_1_pred, fc_1_pred) * sg$gdp + mu$gdp

nc_df <- 
  tibble(
  date = seq(
    gdp$date %>% last %m+% months(3),
    gdp$date %>% last %m+% months(3 * length(fc_fin)),
    by = 'quarter'
    ),
  value_sa = gdp$value_sa %>% last() * cumprod(1 + fc_fin),
  type = 'nowcast'
)

out <- 
  gdp %>%
  select(date,
         value,
         value_sa
         ) %>%
  mutate(type = 'fact') %>%
  bind_rows(nc_df) %>%
  arrange(date) %>%
  mutate(
    value_gr_sa_yoy = value_sa / lag(value_sa, 4),
    value = if_else(
      date >= min(nc_df$date),
      value_gr_sa_yoy * lag(value, 4),
      value)
  ) %>%
  mutate(
    qoq = 100 * (value_sa - lag(value_sa, 1)) / lag(value_sa, 1),
    yoy = 100 * (value - lag(value, 4)) / lag(value, 4)
  ) %>%
  relocate(
    type, .after = date
  ) %>%
  select(-value_gr_sa_yoy)

out_y <-
  out %>%
  mutate(year = floor_date(date, 'year')) %>%
  reframe(value = sum(value, na.rm = T),
          .by = year
          ) %>%
  mutate(
    pchange = 100 * (value / lag(value, 1) - 1)
    )

list(out, out_y) %>%
  writexl::write_xlsx('data_out/gdp_nowcast.xlsx')
