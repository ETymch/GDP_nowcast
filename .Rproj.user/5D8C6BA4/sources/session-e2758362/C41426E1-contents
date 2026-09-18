## Заголовок ====================================================================
##
## Название: Основной файл проекта
##
## Проект: Наукаст ВВП
##
## Автор: Евгений Тымченко
##
## Почта: tymchenko.e@hse.ru
##
## Дата: 2026-09-06
##
## Примечания: 
##

# Библиотеки ===================================================================

library(httr)
library(readr)
library(arrow)
library(tidyverse)
library(lubridate)

source('R/functions/functions_api.R')

# Начало =======================================================================

# Загрузка данных ==============================================================

# ВВП

gdp <- rst_get(
  indicator = 'rosstat_files/gdp',
  class = 'CONST2021'
) %>%
  filter(freq == 'Q') %>%
  select(series_key, date = period, value) %>%
  mutate(series = 'gdp')

# Среднесписочная численность работников

wf <- rst_get(
  indicator = 'fedstat/indicator_57848',
  class = '101.АГ'
) %>%
  select(series_key, date = period, value) %>%
  mutate(series = 'wf')

# ИПЦ (для дефлирования)

cpi <- 
  rst_get(
    indicator = 'fedstat/indicator_31074',
    class = '1'
  ) %>%
  filter(measure == 'MOM',
         period >= ymd('2014-06-01')) %>%
  select(series_key, date = period, value) %>%
  arrange(date) %>%
  mutate(series = 'cpi',
         value = cumprod(value / 100)
        )

# Оборот розничной торговли (в текущих ценах!)

retail <- rst_get(
  indicator = 'fedstat/indicator_31260',
  class = '1'
  ) %>%
  select(series_key, date = period, value) %>%
  mutate(series = 'retail') %>%
  bind_rows(cpi) %>%
  select(-series_key) %>%
  pivot_wider(names_from = series,
              values_from = value) %>%
  mutate(retail = retail / cpi) %>%
  select(date, value = retail) %>%
  mutate(series = 'retail')

# Индекс промпроизводства, м/м.

ipp <- rst_get(
  indicator = 'fedstat/indicator_57806',
  class = '1323500.029.31',
  labels = 1
) %>%
  filter(measure == 'MOM') %>%
  select(series_key, date = period, value) %>%
  arrange(date) %>%
  mutate(value = cumprod(value / 100) / (first(value)/100)
          ) %>%
  mutate(series = 'ipp')

# Индекс Мосбиржи

imoex <- download_index() %>%
  select(date = TRADEDATE, value = CLOSE) %>%
  mutate(date = floor_date(date, unit = 'month')) %>%
  reframe(
    value = mean(value, na.rm = T),
    .by = 'date'
  ) %>%
  mutate(series = 'imoex')

# Индекс бизнес климата Банка России

ibk <- download_ibk() %>%
  filter(seas == 'sa',
         series == 'ibk') %>%
  mutate(date = date %m+% months(1)
         ) %>%
  select(-seas)

# Объединение данных

df <- bind_rows(
  gdp, wf, retail, imoex, ibk, ipp
) %>%
  select(-series_key) %>%
  pivot_wider(names_from = 'series',
              values_from = 'value'
              ) %>%
  filter(date >= ymd('2015-01-01')) %>%
  arrange(date)

df %>%
  mutate(gdp_yoy = gdp / lag(gdp, 12)) %>%
  tail() %>%
  pull(gdp_yoy)
  
df_q <-
  df %>%
  mutate(date = floor_date(date, unit = 'quarter')) %>%
  reframe(
    across(
      everything(), 
      ~mean(., na.rm = T)
      ),
    .by = date
  ) %>%
  arrange(date) %>%
  mutate(
    across(
      -c(date, ibk),
      ~ seas_adj(.x, freq = 4)
    )
  ) %>%
  mutate(
    across(
      c(gdp, wf, retail, imoex, ipp),
      ~log(.x / first(.x))
    )
  )

df_q %>%
  write_parquet('data_input/data_q.parquet')

gdp %>%
  write_parquet('data_input/gdp_raw.parquet')

df_q %>%
  mutate(gdp_yoy = gdp - lag(gdp, 4)) %>%
  tail()

# Месячная панель (для mixed-frequency MARSS) ==================================
#
# В df ВВП лежит на первом месяце квартала (янв/апр/июл/окт). Это датировка
# Росстата, но не датировка по правилу Mariano-Marasawa.
# Поэтому переносим ВВП на последний месяц квартала (мар/июн/сен/дек):
# тогда Q1 = янв+фев+мар читается в марте.
#
# ИБК уже SA у ЦБ — второй раз не сезонно корректируем.

gdp_m <- df %>%
  filter(!is.na(gdp)) %>%
  select(date, gdp) %>%
  arrange(date) %>%
  mutate(
    gdp  = seas_adj(gdp, freq = 4),
    gdp  = log(gdp / first(gdp)),
    date = date %m+% months(2)
  )

df_m <- df %>%
  select(-gdp) %>%
  arrange(date) %>%
  mutate(
    across(c(wf, retail, imoex, ipp), ~ seas_adj(.x, freq = 12)),
    across(c(wf, retail, imoex, ipp), ~ log(.x / first(.x)))
  ) %>%
  left_join(gdp_m, by = "date") %>%
  select(date, gdp, wf, retail, imoex, ibk, ipp)

df_m %>%
  write_parquet('data_input/data_m.parquet')
