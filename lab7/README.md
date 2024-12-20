# Практика 7
oreshin_ilya@bk.ru

# Практика 7

## Цель работы

1.  Изучить возможности технологии Apache Arrow для обработки и анализ
    больших данных
2.  Получить навыки применения Arrow совместно с языком программирования
    R
3.  Получить навыки анализа метаинфомации о сетевом трафике
4.  Получить навыки применения облачных технологий хранения, подготовки
    и анализа данных: Yandex Object Storage, Rstudio Server.

## План

1.  Импортировать данные с помощью функции open_dataset
2.  Выполнить задания

## Шаги

1.  Импортируем данные с помощью библиотеки arrow

``` r
library(arrow)
```

    Warning: package 'arrow' was built under R version 4.4.2


    Attaching package: 'arrow'

    The following object is masked from 'package:utils':

        timestamp

``` r
library(dplyr)
```

    Warning: package 'dplyr' was built under R version 4.4.2


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    Warning: package 'tidyverse' was built under R version 4.4.2

    Warning: package 'ggplot2' was built under R version 4.4.2

    Warning: package 'tidyr' was built under R version 4.4.2

    Warning: package 'readr' was built under R version 4.4.2

    Warning: package 'purrr' was built under R version 4.4.2

    Warning: package 'forcats' was built under R version 4.4.2

    Warning: package 'lubridate' was built under R version 4.4.2

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats   1.0.0     ✔ readr     2.1.5
    ✔ ggplot2   3.5.1     ✔ stringr   1.5.1
    ✔ lubridate 1.9.4     ✔ tibble    3.2.1
    ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ lubridate::duration() masks arrow::duration()
    ✖ dplyr::filter()       masks stats::filter()
    ✖ dplyr::lag()          masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
#download.file("https://storage.yandexcloud.net/arrow-datasets/tm_data.pqt",destfile = "tm_data.pqt")
df <- arrow::open_dataset(sources = "tm_data.pqt", format = "parquet")
glimpse(df)
```

    FileSystemDataset with 1 Parquet file
    105,747,730 rows x 5 columns
    $ timestamp <double> 1.578326e+12, 1.578326e+12, 1.578326e+12, 1.578326e+12, 1.5…
    $ src       <string> "13.43.52.51", "16.79.101.100", "18.43.118.103", "15.71.108…
    $ dst       <string> "18.70.112.62", "12.48.65.39", "14.51.30.86", "14.50.119.33…
    $ port       <int32> 40, 92, 27, 57, 115, 92, 65, 123, 79, 72, 123, 123, 22, 118…
    $ bytes      <int32> 57354, 11895, 898, 7496, 20979, 8620, 46033, 1500, 979, 103…
    Call `print()` for full schema details

``` r
glimpse(df)
```

    FileSystemDataset with 1 Parquet file
    105,747,730 rows x 5 columns
    $ timestamp <double> 1.578326e+12, 1.578326e+12, 1.578326e+12, 1.578326e+12, 1.5…
    $ src       <string> "13.43.52.51", "16.79.101.100", "18.43.118.103", "15.71.108…
    $ dst       <string> "18.70.112.62", "12.48.65.39", "14.51.30.86", "14.50.119.33…
    $ port       <int32> 40, 92, 27, 57, 115, 92, 65, 123, 79, 72, 123, 123, 22, 118…
    $ bytes      <int32> 57354, 11895, 898, 7496, 20979, 8620, 46033, 1500, 979, 103…
    Call `print()` for full schema details

2.  Приступаем к выполнению заданий

<!-- -->

1.  Найдите утечку данных из вашей сети

``` r
task1 <- df %>% filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>% filter(!str_detect(dst, "^12.") & !str_detect(dst, "^13.") & !str_detect(dst, "^14."))  %>% group_by(src) %>% summarise("sum" = sum(bytes)) %>% arrange(desc(sum)) %>% head(1) %>% select(src) 
task1 %>% collect() %>% knitr::kable()
```

| src          |
|:-------------|
| 13.37.84.125 |

2.  Найдите утечку данных 2

Для начала нужно определить рабочее время. Для этого можно использовать
нагрузку на трафик, и выцепить час с сортировкой по количеству трафика.

``` r
task21 <- df %>% select(timestamp, src, dst, bytes) %>% mutate(trafic = (str_detect(src, "^((12|13|14)\\.)") & !str_detect(dst, "^((12|13|14)\\.)")),time = hour(as_datetime(timestamp/1000))) %>% filter(trafic == TRUE, time >= 0 & time <= 24) %>% group_by(time) %>%
summarise(trafictime = n()) %>% arrange(desc(trafictime))
task21 %>% collect() %>% knitr::kable()
```

| time | trafictime |
|-----:|-----------:|
|   16 |    4490576 |
|   22 |    4489703 |
|   18 |    4489386 |
|   23 |    4488093 |
|   19 |    4487345 |
|   21 |    4487109 |
|   17 |    4483578 |
|   20 |    4482712 |
|   13 |     169617 |
|    7 |     169241 |
|    0 |     169068 |
|    3 |     169050 |
|   14 |     169028 |
|    6 |     169015 |
|   12 |     168892 |
|   10 |     168750 |
|    2 |     168711 |
|   11 |     168684 |
|    1 |     168539 |
|    4 |     168422 |
|   15 |     168355 |
|    5 |     168283 |
|    9 |     168283 |
|    8 |     168205 |

По данным выясняем, что рабочим временем является 16-23.

``` r
task22 <- df %>% mutate(time = hour(as_datetime(timestamp/1000))) %>% 
filter(!str_detect(src, "^13.37.84.125")) %>%  filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>% filter(!str_detect(dst, "^12.") | !str_detect(dst, "^13.") | !str_detect(dst, "^14."))  %>% filter(time >= 1 & time <= 15) %>%  group_by(src) %>% summarise("sum" = sum(bytes)) %>% arrange(desc(sum)) %>% head(1) %>% select(src) 
task22 %>% collect() %>% knitr::kable()
```

| src         |
|:------------|
| 12.55.77.96 |

3.  Найдите утечку данных 3

``` r
task31 <- df %>% filter(!str_detect(src, "^13.37.84.125")) %>% filter(!str_detect(src, "^12.55.77.96")) %>% filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>% filter(!str_detect(dst, "^12.") & !str_detect(dst, "^13.") & !str_detect(dst, "^14."))  %>% select(src, bytes, port) 


task31 %>%  group_by(port) %>% summarise("mean"=mean(bytes), "max"=max(bytes), "sum" = sum(bytes)) %>% 
  mutate("Raz"= max-mean)  %>% filter(Raz!=0) %>% arrange(desc(Raz)) %>% head(1) %>% collect() %>% knitr::kable()
```

| port |     mean |    max |         sum |    Raz |
|-----:|---------:|-------:|------------:|-------:|
|   37 | 35089.99 | 209402 | 32136394510 | 174312 |

``` r
task32 <- task31  %>% filter(port==37) %>% group_by(src) %>% summarise("mean"=mean(bytes)) %>% arrange(desc(mean)) %>% head(1) %>% select(src)
task32 %>% collect() %>% knitr::kable()
```

| src          |
|:-------------|
| 14.31.107.42 |

## Оценка результата

Был скачан и проанализирован пакет данных tm_data, были выполнены три
задания.

## Вывод

Я ознакомился с применением облачных технологий хранения, подготовки и
анализа данных, а также проанализировал метаинформацию о сетевом
трафике.
