library(readxl)
library(lubridate)
library(magrittr)
library(geofacet)
library(extrafont)
library(tidyverse)

#данные по регионам
reg <- read_xlsx("crimes.xlsx", sheet = 1, trim_ws = TRUE)


#Госстат дает данные по количеству преступлений
#в 2012 год с приметкой по 20.11.2012 включительно,
#поэтому добавляем за ноябрь-декабрь 2012 (т.е. до конца года)
#информацию по среднему кол-ву преступлений за день
days_of_monitoring <-
  (ymd("2012-11-20") - ymd("2012-01-01")) / ddays(1)

reg %<>% within(value_reg[year == 2012 &
                            factor == "crimes"] <-
                  #2012 - високосный год
                  (364 * value_reg[year == 2012 &
                                     factor == "crimes"]) / days_of_monitoring)


#индекс преступности
reg %<>%
  spread(key = factor, value = value_reg) %>%
  mutate(index_reg = crimes / population * 10)


#определяем индекс по последнему году
#для заливки geom_area
cur <- reg %>% group_by(name) %>%
  summarise(index_last = index_reg[year == max(year)])

reg %<>% inner_join(cur, by = "name")


#данные по Украине + корректировка за 2012 + рассчет индекса
avg <- read_xlsx("crimes.xlsx", sheet = 2, trim_ws = TRUE)

avg %<>%
  spread(key = factor, value = value_avg)

avg$crimes[avg$year == 2012] <-
  (364 * avg$crimes[avg$year == 2012]) / days_of_monitoring

avg %<>% mutate(index_avg = crimes / population * 10)


#custom grid by https://hafen.github.io/grid-designer/
ukraine <- data.frame(
  name = as.character(
    c(
      "Сумська",
      "Чернігівська",
      "Волинська",
      "м. Київ",
      "Київська",
      "Житомирська",
      "Рівненська",
      "Полтавська",
      "Харківська",
      "Луганська",
      "Львівська",
      "Черкаська",
      "Вінницька",
      "Хмельницька",
      "Тернопільська",
      "Дніпропетровська",
      "Кіровоградська",
      "Донецька",
      "Закарпатська",
      "Івано-Франківська",
      "Запорізька",
      "Одеська",
      "Миколаївська",
      "Херсонська",
      "Чернівецька",
      "АР Крим",
      "м. Севастополь"
    )
  ),
  row = c(1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5),
  col = c(6, 5, 1, 4, 5, 3, 2, 6, 7, 8, 1, 5, 4, 3, 2, 7, 6, 8, 1, 2, 7, 4, 5, 6, 3, 6, 7),
  code = c(1:27),
  stringsAsFactors = FALSE
)


png(filename = "crimes.png",
    width = 1200,
    height = 950)

ggplot(data = reg, mapping = aes(x = year)) +
  geom_line(data = avg,
            mapping = aes(y = index_avg),
            color = "#7F8590") +
  geom_line(mapping = aes(y = index_reg),
            size = 1,
            color = "#a50f15") +
  geom_area(mapping = aes(y = index_reg, fill = index_last),
            alpha = 0.3) +
  facet_geo( ~ name, grid = ukraine) +
  scale_fill_gradient(low = "#fee5d9", high = "#a50f15") +
  scale_x_continuous(breaks = seq(1995, 2016, 7),
                     labels = c("'95", "'02", "'09", "'16")) +
  scale_color_manual(values = c("#7F8590", "#a50f15"),
                     labels = c("Україна", "регіон ")) +
  labs(title = "Рівень злочинності в Україні в 1995-2016 роках",
       subtitle = "кількість правопорушень на 10 000 осіб по регіонам і середня по Україні",
       caption = "За даними територіальних органів Держстату | Візуалізація: Голомб Володимир") +
  theme_minimal(base_family = "PT Sans") +
  theme(
    text = element_text(
      family = "PT Sans",
      face = "plain",
      color = "#3A3F4A",
      size = 30
    ),
    axis.title = element_blank(),
    axis.text = element_text(size = rel(0.45)),
    strip.text.x = element_text(size = rel(0.6)),
    panel.spacing.x = unit(1.25, 'lines'),
    panel.spacing.y = unit(1.50, 'lines'),
    legend.position = "none",
    panel.grid.major = element_line(
      size = 0.3,
      linetype = "dotted",
      color = "#A3A9B3"
    ),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold",
                              margin = margin(b = 10, t = 20)),
    plot.subtitle = element_text(
      size = rel(0.7),
      face = 'plain',
      margin = margin(b = 30)
    ),
    plot.caption = element_text(
      size = rel(0.65),
      margin = margin(b = 10, t = 50),
      hjust = 0.5,
      color = '#5D646F'
    ),
    plot.background = element_rect(fill = '#EFF2F4'),
    plot.margin = unit(c(2, 2, 2, 2), 'cm')
  )

dev.off()
