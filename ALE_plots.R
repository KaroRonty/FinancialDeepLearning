library(signs)
library(tidyverse)

results <- list.files("Results", pattern = ".RDS") %>% 
  map(~readRDS(paste0("Results/", .x)) %>% 
        mutate(coin = .x)) %>% 
  reduce(full_join)

results_clean <- results %>% 
  mutate(crypto = case_when(str_detect(coin, "binance") ~ "Binance Coin",
                            str_detect(coin, "bitcoin") ~ "Bitcoin",
                            str_detect(coin, "eth") ~ "Ether",
                            str_detect(coin, "xrp") ~ "XRP",
                            str_detect(coin, "iota") ~ "IOTA")) %>% 
  mutate(model = case_when(str_detect(coin, "gru") ~ "GRU",
                           TRUE ~ "LSTM"))

results_clean %>% 
  filter(!(feature %in% c("sentiment_past", "volatility_10_day")),
         model == "LSTM") %>% 
  ggplot(aes(x.values, f.values, color = crypto)) +
  geom_line(size = 1) +
  facet_wrap(~feature) +
  scale_x_continuous(labels = signs_format(1, suffix = "%", 
                                           add_plusses = TRUE, 
                                           format = scales::percent)) +
  scale_y_continuous(labels = signs_format(1, suffix = "%", 
                                           add_plusses = TRUE, 
                                           format = scales::percent)) +
  labs(title = "Effect of price-related features to future 10-day average volatility",
       subtitle = "LSTM model only",
       x = "Percentual change in feature",
       y = "Percentual change in future 10-day average volatility",
       color = "Coin") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(margin = margin(10, 0, 15, 0)))

results_clean %>% 
  filter(!(feature %in% c("sentiment_past", "volatility_10_day")),
         model == "GRU") %>% 
  ggplot(aes(x.values, f.values, color = crypto)) +
  geom_line(size = 1) +
  facet_wrap(~feature) +
  scale_x_continuous(labels = signs_format(1, suffix = "%", 
                                           add_plusses = TRUE, 
                                           format = scales::percent)) +
  scale_y_continuous(labels = signs_format(1, suffix = "%", 
                                           add_plusses = TRUE, 
                                           format = scales::percent)) +
  labs(title = "Effect of price-related features to future 10-day average volatility",
       subtitle = "GRU model only",
       x = "Percentual change in feature",
       y = "Percentual change in future 10-day average volatility",
       color = "Coin") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(margin = margin(10, 0, 15, 0)))

results_clean %>% 
  filter(feature == "sentiment_past",
         model == "LSTM") %>% 
  ggplot(aes(x.values, f.values, color = crypto)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = signs_format(0.001,  
                                           add_plusses = TRUE, 
                                           format = scales::number)) +
  scale_y_continuous(labels = signs_format(1, suffix = "%", 
                                           add_plusses = TRUE, 
                                           format = scales::percent)) +
  labs(title = "Effect of 30-day sentiment to future 10-day average volatility",
       subtitle = "LSTM model only",
       x = "Sentiment score",
       y = "Percentual change in future 10-day average volatility",
       color = "Coin") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(margin = margin(10, 0, 15, 0)))

results_clean %>% 
  filter(feature == "sentiment_past",
         model == "GRU") %>% 
  ggplot(aes(x.values, f.values, color = crypto)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = signs_format(0.001,  
                                           add_plusses = TRUE, 
                                           format = scales::number)) +
  scale_y_continuous(labels = signs_format(1, suffix = "%", 
                                           add_plusses = TRUE, 
                                           format = scales::percent)) +
  labs(title = "Effect of 30-day sentiment to future 10-day average volatility",
       subtitle = "GRU model only",
       x = "Sentiment score",
       y = "Percentual change in future 10-day average volatility",
       color = "Coin") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(margin = margin(10, 0, 15, 0)))
