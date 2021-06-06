library(tm)
library(tidyverse)
library(lubridate)
library(data.table)
library(SentimentAnalysis)

coindesk_news <- fread("https://raw.githubusercontent.com/Zmaznevegor/defi_uncertainty/main/data/news/coindesk.csv") %>% 
  mutate(date = as_date(ymd_hms(date))) %>% 
  as_tibble()

coindesk_news_dtm <- coindesk_news %>%
  pull(text) %>% 
  VectorSource() %>% 
  Corpus() %>% 
  DocumentTermMatrix(control = list(removePunctuation = TRUE,
                                    stopwords = TRUE,
                                    removeNumbers = TRUE,
                                    setmming = TRUE,
                                    tolower = TRUE,
                                    wordLengths = c(5, 20)))

analyzed_sentiment <- coindesk_news %>% 
  mutate(SentimentLM = analyzeSentiment(
    coindesk_news_dtm, 
    removeStopwords = TRUE, 
    stemming = TRUE,
    rules = list("SentimentLM" = list(ruleSentiment, 
                                      loadDictionaryLM()))) %>% 
      pull(SentimentLM))

crypto_data <- map(list.files(pattern = "*.csv", 
                              full.names = TRUE),
                   ~read.csv(.x)) %>% 
  reduce(bind_rows) %>%
  mutate(Date = as_date(ymd_hms(Date)))

analyzed_sentiment %>% 
  filter(SentimentLM != 0) %>% 
  mutate(lower_text = tolower(text),
         mentioned_coin = case_when(
           str_detect(lower_text, "binance coin|bnb") ~ "Binance Coin",
           str_detect(lower_text, "bitcoin|btc") ~ "Bitcoin",
           str_detect(lower_text, "ethereum|eth") ~ "Ethereum",
           str_detect(lower_text, "iota|miota") ~ "IOTA",
           str_detect(lower_text, "ripple|xrp") ~ "XRP",
           TRUE ~ "Other")) %>% 
  filter(!str_detect(lower_text, "sponsored")) %>% 
  ggplot(aes(date, SentimentLM, color = mentioned_coin)) +
  geom_point() +
  geom_smooth(color = "black") +
  geom_hline(yintercept = 0, color = "gray20",  linetype = "dashed") +
  facet_wrap(~mentioned_coin) +
  scale_x_date(breaks = seq.Date(ymd("2016-01-01"),
                                 ymd("2021-01-01"),
                                 "1 year"),
               labels = 2016:2021) +
  theme_minimal() +
  theme(legend.position = "none")
