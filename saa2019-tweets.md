SAA2019 Exploratory Data Analysis
================

# Acquire the tweets from the TAGS Google sheet

``` r
library(googlesheets)

tags_sheet <- "https://docs.google.com/spreadsheets/d/17EOoOeGllBxk5x9TS3vEc0zdUwmh_fL0abdEdrOz1Pg/edit#gid=400689247"

saa2019_tweet_archive <- 
tags_sheet %>% 
  gs_url() %>% 
    gs_read(ws = "Archive")

write_csv(saa2019_tweet_archive,
          "saa2019_tweet_archive.csv")
```

# Volume of tweets over time

The table inset in the plot shows the ten most retweeted tweets during
the two hours of the highest volume of tweets.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
saa2019_tweet_archive <- 
read_csv("saa2019_tweet_archive.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   id_str = col_double(),
    ##   from_user = col_character(),
    ##   text = col_character(),
    ##   created_at = col_character(),
    ##   time = col_character(),
    ##   geo_coordinates = col_logical(),
    ##   user_lang = col_character(),
    ##   in_reply_to_user_id_str = col_double(),
    ##   in_reply_to_screen_name = col_character(),
    ##   from_user_id_str = col_double(),
    ##   in_reply_to_status_id_str = col_double(),
    ##   source = col_character(),
    ##   profile_image_url = col_character(),
    ##   user_followers_count = col_double(),
    ##   user_friends_count = col_double(),
    ##   user_location = col_character(),
    ##   status_url = col_character(),
    ##   entities_str = col_character()
    ## )

``` r
saa2019_tweet_archive_dttm <- 
saa2019_tweet_archive %>% 
  mutate(time_dttm = dmy_hms(time)) %>% 
  group_by(hour = floor_date(time_dttm, "hour")) 

saa2019_tweet_archive_dttm_hourly_counts <- 
saa2019_tweet_archive_dttm %>% 
  count()  

top_hours <- 
  saa2019_tweet_archive_dttm_hourly_counts %>% 
  arrange(desc(n)) %>% 
  ungroup %>% 
  slice(1:2)

saa2019_tweet_archive_dttm_top_hours <- 
saa2019_tweet_archive_dttm %>% 
  filter(hour %in% top_hours$hour) %>% 
  group_by(text) %>% 
  count(sort = TRUE) %>% 
  ungroup %>%
  mutate(text = str_wrap(text, 100)) %>% 
  slice(1:10)
  
  ggplot(saa2019_tweet_archive_dttm_hourly_counts,
         aes(hour, n)) +
  geom_line() +
  geom_vline(xintercept = top_hours$hour,
             colour = "red") +
  theme_minimal(base_size = 12) +
  scale_x_datetime(date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 6)) +
  labs(y = "Number of tweets per hour",
       x = "Date and time") +
  annotation_custom(tableGrob(saa2019_tweet_archive_dttm_top_hours,
                              rows=NULL,
                              theme = ttheme_default(base_size = 4)), 
                    xmin= ymd_hms("2019-03-18 16:00:00 UTC"), 
                    xmax= ymd_hms("2019-04-08 16:00:00 UTC"), 
                    ymin= 250, 
                    ymax= 100)
```

<img src="saa2019-tweets_files/figure-gfm/unnamed-chunk-2-1.png" width="2100" />

# Number of Twitter accounts using \#SAA2019

The total number of unique accounts in this data set is 3325.

Here are the top 50 twitter accounts in these data:

``` r
saa2019_tweet_archive_dttm_accounts_count <- 
  saa2019_tweet_archive_dttm %>% 
  ungroup %>% 
  count(from_user, sort = TRUE) 

saa2019_tweet_archive_dttm_accounts_count %>% 
  slice(1:50) %>% 
ggplot(aes(reorder(from_user, n), n)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 10) +
  labs(y = "Number of tweets",
       x = "Top 50 Twitter accounts by volume") 
```

<img src="saa2019-tweets_files/figure-gfm/unnamed-chunk-3-1.png" width="2100" />

Here is the count of all twitter accounts active in each hour:

``` r
saa2019_tweet_archive_dttm_accounts <- 
  saa2019_tweet_archive_dttm %>% 
  group_by(hour) %>% 
  summarise(n_accounts = n_distinct(from_user))

  ggplot(saa2019_tweet_archive_dttm_accounts,
         aes(hour, n_accounts)) +
  geom_line() +
  theme_minimal(base_size = 12) +
  scale_x_datetime(date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5,
                                   size = 6)) +
  labs(y = "Number of unique Twitter accounts posting per hour",
       x = "Date and time") 
```

<img src="saa2019-tweets_files/figure-gfm/unnamed-chunk-4-1.png" width="2100" />

# Sentiment analysis

``` r
library(tidytext)

saa2019_tweet_archive_dttm %>% 
  ungroup %>% 
  filter(time_dttm >= "2019-04-11 00:00:00" & time_dttm <= "2019-04-17 00:00:00" ) %>%
  select(text) %>%
  mutate(text = str_remove(text, "\\brt\\b|\\bRT\\b|\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)|https://*")) %>% 
  mutate(text = tm::removeWords(text, c(stop_words$word, "the", "The")))  %>%
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("bing"))
```

# Co-occuring words

``` reval
library(igraph)
library(ggraph)

saa2019_tweet_archive_dttm %>% 
  ungroup %>% 
  filter(time_dttm >= "2019-04-11 00:00:00" & time_dttm <= "2019-04-17 00:00:00" ) %>%
  select(text) %>%
  mutate(text = str_remove(text, "\\brt\\b|\\bRT\\b|\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)|https://*")) %>% 
  mutate(text = tm::removeWords(text, c(stop_words$word, "the", "The"))) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2) %>% 
  separate(paired_words, c("word1", "word2"), sep = " ") %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: for SAA2019",
       x = "", y = "") +
  theme_void()
```
