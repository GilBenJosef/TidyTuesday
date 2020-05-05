# Data and Packages Import ------------------------------------------------

pacman::p_load("tidyverse", "tidytext", "readtext", 
               "stringi", "gridExtra", "extrafont")

# font_import()
# loadfonts(device = "win")

critic <- readtext::readtext('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readtext::readtext('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')

bing <- get_sentiments("bing")

# Setting a Theme ---------------------------------------------------------

theme <- theme_dark(base_size = 14) +
  theme(axis.text = element_text(color = "#757575", family = "Bahnschrift"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        panel.background = element_rect(fill = "#212121", color = "#212121"),
        panel.grid.major = element_line(color = "#424242", size = rel(0.5)),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#212121", color = "#212121"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#424242"),
        text = element_text(color = "#EEEEEE", family = "Bahnschrift"))

theme_set(theme)

# Critics Reviews ------------------------------------------------------

critic_tidy <- critic %>% 
  select(doc_id, grade = text, text = text.1) %>% 
  mutate(id = row_number())

critic_tidy$text <- stri_replace_all(critic_tidy$text, "", regex = "â€™")
critic_tidy$text <- stri_replace_all(critic_tidy$text, "", regex = "#46 â€")

critic_tidy$newg <- ifelse(critic_tidy$grade >= 95, 1, 
                           ifelse(critic_tidy$grade < 95 & critic_tidy$grade >= 90, 2, 
                                  ifelse(critic_tidy$grade < 90 & critic_tidy$grade >= 81, 3,
                                         ifelse(critic_tidy$grade < 80, 4, 5))))

new_critic <- critic_tidy %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  left_join(bing) %>% 
  drop_na()

new_critic$newg <- factor(new_critic$newg, 
                          labels = c("Highest Scores (+95)", "High Scores (90-95)",
                                        "Medium Scores (81-90)", "Low Scores (-80)"))

final1 <- new_critic %>% 
  count(newg, index = id %% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

combined <- ggplot(final1, aes(index, sentiment, fill = newg)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "Critics' Reviews Sentiment Analysis",
       subtitle = "All-in-One Format and Seperated by Scores",
       x = "Review Index", 
       y = "Intensity") + 
  scale_fill_manual(values = c("#00a989", "#0092d6", 
                               "#ffce0b", "#e83f68"))

separated <- ggplot(final1, aes(index, sentiment, fill = newg)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~newg, ncol = 2, scales = "free_x") + 
  scale_fill_manual(values = c("#00a989", "#0092d6", 
                               "#ffce0b", "#e83f68")) + 
  labs(x = "", y = "")

grid.arrange(combined, separated)

# Users Reviews -----------------------------------------------------------

user_tidy <- user_reviews %>% 
  select(doc_id, grade = text, text = text.1) %>% 
  mutate(id = row_number())

user_tidy$text <- stri_replace_all(user_tidy$text, "", regex = "â€™")
user_tidy$text <- stri_replace_all(user_tidy$text, "", regex = "#46 â€")

user_tidy$newg <- ifelse(user_tidy$grade >= 9, 1, 
                           ifelse(user_tidy$grade < 9 & user_tidy$grade >= 7, 2, 
                                  ifelse(user_tidy$grade < 7 & user_tidy$grade >= 5, 3,
                                         ifelse(user_tidy$grade < 5, 4, 5))))

new_user <- user_tidy %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  left_join(bing) %>% 
  drop_na()

new_user$newg <- factor(new_user$newg, 
                          labels = c("Highest Scores (+95)", "High Scores (90-95)",
                                     "Medium Scores (81-90)", "Low Scores (-80)"))

final2 <- new_user %>% 
  count(newg, index = id %% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

combined <- ggplot(final2, aes(index, sentiment, fill = newg)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "Users' Reviews Sentiment Analysis",
       subtitle = "All-in-One Format and Seperated by Scores",
       x = "Review Index", 
       y = "Intensity") + 
  scale_fill_manual(values = c("#00a989", "#0092d6", 
                               "#ffce0b", "#e83f68"))

separated <- ggplot(final2, aes(index, sentiment, fill = newg)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~newg, ncol = 2, scales = "free_x") + 
  scale_fill_manual(values = c("#00a989", "#0092d6", 
                               "#ffce0b", "#e83f68")) + 
  labs(x = "", y = "")

grid.arrange(combined, separated)
