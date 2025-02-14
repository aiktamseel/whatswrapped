# Hello
# This is an R script to visualize WhatsApp chat data

# Firstly, open a WhatsApp chat. Click the three dots,
# then click More, then click Export Chat
# The chat will be exported in txt format.
# Place the txt file in the folder for current R project
# and provide the name of the txt file below

path <- "example.txt"


# Great, now we need some packages. Run the code
# below and it will load/install all required packages

packages <- c("rwhatsapp", "tidyverse", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages())))
invisible(lapply(packages, library, character.only = TRUE)); rm(packages)


# Now, we will read the txt file into a dataframe named "chat"
# and also add a new column "type" that tells if a message
# is text or media or deleted or a setting message (e.g. new members added)
chat <- rwa_read(path) %>%
  mutate(type = case_when(
    is.na(author) ~ "settings",
    text == "<Media omitted>" ~ "media",
    text == "This message was deleted" ~ "deleted",
    TRUE ~ "text"
  )) %>%
  mutate(type = factor(type, levels = c("text", "media", "deleted", "settings")))


# Great, you have successfully imported and cleaned the data
# Now, you can play with data and explore different things.


# The next step is optional. If in your txt file, names of authors were saved
# with a different name, or if they appear as phone number, and you want to
# change them to a different name, you can proceed with the following steps.

# First, we create a CSV file with unique author names
write.csv(data.frame(old = unique(as.character(na.omit(chat$author))), new = ""),
          "mapping.csv", row.names = FALSE, quote = FALSE)
# Now from your project folder, open the "mapping.csv" file
# You can see it contains unique authors in old column and an empty new column
# In the new column, enter the new names to be used for each author and
# save the CSV file. Make sure that there is no empty cell in the "new" column.
# Now, un-comment the code below and run it.


# mapping <- read.csv("mapping.csv")
# replacement_map <- setNames(mapping$new, mapping$old)
# mapped <- replacement_map[as.character(chat$author)]
# chat$author <- factor(ifelse(is.na(mapped), chat$author, mapped), levels = unique(mapping$new))
# rm(replacement_map, mapped, mapping)


# Now, you can see that author names have been replaced accordingly
# That's all of the data cleaning. Now you can create visualizations
# and do analysis that provides proper author names.



# Now, for visualizations, we don't need "settings" type messages,
# so I'm creating a new dataframe with "settings" messages removed.

df <- chat %>% filter(type != "settings")


# Message Frequency Timeline
timeline <- df %>%
  # You can uncomment the line below to get plot for given author(s)
  #filter(author == "Member 1") %>%
  mutate(day = date(time), weekday = factor(wday(day, label = TRUE, week_start = 1))) %>%
  count(day, weekday) %>%
  ggplot(aes(x = day, y = n, fill = weekday)) +
  geom_bar(stat = "identity") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b '%y",
    expand = c(0.02, 0)
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90", linetype = "solid", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot",
    legend.title = element_blank()
  ) +
  ylab("") + xlab("") +
  ggtitle("Messages per day"); print(timeline)


# Top 25 Senders
most_active <- df %>%
  count(author, sort = TRUE) %>%
  top_n(25, n) %>% # Change the value here
  ggplot(aes(x = reorder(author, n), y = n, fill = author)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  xlab("") + ylab("Message Count") +
  ggtitle("Top 25 Senders"); print(most_active)



# Day-Hour Heatmap
chatmap <- df %>%
  # You can uncomment the line below to get plot for given author(s)
  #filter(author == "Member 1") %>%
  mutate(
    day_of_week = wday(time, label = TRUE, abbr = FALSE, week_start = 1),
    hour_format = factor(
      case_when(
        hour(time) == 0 ~ "12 am",
        hour(time) < 12 ~ paste(hour(time), "am"),
        hour(time) == 12 ~ "12 pm",
        TRUE ~ paste(hour(time) - 12, "pm")
      ),
      levels = c("12 am", paste(1:11, "am"), "12 pm", paste(1:11, "pm"))
    )
  ) %>%
  count(day_of_week, hour_format) %>%
  complete(
    day_of_week = wday(1:7, label = TRUE, abbr = FALSE, week_start = 1),
    hour_format,
    fill = list(n = 0)
  ) %>%
  mutate(day_of_week = fct_rev(day_of_week)) %>%
  ggplot(aes(x = hour_format, y = day_of_week, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen", name = "No. of\nMessages") +
  labs(title = "Message Frequency by Day and Hour", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ); print(chatmap)

# Most Used Emojis
emoji_plot <- df %>%
  unnest(emoji) %>%
  count(emoji, sort = TRUE) %>%
  top_n(n = 10, n) %>% # You can change the value here
  ggplot(aes(x = reorder(emoji, n), y = n, fill = emoji)) +
  geom_col() +
  ylab("") +
  xlab("") +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  ggtitle("Most Used Emojis"); print(emoji_plot)


# Most Used Words
library("tidytext") #install if not already installed
library("stopwords")

to_remove <- c(stopwords(language = "en"), "https", "gmail.com") #words to exclude

word_plot <- df %>%
  filter(type == "text") %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% to_remove) %>% # Exclude words in to_remove vector
  filter(nchar(word) > 3) %>%  # Exclude words with 3 or fewer letters
  count(word, sort = TRUE) %>%
  slice_max(n, n = 50) %>% # Change the number here
  ggplot(aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  ylab("") +
  xlab("") +
  coord_flip() +
  ggtitle("Most Used Words"); print(word_plot)
