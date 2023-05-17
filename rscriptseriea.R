#--------IMPORTAZIONE DELLE LIBRERIE NECESSARIE E MANDIAMO IL FILE IN LETTURA-------------------
library(ggplot2)
library(ggrepel)
library(plotly)
library(tidyverse)

#---------------GOL FATTI IN SERIE A E RAPPORTO CON PARTITE GIOCATE--------------------

# Calcola la media dei gol fatti
average_goals <- mean(data$goals)

# Calcola le dimensioni dei pallini in base al numero di gol
data <- data %>% mutate(size = sqrt(goals) * 2)

# Crea una palette di colori
color_palette <- scales::col_numeric(palette = "plasma", domain = data$goals)(data$goals)

# Crea il grafico
p <- ggplot(data, aes(x = matches_played, y = goals, label = name, size = size, color = color_palette)) +
  geom_point(alpha = 0.8, shape = 21) +
  geom_text_repel(aes(label = ifelse(goals >= 10, name, "")),
                  nudge_x = 0.3, nudge_y = 0.3, size = 3, color = "black") +
  geom_hline(yintercept = average_goals, color = scales::col_numeric(palette = "plasma", domain = c(0, max(data$goals)))(average_goals),
             linetype = "dashed", size = 0.5) +
  labs(x = "Partite Giocate", y = "Gol") +
  ggtitle("Gol Fatti in Serie A e rapporto con partite giocate (Serie A 2018-2019)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major = element_line(color = "#EFEFEF", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "none")

# Rendi il grafico interattivo con plotly
ggplotly(p) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))



#--------------MIGLIORI MEDIE RAPPORTO GOL PER PARTITA------------------------------------------------
library(ggplot2)
library(ggrepel)
library(plotly)
library(tidyverse)

# Leggi il file CSV
data <- read.csv("data/serie_a_italy_2021.csv")

# Calcola il rapporto di gol per partita
data <- data %>% mutate(goals_per_match = goals / matches_played)

# Seleziona i primi 20 giocatori per il grafico
top_players <- head(data[order(data$goals_per_match, decreasing = TRUE),], 40)

# Crea il grafico con la libreria ggplot2
p <- ggplot(top_players, aes(x = reorder(name, goals_per_match), y = goals_per_match)) +
  geom_col(fill = "#BEBD7F", width = 0.6) +
  geom_hline(yintercept = mean(top_players$goals_per_match), color = "#E52B50", size = 0.5, linetype = "dashed") +
  geom_text(aes(label = sprintf("%.2f", goals_per_match)), vjust = -0.5, color = "black",
            size = 2, angle = 45, hjust = 1) +
  labs(x = NULL, y = "Gol per Partita") +
  ggtitle("Gol per Partita dei Giocatori (SERIE A 2018-2019)") +
  theme_light() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1, margin = margin(t = 10)),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 8),
        panel.grid.major.y = element_line(color = "#F5F5DC", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(10, 10, 10, 10))

# Rendi il grafico interattivo con plotly
ggplotly(p)





