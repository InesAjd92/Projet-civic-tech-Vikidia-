library(tidyverse)

read.csv("enquete_vikidia.csv")


#Visualisation contexte 


# Table de fréquences des réponses
table <- enquete_vikidia %>% select(21:26) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(percent = round((V1 / sum(V1))*100, 0))
table$rowname <- gsub("\\s*\\([^\\)]+\\)", "", as.character(table$rowname))
table$rowname <- table$rowname %>% str_replace_all(c("Autre...26" = "Autre",
                                                     "mon " = "le ",
                                                     "ma " = "la ",
                                                     "simple " = ""))

# Create test data.

donut_chart_contexte <- enquete_vikidia %>% 
  select(21:26) %>% 
  gather(key = "category", value = "count") %>%
  na.omit()  # Supprimer les lignes avec des valeurs manquantes, si nécessaire

data_contexte_graph <- enquete_vikidia %>% 
  select(21:26)

data_contexte_graph <- data_contexte_graph %>%
  summarise_all(~ sum(!is.na(.))) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  mutate(category = rowname,
         count = V1) %>%
  select(category, count)


write.csv(data_contexte_graph, "contexte_graph.csv")


library(ggplot2)

data$fraction = data$count / sum(data$count)

data_contexte_graph$fraction = round((data_contexte_graph$count / sum(data_contexte_graph$count))*100)

data_contexte_graph$ymax = cumsum(data_contexte_graph$fraction)

data_contexte_graph$ymin = c(0, head(data_contexte_graph$ymax, n=-1))

data_contexte_graph$labelPosition <- (data_contexte_graph$ymax + data_contexte_graph$ymin) / 2

data_contexte_graph$label <- paste0(data_contexte_graph$category, "\n value: ", data_contexte_graph$count)

# Renommer les labels dans la colonne "label"
data_contexte_graph$label <- c("A la maison", "En classe", "Pour ma culture", "Pour le travail", "Pour mon loisir", "Autre")  # Ajoutez les nouveaux noms de labels selon vos besoins

data_contexte_graph$percentage <- paste0(round(data_contexte_graph$fraction), "%")




library()

install.packages("bslib", type = "source")
install.packages("shiny", type = "source")
Sys.which("make")




# Plot
graph <- table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
  ggplot(aes(x = rowname, y = V1)) +
  geom_bar(stat = "identity", width = .6, fill = "#21468d") +
  coord_flip() +
  labs(x = "", y = "Nombre de réponses", title = "Les contextes d'utilisation de Vikidia", subtitle = "\nDans quel(s) contexte(s) utilisez-vous Vikidia ?") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  geom_text(aes(y = V1 + 14, x = rowname, label = paste(percent, "%", sep = "")), 
            color = "#333333", size = 3, check_overlap = T)
graph










saving_plot_custom(graph, "4_barplot_contextes", 8, 6)
