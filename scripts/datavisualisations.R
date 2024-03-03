
### Dataviz


# Librairies
packages = c("tidyverse", "tm","textstem", "lemon", "extrafont", "ggraph", "igraph", "flexdashboard", "waffle", "hrbrthemes", "ggpubr", "fmsb", "glue", "treemap", "ggforce", "ggwordcloud", "ggradar")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE) #devtools::install_github("ricardo-bion/ggradar")
      library(x, character.only = TRUE)}})



package.check

                ###########
                ## RAW DATA
                ###########




# Import raw data
enquete_vikidia <- read_delim("raw_data.tsv", 
  delim = "\t", escape_double = FALSE, 
  trim_ws = TRUE, skip = 2)

# Format des variables
str(enquete_vikidia)

# Suppression de doublons
enquete_vikidia <- enquete_vikidia %>% filter(S�quentiel != 474, S�quentiel != 475)




        ### TRAITEMENT DES VARIABLES


# CRÉATION : Le répondant est-il contributeur ?
enquete_vikidia <- enquete_vikidia %>% mutate(is_contrib = case_when(`Quel est votre degr� de contribution ?` == "J'ai d�j� contribué une fois ou deux" | 
                                                                 `Quel est votre degré de contribution ?` == "Je contribue une fois par semaine ou plus" | 
                                                                 `Quel est votre degré de contribution ?` == "Je contribue plusieurs fois par mois" | 
                                                                 `Quel est votre degré de contribution ?` == "Je contribue environ une fois par mois, ou moins" ~ 1,
                                                                 TRUE ~ 0))

# AGRÉGATION : activité + niveau d'étude
enquete_vikidia <- enquete_vikidia %>% mutate(activite = case_when(`Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "Collège" ~ "Élève au collège",
                                                                   `Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "École primaire (du CP au CM2)" ~ "Élève en école primaire",
                                                                   `Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "Études supérieures" ~ "Étudiant",
                                                                   `Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "Lycée" ~ "Élève au lycée",
                                                                   TRUE ~ `Vous êtes ...`))

# EXTRACTION : champ libre 'année'
annee <- as.data.frame(table(enquete_vikidia$`En quelle année avez-vous rejoint la communauté Vikidienne ?`))
enquete_vikidia <- enquete_vikidia %>% mutate(annee = str_extract(`En quelle année avez-vous rejoint la communauté Vikidienne ?`, "(1|2)\\d{3}"))


# LEMMATISATION / HARMONISATION : champ libre 'motivation à contribuer'
motivations_contrib <- as.data.frame(table(enquete_vikidia$`Qu'est ce qui vous a motivé à contribuer pour la première fois ?`))
enquete_vikidia <- enquete_vikidia %>% mutate(motivations_contrib = NA) #à compléter sur le drive


# LEMMATISATION / HARMONISATION : champ libre 'alternatives à Vikidia'
alternatives <- as.data.frame(table(enquete_vikidia$`Lorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?`))
    # réponses sans accent et en minuscule
enquete_vikidia <- enquete_vikidia %>% mutate(alternatives = tolower(`Lorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?`))
enquete_vikidia <- data.table::data.table(enquete_vikidia)
enquete_vikidia[, alternatives := stringi::stri_trans_general (str = alternatives, id = "Latin-ASCII")]
    # extraction des lignes avec les réponses les plus courantes
alternatives_df <- enquete_vikidia %>% 
    mutate(alternatives = str_match_all(alternatives, "wikipedia|internet|net|google|moteur de recherche|fandom|dicoado.org|ecosia|opera mini|persee|kikourou|wikimini|universalis|youtube|livres|bibliotheque|larousse|dictionnaire|encyclopedie|depen|autre")) %>% 
    unnest(alternatives) %>% #quand plusieurs matchs les valeurs sont mises dans une liste donc unnest la liste
    mutate(alternatives = str_replace_all(alternatives, "depen", "ça depend")) %>% 
    select(Séquentiel, `Lorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?`, alternatives) 
    # extraction des autres réponses
seq_alt <- alternatives_df %>% select(1) %>% unique()
seq_all <- enquete_vikidia %>% select(1) %>% unique()
manquant <- setdiff(seq_all, seq_alt)
manquant <- left_join(manquant, enquete_vikidia, by = "Séquentiel")
manquant <- manquant %>% select(1,59)

    



                ###############
                ## PROCESS DATA
                ###############



# Import des 2 bases (complétées sur le drive)
process_annee <- read_csv("data/process_annee.csv")
process_motiv_contrib <- read_csv("data/process_motiv-contrib.csv")
process_alternatives <- read_csv("data/process_alternatives.csv") %>% select(1,4,6)

# Jointure des bases 
enquete_vikidia <- left_join(process_annee, process_motiv_contrib[,c(1,4)], by = "Séquentiel") %>% arrange(Séquentiel)



    ### Nettoyage



# Suppression des trolls
enquete_vikidia <- enquete_vikidia %>% filter(Séquentiel != 43, Séquentiel != 75, Séquentiel != 317, Séquentiel != 465, Séquentiel != 479, Séquentiel != 484, Séquentiel != 498, Séquentiel != 499, Séquentiel != 524, Séquentiel != 537, Séquentiel != 594, Séquentiel != 598, Séquentiel != 613, Séquentiel != 614, Séquentiel != 621, Séquentiel != 630, Séquentiel != 676, Séquentiel != 677, Séquentiel != 717, Séquentiel != 722, Séquentiel != 780, Séquentiel != 793)
process_alternatives <- process_alternatives %>% filter(Séquentiel != 43, Séquentiel != 75, Séquentiel != 317, Séquentiel != 465, Séquentiel != 479, Séquentiel != 484, Séquentiel != 498, Séquentiel != 499, Séquentiel != 524, Séquentiel != 537, Séquentiel != 594, Séquentiel != 598, Séquentiel != 613, Séquentiel != 614, Séquentiel != 621, Séquentiel != 630, Séquentiel != 676, Séquentiel != 677, Séquentiel != 717, Séquentiel != 722, Séquentiel != 780, Séquentiel != 793)


# On attribue une activité aux 3 répondants 'Étudiant, élève' n'ayant pas précisé leur niveau d'étude
enquete_vikidia[92, 96] <- "Étudiant"
enquete_vikidia[254, 96] <- "Élève au collège"
enquete_vikidia[419, 96] <- "Élève au lycée"

# On "neutralise" les réponses
  # genre
enquete_vikidia <- enquete_vikidia %>% mutate(`Quel âge avez-vous ?` = str_replace_all(`Quel âge avez-vous ?`, 
                                                                                       c("Je préfère ne pas le dire" = "Préfère ne pas le dire")))
  # genre
enquete_vikidia <- enquete_vikidia %>% mutate(`Êtes vous ...` = str_replace_all(`Êtes vous ...`, c("Un homme" = "masculin",
                                                                                                   "Une femme" = "féminin",
                                                                                                   "Je préfère ne pas le dire" = "préfère ne pas le dire")))
  # degré de contribution
enquete_vikidia <- enquete_vikidia %>% mutate(`Quel est votre degré de contribution ?` = str_replace_all(`Quel est votre degré de contribution ?`, 
                                                                                                         c("C'est la première fois que je consulte Vikidia" = "Consulte Vikidia pour la première fois",
                                                                                                           "Je consulte Vikidia mais je n'ai jamais contribué" = "Consulte Vikidia mais n'y contribue pas",
                                                                                                           "J'ai déjà contribué une fois ou deux" = "A contribué une ou deux fois",
                                                                                                           "Je contribue environ une fois par mois, ou moins" = "Contribue une fois par mois ou moins",
                                                                                                           "Je contribue plusieurs fois par mois" = "Contribue plusieurs fois par mois",
                                                                                                           "Je contribue une fois par semaine ou plus" = "Contribue une fois par semaine ou plus")))


# On ordonne les variables aux catégories ordonnées
  # âge
enquete_vikidia$`Quel âge avez-vous ?` <- factor(enquete_vikidia$`Quel âge avez-vous ?`, order = TRUE, 
                                                 levels = c('Moins de 8 ans', 'Entre 8 et 13 ans', 'Entre 14 et 18 ans', 'Entre 19 et 25 ans', 'Entre 26 et 35 ans', 'Entre 36 et 50 ans', 'Entre 51 et 60 ans', 'Plus de 60 ans', 'Préfère ne pas le dire'))
  # activité
enquete_vikidia$activite <- factor(enquete_vikidia$activite, order = TRUE, 
                                   levels = c('Élève en école primaire', 'Élève au collège', 'Élève au lycée', 'Étudiant', "En recherche d'emploi", 'En activité', 'Retraité'))
  # degré de contribution
enquete_vikidia$`Quel est votre degré de contribution ?` <- factor(enquete_vikidia$`Quel est votre degré de contribution ?`, order = TRUE, 
                                                                   levels = c("Consulte Vikidia pour la première fois", "Consulte Vikidia mais n'y contribue pas", "A contribué une ou deux fois", "Contribue une fois par mois ou moins", "Contribue plusieurs fois par mois", "Contribue une fois par semaine ou plus"))


# Formats
enquete_vikidia <- enquete_vikidia %>% mutate(is_contrib = as.factor(is_contrib))


        ### VISUALISATIONS DES DONNÉES


#remotes::install_version("Rttf2pt1", version = "1.3.8")
#font_import()

# Fonction pour sauvegarder les graphs : format PNG pour les slides et format SVG pour les posters
  # petit format
saving_plot_petit <- function(graph, name) {
  ggsave(file = glue("dataviz/SVG/{name}.svg"), plot=graph, width=9, height=5)
  ggsave(file = glue("dataviz/PNG/{name}.png"), plot=graph, width=9, height=5)
}
  # moyen
saving_plot_moyen <- function(graph, name) {
  ggsave(file = glue("dataviz/SVG/{name}.svg"), plot=graph, width=12, height=8)
  ggsave(file = glue("dataviz/PNG/{name}.png"), plot=graph, width=12, height=8)
}
  # grand
saving_plot_grand <- function(graph, name) {
  ggsave(file = glue("dataviz/SVG/{name}.svg"), plot=graph, width=15, height=8)
  ggsave(file = glue("dataviz/PNG/{name}.png"), plot=graph, width=15, height=8)
}
  # custom
saving_plot_custom <- function(graph, name, width, height) {
  ggsave(file = glue("dataviz/SVG/{name}.svg"), plot=graph, width=width, height=height)
  ggsave(file = glue("dataviz/PNG/{name}.png"), plot=graph, width=width, height=height)
}




# -------------- Rendre compte de la variété des profils


######---


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Êtes vous ...`) %>% summarise(Freq = n()) %>% rename(Genre = `Êtes vous ...`)

  # Paramètres graphiques
table$fraction <- table$Freq / sum(table$Freq)  #percentages
table$proportion <- round((table$Freq / sum(table$Freq))*100)  #percentages
table$ymax <- cumsum(table$fraction)  # cumulative percentages (top of each rectangle)
table$ymin <- c(0, head(table$ymax, n=-1))  #bottom of each rectangle
table$labelPosition <- (table$ymax + table$ymin) / 2  #label position

  # Plot
graph <- ggplot(table, aes(ymax = ymax, ymin = ymin, xmax = 3.3, xmin = 2, fill = Genre)) +
  geom_rect(col = "white", size = 2) +
  geom_text(x=4, aes(y=labelPosition, label = paste(proportion,"%",sep = "")), color = "#333333", size=7) +
  geom_text(aes(x = 0, y = 0, label = sum(Freq)), col = "#333333", alpha=0.8, size=14, fontface="bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c('#fecf5d','#74a466','#82888d')) +
  coord_polar(theta="y") +
  ggtitle("Genre des répondants de l'enquête") +
  xlim(c(0, 4)) +   
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Monsterrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))
graph
saving_plot_petit(graph, "1_donut_poster")


######---


  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% group_by(`Êtes vous ...`) %>% 
  mutate(percent = Freq / sum(Freq) *100,
         percent = ifelse(percent < 0.5, round(percent, 1), round(percent, 0))) %>% 
  rename(Genre = `Êtes vous ...`) %>% ungroup()

  # Plot
graph <- table %>% #filter(percent != 0) %>% 
  ggplot(mapping = aes(x = ifelse(test = Genre == "masculin", yes = -Freq, no = Freq), 
                     y = `Quel âge avez-vous ?`, fill = Genre)) +
  geom_col(col = "white", size = 1.3) + #col = "black"
  scale_x_symmetric(labels = abs) + 
  scale_colour_manual(values = c('#fecf5d','#74a466'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de répondants", y = "",title = "Nombre de répondants selon l'âge et le genre") +
  theme_classic() + theme(legend.position = "right",
                          text = element_text(family = "Monsterrat", size = 12),
                          plot.title = element_text(face = "bold", size = 21),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank()) +
  geom_text(aes(y = `Quel âge avez-vous ?`, x = ifelse(test = Genre == "masculin", yes = -Freq-10, no = Freq+10), label = paste(percent,"%",sep = "")), 
            color = "#333333", 
            size = 3,
            check_overlap = T)

graph
saving_plot_petit(graph, "2_pyramid_age")



######---



  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% group_by(activite, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% group_by(`Êtes vous ...`) %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% 
  rename(Genre = `Êtes vous ...`) %>% ungroup()

  # Plot
graph <- table %>% 
  ggplot(mapping = aes(x = ifelse(test = Genre == "masculin", yes = -Freq, no = Freq), 
                     y = activite, fill = Genre)) +
  geom_col(col = "white", size = 2) + #col = "black"
  scale_x_symmetric(labels = abs) + 
  scale_colour_manual(values = c('#fecf5d','#74a466'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de répondants", y = "", title = "Nombre de répondants selon l'activité et le genre") +
  theme_classic() + theme(legend.position = "right",
                          text = element_text(family = "Monsterrat", size = 12),
                          plot.title = element_text(face = "bold", size = 21),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank()) +
  geom_text(aes(y = activite, x = ifelse(test = Genre == "masculin", yes = -Freq-10, no = Freq+10), label = paste(percent,"%",sep = "")), 
            color = "#333333", 
            size = 3,
            check_overlap = T)

graph
saving_plot_petit(graph, "3_pyramid_activite")



######---


  # Table de fréquences des réponses
table <- enquete_vikidia %>% select(21:26) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(percent = round((V1 / sum(V1))*100, 0))
table$rowname <- gsub("\\s*\\([^\\)]+\\)", "", as.character(table$rowname))
table$rowname <- table$rowname %>% str_replace_all(c("Autre...26" = "Autre",
                                                     "mon " = "le ",
                                                     "ma " = "la ",
                                                     "simple " = ""))

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



######---



  # Table de fréquences des réponses
d1=data.frame(from="origin", to=enquete_vikidia$is_contrib)
d2=data.frame(from = enquete_vikidia$is_contrib, to = enquete_vikidia$`Quel est votre degré de contribution ?`)
edges=rbind(d1, d2) %>% group_by(from, to) %>% mutate(value = n()) %>% distinct()
  
name <- unique(c(as.character(edges$from), as.character(edges$to)))
vertices <- data.frame(
  name=name,
  group=c( rep(NA,3) ,  rep(1, 4), rep(0, 2)),
  cluster=sample(letters[1:4], length(name), replace=T),
  value = c(0, edges$value)
) %>% mutate(percent = paste("  ", round(value / 774 * 100, 0), "% : ", name, sep = ""))


  # Plot
mygraph <- graph_from_data_frame(edges, vertices=vertices)
graph <- ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes(label=percent, filter=leaf, color=as.factor(group)), angle=0, hjust=0, nudge_y=0.1) +
  geom_node_point(aes(filter=leaf, size=value, color=as.factor(group)), alpha=1) +
  scale_size(range = c(4,14)) +
  labs(title = "Degré de contribution des répondants", subtitle = "\nQuel est votre degré de contribution ?") +
  coord_flip() + scale_y_reverse(expand = c(0.01, 0), limits = c(NA,-2)) +
  scale_colour_manual(values = rep(c('#ca97ae', '#dd4627')), aesthetics = "colour") +
  theme_void() +
  theme(legend.position="none",
        plot.subtitle = element_text(face = "italic", size = 15),
        text = element_text(family = "Montserrat", face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))
graph
saving_plot_custom(graph, "5_dendrogram", 9, 6)





# -------------- Dresser le profil du contributeur moyen



# Filtre sur les contributeurs
contributeurs <- enquete_vikidia %>% filter(is_contrib == 1)



######---


  # Table de fréquences des réponses
table <- contributeurs %>% group_by(`Êtes vous ...`) %>% summarise(Freq = n()) %>% rename(Genre = `Êtes vous ...`)

  # Paramètres graphiques
table$fraction <- table$Freq / sum(table$Freq)  #percentages
table$proportion <- round((table$Freq / sum(table$Freq))*100)  #percentages
table$ymax <- cumsum(table$fraction)  # cumulative percentages (top of each rectangle)
table$ymin <- c(0, head(table$ymax, n=-1))  #bottom of each rectangle
table$labelPosition <- (table$ymax + table$ymin) / 2  #label position

  # Plot
graph <- ggplot(table, aes(ymax = ymax, ymin = ymin, xmax = 3.3, xmin = 2, fill = Genre)) +
  geom_rect(col = "white", size = 2) +
  geom_text(x=4, aes(y=labelPosition, label = paste(proportion,"%",sep = "")), color = "#333333", size=7) +
  geom_text(aes(x = 0, y = 0, label = sum(Freq)), col = "#333333", alpha=0.8, size=14, fontface="bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c('#fecf5d','#74a466','#82888d')) +
  coord_polar(theta="y") +
  ggtitle("Genre des répondants contributeurs de l'enquête") +
  xlim(c(0, 4)) +   
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Monsterrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))
graph
saving_plot_petit(graph, "6_donut_contrib")



######---


  # Table de fréquences des réponses
table <- contributeurs %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% group_by(`Êtes vous ...`) %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% 
  rename(Genre = `Êtes vous ...`) %>% ungroup()

  # Plot
graph <- table %>%  
  ggplot(mapping = aes(x = ifelse(test = Genre == "masculin", yes = -Freq, no = Freq), 
                     y = `Quel âge avez-vous ?`, fill = Genre)) +
  geom_col(col = "white", size = 2) + #col = "black"
  scale_x_symmetric(labels = abs) + 
  scale_colour_manual(values = c('#fecf5d','#74a466'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de répondants contributeurs", y = "", title = "Nombre de contributeurs selon l'âge et le genre") +
  theme_classic() + theme(legend.position = "right",
                          text = element_text(family = "Monsterrat", size = 12),
                          plot.title = element_text(face = "bold", size = 21),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank()) +
  geom_text(aes(y = `Quel âge avez-vous ?`, x = ifelse(test = Genre == "masculin", yes = -Freq-2, no = Freq+2), label = paste(percent,"%",sep = "")), 
            color = "#333333", 
            size = 3,
            check_overlap = T)

graph
saving_plot_petit(graph, "7_pyramid_age_contrib")



######---



  # Table de fréquences des réponses
table <- contributeurs %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% group_by(activite, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% group_by(`Êtes vous ...`) %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0)) %>% 
  rename(Genre = `Êtes vous ...`) %>% ungroup()

  # Plot
graph <- table %>% 
  ggplot(mapping = aes(x = ifelse(test = Genre == "masculin", yes = -Freq, no = Freq), 
                     y = activite, fill = Genre)) +
  geom_col(col = "white", size = 2) + #col = "black"
  scale_x_symmetric(labels = abs) + 
  scale_colour_manual(values = c('#fecf5d','#74a466'),
                      aesthetics = c("colour", "fill")) +
  labs(x = "Nombre de répondants contributeurs", y = "", title = "Nombre de contributeurs selon l'activité et le genre") +
  theme_classic() + theme(legend.position = "right",
                          text = element_text(family = "Monsterrat", size = 12),
                          plot.title = element_text(face = "bold", size = 21),
                          axis.title.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank()) +
  geom_text(aes(y = activite, x = ifelse(test = Genre == "masculin", yes = -Freq-2, no = Freq+2), label = paste(percent,"%",sep = "")), 
            color = "#333333", 
            size = 3,
            check_overlap = T)

graph
saving_plot_custom(graph, "8_pyramid_activite_contrib", 9.5, 6)



######---


  # Table de fréquences des réponses
table <- contributeurs %>% select(21:26) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(percent = round((V1 / sum(V1))*100, 0))
table$rowname <- gsub("\\s*\\([^\\)]+\\)", "", as.character(table$rowname))
table$rowname <- table$rowname %>% str_replace_all(c("Autre...26" = "Autre",
                                                     "mon " = "le ",
                                                     "ma " = "la "))

  # Plot
graph <- table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
  ggplot(aes(x=rowname, y=V1))+
  geom_bar(stat="identity", width=.6, fill = "#21468d") +
  coord_flip() +
  labs(x = "", y = "Nombre de réponses", title = "Les contextes d'utilisation de Vikidia chez les contributeurs", subtitle = "\nDans quel(s) contexte(s) utilisez-vous Vikidia ?") +
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
  geom_text(aes(y = V1 + 3, x = rowname, label = paste(percent, "%", sep = "")), 
            color = "#333333", size = 3, check_overlap = T) 
graph
saving_plot_custom(graph, "9_barplot_contextes_contrib", 11, 7)



######---


  # Table de fréquences des réponses
table <- contributeurs %>% select(30:34) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(percent = round((V1 / sum(V1))*100, 0))
table$rowname <- table$rowname %>% str_replace_all(c("Autre...34" = "Autre",
                                                     "J'ai des responsabilités" = "Avoir des responsabilités",
                                                     "J'aime contribuer au projet Vikidia" = "Contribuer au projet Vikidia",
                                                     "Je me plais au sein de la communauté" = "Se plaire dans la communauté",
                                                     "Les domaines abordés m'intéressent" = "Intérêt pour les domaines abordés"))

  # Plot
graph <- table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
  ggplot(aes(x=rowname, y=V1))+
    geom_bar(stat="identity", width=.6, fill = "#21468d") +
    coord_flip() +
    labs(x = "", y = "Nombre de réponses", title = "Pourquoi les Vikidiens continuent de contribuer", subtitle = "\nPourquoi continuez-vous à contribuer ?") +
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
    geom_text(aes(y = V1 + 3, x = rowname, label = paste(percent, "%", sep = "")), 
            color = "#333333", size = 3, check_overlap = T) 
graph
saving_plot_custom(graph, "12_bar_motiv-contrib", 9.5, 6)



######---



  # Table de fréquences des réponses
pat <- contributeurs %>% filter(!is.na(`Patrouilleur`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Patrouilleur")
adm <- contributeurs %>% filter(!is.na(`Administrateur`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Administrateur")
bur <- contributeurs %>% filter(!is.na(`Bureaucrate`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Bureaucrate")
ver <- contributeurs %>% filter(!is.na(`Vérificateur d'IP`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Vérificateur d'IP")
nuk <- contributeurs %>% filter(!is.na(`Nuker`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Nuker")
plu <- contributeurs %>% filter(!is.na(`Plus maintenant`)) %>% select(36:42) %>% summarise_all(funs(sum(!is.na(.)))) %>% rename(Autre = `Autre...42`) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(outil = "Ancien responsable")
    # merge
table <- rbind(pat, adm, bur, ver, nuk, plu) %>% rename(`Types de contributions` = rowname)

  # Neutralisation des réponses
table$`Types de contributions` <- table$`Types de contributions` %>% 
          str_replace_all(c("De l'aide aux nouveaux utilisateurs" = "Aide aux nouveaux utilisateurs",
                            "De la patrouille" = "Patrouille",
                            "Des ajouts sur la partie technique" = "Ajouts sur la partie technique",
                            "Des corrections orthographiques ou typographiques" = "Corrections orthographiques ou typographiques",
                            "Des créations ou améliorations d'articles" = "Créations ou améliorations d'articles",
                            "Des travaux de catégorisation et d'ajout de portail" = "Travaux de catégorisation et d'ajout de portail"))

  # Ordonner les réponses
table$`Types de contributions` <- factor(table$`Types de contributions`, order = TRUE, 
                                            levels = c("Patrouille", 
                                                       "Aide aux nouveaux utilisateurs",
                                                       "Créations ou améliorations d'articles",
                                                       "Ajouts sur la partie technique",
                                                       "Corrections orthographiques ou typographiques",
                                                       "Travaux de catégorisation et d'ajout de portail",
                                                       "Autre"))

  # Plot
graph <- table %>% filter(`Types de contributions` != "Autre") %>% group_by(outil) %>% mutate(percent = round(V1/sum(V1)*100, 0)) %>% ungroup() %>% 
  ggplot(aes(y = V1)) +
  geom_col(aes(x = `Types de contributions`, color = `Types de contributions`, fill = `Types de contributions`), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("white", "white", "white", "white", "white", "white"))+
  scale_fill_manual(values = c("#21468d", "#74a466", "#fecf5d", "#f38337", "#da4729", "#c898ae")) +
  geom_text(aes(y = V1 + 2, x = `Types de contributions`, label = paste(percent, "%", sep = "")), 
            color = "#333333", size = 3, check_overlap = T) +
  labs(x = "Responsabilités", y = "Nombre de réponses", title = "Contributions apportées à Vikidia selon les responsabilités",
       subtitle = "\nQuels types de contributions faites-vous ?, Avez-vous des outils (responsabilités) ?") +
  theme_classic() +
  facet_wrap(~outil, strip.position="bottom") +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        strip.text.x = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  coord_flip()
graph
saving_plot_grand(graph, "13_grouped_contrib-wrap")




######---



  # Table de fréquences des réponses
table <- contributeurs %>% filter(annee > 2005 & annee < 2022) %>% group_by(annee, `Vous êtes ...`) %>% 
  summarise(Freq = n()) %>% ungroup()

  # On ordonne et renomme les catégories
table <- table %>% mutate(`Vous êtes ...` = factor(`Vous êtes ...`, 
                                                    order = TRUE, 
                                                    levels = c("Étudiant, élève", "En recherche d'emploi", "En activité", "Retraité")))

  # Plot
graph <- ggplot(table, aes(x = annee, y = Freq, group = `Vous êtes ...`, colour = `Vous êtes ...`)) +
  geom_line(size=1.7, alpha=0.9, linetype=1) +
  geom_point(colour="white", size = 2, pch = 21, stroke = 1.5) +
  scale_color_manual(values = c("#21468d", "#74a466", "#fecf5d", "#f38337")) +
  labs(x = "Année d'intégration à la communauté", y = "Nombre de contributeurs", title = "Nombre de contributeurs ayant rejoint la communauté par année", subtitle = "\nEn quelle année avez-vous rejoint la communauté ?") +
  scale_x_continuous(breaks = seq(2006, 2021, 1)) +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(colour = guide_legend(reverse = T, title = "Profil du répondant"),
         fill = "none")
graph
saving_plot_custom(graph, "10_timeline1", 10.6, 6)


######---



  # Table de fréquences des réponses
table <- contributeurs %>% filter(annee > 2005 & annee < 2022) %>% group_by(annee, `Veuillez préciser votre niveau d'étude`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% na.omit() %>% rename(`Niveau d'étude` = `Veuillez préciser votre niveau d'étude`)

  # On ordonne et renomme les niveaux d'études
table <- table %>% mutate(`Niveau d'étude` = str_replace_all(`Niveau d'étude`, "École primaire \\(du CP au CM2\\)", "École primaire"),
                          `Niveau d'étude` = factor(`Niveau d'étude`, 
                                                    order = TRUE, 
                                                    levels = c("École primaire", "Collège", "Lycée", "Études supérieures")))

  # Plot
graph <- ggplot(table, aes(x = annee, y = Freq, group = `Niveau d'étude`, colour = `Niveau d'étude`)) +
  geom_line(size=1.7, alpha=0.9, linetype=1) +
  geom_point(colour="white", size = 2, pch = 21, stroke = 1.5) +
  scale_color_manual(values = c("#21468d", "#637DAF", "#A6B5D1", "#E8ECF3")) +
  labs(x = "Année d'intégration à la communauté", y = "Nombre de contributeurs", title = "Nombre de contributeurs scolaires ayant rejoint la communauté par année", subtitle = "\nEn quelle année avez-vous rejoint la communauté ?") +
  scale_x_continuous(breaks = seq(2006, 2021, 1)) +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(colour = guide_legend(reverse = T),
         fill = "none")
graph
saving_plot_custom(graph, "10_timeline2", 12, 7)


######---



  # Table de fréquences des réponses
table <- contributeurs %>% filter(annee > 2005 & annee < 2022) %>% group_by(annee, `Quel est votre degré de contribution ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% rename(`Degré de contribution` = `Quel est votre degré de contribution ?`)

  # Plot
graph <- ggplot(table, aes(x = annee, y = Freq, group = `Degré de contribution`, colour = `Degré de contribution`)) +
  geom_line(size=1.7, alpha=0.9, linetype=1) +
  geom_point(size = .8, stroke = 1.5) +
  scale_color_manual(values = c("#21468d", "#74a466", "#fecf5d", "#f38337")) +
  labs(x = "Année d'intégration à la communauté", y = "Nombre de contributeurs", title = "Nombre de contributeurs ayant rejoint la communauté par année", subtitle = "\nEn quelle année avez-vous rejoint la communauté ?, Quel est votre degré de contribution ?") +
  scale_x_continuous(breaks = seq(2006, 2021, 1)) +
  theme_classic() +
  facet_zoom(x = annee > 2014, split = TRUE) +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        strip.background = element_rect(fill = "grey", colour = "#999999", linetype = 2),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
graph
saving_plot_custom(graph, "10_timeline3", 10.6, 8)






# -------------- Cerner les usages




######---



  # Préparation des données
data_wordcloud <- glimpse(contributeurs)
corpus = Corpus(VectorSource(data_wordcloud$motivations_contrib))
    # mise en forme des mots
corpus = tm_map(corpus, PlainTextDocument) #Conversion to Lowercase
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation) #Removing Punctuation
    # retrait des mots non désirés (pronoms, auxiliaires etc.)
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("french"))) #Remove stopwords
corpus = tm_map(corpus, stripWhitespace) # Eliminate white spaces
corpus[[1]][1] 
    # bon format du df
DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat),decreasing=TRUE) 
word_data <- data.frame(word = names(f),freq=f)

  # Plot
graph <- ggplot(word_data, aes(label = word, size = freq)) +
  geom_text_wordcloud(color = rep_len(c("#fecf5d","#21468d","#f48337"), nrow(word_data)), family = "Montserrat") +
  scale_size_area(max_size = 40) +
  labs(subtitle = "\nQu'est-ce qui vous a motivé à contribuer pour la première fois ?") +
  theme_minimal() +
  theme(text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))
graph
saving_plot_moyen(graph, "11_wordcloud")



#------

### Regroupement des catégories de tranches d'âge
enquete_vikidia <- enquete_vikidia %>% mutate(`Quel âge avez-vous ?` = str_replace_all(`Quel âge avez-vous ?`, 
                                                                                       c("Moins de 8 ans" = "Moins de 14 ans",
                                                                                         "Entre 8 et 13 ans" = "Moins de 14 ans",
                                                                                         "Entre 19 et 25 ans" = "Entre 19 et 35 ans",
                                                                                         "Entre 26 et 35 ans" = "Entre 19 et 35 ans",
                                                                                         "Entre 36 et 50 ans" = "Entre 36 et 60 ans",
                                                                                         "Entre 51 et 60 ans" = "Entre 36 et 60 ans")))


# On ordonne les variables aux catégories ordonnées
  # âge
enquete_vikidia$`Quel âge avez-vous ?` <- factor(enquete_vikidia$`Quel âge avez-vous ?`, order = TRUE, 
                                                 levels = c('Moins de 14 ans', 'Entre 14 et 18 ans', 'Entre 19 et 35 ans', 'Entre 36 et 60 ans', 'Plus de 60 ans', 'Préfère ne pas le dire'))


#------




  # Table de fréquences des réponses
table <- enquete_vikidia %>% select(14:19) %>% summarise_all(funs(sum(!is.na(.)))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% mutate(percent = round((V1 / sum(V1))*100, 0))
table$rowname <- table$rowname %>% str_replace_all(c("Autre...19" = "Autre",
                                                     "Par hasard et au gré de vos navigations sur Internet" = "Par hasard, sur Internet",
                                                     "Par vos proches" = "Par des proches",
                                                     "Par votre milieu professionnel" = "Par le milieu professionnel",
                                                     "Par votre milieu scolaire, étudiant" = "Par le milieu scolaire, étudiant"))

  # Plot
graph <- table %>% mutate(rowname = fct_reorder(rowname, V1)) %>% 
  ggplot(aes(x=rowname, y=V1))+
    geom_bar(stat="identity", fill="#21468d", width=.6) +
    coord_flip() +
    labs(x = "", y = "Nombre de réponses", title = "Canaux par lesquels les répondants ont connu Vikidia", 
         subtitle = "\nComment avez-vous entendu parler de Vikidia pour la première fois ?") +
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
    geom_text(aes(y = V1 + 15, x = rowname, label = paste(percent, "%", sep = "")), 
            color = "#333333", size = 3, check_overlap = T)
graph
saving_plot_custom(graph, "14_bar_connu-outil", 10, 7)



######---



  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(activite, `Selon vous, comment Vikidia est-elle perçue par le grand public ?`) %>% summarise(n = n()) %>% 
  filter(`Selon vous, comment Vikidia est-elle perçue par le grand public ?` != "Autre")

  # Neutralisation des réponses
table <- table %>% 
  mutate(activite = str_replace_all(activite, c("Élève en école primaire" = "Enseignement primaire",
                                                "Élève au collège" = "Enseignement secondaire",
                                                "Élève au lycée" = "Enseignement secondaire",
                                                "Étudiant" = "Enseignement supérieur")),
         `Selon vous, comment Vikidia est-elle perçue par le grand public ?` = str_replace_all(`Selon vous, comment Vikidia est-elle perçue par le grand public ?`, 
                                              c("Personne ne connaît Vikidia en dehors de la communauté" = "Inconnue en dehors de la communauté",
                                                "Vikidia est peu connue, et le grand public en est méfiant" = "Peu connue et avec une certaine méfiance",
                                                "Vikidia devrait gagner en popularité" = "Devrait gagner en popularité",
                                                "Vikidia est de plus en plus connue et elle gagne en confiance" = "De plus en plus connue, gagne en confiance",
                                                "Tout le monde connaît Vikidia !" = "Connue par tous"))) %>% 
  group_by(activite, `Selon vous, comment Vikidia est-elle perçue par le grand public ?`) %>% mutate(n = sum(n)) %>% unique()

  # Ordre des réponses
table <- table %>% 
  mutate(`Selon vous, comment Vikidia est-elle perçue par le grand public ?` = factor(`Selon vous, comment Vikidia est-elle perçue par le grand public ?`, 
                                                                                      order = TRUE, 
                                                                                      levels = c("Inconnue en dehors de la communauté", 
                                                                                                 "Peu connue et avec une certaine méfiance",
                                                                                                 "Devrait gagner en popularité",
                                                                                                 "De plus en plus connue, gagne en confiance",
                                                                                                 "Connue par tous")),
         activite = factor(activite, 
                           order = TRUE, 
                           levels = c("Enseignement primaire", 
                                      "Enseignement secondaire",
                                      "Enseignement supérieur",
                                      "En recherche d'emploi", 
                                      'En activité', 
                                      'Retraité')))


  # Paramètres graphiques (forcer l'ordre des catégories sur le graphique: même ordre que dans les données)
table <- data.table::as.data.table(table)
table <- table[j = group := fct_rev(`Selon vous, comment Vikidia est-elle perçue par le grand public ?`)]
data.table::setorder(table, `Selon vous, comment Vikidia est-elle perçue par le grand public ?`)

  # Plot
graph <- ggplot(table, aes(fill = `Selon vous, comment Vikidia est-elle perçue par le grand public ?`, values = n, color = `Selon vous, comment Vikidia est-elle perçue par le grand public ?`)) +
  geom_waffle(size = .95, flip = T, color = "white", make_proportional = F) +
  facet_wrap(~activite, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10,
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#21468d", "#74a466", "#fecf5d", "#f38337", "#da4729")) +
  coord_equal() +
  labs(title = "Niveau de popularité estimé par le répondant selon son activité",
       x = "Activité",
       y = "Nombre de répondants",
       subtitle = "\nQuel est le niveau de popularité de Vikidia dans le grand public selon vous ?") +
  theme_minimal(base_family = "Montserrat") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
        legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white")) +
  guides(fill = guide_legend(reverse = T, title = "Perception estimée"),
         color = 'none')
graph
saving_plot_grand(graph, "15_waffle")



######---



  # Table de fréquences des réponses
filtre <- enquete_vikidia %>% filter(!is.na(`Les articles y sont plus faciles à comprendre`))
fac <- as.data.frame(table(filtre$`Quel âge avez-vous ?`)) %>% mutate(raisons = "Simplicité des articles")
filtre <- enquete_vikidia %>% filter(!is.na(`Vikidia est plus adaptée à mon niveau/âge`))
age <- as.data.frame(table(filtre$`Quel âge avez-vous ?`)) %>% mutate(raisons = "Outil adapté au niveau/âge")
filtre <- enquete_vikidia %>% filter(!is.na(`Je préfère l'ambiance au sein de la communauté vikidienne`))
amb <- as.data.frame(table(filtre$`Quel âge avez-vous ?`)) %>% mutate(raisons = "Ambiance favorable")
filtre <- enquete_vikidia %>% filter(!is.na(`La communauté vikidienne est de plus petite taille`))
tai <- as.data.frame(table(filtre$`Quel âge avez-vous ?`)) %>% mutate(raisons = "Communauté plus petite")
filtre <- enquete_vikidia %>% filter(!is.na(`Je consulte peu Vikidia, je suis surtout contributeur`))
con <- as.data.frame(table(filtre$`Quel âge avez-vous ?`)) %>% mutate(raisons = "Consulte peu, contribue pincipalement")
filtre <- enquete_vikidia %>% filter(!is.na(`Je préfère Wikipédia`))
wik <- as.data.frame(table(filtre$`Quel âge avez-vous ?`)) %>% mutate(raisons = "Préfèrence pour Wikipédia")
    # merge
table <- rbind(fac,age,amb,tai,con,wik)

  # Ordonner les réponses
table$raisons <- factor(table$raisons, order = TRUE, 
                                            levels = c("Préfèrence pour Wikipédia",
                                                       "Consulte peu, contribue pincipalement",
                                                       "Ambiance favorable",
                                                       "Communauté plus petite",
                                                       "Simplicité des articles", 
                                                       "Outil adapté au niveau/âge"))

  # Plot
graph <- table %>% filter(Var1 != "Préfère ne pas le dire") %>% 
  ggballoonplot(x = "Var1", y = "raisons", size = "Freq", fill = "#21468d", color = "white", ggtheme = theme_bw()) + 
  labs(title = "Pourquoi les Vikidiens utilisent l'outil", 
       subtitle = "\nPourquoi consultez vous plutôt les articles sur Vikidia plutôt que Wikipédia ?") +
  theme(text = element_text(family = "Montserrat", size = 12),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.title = element_text(face = "bold", size = 21))+
  guides(size = guide_legend(reverse = T, title = "Nombre de réponses"),
         fill = "none")
graph
saving_plot_custom(graph, "16_ballonplot1", 10, 7)



######---


  # Table
filtre <- enquete_vikidia %>% filter(!is.na(`Les articles y sont plus faciles à comprendre`), `Êtes vous ...` != "préfère ne pas le dire")
amb <- as.data.frame(table(filtre$`Quel âge avez-vous ?`, filtre$`Êtes vous ...`)) %>% mutate(raisons = "Ambiance favorable")
filtre <- enquete_vikidia %>% filter(!is.na(`La communauté vikidienne est de plus petite taille`), `Êtes vous ...` != "préfère ne pas le dire")
tai <- as.data.frame(table(filtre$`Quel âge avez-vous ?`, filtre$`Êtes vous ...`)) %>% mutate(raisons = "Communauté plus petite")
    # merge
table <- rbind(amb,tai)


  # Plot
graph <- table %>% filter(Var1 != "Préfère ne pas le dire") %>%
  ggballoonplot(x = "raisons", y = "Var1", size = "Freq", fill = "Var2", color = "Var2", ggtheme = theme_bw()) + 
  labs(title = "Pourquoi les Vikidiens utilisent l'outil", 
       subtitle = "\nPourquoi consultez vous plutôt les articles sur Vikidia plutôt que Wikipédia ?") +
  facet_wrap(~Var2) +
  scale_fill_manual(values = c("#fecf5d", "#74a466")) +
  scale_color_manual(values = c("#fecf5d", "#74a466")) +
  theme(text = element_text(family = "Montserrat", size = 12),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.title = element_text(face = "bold", size = 21),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 12, face = "bold"))+
  guides(size = guide_legend(reverse = T, title = "Nombre de réponses"),
         fill = "none", color = "none")
graph
saving_plot_custom(graph, "16_ballonplot2", 8.6, 6)






######---



  # Table de fréquences des réponses
table <- process_alternatives %>% group_by(`Grandes catégories...6`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% na.omit() %>% mutate(proportion = round(Freq / sum(Freq) *100, 0))

  # Ordre des catégories
table$`Grandes catégories...6` <- factor(table$`Grandes catégories...6`, ordered = TRUE, levels = c("ressources numériques", 
                                                                                                    "ressources physiques",
                                                                                                    "ressources humaines"))

  # Paramètres graphiques
table$fraction <- table$Freq / sum(table$Freq)  #percentages
table$ymax <- cumsum(table$fraction)  # cumulative percentages (top of each rectangle)
table$ymin <- c(0, head(table$ymax, n=-1))  #bottom of each rectangle
table$labelPosition <- (table$ymax + table$ymin) / 2  #label position
table[1,7] <- 0.02
table[3,7] <- 0.97

  # Plot
graph <- ggplot(table, aes(ymax = ymax, ymin = ymin, xmax = 2.8, xmin = 1.5, fill = `Grandes catégories...6`)) +
  geom_rect(col = "white", size = 1) +
  geom_text(x=3.5, aes(y=labelPosition, label = paste(proportion,"%",sep = "")), color = "#333333", size=7, check_overlap = T) +
  scale_fill_manual(values = c("#21468d", "#fecf5d", "#f48337")) +
  coord_polar(theta="y") +
    labs(x = "", title = "Type de ressources utilisées comme alternatives à Vikidia", 
         subtitle = "\nLorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?") +
  xlim(c(0, 4)) +   
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Monsterrat", size = 12),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.title = element_text(face = "bold", size = 21),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white")) +
  guides(fill = guide_legend(title = "Type de ressource"))
graph
saving_plot_custom(graph, "17_barplot_alternatives", 11, 5)



######---

  # Table de fréquences des réponses
table <- process_alternatives %>% group_by(`Grandes catégories...6`, `Petites catégories`) %>% 
  summarise(n = n()) %>% ungroup() %>% na.omit() %>% 
  mutate(`Petites catégories` = str_replace_all(`Petites catégories`, "wiktionnaire wikipédia", "wiktionnaire")) %>% 
  filter(!row_number() %in% 11)
table[29,3] <- 6+2 

  # Plot
graph <- ggplot(table, aes(label = `Petites catégories`, size = n, colour = `Grandes catégories...6`)) +
  geom_text_wordcloud(family = "Montserrat",
                      grid_size = 5) +
  scale_colour_manual(values = c("#f48337", "#21468d", "#fecf5d")) +
  scale_size_area(max_size = 40) +
  labs(subtitle = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nLorsque vous ne trouvez pas des informations sur Vikidia, où allez-vous en priorité ?") +
  theme_minimal() +
  theme(plot.subtitle = element_text(face = "italic", size = 15),
        plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))
graph
ggsave(file = glue("dataviz/SVG/17_wordcloud_alternatives.svg"), plot=graph, width=12, height=9)
ggsave(file = glue("dataviz/PNG/test.png"), plot=graph, width=10, height=9)



# -------------- Analyser les besoins des utilisateurs



######---

  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Quel âge avez-vous ?`, `Trouvez-vous les articles de Vikidia accessibles ?`) %>% 
  summarise(n = n()) %>% ungroup() %>% rename(`Accessibilité des articles` = `Trouvez-vous les articles de Vikidia accessibles ?`) %>% 
  group_by(`Quel âge avez-vous ?`) %>% mutate(percent = n / sum(n) *100)

  # Neutralisation des réponses
table$`Accessibilité des articles` <- table$`Accessibilité des articles` %>% 
          str_replace_all(c("Pas du tout accessibles" = "Inexistante",
                            "Peu accessibles" = "Insuffisante",  # faible / basse ?
                            "Moyennement accessibles" = "Moyenne",
                            "Plutôt accessibles" = "Correcte",
                            "Très accessibles" = "Élevée"))

  # Ordonner les réponses
table$`Accessibilité des articles` <- factor(table$`Accessibilité des articles`, order = TRUE, 
                                            levels = c("Élevée",
                                                       "Correcte",
                                                       "Moyenne",
                                                       "Insuffisante",
                                                       "Inexistante"))

  # Plot
graph <- table %>% filter(`Quel âge avez-vous ?` != "Préfère ne pas le dire") %>%
  ggplot(aes(x = `Quel âge avez-vous ?`, y = percent)) +
  geom_col(aes(fill = `Accessibilité des articles`), position = "stack", width = 0.7, col = "white", size = 1.5) +
  scale_fill_manual(values = c("#21468d", "#74a466", "#fecf5d", "#f38337", "#da4729")) +
  labs(x = "Âge", y = "Pourcentage", title = "Évaluation de l'accessibilité des articles selon l'âge",
       subtitle = "\nTrouvez-vous les articles de Vikidia accessibles ?") +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
graph
saving_plot_custom(graph, "18_stacked_accessibilite", 10, 6)




######---


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(is_contrib, `Comment évaluez vous votre participation aux canaux de discussion ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% arrange(Freq)

  # Neutralisation des réponses
table$`Comment évaluez vous votre participation aux canaux de discussion ?` <- table$`Comment évaluez vous votre participation aux canaux de discussion ?` %>% 
          str_replace_all(c("[^[:alnum:]]" = " ",   #remove "??" characters
                            "Quels canaux de discussion" = "N'a pas connaissance des canaux",
                            "Je les connais mais je n y ai jamais participé" = "A connaissance sans y participer", 
                            "J ai déjà écrit un ou deux messages" = "Participe de temps à autre",
                            "Il m arrive de répondre aux messages" = "Participe de temps à autre",
                            "Je suis plutôt actif sur les canaux" = "Actif sur les canaux",
                            "Je suis très actif sur les canaux" = "Actif sur les canaux")) 

  # Ordonner les réponses
table$`Comment évaluez vous votre participation aux canaux de discussion ?` <- factor(table$`Comment évaluez vous votre participation aux canaux de discussion ?`, order = TRUE, levels = c("N'a pas connaissance des canaux   ", "A connaissance sans y participer", "Participe de temps à autre", "Actif sur les canaux"))

  # Regroupement des catégories : somme par groupe
table <- table %>% group_by(`Comment évaluez vous votre participation aux canaux de discussion ?`, is_contrib) %>% mutate(Freq = sum(Freq)) %>% distinct() %>% 
  ungroup() %>% group_by(is_contrib) %>% mutate(percent = Freq / sum(Freq) *100,
                                                is_contrib = case_when(is_contrib == 0 ~ "lecteurs",
                                                                       is_contrib == 1 ~ "contributeurs"),
                                                is_contrib = factor(is_contrib, order = TRUE, levels = c("lecteurs", "contributeurs")))

  # Paramètres graphiques
table <- table %>% 
  arrange(desc(`Comment évaluez vous votre participation aux canaux de discussion ?`)) %>%
  mutate(ypos = cumsum(percent) - 0.5*percent )
table[1,5] <- 1
table[3,5] <- 4

  # Plot
graph <- ggplot(table, aes(x="", y=percent, fill=`Comment évaluez vous votre participation aux canaux de discussion ?`)) +
  geom_bar(stat="identity", width=1, color="white", size = 2) +
  coord_polar("y", start=0) +
  facet_wrap(~is_contrib, strip.position="bottom") +
  scale_fill_manual(values = c("#21468d", "#74a466", "#fecf5d", "#f38337")) +
  labs(title = "Participation aux canaux de discussion", subtitle = "\nComment évaluez vous votre participation aux canaux de discussion ?") +
  theme_classic() + 
  theme(legend.position="right",
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Montserrat", size = 12),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.title = element_text(face = "bold", size = 21)) +
  guides(fill = guide_legend(reverse = T, title = " ")) +
  geom_text(aes(y = ypos, x = 1.67, label = paste(round(percent,0), "%", sep="")), color = "#333333", size=3, check_overlap = T)
graph
saving_plot_petit(graph, "19_pie1")





######---


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(is_contrib, `Que pensez-vous des pages d'aide ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% arrange(Freq)

  # Neutralisation des réponses
table$`Que pensez-vous des pages d'aide ?` <- table$`Que pensez-vous des pages d'aide ?` %>% 
          str_replace_all(c("[^[:alnum:]]" = " ",   #remove "??" characters
                            "Il existe des pages d aide" = "N'a pas connaissance des pages d'aide",
                            "Je n en ai jamais eu besoin" = "N'en a pas besoin",  
                            "Je ne les trouve pas utiles" = "N'en a pas besoin",
                            "Je pense qu elles pourraient être améliorées" = "Elles pourraient être améliorées",
                            "Elles sont trop complexes" = "Elles pourraient être améliorées",
                            "Elles sont suffisamment claires" = "Elles sont claires"
                            )) 

  # Ordonner les réponses
table$`Que pensez-vous des pages d'aide ?` <- factor(table$`Que pensez-vous des pages d'aide ?`, order = TRUE, levels = c("N'a pas connaissance des pages d'aide   ", "N'en a pas besoin", "Elles pourraient être améliorées", "Elles sont claires"))

  # Regroupement des catégories : somme par groupe
table <- table %>% group_by(is_contrib, `Que pensez-vous des pages d'aide ?`) %>% mutate(Freq = sum(Freq)) %>% distinct() %>% 
  ungroup() %>% group_by(is_contrib) %>% 
  mutate(percent = Freq / sum(Freq) *100,
         is_contrib = case_when(is_contrib == 0 ~ "lecteurs",
                                is_contrib == 1 ~ "contributeurs"),
         is_contrib = factor(is_contrib, order = TRUE, levels = c("lecteurs", "contributeurs")))

  # Paramètres graphiques
table <- table %>% 
  arrange(desc(`Que pensez-vous des pages d'aide ?`)) %>%
  mutate(ypos = cumsum(percent) - 0.5*percent )

  # Plot
graph <- ggplot(table, aes(x="", y=percent, fill=`Que pensez-vous des pages d'aide ?`)) +
  geom_bar(stat="identity", width=1, color="white", size = 2) +
  coord_polar("y", start=0) +
  facet_wrap(~is_contrib, strip.position="bottom") +
  scale_fill_manual(values = c("#21468d", "#74a466", "#fecf5d", "#f38337")) +
  labs(title = "Avis sur les pages d'aide", subtitle = "\nQue pensez-vous des pages d'aide ?") +
  theme_classic() + 
  theme(legend.position="right",
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Montserrat", size = 12),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.title = element_text(face = "bold", size = 21)) +
  guides(fill = guide_legend(reverse = T, title = " ")) +
  geom_text(aes(y = ypos, x = 1.67, label = paste(round(percent,0), "%", sep="")), color = "#333333", size=3, check_overlap = T)
graph
saving_plot_petit(graph, "19_pie2")




######---


  # Table
table <- enquete_vikidia %>% group_by(is_contrib, `Comment évaluez vous votre participation aux canaux de discussion ?`, `Que pensez-vous des pages d'aide ?`) %>% summarise(n = n())%>% mutate(is_contrib = case_when(is_contrib == 0 ~ "lecteurs", is_contrib == 1 ~ "contributeurs"), is_contrib = factor(is_contrib, order = TRUE, levels = c("lecteurs", "contributeurs")))

  # Neutralisation des réponses
table$`Comment évaluez vous votre participation aux canaux de discussion ?` <- table$`Comment évaluez vous votre participation aux canaux de discussion ?` %>% 
          str_replace_all(c("[^[:alnum:]]" = " ",   #remove "??" characters
                            "Quels canaux de discussion" = "N'a pas connaissance des canaux",
                            "Je les connais mais je n y ai jamais participé" = "A connaissance sans y participer", 
                            "J ai déjà écrit un ou deux messages" = "Participe de temps à autre",
                            "Il m arrive de répondre aux messages" = "Participe de temps à autre",
                            "Je suis plutôt actif sur les canaux" = "Actif sur les canaux",
                            "Je suis très actif sur les canaux" = "Actif sur les canaux")) 
table$`Que pensez-vous des pages d'aide ?` <- table$`Que pensez-vous des pages d'aide ?` %>% 
          str_replace_all(c("[^[:alnum:]]" = " ",   #remove "??" characters
                            "Il existe des pages d aide" = "N'a pas connaissance des pages d'aide",
                            "Je n en ai jamais eu besoin" = "N'en a pas besoin",  
                            "Je ne les trouve pas utiles" = "N'en a pas besoin",
                            "Je pense qu elles pourraient être améliorées" = "Elles pourraient être améliorées",
                            "Elles sont trop complexes" = "Elles pourraient être améliorées",
                            "Elles sont suffisamment claires" = "Elles sont claires")) 

  # Ordonner les réponses
table$`Comment évaluez vous votre participation aux canaux de discussion ?` <- factor(table$`Comment évaluez vous votre participation aux canaux de discussion ?`, order = TRUE, levels = c("N'a pas connaissance des canaux   ", "A connaissance sans y participer", "Participe de temps à autre", "Actif sur les canaux"))
table$`Que pensez-vous des pages d'aide ?` <- factor(table$`Que pensez-vous des pages d'aide ?`, order = TRUE, levels = c("N'a pas connaissance des pages d'aide   ", "N'en a pas besoin", "Elles pourraient être améliorées", "Elles sont claires"))

  # Regroupement des catégories : somme par groupe
table <- table %>% group_by(`Comment évaluez vous votre participation aux canaux de discussion ?`, `Que pensez-vous des pages d'aide ?`, is_contrib) %>% mutate(n = sum(n)) %>% distinct()

  # Plot
graph <- ggballoonplot(table, x = "Que pensez-vous des pages d'aide ?", y = "Comment évaluez vous votre participation aux canaux de discussion ?", size = "n", fill = "is_contrib", color = "is_contrib", ggtheme = theme_bw()) + 
  facet_wrap(~is_contrib) +
  scale_fill_manual(values = c("#c898ae", "#dd4627")) +
  scale_color_manual(values = c("#c898ae", "#dd4627")) +
  theme(text = element_text(family = "Montserrat", size = 12),
        strip.text.x = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.title = element_text(face = "bold", size = 21),
        axis.title = element_text()) + #censé afficher xlab et ylab
  guides(size = guide_legend(reverse = T, title = "Nombre de répondants"), fill = "none", color = "none") +
  labs(x = "Évaluation des pages",
       y = "Participation aux canaux de discussion",
       title = "Avis sur les pages d'aide selon la participation aux 
canaux de discussion",
       subtitle = "\nComment évaluez vous votre participation aux canaux de discussion ?, \nQue pensez-vous des pages d'aide ?")
graph
saving_plot_custom(graph, "20_ballonplot", 10, 7)






# -------------- Comment se sent-on au sein de la communauté ?



######---

  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(is_contrib, `Vous sentez-vous suffisamment intégré à la communauté vikidienne ?`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% arrange(Freq) %>% 
  group_by(is_contrib) %>% mutate(total = sum(Freq), percent = round(Freq / total *100, 0)) %>% 
  filter(`Vous sentez-vous suffisamment intégré à la communauté vikidienne ?` == "Oui") %>% ungroup() %>% 
  mutate(is_contrib = str_replace_all(is_contrib, c("1" = "contributeurs", "0" = "lecteurs")))

  # Plot
graph <- ggplot(table, aes(x = is_contrib)) + 
  geom_bar(aes(y = 100), width = 0.3, stat = "identity", fill = "white", col = c("#dd4627", "#c898ae"), size = 3) +
  geom_bar(aes(y = percent), width = 0.3, stat = "identity", fill = c("#dd4627", "#c898ae"), col = c("#dd4627", "#c898ae")) +
  geom_text(aes(x = is_contrib, y = percent - 9), label = paste(table$percent, "%"),
            colour = "white", size = 8, fontface = "bold") +
  coord_flip() + labs(title = "Part des personnes se sentant intégrées à la communauté de Vikidia, 
selon s'ils contribuent ou pas à Vikidia", y = "Pourcentage", x = "", subtitle = "\nVous sentez-vous suffisamment intégré à la communauté vikidienne ?") +
  theme_classic() +
  theme(text = element_text(family = "Montserrat", size = 12),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
graph
saving_plot_moyen(graph, "21_gauge_integration")



######---

  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(is_contrib == 0 & `Vous sentez-vous suffisamment intégré à la communauté vikidienne ?` == "Oui") %>% 
  group_by(`Trouvez-vous les articles de Vikidia accessibles ?`) %>% summarise(n = n()) %>% 
  rename(`Accessibilité des articles` = `Trouvez-vous les articles de Vikidia accessibles ?`) %>% 
  mutate(percent = round(n / sum(n) *100, 0))

  # Neutralisation des réponses
table$`Accessibilité des articles` <- table$`Accessibilité des articles` %>% 
          str_replace_all(c("Pas du tout accessibles" = "Inexistante",
                            "Peu accessibles" = "Insuffisante",  # faible / basse ?
                            "Moyennement accessibles" = "Moyenne",
                            "Plutôt accessibles" = "Correcte",
                            "Très accessibles" = "Élevée"))

  # Ordonner les réponses
table$`Accessibilité des articles` <- factor(table$`Accessibilité des articles`, order = TRUE, 
                                            levels = c("Inexistante",
                                                       "Insuffisante",
                                                       "Moyenne",
                                                       "Correcte",
                                                       "Élevée"))

  # Plot
graph <- ggplot(table, aes(x = `Accessibilité des articles`, y = n)) +
  geom_col(aes(color = `Accessibilité des articles`, fill = `Accessibilité des articles`), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("white", "white", "white", "white", "white"))+
  scale_fill_manual(values = c("#D2DAE8", "#A6B5D1", "#7990BA", "#4D6BA3", "#21468d")) +
  labs(x = "Accessibilité des articles", y = "Nombre de répondants", title = "Évaluation de l'accessibilité des articles par les lecteurs 
se sentant integrés à la communauté",
       subtitle = "\nTrouvez-vous les articles de Vikidia accessibles ?") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  geom_text(aes(y = n + 6, x = `Accessibilité des articles`, label = paste(round(percent,0), "%", sep="")), color = "#333333", size=3, check_overlap = T)
graph
saving_plot_custom(graph, "22_bar_accessibilite", 9, 7)





######---

  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(is_contrib == 1 & `Vous sentez-vous suffisamment intégré à la communauté vikidienne ?` == "Oui") %>% 
  group_by(`Comment évaluez vous votre participation aux canaux de discussion ?`) %>% summarise(n = n())

  # Neutralisation des réponses
table$`Comment évaluez vous votre participation aux canaux de discussion ?` <- table$`Comment évaluez vous votre participation aux canaux de discussion ?` %>% 
          str_replace_all(c("[^[:alnum:]]" = " ",   #remove "??" characters
                            "Quels canaux de discussion" = "N'a pas connaissance",
                            "Je les connais mais je n y ai jamais participé" = "A connaissance sans y participer", 
                            "J ai déjà écrit un ou deux messages" = "Participe de temps à autre",
                            "Il m arrive de répondre aux messages" = "Participe de temps à autre",
                            "Je suis plutôt actif sur les canaux" = "Actif sur les canaux",
                            "Je suis très actif sur les canaux" = "Actif sur les canaux")) 

  # Ordonner les réponses
table$`Comment évaluez vous votre participation aux canaux de discussion ?` <- factor(table$`Comment évaluez vous votre participation aux canaux de discussion ?`, order = TRUE, levels = c("N'a pas connaissance   ", "A connaissance sans y participer", "Participe de temps à autre", "Actif sur les canaux"))

  # Regroupement des catégories : somme par groupe
table <- table %>% group_by(`Comment évaluez vous votre participation aux canaux de discussion ?`) %>% mutate(n = sum(n)) %>% distinct() %>% 
  ungroup() %>% mutate(percent = round(n / sum(n) *100, 0))

  # Plot
graph <- ggplot(table, aes(x = `Comment évaluez vous votre participation aux canaux de discussion ?`, y = n)) +
  geom_col(aes(color = `Comment évaluez vous votre participation aux canaux de discussion ?`, fill = `Comment évaluez vous votre participation aux canaux de discussion ?`), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("white", "white", "white", "white"))+
  scale_fill_manual(values = c("#E8ECF3", "#A6B5D1", "#637DAF", "#21468d")) +
  labs(x = "Participation aux canaux", y = "Nombre de répondants", title = "Participation des contributeurs se sentant integrés à la 
communauté, aux canaux de discussion",
       subtitle = "\nComment évaluez vous votre participation aux canaux de discussion ?") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  geom_text(aes(y = n + 1, x = `Comment évaluez vous votre participation aux canaux de discussion ?`, label = paste(round(percent,0), "%", sep="")), color = "#333333", size=3, check_overlap = T)
graph
saving_plot_custom(graph, "22_bar_canaux", 9, 7)




######---



  # Table de fréquences des réponses
table <- enquete_vikidia %>% filter(`Êtes vous ...` != "préfère ne pas le dire") %>% 
  group_by(`Comment avez-vous jugé votre accueil sur Vikidia ?`, `Êtes vous ...`) %>% 
  summarise(Freq = n()) %>% ungroup() %>% arrange(desc(Freq)) %>%
  group_by(`Êtes vous ...`) %>% mutate(percent = round(Freq / sum(Freq) *100, 0))

  # Ordonner les réponses
table$`Comment avez-vous jugé votre accueil sur Vikidia ?` <- factor(table$`Comment avez-vous jugé votre accueil sur Vikidia ?`, order = TRUE, 
                                                                     levels = c("Laisse à désirer",
                                                                                "Moyen",
                                                                                "Bon",
                                                                                "Excellent"))

  # Plot
graph <- table %>% 
  ggplot(aes(x=`Comment avez-vous jugé votre accueil sur Vikidia ?`, y=Freq, fill = `Êtes vous ...`, group = `Êtes vous ...`))+
    geom_bar(stat="identity", position = "dodge", width=.6, col = "white", size = 2) +
    coord_flip() +
    labs(x = "", y = "Nombre de répondants", title = "Évaluation de l'accueil sur Vikidia", 
       subtitle = "\nComment avez vous jugé votre accueil sur Vikidia ?") +
    theme_classic() +
    scale_fill_manual(values = c('#fecf5d', '#74a466')) +
    theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    guides(fill = guide_legend(title = "Genre", reverse = T), color = "none") +
    geom_text(aes(y = Freq + 5, x = `Comment avez-vous jugé votre accueil sur Vikidia ?`, 
                  label = paste(round(percent,0), "%", sep="")), 
              color = "#333333", size=3, check_overlap = T,
              position = position_dodge(width = .5))
graph
saving_plot_petit(graph, "23_bar_accueil")




# -------------- Quels outils pour animer la communauté ?


 # Table de fréquences des réponses
disc <- enquete_vikidia %>% filter(!grepl(c("ucun|jamais|rien|pas|nul|zero|sans"), `Veuillez préciser...92`), !is.na(`Discord`), `Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% summarise(n = n()) %>% mutate(outils = "Discord")
bav <- enquete_vikidia %>% filter(!grepl(c("ucun|jamais|rien|pas|nul|zero|sans"), `Veuillez préciser...92`), !is.na(`Les Bavardages`), `Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% summarise(n = n()) %>% mutate(outils = "Bavardages")
irc <- enquete_vikidia %>% filter(!grepl(c("ucun|jamais|rien|pas|nul|zero|sans"), `Veuillez préciser...92`), !is.na(`IRC`), `Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% summarise(n = n()) %>% mutate(outils = "IRC")
pag <- enquete_vikidia %>% filter(!grepl(c("ucun|jamais|rien|pas|nul|zero|sans"), `Veuillez préciser...92`), !is.na(`Les pages de discussion`), `Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% summarise(n = n()) %>% mutate(outils = "Pages de discussion")
aut <- enquete_vikidia %>% filter(!grepl(c("ucun|jamais|rien|pas|nul|zero|sans"), `Veuillez préciser...92`), !is.na(`Autres canaux de discussion`), `Êtes vous ...` != "préfère ne pas le dire") %>% group_by(`Quel âge avez-vous ?`, `Êtes vous ...`) %>% summarise(n = n()) %>% mutate(outils = "Autres canaux")
    # merge
table <- rbind(disc,bav,irc,pag,aut)

  # Plot
graph <- table %>% filter(`Quel âge avez-vous ?` != "Préfère ne pas le dire") %>% 
  ggplot(aes(x = outils, y = n)) +
  geom_col(aes(color = `Quel âge avez-vous ?`, fill = `Quel âge avez-vous ?`), position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~`Êtes vous ...`) +
  scale_color_manual(values = c("white", "white", "white", "white", "white"))+
  scale_fill_manual(values = c("#21468d", "#74a466", "#fecf5d", "#f38337", "#da4729")) +
  labs(x = "Canaux de discussion", y = "Nombre de réponses", title = "Canaux de discussion des répondants selon l'âge et le genre",
       subtitle = "\nSur quel(s) canal(ux) de discussions êtes-vous actif.ve ?") +
  theme_bw() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill = "transparent"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(fill = guide_legend(title = "Âge du répondant", reverse = F), color = "none")
graph
saving_plot_grand(graph, "24_grouped_canaux")





# -------------- Quelle confiance est accordée à Vikidia ?



######---


  # Table de fréquences des réponses
table <- enquete_vikidia %>% 
  filter(is_contrib == 0) %>% select(63:68) %>% 
  mutate_at(vars(`La qualité des articles`, `L'accessibilité des sujets traités`, `La qualité et la pertinence des illustrations`, `La diversité des sujets traités`, `La qualité synthaxique et typographique`, `Le projet Vikidia en général`), ~ str_replace_all(., c("Très bonne" = "5", "Plutôt bonne" = "4", "Moyenne" = "3", "Plutôt mauvaise" = "2", "Très mauvaise" = "1")))
table[,1:6] <- lapply(table[,1:6], as.numeric)
table <- table %>% ungroup() %>% summarise_all(funs(mean(.))) %>% #mutate(total = "total", .before = `La qualité des articles`) %>% 
  rename(`La qualité syntaxique \net typographique` = `La qualité synthaxique et typographique`,
         `La qualité et la pertinence \ndes illustrations` = `La qualité et la pertinence des illustrations`)

  # Plot
graph <- table %>% 
  ggradar(values.radar = c("1", "3", "5"),
          grid.min = 1, grid.mid = 3, grid.max = 5,
          # Polygones
          group.line.width = 1, 
          group.point.size = 3,
          group.colours = "#21468d",
          # Arrière-plan et lignes de grille
          background.circle.colour = "white",
          gridline.mid.colour = "grey") +
  xlim(-15,15) +
  labs(title = "Évaluation de Vikidia par ses lecteurs",
       subtitle = "\nVeuillez noter votre satisfaction sur les sujets suivants") +
  ggplot2::annotate("text", x = 10, y = -1, label = "\ 1 : Très mauvaise \n5 : Très bonne", col = "#666666") +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15))
graph
saving_plot_petit(graph, "25_radar_eval")



######---



  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`) %>% summarise(Freq = n()) %>% rename(`Fiabilités des articles` = `Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`) %>% mutate(percent = round(Freq / sum(Freq) *100, 0))

  # Neutralisation des réponses
table$`Fiabilités des articles` <- table$`Fiabilités des articles` %>% 
          str_replace_all(c("Je me méfie des informations que je trouve sur Vikidia" = "Méfiance vis-à-vis du contenu", 
                            "Les articles de Vikidia sont plutôt fiables" = "Plutôt fiables",
                            "Les articles de Vikidia sont tout à fait fiables" = "Tout à fait fiables",  
                            "Les informations trouvées sur Vikidia ne sont pas toujours fiables" = "Pas toujours fiables",
                            "Les informations trouvées sur Vikidia sont à prendre avec précaution" = "À prendre avec précaution")) 

  # Ordonner les réponses
table$`Fiabilités des articles` <- factor(table$`Fiabilités des articles`, order = TRUE, levels = c("Méfiance vis-à-vis du contenu", "À prendre avec précaution", "Pas toujours fiables", "Plutôt fiables", "Tout à fait fiables"))

  # Plot
graph <- ggplot(table, aes(y = Freq)) +
  geom_col(aes(x = `Fiabilités des articles`, fill = `Fiabilités des articles`), position = position_dodge(0.8), width = 0.7, col = "white") +
  scale_fill_manual(values = c("#D2DAE8", "#A6B5D1", "#7990BA", "#4D6BA3", "#21468d")) +
  labs(x = "Fiabilités des articles", y = "Nombre de répondants", title = "Évaluation de la fiabilité des articles",
       subtitle = "\nQuel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  guides(fill = guide_legend(title = " ", reverse = F), color = "none") +
  geom_text(aes(y = Freq + 10, x = `Fiabilités des articles`, label = paste(percent, "%", sep="")), color = "#333333", size=3, check_overlap = T) +
  coord_flip()
graph
saving_plot_custom(graph, "27_grouped_fiabilite", 11, 6.5)




######---


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(activite) %>% select(60) %>% na.omit() %>% 
  mutate(`Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?` = str_replace_all(`Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`, c("Les articles de Vikidia sont tout à fait fiables" = "5", "Les articles de Vikidia sont plutôt fiables" = "4", "Les informations trouvées sur Vikidia ne sont pas toujours fiables" = "3", "Les informations trouvées sur Vikidia sont à prendre avec précaution" = "2", "Je me méfie des informations que je trouve sur Vikidia" = "1"))) %>% rename(Moy = `Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`)
table$Moy <- as.numeric(table$Moy)
table <- table %>% group_by(activite) %>% summarise_all(funs(mean(.))) %>% ungroup() %>% arrange(desc(Moy))

  # Plot
graph <- table %>% 
  ggplot(aes(x=activite, y=Moy))+
    geom_segment(aes(xend=activite, yend=0), size = .7, color="#21468d") +
    geom_point(size=6, color="#21468d") +
    geom_text(aes(x = activite, y = Moy+0.2, label = round(Moy, 1)), color = "#333333", size=3) +
    coord_flip() +
    labs(x = "", y = "Note", title = "Évaluation de la fiabilité des articles selon l'activité",
       subtitle = "\nQuel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?") +
    lims(y = c(0, 5)) +
    theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
graph
saving_plot_custom(graph, "26_lollipop1", 10, 6.7)




######---


  # Table de fréquences des réponses
table <- enquete_vikidia %>% group_by(`Quel âge avez-vous ?`) %>% select(60) %>% na.omit() %>% 
  mutate(`Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?` = str_replace_all(`Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`, c("Les articles de Vikidia sont tout à fait fiables" = "5", "Les articles de Vikidia sont plutôt fiables" = "4", "Les informations trouvées sur Vikidia ne sont pas toujours fiables" = "3", "Les informations trouvées sur Vikidia sont à prendre avec précaution" = "2", "Je me méfie des informations que je trouve sur Vikidia" = "1"))) %>% rename(Moy = `Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`)
table$Moy <- as.numeric(table$Moy)
table <- table %>% group_by(`Quel âge avez-vous ?`) %>% summarise_all(funs(mean(.))) %>% ungroup() %>% arrange(desc(Moy))

  # Plot
graph <- table %>% filter(`Quel âge avez-vous ?` != "Préfère ne pas le dire") %>% 
  ggplot(aes(x=`Quel âge avez-vous ?`, y=Moy))+
    geom_segment(aes(xend=`Quel âge avez-vous ?`, yend=0), size = .7, color="#21468d") +
    geom_point(size=6, color="#21468d") +
    geom_text(aes(x = `Quel âge avez-vous ?`, y = Moy+0.2, label = round(Moy, 1)), color = "#333333", size=3) +
    coord_flip() +
    labs(x = "", y = "Note", title = "Évaluation de la fiabilité des articles selon l'âge",
       subtitle = "\nQuel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?") +
    lims(y = c(0, 5)) +
    theme_classic() +
  theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(face = "bold", size = 21),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
graph
saving_plot_custom(graph, "26_lollipop2", 10, 6.7)


