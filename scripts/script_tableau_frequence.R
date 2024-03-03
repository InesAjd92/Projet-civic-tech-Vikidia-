
library(tidyverse)


read.csv("enquete_vikidia.csv")


# AGRÉGATION : activité + niveau d'étude
graph_fiabilité <- enquete_vikidia %>% mutate(activite = case_when(`Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "Collège" ~ "Élève au collège",
                                                                   `Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "École primaire (du CP au CM2)" ~ "Élève en école primaire",
                                                                   `Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "Études supérieures" ~ "Étudiant",
                                                                   `Vous êtes ...` == "Étudiant, élève" & `Veuillez préciser votre niveau d'étude` == "Lycée" ~ "Élève au lycée",
                                                                   TRUE ~ `Vous êtes ...`))

data_fiabilité <- enquete_vikidia %>% 
  select(13,60)






# Table de fréquences des réponses pour chaque catégorie de "Vous êtes"
table_vous_etes <- enquete_vikidia %>%
  group_by(`Vous êtes ...`, `Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`) %>% 
  summarise(Freq = n()) %>% 
  rename(`Fiabilité des articles` = `Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`) %>% 
  mutate(percent = round(Freq / sum(Freq) * 100, 0))

write.csv(table_vous_etes, "table_activ_confiance.csv")


# Table de fréquences des réponses
table2 <- enquete_vikidia %>%
  group_by(`Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`) %>% 
  summarise(Freq = n()) %>% 
  rename(`Fiabilités des articles` = `Quel est votre niveau de confiance en ce qui concerne la fiabilité des articles de Vikidia ?`) %>% 
  mutate(percent = round(Freq / sum(Freq) *100, 0))


write.csv(table2, "table_freq_activ_confiance.csv")



# Neutralisation des réponses
table$`Fiabilités des articles` <- table$`Fiabilités des articles` %>% 
  str_replace_all(c("Je me méfie des informations que je trouve sur Vikidia" = "Méfiance vis-à-vis du contenu", 
                    "Les articles de Vikidia sont plutôt fiables" = "Plutôt fiables",
                    "Les articles de Vikidia sont tout à fait fiables" = "Tout à fait fiables",  
                    "Les informations trouvées sur Vikidia ne sont pas toujours fiables" = "Pas toujours fiables",
                    "Les informations trouvées sur Vikidia sont à prendre avec précaution" = "À prendre avec précaution")) 

# Ordonner les réponses
table$`Fiabilités des articles` <- factor(table$`Fiabilités des articles`, order = TRUE, levels = c("Méfiance vis-à-vis du contenu", "À prendre avec précaution", "Pas toujours fiables", "Plutôt fiables", "Tout à fait fiables"))

# évolution du nombre de contributeurs au fil des ans ? 


# EXTRACTION : champ libre 'année'

annee <- as.data.frame(table(enquete_vikidia$`En quelle année avez-vous rejoint la communauté Vikidienne ?`))
annee_contrib <- enquete_vikidia %>% 
  select("En quelle année avez-vous rejoint la communauté Vikidienne ?", "Quel est votre degré de contribution ?")
  
write.csv(annee_contrib, "annee_contrib.csv")

annee_contrib <- annee_contrib$`En quelle année avez-vous rejoint la communauté Vikidienne ?` <- gsub(".*([0-9]{4}).*", "\\1", annee_contrib$"En quelle année avez-vous rejoint la communauté Vikidienne ?")

# Convertir en format numérique
annee_contrib$"En quelle année avez-vous rejoint la communauté Vikidienne ?" <- as.numeric(annee_contrib$"En quelle année avez-vous rejoint la communauté Vikidienne ?")


  
  mutate(annee = str_extract(`En quelle année avez-vous rejoint la communauté Vikidienne ?`, "(1|2)\\d{3}"))
