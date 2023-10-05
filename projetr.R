getwd()
# Analyse sur dataset City
data<-read.csv('City.csv')
head(data)
View(data)
class(data)
summary(data)
data$Users <- as.numeric(gsub("," , "" , data$Users))
head(data)
summary(data)
data$Population <- as.numeric(gsub("," , "" , data$Population))
summary(data)
pop<-c()
user<-c(data$Users)
data$percentage_users<-(data$Users/data$Population)*100
print(data$percentage_users)
head(data)

top <- data[order(data$percentage_users, decreasing = TRUE), ]
install.packages("ggplot2")
library(ggplot2)
ggplot(top, aes(x = percentage_users, y = reorder(City, percentage_users), fill = City)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(percentage_users, 1), "%")), hjust = 0.5, vjust = -0.2) +
  labs(title = "Pourcentage d'Utilisateurs par Ville",
       x = "percentage_users",
       y = "City") +
  theme_minimal() +
  scale_fill_discrete(name = "Ville") +
  guides(fill = guide_legend(title = "Ville")) +
  scale_x_continuous(labels = scales::percent_format(scale = 1))

head(data)



# Analyse sur dataset Transaction
data_T<-read.csv('Transaction_ID.csv')
head(data_T)
View(data_T)
class(data_T)
summary(data_T)
# Charger la bibliothèque ggplot2 si elle n'est pas déjà chargée
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
# Vous pouvez compter le nombre de paiements par mode de paiement
payment_counts <- table(data_T$Payment_Mode)

# Créer un DataFrame à partir des comptages
payment_data <- data.frame(Payment_Mode = names(payment_counts), Count = as.numeric(payment_counts))
palette_couleurs <- rainbow(length(unique(payment_data$Payment_Mode)))
# Créer l'histogramme avec des couleurs différentes
histogram <- ggplot(payment_data, aes(x = Payment_Mode, y = Count, fill = Payment_Mode)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +  # Ajouter les étiquettes de nombre
  labs(title = "Histogramme du Nombre de Paiements par Mode de Paiement",
       x = "Mode de Paiement",
       y = "Nombre de Paiements") +
  scale_fill_manual(values = palette_couleurs) +  # Utiliser la palette de couleurs
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Faire pivoter les étiquettes de l'axe x pour une meilleure lisibilité

print(histogram)


# Analyse sur dataset Customer
data_C<-read.csv('Customer_ID.csv')
head(data_C)
View(data_C)
class(data_C)
summary(data_C)
# Compter le nombre de clients dans le DataFrame data_C
nombre_de_clients <- nrow(data_C)

# Afficher le nombre de clients
cat("Le nombre de clients est :", nombre_de_clients, "\n")
# Compter le nombre de clients dans le DataFrame data_C
nombre_de_clients <- length(data_C$Customer.ID)

# Afficher le nombre de clients
cat("Le nombre de clients est :", nombre_de_clients, "\n")

# Charger la bibliothèque ggplot2 si elle n'est pas déjà chargée
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
# Créer une nouvelle colonne "Age_Category" en découpant l'âge en deux catégories avec des noms significatifs
data_C$Age_Category <- cut(data_C$Age, 
                           breaks = c(0, 40, max(data_C$Age)),  # Plages d'âge : 0-40, 41+
                           labels = c("Adultes", "Personnes âgées"),  # Noms significatifs
                           right = FALSE)
head(data_C)

# Compter le nombre de clients dans chaque catégorie d'âge
age_counts <- table(data_C$Age_Category)
age_data <- data.frame(Age_Category = names(age_counts), Count = as.numeric(age_counts))
age_data$Percentage <- age_data$Count / sum(age_data$Count) * 100
pie_chart <- ggplot(age_data, aes(x = "", y = Count, fill = Age_Category)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste(Age_Category, "\n(", round(Percentage, 1), "%)", sep = "")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Répartition par Catégorie d'Âge") +
  theme_void() +
  scale_fill_manual(values = c("Adultes" = "lightgreen", "Personnes âgées" = "salmon"))  # Personnaliser les couleurs ici
print(pie_chart)


# Charger la bibliothèque dplyr
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# Exécuter à nouveau le code pour créer summary_data
summary_data <- data_C %>%
  group_by(Age_Category, Gender) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


# Créer un histogramme des genres par plage d'âge avec nombre et pourcentage
histogram <- ggplot(summary_data, aes(x = Age_Category, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste(Count, "(", round(Percentage, 1), "%)", sep = "")), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Répartition des Genres par Plage d'Âge",
       x = "Plage d'Âge",
       y = "Nombre de Clients",
       fill = "Genre") +
  theme_minimal()

# Afficher l'histogramme
print(histogram)


# Charger la bibliothèque ggplot2 si elle n'est pas déjà chargée
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Trier le DataFrame par revenu en ordre décroissant
data_C_sorted <- data_C[order(-data_C$Income..USD.Month.), ]

# Sélectionner les 10 premières lignes du DataFrame trié
data_C_top10 <- head(data_C_sorted, 10)

# Créer un graphique à barres du nombre de clients en fonction de leur revenu (10 premiers)
barplot <- ggplot(data_C_top10, aes(x = reorder(Customer.ID, -Income..USD.Month.), y = Income..USD.Month., fill = Income..USD.Month.)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Income..USD.Month.), vjust = -0.2, size = 3) +  # Ajouter les étiquettes de revenu
  labs(title = "10 Premiers Clients en Fonction du Revenu",
       x = "Client",
       y = "Revenu") +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +  # Dégradé de couleurs basé sur le revenu
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les étiquettes de l'axe x pour une meilleure lisibilité

print(barplot)




# Charger la bibliothèque dplyr si elle n'est pas déjà chargée
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# Calculer la somme totale du revenu
total_income <- data_C %>%
  summarise(Total_Income = sum(`Income..USD.Month.`))

# Créer une table croisée entre le genre (Male, Female) et la somme du revenu
crosstab <- data_C %>%
  mutate(Gender = ifelse(Gender == "Male", "Male", "Female")) %>%  # Regrouper le genre en deux classes
  group_by(Gender) %>%
  summarise(Sum_Income = sum(`Income..USD.Month.`)) %>%
  mutate(Percentage = (Sum_Income / total_income$Total_Income) * 100)  # Calculer le pourcentage par rapport au total global

# Formater le pourcentage en ajoutant '%' à la valeur
crosstab$Percentage <- paste0(crosstab$Percentage, '%')
if (!require(knitr)) {
  install.packages("knitr")
  library(knitr)
}
kable(crosstab)
# Afficher la table croisée
print(crosstab)


# Charger la bibliothèque dplyr si elle n'est pas déjà chargée
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# Calculer la somme totale du revenu
total_income <- data_C %>%
  summarise(Total_Income = sum(`Income..USD.Month.`))

# Créer une table croisée entre la plage d'âge et la somme du revenu
crosstab_age <- data_C %>%
  group_by(Age_Category) %>%
  summarise(Sum_Income = sum(`Income..USD.Month.`)) %>%
  mutate(Percentage = (Sum_Income / total_income$Total_Income) * 100)  # Calculer le pourcentage par rapport au total global

# Formater le pourcentage en ajoutant '%' à la valeur
crosstab_age$Percentage <- paste0(crosstab_age$Percentage, '%')

# Afficher la table croisée
if (!require(knitr)) {
  install.packages("knitr")
  library(knitr)
}
kable(crosstab_age)
print(crosstab_age)


# Analyse sur dataset cab_data
data_Cab<-read.csv('Cab_Data-1.csv')
head(data_Cab)
View(data_Cab)
class(data_Cab)
summary(data_Cab)

date_originale <- as.Date("1900-01-01")
data_Cab$date_reel <- date_originale + data_Cab$Date.of.Travel
data_Cab$date_reel <- format(data_Cab$date_reel, "%d/%m/%Y")
head(data_Cab)


library(dplyr)


# Calcul du revenu pour chaque transaction
data_Cab <- data_Cab %>%
  mutate(Revenue = Price.Charged - Cost.of.Trip)

# Calcul du revenu total de Pink Cab
pink_cab_revenue <- data_Cab %>%
  filter(Company == "Pink Cab") %>%
  summarise(Total_Revenue = sum(Revenue))

# Calcul du revenu total de Yellow Cab
yellow_cab_revenue <- data_Cab %>%
  filter(Company == "Yellow Cab") %>%
  summarise(Total_Revenue = sum(Revenue))




# Somme des revenus totaux des deux sociétés
total_revenue <- pink_cab_revenue$Total_Revenue + yellow_cab_revenue$Total_Revenue

# Calcul du pourcentage de Pink Cab par rapport au total
percentage_pink_cab <- (pink_cab_revenue$Total_Revenue / total_revenue) * 100

# Calcul du pourcentage de Yellow Cab par rapport au total
percentage_yellow_cab <- (yellow_cab_revenue$Total_Revenue / total_revenue) * 100

# Affichage des résultats
cat("Revenu total de Pink Cab:", pink_cab_revenue$Total_Revenue, "\n")
cat("Revenu total de Yellow Cab:", yellow_cab_revenue$Total_Revenue, "\n")
cat("Revenu total combiné des deux sociétés:", total_revenue, "\n")
cat("Pourcentage de Pink Cab par rapport au total:", percentage_pink_cab, "%\n")
cat("Pourcentage de Yellow Cab par rapport au total:", percentage_yellow_cab, "%\n")


library(dplyr)
library(ggplot2)

# Convertir la colonne date_reel en objet Date si elle ne l'est pas déjà
data_Cab$date_reel <- as.Date(data_Cab$date_reel, format = "%d/%m/%Y")

# Extraire l'année de la colonne date_reel
data_Cab$Year <- lubridate::year(data_Cab$date_reel)

# Regrouper les données par année et par société, puis calculer le revenu total
revenues_by_year <- data_Cab %>%
  group_by(Year, Company) %>%
  summarise(Total_Revenue = sum(Revenue))

# Créer un graphique de barres pour visualiser les revenus par année et par société
gg <- ggplot(revenues_by_year, aes(x = Year, y = Total_Revenue, fill = Company)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Revenu par année et par société",
       x = "Année",
       y = "Revenu total",
       fill = "Société") +
  theme_minimal()

# Ajouter le nombre de revenus sur les barres
gg + geom_text(aes(label = Total_Revenue), position = position_dodge(width = 1), vjust = -0.5)




library(dplyr)
library(ggplot2)

# Convertir la colonne date_reel en objet Date si elle ne l'est pas déjà
data_Cab$date_reel <- as.Date(data_Cab$date_reel, format = "%d/%m/%Y")

# Extraire l'année de la colonne date_reel
data_Cab$Year <- lubridate::year(data_Cab$date_reel)

# Regrouper les données par année et par société, puis calculer le revenu total
revenues_by_year <- data_Cab %>%
  group_by(Year, Company) %>%
  summarise(Total_Revenue = sum(Revenue))

# Créer un graphique en courbes pour visualiser l'évolution des revenus par année et par société
ggplot(revenues_by_year, aes(x = Year, y = Total_Revenue, color = Company, group = Company)) +
  geom_line() +
  labs(title = "Évolution des revenus par année et par société",
       x = "Année",
       y = "Revenu total",
       color = "Société") +
  theme_minimal()


# Étape 1 : Calcul du nombre total de trajets par société
total_trips <- data_Cab %>%
  group_by(Company) %>%
  summarise(Num_Trips = sum(KM.Travelled))

# Étape 2 : Calcul du pourcentage de chaque société
total_trips <- total_trips %>%
  mutate(Percentage = (Num_Trips / sum(Num_Trips)) * 100)
# Étape 3 : Création du pie chart
ggplot(total_trips, aes(x = "", y = Num_Trips, fill = Company)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste(Company, "\n", round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des trajets par société",
       fill = "Société") +
  theme_void()

library(dplyr)


# Étape 1 : Calcul du revenu total par société
total_revenue_by_company <- data_Cab %>%
  group_by(Company) %>%
  summarise(Total_Revenue = sum(Revenue))

# Étape 2 : Calcul du nombre total de trajets par société
total_trips_by_company <- data_Cab %>%
  group_by(Company) %>%
  summarise(Num_Trips = sum(KM.Travelled))

# Étape 3 : Calcul du revenu moyen par trajet pour chaque société sous forme de nombre
average_revenue_per_trip <- total_revenue_by_company$Total_Revenue / total_trips_by_company$Num_Trips

# Afficher le revenu moyen par trajet pour chaque société
result <- data.frame(Company = total_revenue_by_company$Company, Average_Revenue_Per_Trip = average_revenue_per_trip)
print(result)

library(dplyr)


# Jointure entre data_C et data_T par Customer.ID
data_join1 <- inner_join(data_C, data_T, by = "Customer.ID")

# Jointure entre data_join1 et data_Cab par Transaction.ID
data_join_all <- inner_join(data_join1, data_Cab, by = "Transaction.ID")
head(data_join_all)
View(data_join_all)

library(dplyr)

# Identification des clients actifs dans data_join_all
active_customers <- data_join_all %>%
  group_by(Company, Customer.ID) %>%
  summarise(Num_Transactions = n_distinct(Transaction.ID)) %>%
  filter(Num_Transactions > 1)

# Calcul du nombre de clients actifs par société
active_customers_by_company <- active_customers %>%
  group_by(Company) %>%
  summarise(Num_Active_Customers = n_distinct(Customer.ID))
if (!require(knitr)) {
  install.packages("knitr")
  library(knitr)
}
kable(active_customers_by_company)


library(dplyr)

# Calculer le nombre de transactions par mode de paiement et société
payment_summary <- data_join_all %>%
  group_by(Company, Payment_Mode) %>%
  summarise(nombre_Transactions = n())

# Trouver le mode de paiement le plus fréquent pour chaque société
most_common_payment <- payment_summary %>%
  group_by(Company) %>%
  filter(nombre_Transactions == max(nombre_Transactions))

# Afficher les modes de paiement les plus fréquents par société

if (!require(knitr)) {
  install.packages("knitr")
  library(knitr)
}
kable(most_common_payment)

library(ggplot2)
library(dplyr)

# Calculer les revenus par ville
revenues_by_city <- data_join_all %>%
  group_by(City) %>%
  summarise(Total_Revenue = sum(Revenue))

# Calculer le pourcentage par rapport au revenu total
revenues_by_city <- revenues_by_city %>%
  mutate(Percentage = Total_Revenue / sum(Total_Revenue) * 100)

# Sélectionner les 7 premières villes par revenus
top_cities <- revenues_by_city %>%
  top_n(7, wt = Total_Revenue)


# Créer un bar plot vertical
bar_plot <- ggplot(top_cities, aes(x = reorder(City, -Total_Revenue), y = Total_Revenue)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste(Total_Revenue, "($", round(Percentage, 1), "%)")), vjust = -0.2, size = 3) +  # Ajouter le nombre de revenus et le pourcentage
  labs(title = "Revenus par Ville (Top 7)",
       x = "Ville",
       y = "Revenus Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Faire pivoter les étiquettes de l'axe x pour une meilleure lisibilité

# Afficher le bar plot
print(bar_plot)

library(ggplot2)
library(dplyr)

# Calculer les kilomètres parcourus par ville
km_by_city <- data_join_all %>%
  group_by(City) %>%
  summarise(Total_KM = sum(KM.Travelled))

# Trier les villes par ordre décroissant de kilomètres parcourus et sélectionner les 7 premières
top_cities <- km_by_city %>%
  arrange(desc(Total_KM)) %>%
  head(7)

# Calculer le pourcentage par rapport au total des kilomètres parcourus
total_km <- sum(top_cities$Total_KM)
top_cities <- top_cities %>%
  mutate(Percentage = Total_KM / total_km * 100)

# Créer un bar plot vertical
bar_plot <- ggplot(top_cities, aes(x = reorder(City, -Total_KM), y = Total_KM)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste(Total_KM, " km (", round(Percentage, 1), "%)")), vjust = -0.2, size = 3) +  # Ajouter le nombre de kilomètres et le pourcentage
  labs(title = "Top 7 des Villes par Kilomètres Parcourus",
       x = "Ville",
       y = "Kilomètres Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Faire pivoter les étiquettes de l'axe x pour une meilleure lisibilité

# Afficher le bar plot
print(bar_plot)

library(ggplot2)
library(dplyr)

# Calculer les revenus par ville par société
revenues_by_city_company <- data_join_all %>%
  group_by(City, Company) %>%
  summarise(Total_Revenue = sum(Revenue))

# Trier les villes par ordre décroissant de revenus pour chaque société et sélectionner les 7 premières pour chaque société
top_cities_by_company <- revenues_by_city_company %>%
  arrange(Company, desc(Total_Revenue)) %>%
  group_by(Company) %>%
  slice_head(n = 7)

# Calculer le total des revenus par société
total_revenues <- top_cities_by_company %>%
  group_by(Company) %>%
  summarise(Total_Revenue = sum(Total_Revenue))

# Rejoindre les données des villes les plus performantes avec les totaux de revenus
top_cities_by_company <- top_cities_by_company %>%
  left_join(total_revenues, by = "Company") %>%
  mutate(Percentage = Total_Revenue.x / Total_Revenue.y * 100)
top_cities_by_company
# Créer un bar plot vertical
bar_plot <- ggplot(top_cities_by_company, aes(x = reorder(City, -Total_Revenue.x), y = Total_Revenue.x, fill = Company)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste(format(Total_Revenue.x, big.mark = ",", scientific = FALSE), " ($", round(Percentage, 1), "%)")), 
            position = position_dodge(0.9), vjust = -0.2, size = 3) +  # Ajouter le nombre de revenus et le pourcentage
  labs(title = "Top 7 des Villes par Revenus par Société",
       x = "Ville",
       y = "Revenus Total",
       fill = "Société") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Faire pivoter les étiquettes de l'axe x pour une meilleure lisibilité

# Afficher le bar plot
print(bar_plot)
