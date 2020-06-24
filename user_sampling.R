library(stringr)
library(maps)
library(qdapRegex)
library(dplyr)
library(ggplot2)
library(tm)
library(qdap)
library(wordcloud)
library(rtweet) 
library(httpuv)
library(reshape)
library(tidyverse)
library(tidyr)
library(data.table)
library(igraph)
library(lubridate)

threshold <- 89

Tweet_df_selected <- read.cscv ("C:\\ path ... ")
users_Tweet_df <- read.cscv ("C:\\ path ... ")

active_user <- Tweet_df_selected%>%  
  group_by(screen_name)%>% 
  count(screen_name)
active_user <- active_user %>% 
  arrange(desc(n)) 



active_user <- active_user %>% select (screen_name, n) 
active_user[is.na(active_user)] <- 0
active_user %>% mutate_all(~replace(., is.na(.), 0)) 
active_user <- active_user %>% filter(n>threshold) 


active_user<- active_user%>% left_join(users_Tweet_df, by= "screen_name")  
active_user <- active_user %>% select (screen_name, n, followers_count)
active_user <- active_user %>% arrange(desc(n)) 

write.csv(active_user, "C:\\path\\active_user.csv") 

active_connected_user <- active_user %>% filter( !is.na(followers_count)) 
active_connected_user_filtered <- active_connected_user %>% filter (followers_count>threshold) 

active_connected_user_filtered <- unique(active_connected_user_filtered) 

sampled_dataset<- active_connected_user_filtered
write.csv(sampled_dataset, "C:\\path\\sampled_dataset.csv")

network<- network_data(sampled_dataset, "retweet,mention,reply")
network
attr(network, "idsn") 
if (requireNamespace("igraph", quietly = TRUE)) {
  net_graph <- network_graph(sampled_dataset)
  plot(net_graph)
}

write.graph(net_graph, "C:\\path...\\netgraph.csv", "edgelist")

########### 

Sampled_users_italy<- sampled_dataset %>% filter (location == "Italy" | location == "Roma,Lazio" | location == "provincia di Gardaland (cit.)" | location== "Livorno, Tuscany" | location == "Roma - Italia" | location == "Genova" | location == "Bologna,Emilia Romagna" | location == "Rome, Italy" | location == "Roma" | location == "Trieste" | location == "Milano" | location == "Palermo" | location == "Italia" | location == "San Gimignano, Siena" | location == "Milano, Lombardia" | location == "Napoli, Campania" | location == "Italy" | location == "Roma, Lazio" | location == "Siracusa" | location == "Milan, Lombardy" | location == "Campania" | location == "Verona, Veneto") 
Users_Italy <- Sampled_users_italy %>% select(screen_name)

Sampled_users_china <- sampled_dataset %>% filter (location == "Beijing, China" | location == "Headquartered in Beijing, PRC" | location == "Shanghai" | location== "NY/Delhi/Rio/Beijing/Nairobi/Joburg/London" | location == "Beijing" | location == "Shanghai, China" | location == "Hong Kong" | location == "china" | location == "No.97A, Xuanwumen Xidajie, Xicheng District, Beijing, China")
Users_China <- Sampled_users_china %>% select(screen_name)


Sampled_users_france <- sampled_dataset %>% filter (location == "France" | location == "Paris, France" | location == "Paris" | location== "qq. part entre le Nord de la France-Tokyo-Shanghai" | location == "France" | location == "Dans mes pensées!" | location == "Paris" | location == "Le Mans, France" | location == "Paris - Grenoble" | location == "France" | location == "France-Paris Île de France" | location == "Saint-Avold, France" | location == "Metz, France" | location == "Paris, France" | location == "Metz" | location == "Lourdes" | location == "France 92" | location == "Baltimore & Paris" | location == "Moselle, Lorraine" | location == "Ile-de-France, France" | location == "Tehran, Iran, Paris, France" | location == "Partout" | location == "Paris, France" | location == "France" | location == "Chantilly, France" | location == "Evreux, France" | location == "Nice, France - Côte d'Azur" | location == "Moissac Paris" | location == "Bouches-du-Rhône (13)") 
Users_france <- Sampled_users_france %>% select(screen_name)


Sampled_users_spain <- sampled_dataset %>% filter (location == "Madrid" | location == "Madrid (España)" | location == "Barcelona, Spain" | location== "Spain" | location == "Toledo - Spain" | location == "Torreón, Coahuila de Zaragoza" | location == "Málaga" | location == "Madrid, Comunidad de Madrid" | location == "Ciudad Imagen" | location == "Granada" | location == "Málaga (Andalucía, España)" | location == "Huelva" | location == "Cádiz" | location == "Villahermosa, Tabasco" | location == "Murcia - España" | location == "España" | location == "Catalunya" | location == "Córdoba - España" | location == "Valencia" | location == "Madrid & Seoul" | location == "Madrid" | location == "Sevilla, Spain" | location == "Madrid, España" | location == "BCN-Catalunya" | location == "Madrid - Valladolid <U+0001F1EA><U+0001F1F8>" | location == "España<U+0001F1EA><U+0001F1E6>" | location == "Badalona / Mollet del Vallès" | location == "España. ¡Gran País! ¡Mí País!" | location == "Jerez de la Frontera" | location == "Zaragoza, Aragón" | location == "España Madrid - Chile" | location == "Tarragona, España <U+0001F1EA><U+0001F1F8>" | location == "Granada (España)" | location == "Barcelona, Spain" | location == "Reino de España <U+0001F1EA><U+0001F1F8>" | location == "De #Madrid, al cielo. #España" | location == "Cigales, Valladolid, Madrid" | location == "Madrid, Comunidad de Madrid" | location == "Madrid, Spain" | location == "Madrid - Donostia" | location == "Madrid - Spain" | location == "Fuencarral (pueblo)" | location == "Barcelona" | location == "Castille and Leon, Spain" | location == "Majadahonda" | location == "Santa Cruz de Tenerife, Spain" | location == "Almería" | location == "ESPAÑA (Iberia)" | location == "Palma, Spain" | location == "Barcelona, España.") 
Users_spain <- Sampled_users_spain %>% select(screen_name)

Sampled_users_germany <- sampled_dataset %>% filter (location == "Mysore and BERLIN" | location == "München, Bayern" | location == "Berlin, Germany" | location== "Frankfurt & San Francisco" | location == "Frankfurt am Main" | location == "Baden-Württemberg, D" | location == "Frankfurt on the Main, Germany" | location == "Hennef, NRW, Germany" | location == "Berlin,Dubai" | location == "Germany" | location == "Nordrhein-Westfalen, Deutschland" | location == "Monaco di Baviera" | location == "Solingen, Germany" | location == "Düsseldorf" | location == "deutscher Sprachraum" | location == "Berlin" | location == "Munich, Bavaria" | location == "Jestetten, Deutschland" | location == "Berlin, NJ" | location == "Deutschland") 
Users_germany <- Sampled_users_germany %>% select(screen_name)


Sampled_users_uk<- sampled_dataset %>% filter (location == "London" | location == "London, UK" | location == "#RemoteWork #Boston #NYC" | location== "Langtoun, UK" | location == "Belfast, Northern Ireland" | location == "NY/Delhi/Rio/Beijing/Nairobi/Joburg/London" | location == "London, UK" | location == "North Wales, UK" | location == "UK" | location == "England, United Kingdom" | location == "Scotland" | location == "Ireland" | location == "London, England" | location == "Dublin, Ireland" | location == "Either in UK, AFrica, EU or Ukraine" | location == "Scotland, United Kingdom" | location == "UK" | location == "St Andrews, Scotland" | location == "english@nrttv.com" | location == "London UK" | location == "United Kingdom" | location == "Huddersfield, England" | location == "Glasgow" | location == "Cornwall" | location == "South East, England" | location == "Somerset, England" | location == "Dartford, KENT, UK" | location == "Britain, Europe & Middle East" | location == "Toronto | New York | London UK" | location == "England" | location == "London/Kent" | location == "London / Essex" | location == "Plymouth, England" | location == "210 Dundas Street, Suite 201, London, ON" | location == "Liverpool [UK]" | location == "South Staffordshire" | location == "Co. Wicklow" | location == "Durham, England" | location == "Cambridge, MA" | location == "Hertfordshire, UK") 
Users_uk <- Sampled_users_uk %>% select(screen_name)


Sampled_users_usa <- sampled_dataset %>% filter (location == "Atlanta, GA" | location == "#RemoteWork #Boston #NYC" | location == "Washington" | location== "United States" | location == "Silicon Valley | SF | 39.5K Ft" | location == "USA" | location == "Florida" | location == "Miami, Florida" | location == "Washington, DC" | location == "Seattle" | location == "United States" | location == "Florida" | location == "United States" | location == "Virginia & Washington DC" | location == "Honolulu, Hawaii" | location == "Jacksonville, FL" | location == "USA" | location == "Las Vegas, NV" | location == "Austin, TX" | location == "NY/Delhi/Rio/Beijing/Nairobi/Joburg/London" | location == "Boston, MA" |
                                                         location == "Summerville, S.C." | location == "Philly, natch" | location == "San Francisco, CA" | location == "San Francisco Bay Area" | location == "Orlando" | location == "New York, NY" | location == "Latam - USA" | location == "Frankfurt & San Francisco" | location == "NYC" | location == "Buffalo, N.Y." | location == "Wisconsin, USA" | location == "Chicago, IL" | location == "News & Bible verses USA" | location == "New York, NY" | location == "Las Vegas, Nevada" | location == "Washington, D.C. - EE.UU." | location == "Connecticut" | location == "Dallas, TX" | location == "La Paz, Baja California Sur" | location == "CA Conservative turned TEXAN!" | location == "Houston, TX" | location == "Spokane Valley, WA" | location == "Orlando FL"| location == "Oakland, CA" | location == "New York"| location == "New York, USA" |
                                                         location == "San Francisco, California" | location == "Las Vegas, NV" | location == "Dallas-Fort Worth, Texas" | location == "Bellwether State, USA" | location == "Boston" | location == "New York, NY" | location == "El Paso, Texas" | location == "Washington DC" | location == "Ha-shikkuts Meshomem" | location == "San Francisco, CA" | location == "Toronto" | location == "Pittsburgh" | location == "Toronto, Ontario" | location == "Clarksville, TN" | location =="New York, USA <U+7F8E><U+56FD><U+7EBD><U+7EA6" | location == "Toronto | New York | London UK" | location == "Civic Center Tenderloin" | location == "Washington, D.C." | location == "New York City" | location == "Rochester, NY" | location == "Nashville, TN" | location == "Chicago/Washington D.C." | location == "Los Angeles, CA USA" | location == "Anchorage, AK" | location == "Washington D.C." | location == "TorontoTheGood" | location == "Bellevue, WA, USA" | location == "Hampton Roads, Virginia" | location == "New Jersey" | location == "Seattle, WA" | location == "Charleston, SC" | location == "WA Coast USA" | location == "City of Sacraments CA" | location == "Palo Alto, CA" | location == "Boston, MA" | location == "Mansfield, TX" | location == "honolulu, hi" | location == "Cheektowaga NY USA" | location == "Salt Lake City, UT" | location == "The Continental Hotel New York City" | location == "Albany, NY" | location == "Portland, OR, and New York, NY" | location == "Fresno, CA" | location == "USA <U+0394>ST <U+0001F53A><U+0001F418><U+000" | location == "Minnesota, USA" | location == "San Francisco, California" | location == "Los Angeles" | location == "North Carolina" | location == "California" | location == "White House & elsewhere" | location == "Oregon ~ West Coast Mountains" | location == "Maryland and Maine" | location == "Vero Beach, FL" | location == "Milwaukee" | location == "Santa Monica, California" | location == "Ohio, USA" | location == "Lima - Los Angeles" | location =="Baltimore, MD" | location == "Montauk, NY" | location == "Iowa" | location == "nyc" | location == "Pittsburgh, PA" | location == "Philadelphia, PA" | location == "Ellenwood, GA -East of ATL" | location == "Detroit, MI" | location =="Hampton Roads, Va." | location == "Florida, USA" | location =="Cleveland, OH" | location =="Borough of Queens, NYC, NY USA" | location == "DC + Silicon Valley + LA" | location == "Manhattan, NY" 
                                                       | location == "Dallas/Fort Worth" | location == "The Big River, Missouri" | location == "Las Vegas" | location == "AA County, Maryland, USA" | location == "Atlanta, GA" | location == "New York City" | location == "South Houston, TX" | location == "North Carolina, USA" | location == "San Francisco, CA | Washington DC" | location == "Fort Lauderdale | FL - USA" | location == "Tampa, FL" | location == "U.S.A." | location == "Laguna Beach, Ca" | location == "Redlands, CA" | location == "Silicon Valley" | location == "Gardena, CA" | location == "Cincinnati, OH" | location == "Idaho, USA"| location =="London, Ontario, Canada" | location == "San Diego, California" | location == "West Central Florida" | location == "Washington DC <U+0001F1FA><U+0001F1F8>via <U+00" | location == "Pittsfield, Massachusetts" | location == "Sacramento, CA" | location == "Riverside, California." | location == "Farragut, TN" | location == "San Jose" | location == "Naples, FL")

Users_usa <- sampled_users_usa %>% select(screen_name)

