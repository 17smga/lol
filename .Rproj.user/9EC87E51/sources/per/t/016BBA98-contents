library(dplyr)
library(ggplot2)

lol <- read.csv('League of Legends 2021 World Championship Play-In Groups Statistics - Raw Data.csv')

summary(lol)
glimpse(lol)
lol$Team <- as.factor(lol$Team)
lol$Player <- as.factor(lol$Player)
lol$Opponent <- as.factor(lol$Opponent)
lol$Position <- as.factor(lol$Position)
lol$Champion <- as.factor(lol$Champion)
lol$Result <- as.factor(lol$Result)

# Forma de filtrar 1
datos_beyond <- lol %>%
  filter(Team == 'BYG')

datos_dfm <- lol %>%
  filter(Team == 'DFM')

# Forma de filtrar 2
lol %>%
  filter(Team %in% c('BYG', 'DFM')) %>%
  group_by(Team) %>%
  summarize(minions_media = mean(Creep.Score),
            vision_media = mean(Ward.Interactions))

# Histograma
lol %>%
  # Filter for rice food category
  filter(Position == "Adc") %>%
  # Create histogram of co2_emission
  ggplot(aes(Creep.Score)) +
  geom_histogram()

#### Cuantiles ####
quantile(lol$Gold.Earned)
quantile(lol$Gold.Earned, seq(0,1,0.2)) # Quintiles

#### Varianza y desv #######
lol %>% 
  group_by(Position) %>% 
  summarize(var_minions = var(Creep.Score),
            sd_minions = sd(Creep.Score))

ggplot(data = lol, mapping = aes(Creep.Score)) +
  # Create a histogram
  geom_histogram() +
  # Create a separate sub-graph for each food_category
  facet_wrap(~ Position)

####### Outliers danio por jugador promedio ########
danio_por_jug <- lol %>%
  group_by(Player) %>%
  summarize(danio_prom = mean(Champion.Damage.Share))

summary(danio_por_jug)

q1 <- quantile(danio_por_jug$danio_prom, 0.25)
q3 <- quantile(danio_por_jug$danio_prom, 0.75)
iqr <- q3-q1

lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

# Filtramos
outliers <- danio_por_jug %>%
  filter
############### Outliers minions  ###########

minion_pos <- lol %>%
  select(Position, Player, Champion, Team, Opponent, Creep.Score) %>%
  filter(Position == 'Support')


q1 <- quantile(minion_pos$Creep.Score, 0.25)
q3 <- quantile(minion_pos$Creep.Score, 0.75)
iqr <- q3-q1

lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

# Filtramos
outliers <- minion_pos %>%
  filter(Creep.Score > upper | Creep.Score < lower)

################# Regresion ################################
regresion <- lm(data = lol, formula = lol$Dragons.For ~ lol$Wards.Placed)

vision_equipos <- lol %>%
  select(Team, Dragons.For, Ward.Interactions) %>%
  group_by(Team) %>%
  summarize(vision_prom = mean(Ward.Interactions),
            dragones = mean(Dragons.For))


# cbind

ggplot(data = vision_equipos, aes(x = vision_prom, y = dragones)) + 
  geom_point(colour = "blue4") +
  ggtitle("Diagrama de dispersion") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

######################################################
lol %>%
  count(Team) %>%
  mutate(probs = n/sum(n)) # Agrega una nueva columna

lol %>%
  sample_n(10) # replace = F

lol %>%
  punif()

#########################################################

# lapply(vector, funcion, args) siempre regresa una lista donde se aplica la funcion a cada alemento del vector
# sapply simplifica la salida de un lapply, y devuelve vectores o matrices, si no puede, devuelve lo mismo que un lapply normal
# vapply(vector, funcion, fun.value) igual que sapply pero con fun.value podemos especificar el tipo de salida
# que queremos

##################################################################33
# 
# REGEX    ?regex
# 
# grepl(pattern = regex, x = string) regresa un boolean
# 
# grep(pattern, = regex, x = string) regresa los indices donde se dieron los matches
# 
# sub(pattern =regex, replacement = str, x = string) sustituye, pero solo busca hasta la primera coincidencia
# 
# gsub() reemplaza todos los matches

########################################################################3
# 
# TIMES AND DATES
# 
# Sys.Date() Fecha, suma dias
# 
# Sys.time() # Horario, suma segundos
# 
# packages lubridate, zoo, xts
# 
# as.Date(string, format = ??) 
# %Y: 4-digit year (1982)
# %y: 2-digit year (82)
# %m: 2-digit month (01)
# %d: 2-digit day of the month (13)
# %A: weekday (Wednesday)
# %a: abbreviated weekday (Wed)
# %B: month (January)
# 
# as.POSIXct(STRING, FORMAT = ??)
# %H: hours as a decimal number (00-23)
# %I: hours as a decimal number (01-12)
# %M: minutes as a decimal number
# %S: seconds as a decimal number
# %T: shorthand notation for the typical format %H:%M:%S
# %p: AM/PM indicator
# %b: abbreviated month (Jan)