# LCK 2021

library(dplyr)

lck <- read.csv('data_raw - LCK 2021.csv')
str(lck)
names(lck)

# "Team" - Equipo
# [1] "Player" - Jugador
# [1] "Role" - Posicion
# [1] "Kills" 
# [1] "Deaths"
# [1] "Assists"
# [1] "KDA"
# [1] "CS" - Farmeo
# [1] "CS.in.Team.s.Jungle" - Farmeo en propia jungla
# [1] "CS.in.Enemy.Jungle" - Farmeo en jungla enemiga
# [1] "CSM" - Farmeo por minuto
# [1] "Golds" - Oro
# [1] "GPM" - Oro por minuto
# [1] "GOLD." - % de oro del team
# [1] "Vision.Score" - Puntuacion de vision
# [1] "Wards.placed" - Wards colocados
# [1] "Wards.destroyed" - Wards destruidos
# [1] "Control.Wards.Purchased" - Pinks comprados
# [1] "VSPM" - Vision por minuto por juego promedio
# [1] "WPM" - Wards por minuto
# [1] "VWPM" - Wards de vision por minuto
# [1] "WCPM" - Wards limpiados por minuto
# [1] "VS." - % de Vision
# [1] "Total.damage.to.Champion" - Dano total a campeones
# [1] "Physical.Damage" - Dano fisico
# [1] "Magic.Damage" - Dano magico
# [1] "True.Damage" - Dano verdadero
# [1] "DPM" - Dano por minuto 
# [1] "DMG." - % de dano 
# [1] "K.A.Per.Minute" - Kill + Assist por minuto
# [1] "KP." - Kill participation
# [1] "Solo.kills" - Solo kill
# [1] "Double.kills"
# [1] "Triple.kills"
# [1] "Quadra.kills"
# [1] "Penta.kills"
# [1] "GD.15" - Diferencia de oro al 15
# [1] "CSD.15" - Farmeo de diferencia al 15
# [1] "XPD.15" - Exp de diferencia al 15
# [1] "LVLD.15" - Lvl de diferencia al 15
# [1] "Damage.dealt.to.turrets" - Dano a torretas
# [1] "Total.heal" - Cura total
# [1] "Damage.self.mitigated" - Dano automitigado
# [1] "Time.ccing.others" - Tiempo ccando
# [1] "Total.damage.taken" - Dano recibido
# [1] "Win" 

# summary(lck)
# visiones <- lck$Vision.Score > 200
# lck[visiones,]


lck %>%
  filter(Vision.Score > 150, Role == 'JUNGLE')

lck %>%
  arrange(KDA)

lck %>%
  select(Team, Player, Role, KDA, Win) %>%
  arrange(desc(KDA))

lck %>%
  # Filter for rice food category
  filter(Role == "ADC") %>%
  # Create histogram of co2_emission
  ggplot(aes(CSM)) +
  geom_histogram()

minion_pos <- lck %>%
  select(Role, Player, Team, CS, CSM) %>%
  filter(Role == 'JUNGLE') %>%
  mutate(duration = CS / CSM)


q1 <- quantile(minion_pos$CS, 0.25)
q3 <- quantile(minion_pos$CS, 0.75)
iqr <- q3-q1

lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

# Filtramos
outliers <- minion_pos %>%
  filter(CS > upper | CS < lower) %>%
  arrange(desc(CS))