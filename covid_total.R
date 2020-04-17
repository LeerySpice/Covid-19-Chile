library(readr)
library(ggplot2)

CT <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/resumen_nacional.csv")

# tc <- unfactor(tc)
CT$dia <- as.Date(CT$dia, "%m/%d")

CT.long <- melt(CT, id = "dia", measure = c("confirmados","muertes","recuperados"))
ggplot(data = CT.long, aes(dia, value, colour = variable)) +  geom_point() + theme(axis.text.x = element_text(angle=65, hjust=1, vjust = 1)) 

# ajuste x^4
x = 1:length(CT$dia)

modelo_poli4 <- lm(confirmados ~ poly(x, 2), data = CT)
summary(modelo_poli4)

ggplot(data = CT, aes(x, y = confirmados))  + xlim(0, 40) + ylim(0,8000) +
  geom_point(color = "grey30", alpha = 0.3) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color = "red") 

limites <- range(x)
nuevos_puntos <- seq(from = limites[1], to = limites[2] + 30 , by = 0.1)
nuevos_puntos <- data.frame(x = nuevos_puntos)

predicciones <- predict(modelo_poli4, newdata = nuevos_puntos, se.fit = TRUE,
                        level = 0.95)

intervalo_conf <- data.frame(inferior = predicciones$fit -
                               1.96*predicciones$se.fit,
                             superior = predicciones$fit +
                               1.96*predicciones$se.fit)

attach(CT)
plot(x, y = confirmados, pch = 20, col = "dark red", ylim =c(0,40000), xlim =c(0,70))
title("Estimacion de grado 4: Acumulados ~ dia")
points(x = nuevos_puntos$x, predicciones$fit, col = "red", pch = 20)
points(x = nuevos_puntos$x, intervalo_conf$inferior, col = "blue", pch = 4)
points(x = nuevos_puntos$x, intervalo_conf$superior, col = "blue", pch = 4)
