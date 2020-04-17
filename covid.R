install.packages("varhandle")
library(varhandle)
library(readr)

confirmados <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/confirmados.csv")

tc <- as.data.frame(t(confirmados))
names(tc) <- lapply(tc[2, ], as.character)
tc <- tc[-1:-2,] 
tc$date <- substr(rownames(tc), start = 1, stop = 5)
rownames(tc) <- c()
tc <- unfactor(tc)
tc$date <- as.Date(tc$date, "%m/%d")

# plot.default(AR,type="b", main = "COVID-19 RM", xlab = "DÃ­as", ylab = "Acumulado", ylim =c(0,4000)) + points(RM, col = "dark red")
tc.long <- melt(tc, id = "date", measure = c("Metropolitana", "Araucanía", "Ñuble", "Tarapacá"))
ggplot(data = tc.long, aes(date, value, colour = variable)) +  geom_point() + theme(axis.text.x = element_text(angle=65, hjust=1, vjust = 1)) 


# ajuste x^4
x = 1:length(tc$date)

modelo_poli4 <- lm(Metropolitana ~ poly(x, 4), data = tc)
summary(modelo_poli4)

ggplot(data = tc, aes(x, y = Metropolitana))  + xlim(0, 40) + ylim(0,4000) +
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

attach(tc)
plot(x, y = Metropolitana, pch = 20, col = "dark red", ylim =c(0,40000), xlim =c(0,70))
title("EstimaciÃ³n de grado 4: Acumulados ~ dÃ­a")
points(x = nuevos_puntos$x, predicciones$fit, col = "red", pch = 20)
points(x = nuevos_puntos$x, intervalo_conf$inferior, col = "blue", pch = 4)
points(x = nuevos_puntos$x, intervalo_conf$superior, col = "blue", pch = 4)

