library(readr)
library(varhandle)
library(reshape2)
library(ggplot2)

confirmados_comunas <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/confirmados_comunas.csv")

index <- which(confirmados_comunas$codigo_region==13)
RMC <- as.data.frame(t(confirmados_comunas[index, ]))
names(RMC) <- lapply(RMC[4, ], as.character)
RMC <- RMC[-1:-4,]

RMC$date <- substr(rownames(RMC), start = 1, stop = 5)
rownames(RMC) <- c()
RMC <- unfactor(RMC)
RMC[,][RMC[,] == "-"] <- as.numeric(0)
RMC <- unfactor(RMC)
RMC$date <- as.Date(RMC$date, "%m/%d")

RMC.long <- melt(RMC, id = "date", measure = c("La Florida", "Vitacura", "Puente Alto", "Lo Barnechea", "Maipú", "El Bosque", "Las Condes", "Ñuñoa", "Providencia","Santiago"))
ggplot(data = RMC.long, aes(date, value, colour = variable)) +  geom_line() + theme(axis.text.x = element_text(angle=65, hjust=1, vjust = 1)) 

