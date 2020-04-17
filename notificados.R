library(readr)
library(ggplot2)

n <- read_csv("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/notificaciones.csv")
N <- as.data.frame(t(n))
names(N) <- lapply(N[1, ], as.character); N <- N[-1,] 
N$date <- substr(rownames(N), start = 1, stop = 5)
rownames(N) <- c()
N <- unfactor(N)
N$date <- as.Date(N$date, "%m/%d")

z <- with(N, ISP + Privados + Hospitales)
N <- data.frame(N, z)

N.long <- melt(N, id = "date", measure = c("ISP", "Privados", "Hospitales", "z"))
ggplot(data = N.long, aes(date, value, colour = variable)) +  geom_point() + theme(axis.text.x = element_text(angle=65, hjust=1, vjust = 1)) 



