library(ggplot2)

# Constants data
# File K1.csv
k1.file <- "K1.CSV"
# Temperatura (K)
# K1= PCO.PH2O / PCO2�PH2
# K2=P�CO/PCO2

k1.data <- read.csv(k1.file,
                    dec = ",",
                    sep = ";",
                    col.names = c("T", "K1", "K2"),
                    skip = 1
                    )


View(k1.data)

ggplot(k1.data, aes(x = T, y = K1)) +
    geom_line()


ggplot(k1.data, aes(x = T, y = K2)) +
    geom_line()