library(dplyr)
library(tidyr)
library(ggplot2)

entalp.file <- "ENTALP.CSV"


entalp.data <- read.csv(entalp.file,
                        dec = ",",
                        sep = ";"
                        )

View(entalp.data)

entalp.data <- entalp.data %>%
    gather(key = FORMULA, value = ENTHALPY, -TEMP)

ggplot(entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) + 
    ylab("Concentration") + 
    geom_line()


# Constants data
# File K1.csv
k1.file <- "K1.CSV"
# Temperatura (K)
# K1= PCO.PH2O / PCO2·PH2
# K2=P²CO/PCO2

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
