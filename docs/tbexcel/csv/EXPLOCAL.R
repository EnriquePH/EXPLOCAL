library(dplyr)
library(tidyr)
library(ggplot2)


entalp.file <- "ENTALP.CSV"

entalp.data <- read.csv(entalp.file,
                        dec = ",",
                        sep = ";"
                        )

explosion.products <- as.data.frame(colnames(entalp.data), optional = TRUE)
colnames(explosion.products) <- c("FORMULA")
explosion.products  <- filter(explosion.products, !grepl("TEMP", FORMULA))

carbonates <- filter(explosion.products,  grepl("CO3", FORMULA))

dioxides <- filter(explosion.products,  grepl("O2", FORMULA))

entalp.data <- entalp.data %>%
    gather(key = FORMULA, value = ENTHALPY, -TEMP)

carbonates.entalp.data <- entalp.data %>%
    filter(FORMULA %in% carbonates$FORMULA)


ggplot(carbonates.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) + 
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") + 
    geom_line()

dioxides.entalp.data <- entalp.data %>%
    filter(FORMULA %in% dioxides$FORMULA)

ggplot(dioxides.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) + 
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") + 
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
