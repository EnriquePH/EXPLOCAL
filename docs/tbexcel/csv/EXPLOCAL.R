library(dplyr)
library(tidyr)
library(ggplot2)
library(latex2exp)

entalp.file <- "ENTALP.CSV"

entalp.data <- read.csv(entalp.file,
                        dec = ",",
                        sep = ";"
                        )

explosion.products <- as.data.frame(colnames(entalp.data), optional = TRUE)
colnames(explosion.products) <- c("FORMULA")

explosion.products  <- filter(explosion.products, !grepl("TEMP", FORMULA))

carbonates <- filter(explosion.products,  grepl("CO3", FORMULA))
dioxides <- filter(explosion.products,
                   grepl("O2", FORMULA) & nchar(as.character(FORMULA)) == 4
                   )

sesquioxides <- filter(explosion.products,  grepl("2O3", FORMULA))

#elemental <- filter(explosion.products,  FORMULA %in% c("C", "Hg"))

elements <- data.frame(FORMULA = c("C", "Hg"))


entalp.data <- entalp.data %>%
    gather(key = FORMULA, value = ENTHALPY, -TEMP)

carbonates.entalp.data <- entalp.data %>%
    filter(FORMULA %in% carbonates$FORMULA)


ggplot(carbonates.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) +
    ggtitle("Carbonates") +
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") + 
    geom_line()

dioxides.entalp.data <- entalp.data %>%
    filter(FORMULA %in% dioxides$FORMULA)

ggplot(dioxides.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) +
    ggtitle("Dioxides") +
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") + 
    geom_line()

sesquioxides.entalp.data <- entalp.data %>%
    filter(FORMULA %in% sesquioxides$FORMULA)

ggplot(sesquioxides.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) +
    ggtitle("Sesquioxides") +
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") +
    geom_line()

elements.entalp.data <- entalp.data %>%
    filter(FORMULA %in% elements$FORMULA)

ggplot(elements.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) +
    ggtitle("Elements") +
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") +
    geom_line()

# Constants data
# File K1.csv
k1.file <- "K1.CSV"
# Temperatura (K)
# K1=PCO.PH2O/PCO2·PH2
# K2=P²CO/PCO2

k1.data <- read.csv(k1.file,
                    dec = ",",
                    sep = ";",
                    col.names = c("TEMP", "K1", "K2"),
                    skip = 1
                    )


label.k1.plot <- latex2exp("$K_{1} = P_{CO} . P_{H_{2}O}/P_{CO_{2}} . P_{H_{2}}$")


ggplot(k1.data, aes(x = TEMP, y = K1)) +
    xlab("Temperature (K)") +
    ylab(label.k1.plot) +
    annotate("text", x = 1500, y = 7.5, label = "frac(P[CO], P[H2O]) * frac(P[CO2], P[H2])" , parse = TRUE) + 
    geom_line()
    

label.k2.plot <- latex2exp("$K_{2}= P_{CO}^2/P_{CO_2}$")

ggplot(k1.data, aes(x = TEMP, y = K2)) +
    xlab("Temperature (K)") +
    ylab(label.k2.plot) +
    geom_line()

