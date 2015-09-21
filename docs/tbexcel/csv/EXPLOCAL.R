#  ----------------------------------------------------------------------------
#  EXPLOCAL
#  Explocal project plots
#  File: EXPLOCAL.R
#  Enrique Pérez Herrero
#  21/Sep/2015
#  ----------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(latex2exp)

# Chemical formula to expression

formula.expression <- function(formula){
    x <- gsub("(\\d+)", "[\\1]*", formula)
    if(substr(x, nchar(x), nchar(x)) == "*"){
        x <- substr(x, 1, nchar(x) - 1)
        x <- as.expression(x)
        x <- parse(text = x)
        }
    return(x)
}

# Read Enthalpy data file
entalp.file <- "ENTALP.CSV"
entalp.data <- read.csv(entalp.file, dec = ",", sep = ";")

explosion.products <- as.data.frame(colnames(entalp.data), optional = TRUE)
colnames(explosion.products) <- c("FORMULA")
explosion.products  <- filter(explosion.products, !grepl("TEMP", FORMULA))


entalp.data <- entalp.data %>%
    gather(key = FORMULA, value = ENTHALPY, -TEMP)

#  ----------------------------------------------------------------------------
#    Carbonates
#  ----------------------------------------------------------------------------

carbonates <- filter(explosion.products,  grepl("CO3", FORMULA))

carbonates.entalp.data <- entalp.data %>%
    filter(FORMULA %in% carbonates$FORMULA)

carbonates.labels <- lapply(carbonates$FORMULA, formula.expression)

ggplot(carbonates.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) +
    ggtitle("Carbonates") +
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") +
    scale_colour_hue(labels = carbonates.labels) +
    geom_line()

#  ----------------------------------------------------------------------------
#    Dioxides
#  ----------------------------------------------------------------------------

dioxides <- filter(explosion.products,
                   grepl("O2", FORMULA) & nchar(as.character(FORMULA)) == 4
                   )

dioxides.entalp.data <- entalp.data %>%
    filter(FORMULA %in% dioxides$FORMULA)

dioxides.labels <- lapply(dioxides$FORMULA, formula.expression)

ggplot(dioxides.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) +
    ggtitle("Dioxides") +
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") +
    scale_colour_hue(labels = dioxides.labels) +
    geom_line()

#  ----------------------------------------------------------------------------
#   Sesquioxides
#  ----------------------------------------------------------------------------
sesquioxides <- filter(explosion.products,  grepl("2O3", FORMULA))

sesquioxides.entalp.data <- entalp.data %>%
    filter(FORMULA %in% sesquioxides$FORMULA)

sesquioxides.labels <- lapply(sesquioxides$FORMULA, formula.expression)

ggplot(sesquioxides.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) +
    ggtitle("Sesquioxides") +
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") +
    scale_colour_hue(labels = sesquioxides.labels) +
    geom_line()

#  ----------------------------------------------------------------------------
#    Elements
#  ----------------------------------------------------------------------------

elements <- data.frame(FORMULA = c("C", "Hg"))

elements.entalp.data <- entalp.data %>%
    filter(FORMULA %in% elements$FORMULA)

ggplot(elements.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) +
    ggtitle("Elements") +
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") +
    geom_line()


#  ----------------------------------------------------------------------------
#    Halides
#  ----------------------------------------------------------------------------

halides <- filter(explosion.products, grepl("H$", FORMULA))

halides.entalp.data <- entalp.data %>%
    filter(FORMULA %in% halides$FORMULA)

ggplot(halides.entalp.data, aes(TEMP , ENTHALPY, col = FORMULA)) +
    ggtitle("Halides") +
    xlab("Temperature (K)") +
    ylab("H(T)-H(298) kcal/mol") +
    geom_line()


#  ----------------------------------------------------------------------------
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

#  ----------------------------------------------------------------------------

ylab.k1.plot <- expression(K[1])
label.k1.plot <- "K[1] == frac(P[CO], P[H[2]*O]) * frac(P[CO[2]], P[H[2]])" 

ggplot(k1.data, aes(x = TEMP, y = K1)) +
    xlab("Temperature (K)") +
    ylab(ylab.k1.plot) +
    theme(axis.title.y = element_text(angle = 0)) + 
    annotate("text", x = 4000, y = 3.5, label = label.k1.plot, parse = TRUE) + 
    geom_line(size = 1, colour = "blue")

#  ----------------------------------------------------------------------------
ylab.k2.plot <- expression(K[2])
label.k2.plot <- "K[2] == frac(P[CO]^2, P[CO[2]])"

ggplot(k1.data, aes(x = TEMP, y = K2)) +
    xlab("Temperature (K)") +
    ylab(ylab.k2.plot) +
    theme(axis.title.y = element_text(angle = 0)) + 
    annotate("text", x = 2000, y = 5000, label = label.k2.plot, parse = TRUE) + 
    geom_line(size = 1, colour = "red")

