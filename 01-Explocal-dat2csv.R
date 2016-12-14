#-------------------------------------------------------------------------------
#  kaggle - OEIS Integer Prediction
#  File: 01-Explocal-dat2csv.R
#  (c) 2016 - Enrique Pérez Herrero
#  Apache License Version 2.0, January 2004
#  Start: 14/Dic/2016
#  End:   14/Dic/2016
#-------------------------------------------------------------------------------
# DESCRIPTION: Read, tidy old *.DAT files and save to *.csv
#-------------------------------------------------------------------------------

# 0) LOAD PACKAGES

library(ggplot2)

input_path <- "bin/"
data_path <- "data/"

# 1) Constants K1 and K2 from CONSTANT.DAT file

# 1.1) Read file
file_constants <- "CONSTANT.DAT"
path_constants <- paste0(input_path, file_constants)

CONSTANT <- readLines(path_constants)

# 1.2 ) Constant K1

K1 <- CONSTANT[9:67]
K1 <- as.data.frame(K1)
names(K1) <- "text"
K1$Temp <- lapply(K1$text, function(x) sub("=(.+)", "", x))
K1$Temp <- lapply(K1$Temp, function(x) sub("t", "", x))
K1$Temp <- as.numeric(K1$Temp)

K1$Value <- lapply(K1$text, function(x) sub("t(.+)=", "", x))
K1$Value <- as.numeric(K1$Value)
K1$text <- NULL

write.csv(K1,
          file = paste0(data_path, "K1.csv"),
          row.names = FALSE)


# 1.3 ) Constant K2

K2 <- CONSTANT[73:130]
K2 <- as.data.frame(K2)
names(K2) <- "text"
K2$Temp <- lapply(K2$text, function(x) sub("=(.+)", "", x))
K2$Temp <- lapply(K2$Temp, function(x) sub("t", "", x))
K2$Temp <- as.numeric(K2$Temp)

K2$Value <- lapply(K2$text, function(x) sub("t(.+)=", "", x))
K2$Value <- as.numeric(K2$Value)
K2$text <- NULL

write.csv(K2,
          file = paste0(data_path, "K2.csv"),
          row.names = FALSE)


# 2) Reactives data from "REACTIVO.DAT"

file_reactives <- "REACTIVO.DAT"
path_reactives <- paste0("bin/", file_reactives)

REACTIVES <- readLines(path_reactives)

REACTIVES
