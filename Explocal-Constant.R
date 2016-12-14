library(ggplot2)

file_name <- "CONSTANT.DAT"
file_path <- paste0("bin/", file_name)

CONSTANT <- readLines(file_path)



K1 <- CONSTANT[9:67]
K1 <- as.data.frame(K1)
names(K1) <- "text"
K1$Temp <- lapply(K1$text, function(x) sub("=(.+)", "", x))
K1$Temp <- lapply(K1$Temp, function(x) sub("t", "", x))
K1$Temp <- as.numeric(K1$Temp)

K1$Value <- lapply(K1$text, function(x) sub("t(.+)=", "", x))
K1$Value <- as.numeric(K1$Value)
K1$text <- NULL

ggplot(K1, aes(x = Temp, y = Value)) +
  geom_line()



K2 <- CONSTANT[73:130]
K2 <- as.data.frame(K2)
names(K2) <- "text"
K2$Temp <- lapply(K2$text, function(x) sub("=(.+)", "", x))
K2$Temp <- lapply(K2$Temp, function(x) sub("t", "", x))
K2$Temp <- as.numeric(K2$Temp)

K2$Value <- lapply(K2$text, function(x) sub("t(.+)=", "", x))
K2$Value <- as.numeric(K2$Value)
K2$text <- NULL

ggplot(K2, aes(x = Temp, y = Value)) +
  geom_line()
