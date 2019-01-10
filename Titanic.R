train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# add variavel survived dentro de test em um novo conjunto de dados test.survived
test.survived <-
  data.frame(survived = rep("None", nrow(test)), test[, ])

data.combined <- rbind(train, test.survived)

str(data.combined)

# transforma as colunas suvived e pclass em Factors
data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)

table(data.combined$survived)
table(data.combined$pclass)

# carrega bilioteca
library(ggplot2)

# Hipotese - Pessoas ricas sobrevivem com uma maior taxa
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Examinar os primeiros nomes nos dados de treino
head(as.character(train$name))

# Quantos nomes sao unicos?
length(unique(as.character(data.combined$name)))

# Existem nomes duplicados no conjunto de dados combinados (treino e teste)
# Obter os nomes duplicados e guardar como vetor
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Ver os dados repetidos em data.combined
data.combined[which(data.combined$name %in% dup.names), ]

# Identificar os nomes com Miss. Mr. ...
library(stringr)



misses <- data.combined[which(str_detect(data.combined$name, "Miss.")), ]
misses[1:5, ]


mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5, ]

male <- data.combined[which(train$sex == "male"), ]
male[1:5, ]

# Criar uma nova coluna 'Title' para Miss., Mrs., Mr., Master.
extractTitle <- function(name) {
  name <- as.character(name)

  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "name"]))
}

data.combined$title <- as.factor(titles)

# Usar somente as primeiras 891 primeiras linhas  (pois sao as que correspondem
# aos dados de treino - train)
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
  
  
  
  



