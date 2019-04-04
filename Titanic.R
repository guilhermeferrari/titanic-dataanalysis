# Carregar todas as bibliotecas no comeco. 
library(ggplot2)
library(stringr)
library(randomForest)
library(caret)
library(doSNOW)
library(rpart)
library(rpart.plot)

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# add variavel survived dentro de test em um novo conjunto de dados test.survived
test.survived <- data.frame(survived = rep("None", nrow(test)), test[, ])

data.combined <- rbind(train, test.survived)

str(data.combined)

# transforma as colunas suvived e pclass em Factors
data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)

table(data.combined$survived)
table(data.combined$pclass)

# --- relacao sobreviventes com relacao a PCLASS
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




misses <- data.combined[which(str_detect(data.combined$name, "Miss.")), ]
misses[1:5, ]


mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5, ]

master <- data.combined[which(str_detect(data.combined$name, "Master.")), ]
master[1:5, ]

male <- data.combined[which(train$sex == "male"), ]
male[1:5, ]

# --- relacao sobreviventes com relacao ao titulo Mr., Miss...
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
  
# --- relacao sobreviventes male/female
# Distribuicao male/female em test e train
table(data.combined$sex)

# Ver a distribuicao de sobreviventes separados por male/female em cada Pclass
ggplot(data.combined[1:891, ], aes(x = sex, fill = survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# --- relacao sobreviventes e idade
# Distribuicao
summary(data.combined$age)
summary(data.combined[1:891, "age"])

# Ver a distribuicao de sobreviventes, com base na idade, sexo e pclass
ggplot(data.combined[1:891, ], aes(x = age, fill = survived)) +
  facet_wrap(~ sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Ver qual as idades das pessoas com titulo Master.
boys <- data.combined[which(data.combined$title == "Master."), ]
summary(boys$age)

# Analisar pessoas com titulos Miss.
summary(misses$age)

ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for Miss. by Pclass") +
  xlab("Age") +
  ylab("Total Count")


# criacao de um conjunto de dados com todas as Misses que viajam sozinhas
# talvez util pra usar mias tarde
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))


# --- encontrar informacao relevante na coluna subsp
summary(data.combined$sibsp)

length(unique(data.combined$sibsp))
data.combined$sibsp <- as.factor(data.combined$sibsp)

ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_bar(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = " Survived")

# Pais//filhos
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_bar(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = " Survived")

# Alguns dados com relacao ao tamanho das familias
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

ggplot(data.combined[1:891, ], aes(x = family.size, fill = survived)) +
  geom_bar(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") + 
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Converter de Factor para Inteiro
# usar as.numeric diretamente so converte os indices do Factor
temp.convTest <- as.numeric(levels(data.combined$family.size))[data.combined$family.size]
summary(temp.convTest)

# Analisar a variavel ticket
str(data.combined$ticket)

# Ticket eh um Factor com 929 niveis, considerando que existem 1309 linhas de dados
# Sao muitos niveis para ser um Factor, entao sera tratado como string
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]

# Aparentemente os dados nao estao estruturados, tentar encontrar um padrao entao
# Analisar o primeiro char de cada ticket
ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket, 1, 1))
unique(ticket.first.char)

data.combined$ticket.first.char <- as.factor(ticket.first.char)

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) + 
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total count") +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total count") +
  labs(fill = "Survived")

# Aparentemente nao ha uma relacao direta entre os sobreviventes e o numero do ticket
# -----------------------------------------------------------------------------------


# Analisar as taxas pagas (Fares)
summary(data.combined$fare)
length(unique(data.combined$fare))

ggplot(data.combined[1:891,], aes(x = fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total count")

ggplot(data.combined[1:891,], aes(x = fare, fill = survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total count") +
  labs(fill = "Survived")

# Nada muito relevante
# --------------------

# Analise da variavel Cabin
str(data.combined$cabin)

# Por possuir varios Factor provavelmente nao deveria ser um. Converter para char
data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:100]

# Trocar cabines vazias por "U"
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"

# Pegar primeiro char
cabin.first.char <- as.factor(substr(data.combined$cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891, ], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.chat") +
  xlab("cabin.first.char") +
  ylab("Total count") +
  labs(fill = "Survived")

ggplot(data.combined[1:891, ], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total count") +
  labs(fill = "Survived")

# Pessoas com mais de uma cabine
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Aparentemente nada relevante
# ----------------------------


# Avaliar onde a pessoa embarcou
str(data.combined$embarked)
levels(data.combined$embarked)

ggplot(data.combined[1:891, ], aes(x = embarked, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")
  

# ---------------------
#
# Video 4
#
#----------------------



# Treinar o algoritmo com os parametros pclass e title
rf.train.1 <- data.combined[1:891, c("pclass", "title")]
rf.label <- as.factor(train$survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Treinar o algoritmo usando pclass, title e sibsp
rf.train.2 <- data.combined[1:891, c("pclass", "title", "sibsp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Treinar usando pclass, title e parch
rf.train.3 <- data.combined[1:891, c("pclass", "title", "parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# Combinar 2 e 3
rf.train.4 <- data.combined[1:891, c("pclass", "title", "sibsp", "parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)


# Treinar usando pclass, title e family.size
rf.train.5 <- data.combined[1:891, c("pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)


# Treinar usando pclass, title, sibsp family.size
rf.train.6 <- data.combined[1:891, c("pclass", "title", "sibsp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

# Treinar usando pclass, title, parch, family.size
rf.train.7 <- data.combined[1:891, c("pclass", "title", "parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

# ---------------------
#
# Video 5
#
#----------------------

# Antes do desenvolvimento de 'recursos de engenharia' (features engineering)
# precisamos estabelecer uma metodologia para estimar nossa taxa de erro no conjunto de teste.
# Isso é importante para evitar 'overfit'. O conceito aqui eh que podemos treinar
# o algoritmo ate atingir quase 100% no nosso conjunto de testes. Porem, uma taxa de acertos
# tao alta, dificilmente eh real. O que acontece no overfit eh que treinamos o algoritmo de uma forma
# muito especifica, para um conjunto especifico de dados. Dessa forma, quando exposto a um
# conjunto diferente, a taxa de erro acaba sendo muito maior.


# 
test.submit.df <- data.combined[892:1309, c("pclass", "title", "family.size")]

# Realizar a analise dos dados de teste
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Arquivo CSV para kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20190114_1.csv", row.names = FALSE)

# O resultado do kaggle foi diferente do que sugeria  OOB do rf.5
# 79.42% kaggle vs 81.82% OOB
rf.5

# Utilizando a biblioteca caret para trabalhar com cross-validation (CV)
help(package = "caret")

# Pelas pesquisas, 10-fold CV repetido 10 vezes eh a melhor maneira de comecar.
# Este processo consiste em pegar seu conjunto de dados, dividi-lo em 10 partes (10-fold),
# dessas 10, uma sera usada como conjunto de teste, e o algoritmo ira treinar com base nas 9 outras
# e validar no conjunto separado. Esse processo sera feito 10 vezes (10 times).
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

table(rf.label)
342/549

table(rf.label[cv.10.folds[[33]]])
308/494

ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

#Configurar doSNOW para treinamento multi-core
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000,
                  trControl = ctrl.1)
stopCluster(cl)

rf.5.cv.1

# Tentar com 5-fold repeated 10 times
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)
stopCluster(cl)

rf.5.cv.2


# 3-fold

set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)
stopCluster(cl)
rf.5.cv.3


# ---------------------
#
# Video 6 - Exploratory Modeling 2
#
#----------------------


# Vamos usar uma single decision tree para entender melhor o que esta acontecendo com
# as nossas features. Random Forests sao melhores do que single trees, 
# mas sao mais faceis para entender.

# Create utility function

rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30,
                    trControl = ctrl)
  
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features
features <- c("pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# Pelo plot, podemos analisar:
#     1 - Com uma taxa de acerto de 83.2%, os titulos
#         Mr. e Other pereceram.
#     2 - Titulos de Master, Miss e Mrs, na 1a e 2a classe
#         estao previstos para sobreviver com uma taxa de acerto de 94.9%
#     3 - Titulos de Master, Miss e Mrs na 3a classe com family.size igual a 5, 6, 7 e 8
#         estao previstos para perecer com 100% de certeza
#     4 - Titulos de Master, Miss e Mrs na 3a classe com family.size NAO igual a 5, 6, 7 e 8
#         estao previstos para sobreviver com uma taxa de acerto de 59.6%
#
# ---
#
# Ainda seguindo a analise do plot, ha um forte indicativo de overfitting
# com relacao ao tamanho da familia:
#   
#   O algoritmo faz a suposicao de sobrevivencia com relacao ao tamanho da familia,
# obtendo uma precisao de 100% para determinados tamanhos de familia.
# Porem, o numero de amostrar para essas quantidades de familia eh muito pequeno, logo, o algoritmo
# faz uma suposicao baseada em poucas amostras. E quando aplicado a um conjunto maior de dados,
# Eh bem provavel que havera discrepancia na acuracia observada no conjunto de teste e na submissao
# ao Kaggle

# Tanto RandomForest como rpart confirmar a importancia do titulo
table(data.combined$title)

# Parse out last name and title
data.combined[1:25, "name"]

name.splits <- str_split(data.combined$name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last names no conjunto de dados para possivel uso futuramente
data.combined$last.name <- last.names

#
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
name.splits[1]
titles <- sapply(name.splits, "[", 2)
unique(titles)

# Olhar as pessoas que possuem o titulo 'the'
data.combined[which(titles == 'the'),]

# Remapear titles
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", 'Major.')] <- "Officer"
table(titles)

# Transformar em factor
data.combined$new.title <- as.factor(titles)

# Ver a nova versao de titles
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) +
  ggtitle("Survival rates for new.title por pclass")

# Collapse titles based on visual analysis
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." | 
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."

ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) +
  ggtitle("Survival rates for new.title por pclass")


# Pegar as features identificadas
features <- c("pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]

# Rodar cross-validation e ver resultados
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

# Plotar
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# Analisar Mr. na primeira classe
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

# Ha um registro de uma mulher como Mr.
first.mr.df[first.mr.df$sex == "female",]

# Corrigir os dados
indexes <- which(data.combined$new.title == "Mr." & data.combined$sex == "female")
data.combined$new.title[indexes] <- "Mrs."

# Verificar se ha mais algum problema com genero.
summary(data.combined$new.title)

length(which(data.combined$sex == "female" & 
               (data.combined$new.title == "Master." |
                data.combined$new.title == "Mr." )))

length(which(data.combined$sex == "male" & 
               (data.combined$new.title == "Mrs." |
                  data.combined$new.title == "Miss.")))

# Atualizar os dados
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]

# Analisar sobrevivencia de Mr. na 1 classe
summary(first.mr.df[first.mr.df$survived == "1,"])
View(first.mr.df[first.mr.df$survived == "1",])

# Ver algumas das taxas altas (valores)
indexes <- which(data.combined$ticket == "PC 17755" | 
                 data.combined$ticket == "PC 17611" |
                 data.combined$ticket == "113760")
View(data.combined[indexes,])

# Ver as taxas de sobrevivencia para primeira classe, pela taxa, para Mr.
ggplot(first.mr.df, aes(x = fare, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st class 'Mr.' Survival rate by fare")

# Desenvolver features baseadas em todos os passageiros com o mesmo ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$ticket)

for(i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare











