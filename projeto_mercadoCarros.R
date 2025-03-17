# Instalar e carregar os pacotes necessários
install.packages(c("dplyr", "ggplot2", "caret", "corrplot", "randomForest"))

library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(randomForest)

# Carregar o dataset
car_data <- read.csv("/home/ianalmeida/Meu RCurso/Meu RCurso/archive/CAR DETAILS FROM CAR DEKHO.csv", stringsAsFactors = FALSE)

# Visualizar as primeiras linhas e estrutura dos dados
head(car_data)
str(car_data)

# Converter colunas categóricas para fatores (sem usar %>%)
car_data$fuel <- as.factor(car_data$fuel)
car_data$seller_type <- as.factor(car_data$seller_type)
car_data$transmission <- as.factor(car_data$transmission)
car_data$owner <- as.factor(car_data$owner)

# Verificar dados faltantes
num_na <- sum(is.na(car_data)) 
cat("Número de valores NA:", num_na, "\n")

# Remover duplicatas
car_data <- distinct(car_data)

# Filtrar valores inconsistentes (ex: ano > 2023)
car_data <- filter(car_data, year <= 2023)

# Criar gráficos exploratórios
ggplot(car_data, aes(x = selling_price)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribuição de Preços de Venda", x = "Preço", y = "Frequência")

ggplot(car_data, aes(x = km_driven, y = selling_price)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Quilometragem vs Preço", x = "Quilometragem", y = "Preço")

# Calcular preço médio por tipo de combustível
fuel_avg_price <- aggregate(selling_price ~ fuel, data = car_data, FUN = mean)

ggplot(fuel_avg_price, aes(x = fuel, y = selling_price, fill = fuel)) +
  geom_bar(stat = "identity") +
  labs(title = "Preço Médio por Tipo de Combustível", x = "Combustível", y = "Preço Médio")

# Separar os dados em treino (80%) e teste (20%)
set.seed(123)
train_index <- createDataPartition(car_data$selling_price, p = 0.8, list = FALSE)
train_data <- car_data[train_index, ]
test_data <- car_data[-train_index, ]

# Criar modelo de regressão linear
model_lm <- lm(selling_price ~ year + km_driven + fuel + seller_type + transmission + owner, data = train_data)
summary(model_lm)

# Fazer previsões e calcular RMSE
predictions <- predict(model_lm, newdata = test_data)
rmse <- sqrt(mean((test_data$selling_price - predictions)^2))
cat("RMSE:", rmse, "\n")

# Matriz de correlação
numeric_data <- car_data[, sapply(car_data, is.numeric)]
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "color")

# Criar modelo Random Forest
model_rf <- randomForest(selling_price ~ ., data = train_data, ntree = 100)
varImpPlot(model_rf, main = "Importância das Variáveis")

# Salvar o modelo e os dados processados
saveRDS(model_lm, "modelo_regressao_linear.rds")
write.csv(car_data, "car_data_processed.csv", row.names = FALSE)
