library(readxl)
datos1 <- read_excel("", 
                                                                                    col_types = c("numeric", "numeric", "numeric", 
                                                                                                  "text", "numeric", "numeric", "text", 
                                                                                                  "text", "numeric", "numeric", "numeric", 
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "numeric", "numeric", "text", "numeric", 
                                                                                                  "text", "text", "text", "text", "numeric", 
                                                                                                  "numeric", "numeric", "text", "text", 
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "text", "text", "text", "numeric", 
                                                                                                  "text", "text", "text", "numeric", 
                                                                                                  "text", "text", "text", "numeric", 
                                                                                                  "numeric", "text", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "text", "numeric", "text", "numeric", 
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "text"))
View(datos1)




df <- subset(datos1, select = -c(Email, Name, `Last modified time`, `Start time`, `Completion time`))



df[9, 14] <- "5"

df[[14]] <- as.numeric(df[[14]])








cols <- c(28, 29, 30, 31)
df <- df[complete.cases(df[, cols]), ]





escala_likert <- c("Totalmente en desacuerdo",
                   "En desacuerdo",
                   "Ni de acuerdo ni en desacuerdo",
                   "De acuerdo",
                   "Totalmente de acuerdo")




for (i in cols) {
  df[[i]] <- factor(df[[i]],
                    levels = escala_likert,
                    ordered = TRUE)
}


moda <- function(x) {
  tmp <- table(x)
  names(tmp)[which.max(tmp)]
}


resumen_ordinal <- function(x) {
  
  # Convertir a código: 1,2,3,4,5
  x_num <- as.numeric(x)
  
  # Mediana en categoría
  mediana_cat <- levels(x)[median(x_num)]
  
  # Cuartiles (Q1, Q2, Q3) en categorías
  qs_num <- quantile(x_num, probs = c(0.25, 0.5, 0.75), type = 2)
  qs_cat <- levels(x)[qs_num]

  iqr_num <- IQR(x_num, type = 2)
  
  list(
    Mediana = mediana_cat,
    Moda = moda(x),
    Cuartiles = qs_cat,
    IQR = iqr_num
  )
}

resultados <- lapply(df[cols], resumen_ordinal)

# Mostrar resultados
resultados

View(df)

# Graficos de Barras

library(ggplot2)

for (i in cols) {
  
  nombre_variable <- names(df)[i]
  
  print(
    ggplot(df, aes(x = df[[i]])) +
      geom_bar(fill = "seagreen3") +
      labs(title = nombre_variable,
           x = "Respuesta",
           y = "Frecuencia") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
  
}

#Analisis Multivariado
install.packages("psych")
library(psych)

# Crear un dataset numérico para policórica
items_poly <- df[, cols]

# Convertir factores ordenados a códigos numéricos 1–5
items_poly[] <- lapply(items_poly, function(x) as.numeric(x))

# Calcular matriz policórica
poly_matrix <- polychoric(items_poly)$rho

library(reshape2)

colnames(poly_matrix) <- cols
rownames(poly_matrix) <- cols

df_poly <- melt(poly_matrix)

# Gráfico
ggplot(df_poly, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1, 1)) +
  geom_text(aes(label = round(value, 2)), size = 5) +
  labs(title = "Matriz de correlaciones policóricas",
       x = "", y = "", fill = "ρ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))



#######

for (i in cols) {
  
  nombre_variable <- paste("Pregunta", i)
  
  print(
    ggplot(df, aes(x = df[[i]], y = df[["¿Cuántos kilogramos pesa?" ]])) +
      geom_boxplot(fill = "skyblue") +
      labs(title = paste("Boxplot de Peso según", nombre_variable),
           x = "Respuesta Likert",
           y = "Peso") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}
