library(readr)
df <- read_csv("D:/ProyectoAED/encuesta_salud_digital.csv")

library(ggplot2)
library(dplyr)

mapa <- c("Nunca"=0, "Casi nunca"=1, "Algunas veces"=2, "Frecuentemente"=3, "Siempre"=4)

variables <- c("Publico", "Scroll", "Comento", "Comparo", "UsoRedes")

for (v in variables) {
  cat("\n-----", v, "-----\n")
  x <- mapa[df[[v]]]
  print(c(
    media = mean(x),
    mediana = median(x),
    varianza = var(x),
    desviacion = sd(x),
    cv = sd(x) / mean(x)
  ))
}





ggplot(df, aes(x = Publico)) +
  geom_bar(fill = "#1f78b4") +
  theme_minimal() +
  labs(title = "Frecuencia de publicación de fotos/historias")
ggsave("frecuencia_publicacion_fotos.png", width = 8, height = 4)

# Tabla de frecuencias absolutas
tabla_pub <- table(df$Publico)
tabla_pub

# Tabla de frecuencias relativas (proporciones)
tabla_pub_prop <- prop.table(tabla_pub)
tabla_pub_prop

# Si quieres porcentajes en %
round(100 * tabla_pub_prop, 1)

ggplot(df, aes(x = Scroll)) +
  geom_bar(fill = "#1f78b4") +
  theme_minimal() +
  labs(title = "Frecuencia de scroll pasivo", x = "Scroll pasivo", y = "Conteo")
ggsave("scroll_pasivo.png", width = 8, height = 4)

tabla_scroll <- table(df$Scroll)
tabla_scroll
round(100 * prop.table(tabla_scroll), 1)

ggplot(df, aes(x = Comento)) +
  geom_bar(fill = "#1f78b4") +
  theme_minimal() +
  labs(title = "Frecuencia con la que comento o reacciono a publicaciones",
       x = "Comento / reacciono",
       y = "Conteo")

ggsave("comento.png", width = 8, height = 4)

tabla_comento <- table(df$Comento)
tabla_comento
round(100 * prop.table(tabla_comento), 1)

ggplot(df, aes(x = Comparo)) +
  geom_bar(fill = "#1f78b4") +
  theme_minimal() +
  labs(title = "Frecuencia con la que me comparo con otras personas por lo que publican",
       x = "Comparo",
       y = "Conteo")

ggsave("comparo.png", width = 8, height = 4)

tabla_comparo <- table(df$Comparo)
tabla_comparo
round(100 * prop.table(tabla_comparo), 1)

ggplot(df, aes(x = UsoRedes)) +
  geom_bar(fill = "#1f78b4") +
  theme_minimal() +
  labs(title = "Frecuencia con la que uso redes para buscar ideas, información o inspiración",
       x = "Uso de redes para ideas",
       y = "Conteo")

ggsave("usoredes.png", width = 8, height = 4)

tabla_usoredes <- table(df$UsoRedes)
tabla_usoredes
round(100 * prop.table(tabla_usoredes), 1)

library(readxl)
Edad_estatura_peso <- read_excel("Edad_estatura_peso.xlsx")
View(Edad_estatura_peso)

# 1. Revisar que tengan el mismo número de filas
nrow(df)
nrow(Edad_estatura_peso)

# 2. Renombrar columnas de Edad_estatura_peso para que sean cómodas
names(Edad_estatura_peso) <- c("Edad", "Estatura", "Peso")

# 3. Unir columnas al df principal
df$Edad     <- Edad_estatura_peso$Edad
df$Estatura <- Edad_estatura_peso$Estatura
df$Peso     <- Edad_estatura_peso$Peso

library(dplyr)

# Asegúrate de que estas columnas existen en df:
# Publico, Scroll, Comento, Comparo, UsoRedes, Edad, Estatura, Peso

# Orden correcto de las categorías
orden <- c("Nunca", "Casi nunca", "Algunas veces", "Frecuentemente", "Siempre")

actividades <- c("Publico", "Scroll", "Comento", "Comparo", "UsoRedes")

df <- df %>%
  mutate(
    # convertir actividades a factores ORDENADOS
    across(all_of(actividades),
           ~ factor(.x, levels = orden, ordered = TRUE)),
    # asegurar que las cuantitativas sean numéricas
    Edad     = as.numeric(Edad),
    Estatura = as.numeric(Estatura),
    Peso     = as.numeric(Peso)
  )

str(df[, c(actividades, "Edad", "Estatura", "Peso")])  # sólo para verificar
library(ggplot2)

orden <- c("Nunca", "Casi nunca", "Algunas veces", "Frecuentemente", "Siempre")

actividades <- c("Publico", "Scroll", "Comento", "Comparo", "UsoRedes")

for(a in actividades){
  df[[a]] <- factor(df[[a]], levels = orden)
}
df$Edad     <- as.numeric(df$Edad)
df$Estatura <- as.numeric(df$Estatura)
df$Peso     <- as.numeric(df$Peso)

library(ggplot2)

actividades <- c("Publico", "Scroll", "Comento", "Comparo", "UsoRedes")
vars_qt     <- c("Edad", "Estatura", "Peso")

for (act in actividades){
  for (v in vars_qt){
    p <- ggplot(df, aes(x = .data[[act]], y = .data[[v]])) +
      geom_violin(fill = "#a6cee3", color = "black", alpha = 0.8) +
      geom_jitter(width = 0.15, alpha = 0.5, color = "#1f78b4") +
      theme_minimal() +
      labs(title = paste(v, "según categorías de", act),
           x = act,
           y = v) +
      coord_flip()
    
    print(p)
    
    ggsave(
      paste0(tolower(act), "_", tolower(v), "_violin.png"),
      plot = p,
      width = 7, height = 4
    )
  }
}

library(ggplot2)
Edad_estatura_peso$Edad <- as.numeric(Edad_estatura_peso$Edad)
Edad_estatura_peso$Estatura <- as.numeric(Edad_estatura_peso$Estatura)
Edad_estatura_peso$Peso <- as.numeric(Edad_estatura_peso$Peso)
Edad_estatura_peso$Peso <- as.numeric(gsub(",", ".", Edad_estatura_peso$Peso))

# Histograma Edad
ggplot(Edad_estatura_peso, aes(x = Edad)) +
  geom_histogram(binwidth = 1, color="black", fill="skyblue") +
  labs(title="Histograma de Edad", x="Edad", y="Frecuencia") +
  theme_minimal()

# Histograma Estatura
ggplot(Edad_estatura_peso, aes(x = Estatura)) +
  geom_histogram(binwidth = 3, color="black", fill="lightgreen") +
  labs(title="Histograma de Estatura", x="Estatura (cm)", y="Frecuencia") +
  theme_minimal()

# Histograma Peso
ggplot(Edad_estatura_peso, aes(x = Peso)) +
  geom_histogram(binwidth = 3, color="black", fill="lightpink") +
  labs(title="Histograma de Peso", x="Peso (kg)", y="Frecuencia") +
  theme_minimal()

# Boxplot Edad
ggplot(Edad_estatura_peso, aes(y = Edad)) +
  geom_boxplot(fill="skyblue") +
  labs(title="Boxplot de Edad", y="Edad") +
  theme_minimal()

# Boxplot Estatura
ggplot(Edad_estatura_peso, aes(y = Estatura)) +
  geom_boxplot(fill="lightgreen") +
  labs(title="Boxplot de Estatura", y="Estatura (cm)") +
  theme_minimal()

# Boxplot Peso
ggplot(Edad_estatura_peso, aes(y = Peso)) +
  geom_boxplot(fill="lightpink") +
  labs(title="Boxplot de Peso", y="Peso (kg)") +
  theme_minimal()

library(dplyr)

graficar_dona <- function(data, variable, titulo){
  data %>%
    count({{ variable }}) %>%
    ggplot(aes(x="", y=n, fill={{ variable }})) +
    geom_col(width=1, color="white") +
    coord_polar("y") +
    geom_text(aes(label = paste0(round(n/sum(n)*100,1), "%")),
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette="Set3") +
    labs(title=titulo, fill=titulo) +
    theme_void()
}
graficar_dona(df, Comento, "Frecuencia con la que comenta")
graficar_dona(df, Comparo, "Frecuencia con la que se compara")
graficar_dona(df, Publico, "Frecuencia con la que publica")
graficar_dona(df, Scroll, "Frecuencia de Scroll")
graficar_dona(df, UsoRedes, "Frecuencia de uso de redes")

library(ggplot2)

# ===========================
# 1. Comento vs UsoRedes
# ===========================

p1 <- ggplot(df, aes(x = Comento, fill = UsoRedes)) +
  geom_bar(position = "fill") +
  labs(title = "Comento vs UsoRedes",
       x = "Comento",
       y = "Proporción",
       fill = "UsoRedes") +
  theme_minimal()

ggsave("comento_vs_usoredes.png",
       plot = p1,
       width = 8,
       height = 5,
       dpi = 300)


# ===========================
# 2. Comento vs Comparo
# ===========================

p2 <- ggplot(df, aes(x = Comento, fill = Comparo)) +
  geom_bar(position = "fill") +
  labs(title = "Comento vs Comparo",
       x = "Comento",
       y = "Proporción",
       fill = "Comparo") +
  theme_minimal()

ggsave("comento_vs_comparo.png",
       plot = p2,
       width = 8,
       height = 5,
       dpi = 300)


# ===========================
# 3. Publico vs Scroll
# ===========================

p3 <- ggplot(df, aes(x = Publico, fill = Scroll)) +
  geom_bar(position = "fill") +
  labs(title = "Publico vs Scroll",
       x = "Publico",
       y = "Proporción",
       fill = "Scroll") +
  theme_minimal()

ggsave("publico_vs_scroll.png",
       plot = p3,
       width = 8,
       height = 5,
       dpi = 300)


# ===========================
# 4. UsoRedes vs Scroll
# ===========================

p4 <- ggplot(df, aes(x = UsoRedes, fill = Scroll)) +
  geom_bar(position = "fill") +
  labs(title = "UsoRedes vs Scroll",
       x = "Uso de Redes",
       y = "Proporción",
       fill = "Scroll") +
  theme_minimal()

ggsave("usoredes_vs_scroll.png",
       plot = p4,
       width = 8,
       height = 5,
       dpi = 300)

