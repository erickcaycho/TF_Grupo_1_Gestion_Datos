#Uso de la librería mongolite para conectarnos al dataset almacenado en Atlas MongoDB
#Uso de la librería ggplot2 para realizar las gráficas
#Uso de la librería dplyr para manipular los datos
#Uso de la librería VIM para explorar e imputar valores faltantes
library(mongolite)
library(ggplot2)
library(dplyr)
library(VIM)


#Se establece la cadena de conexión
connection_string = 'mongodb+srv://user:user1234@clustergrupo1.53efwvx.mongodb.net/'

#Se asigna una variable contacto_collection para invocar a la cadena de conexión e incluir los otros atributos para conectarnos a MongoBD
contacto_collection = mongo(collection="contactabilidad", db="cobranza", url = connection_string)

#Se valida la conexión realizando una operación de conteo de registros. Si la conexión es exitosa, debe retornar la cantidad de registros almacenados
contacto_collection$count()

#Definición del dataframe para trabajar en R
df <- as.data.frame(contacto_collection$find())
head(df,5)

#Summary del dataset
summary(df)

#Estructura del dataset
str(df)


#Variables categoricas a analizar segun resumen de R:
# Cliente: al ser un codigo de cliente no se toma en cuenta (SE DESCARTARA EN ANALISIS)
# nro_vec_cob: se hace analisis para ver posibles valores
# pdps_rotas: se hace analisis para ver posibles valores
# nro_cuotas: se hace analisis para ver posibles valores
# fechallamada: se hace analisis para ver posibles valores
# mes_2: valor numerico, se hace analisis para ver posibles valores
# estatus: se hace analisis para ver posibles valores
# tipocontacto: Variable dependiente que se hace analisis para ver posibles valores

################################################################################
#ANALISIS NUMERO DE VECES QUE CLIENTE CAYO EN COBRANZA (nro_vec_cob)
################################################################################
#Identificar valores únicos en número de veces que cliente cayó en cobranza
cs_nro_veces_cliente_cayo_cobranza <- sort(unique(df$nro_vec_cob))
cs_nro_veces_cliente_cayo_cobranza

#Al descubrir vemos que solo existen 3 valores: nulo, <=10 y mayor a 10
df$vecescobranza <- factor(df$nro_vec_cob, levels = cs_nro_veces_cliente_cayo_cobranza, ordered=T)
frec <- table(df$nro_vec_cob)
porcent <- (prop.table(table(df$nro_vec_cob))*100) %>% round(digits = 1)

#revisamos valores de etiquetas
nombres <- names(frec)

# Reemplazamos solo los nombres vacíos por "NA"
nombres[nombres == ""] <- "NA"

# Construimos las etiquetas
etiquetas <- paste0(nombres, ": ", round(porcent, 1), "%")

# Mostramos gráficamente clases
barplot(frec,
        main = "Distribución de veces que cliente cae en cobranza",
        xlab = " ",
        ylab = "Cantidad de Clientes",
        col = rainbow(length(frec)),
        names.arg = etiquetas,
        las = 2,               # rotar etiquetas verticalmente
        cex.names = 0.8        # tamaño de texto de etiquetas
)


################################################################################
#ANALISIS NUMERO DE PROMESAS QUE CLIENTE HIZO SIN CUMPLIR (pdps_rotas)
################################################################################
#Identificar valores únicos en número de veces que cliente cayó en cobranza
cs_nro_veces_cliente_incumple_promesa <- sort(unique(df$pdps_rotas))
cs_nro_veces_cliente_incumple_promesa


# Paso 1: Calcular la frecuencia de cada valor
df_pie <- df %>%
  count(pdps_rotas) %>%
  mutate(porcentaje = n / sum(n) * 100,
         etiqueta = paste0(pdps_rotas, ": ", round(porcentaje, 1), "%"))

# Paso 2: Graficar el pie chart
ggplot(df_pie, aes(x = "", y = porcentaje, fill = etiqueta)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Incumplimiento de promesas por cliente") +
  theme_void() +
  theme(legend.title = element_blank())


################################################################################
#ANALISIS NUMERO DE CUOTAS ADEUDADAS (nro_cuotas)
################################################################################
#Identificar valores únicos en número de veces que cliente cayó en cobranza
cs_nro_cuotas_adeudadas <- sort(unique(df$nro_cuotas))
cs_nro_cuotas_adeudadas

# Paso 1: Calcular la frecuencia de cada valor
df_pie <- df %>%
  count(nro_cuotas) %>%
  mutate(porcentaje = n / sum(n) * 100,
         etiqueta = paste0(nro_cuotas, ": ", round(porcentaje, 1), "%"))

# Paso 2: Graficar el pie chart
ggplot(df_pie, aes(x = "", y = porcentaje, fill = etiqueta)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Número de cuotas adeudadas") +
  theme_void() +
  theme(legend.title = element_blank())


################################################################################
#ANALISIS FECHA DE LLAMADAS (fechallamada)
################################################################################
#Identificar valores distintos
cs_fecha_llamada <- sort(unique(df$fechallamada))
cs_fecha_llamada

#en este caso por tener todos un formato dd/mm/yyyy lo pasamos a convertir 
#en el dia de la semana que aplica
df$dia_semana <- sapply(as.Date(df$fechallamada, format = "%d/%m/%Y"), weekdays)

# frecuencia solo para casos de exito
df_pie <- df %>%
  filter(tipocontacto == "COEF") %>%
  count(dia_semana) %>%
  mutate(porcentaje = n / sum(n) * 100,
         etiqueta = paste0(dia_semana, ": ", round(porcentaje, 1), "%"))

# grafico
ggplot(df_pie, aes(x = "", y = porcentaje, fill = etiqueta)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Día Semana con contacto exitoso") +
  theme_void() +
  theme(legend.title = element_blank())


################################################################################
#ANALISIS HORA DE LLAMADA (hora)
################################################################################
#Identificar valores distintos
cs_fecha_llamada <- sort(unique(df$hora))
cs_fecha_llamada

#se ve una variabilidad de: 7  8  9 10 11 12 13 14 15 16 17 18 19 20

df <- df %>%
  mutate(periodo = case_when(
    hora <= 12 ~ "mañana",
    hora <= 16 ~ "tarde",
    hora <= 24 ~ "noche"
  ))

# frecuencia por hora del dia
df_pie <- df %>%
  count(periodo) %>%
  mutate(porcentaje = n / sum(n) * 100,
         etiqueta = paste0(periodo, ": ", round(porcentaje, 1), "%"))

# grafico
ggplot(df_pie, aes(x = "", y = porcentaje, fill = etiqueta)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Horario mayormente usado") +
  theme_void() +
  theme(legend.title = element_blank())


#COMPARACION DE NULOS EN TODAS LAS VARIABLES
agreg <- aggr(df,numbers=TRUE)
agreg
summary(agreg)

x11()
matrixplot(df)

#Detección de valores atípicos

# Función para reemplazar outliers por NA usando IQR
replace_outliers <- function(x) {
  if(!is.numeric(x)) return(x)  # Sólo para numéricos
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x_out <- ifelse(x < lower_bound | x > upper_bound, NA, x)
  return(x_out)
}

# Variables numéricas que vamos a limpiar
num_vars <- c("nro_vec_cob", "pdps_rotas", "nro_cuotas")

# Aplicar reemplazo outliers a estas columnas
for (var in num_vars) {
  if (var %in% names(df)) {
    df[[var]] <- replace_outliers(df[[var]])
  } else {
    warning(paste("Variable", var, "no existe en df"))
  }
}
# Función para calcular la moda
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Variables categóricas a imputar
cat_vars <- c("estatus", "tipocontacto")

# Imputación para variables categóricas
for (var in cat_vars) {
  if (var %in% names(df)) {
    mode_value <- get_mode(df[[var]])
    df[[var]][is.na(df[[var]])] <- mode_value
    df[[var]] <- as.factor(df[[var]])  # Convertir a factor
  } else {
    warning(paste("Variable", var, "no existe en df"))
  }
}

# Imputación para variables numéricas
for (var in num_vars) {
  if (var %in% names(df)) {
    median_value <- median(df[[var]], na.rm = TRUE)
    df[[var]][is.na(df[[var]])] <- median_value
  }
}
library(VIM)
agreg2 <- aggr(df, numbers=TRUE)
summary(agreg2)

#Imputación KNN
# Variables a imputar
vars_to_impute <- c("nro_vec_cob", "pdps_rotas", "nro_cuotas", "estatus", "tipocontacto", "mes_2")

# kNN imputación
df_imputed <- kNN(df, variable = vars_to_impute, k = 5)

# Revisar NA después de imputación
sapply(df_imputed[, vars_to_impute], function(x) sum(is.na(x)))

#Comparación de variables imputadas
# Gráfico 1: Comparación NAs antes y después para las variables imputadas
na_comparison_long <- pivot_longer(na_comparison,
                                     +                                    cols = c("Antes", "Despues"),
                                     +                                    names_to = "Estado",
                                     +                                    values_to = "NAs")
ggplot(na_comparison_long, aes(x = Variable, y = NAs, fill = Estado)) +
  +     geom_bar(stat = "identity", position = "dodge") +
  +     theme_minimal() +
  +     labs(title = "Comparación de valores faltantes antes y después de la imputación KNN",
             +          x = "Variable",
             +          y = "Cantidad de NAs") +
  +     theme(plot.title = element_text(hjust = 0.5, face = "bold"))
na_counts_before <- sapply(df[, vars_to_impute], function(x) sum(is.na(x)))
na_comparison <- data.frame(
  Variable = vars_to_impute,
  Antes = na_counts_before,
  Despues = na_counts_after
)
na_comparison_long <- pivot_longer(na_comparison, cols = c("Antes", "Despues"),
                                   names_to = "Estado", values_to = "NAs")

ggplot(na_comparison_long, aes(x = Variable, y = NAs, fill = Estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de NAs antes y después de la imputación",
       y = "Número de valores faltantes") +
  theme_minimal()

#Gráfico 2: Boxplots para detectar valores atípicos en variables numéricas antes y después

# Seleccionar variables numéricas para boxplots
num_vars <- c("nro_vec_cob", "pdps_rotas", "nro_cuotas", "mes_2")

# Antes de imputar (para variables numéricas)
df_before_long <- pivot_longer(df[, num_vars], cols = everything(),
                               names_to = "variable", values_to = "valor")
df_before_long$Estado <- "Antes"

# Después de imputar
df_after_long <- pivot_longer(df_imputed[, num_vars], cols = everything(),
                              names_to = "variable", values_to = "valor")
df_after_long$Estado <- "Después"

# Juntar ambos para comparación
df_boxplot <- rbind(df_before_long, df_after_long)

ggplot(df_boxplot, aes(x = variable, y = valor)) +
  geom_boxplot(aes(color = Estado), outlier.shape = 8, outlier.size = 2, position = position_dodge(0.8)) +
  labs(title = "Boxplot antes y después de la imputación (variables numéricas)",
       y = "Valor", x = "Variable") +
  theme_minimal()

ggplot(na_comparison_long, aes(x = Variable, y = Nulos, fill = Estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Conteo de valores NA antes y después de la imputación",
       x = "Variable",
       y = "Cantidad de NAs") +
  theme_minimal()

library(mongolite)

# Gráfico 3: Matriz de valores faltantes antes y después (para vista rápida)

# Antes
aggr_before <- aggr(df[, vars_to_impute], numbers = TRUE, sortVars = TRUE, main = "Valores faltantes antes de imputar")

# Después
aggr_after <- aggr(df_imputed[, vars_to_impute], numbers = TRUE, sortVars = TRUE, main = "Valores faltantes después de imputar"

