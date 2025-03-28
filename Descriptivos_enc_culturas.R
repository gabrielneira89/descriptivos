# Cargar librerías

#install.packages("pacman")
pacman::p_load(tidyverse, psych, readxl, VIM, car, nortest, rstatix, ggpubr, gplots,
               corrplot)

# Cargar datos
data <- read_csv2("Encuesta_culturas.csv")

# Observar estructura de los datos
glimpse(data) # En Sexo 1 es hombre y 2 es mujer; Edad y Esc (años de escolaridad)
              # son numéricos; en actividad (ocupación) 1 es ocupado, 2 es desocupado y
              # 3 es inactivo; c1 son las palabras que asocian a cultura (cadena); c9a es
              # si ha visitado una expo de arte en los últimos 12 meses, donde 1 es sí y 2
              # es no y; c9b es si han visitado alguna expo en su vida, donde 1 es sí y 2 es
              # no.

# Convertir variables Sexo, Actividad, c9a y c9b a factor
data$Sexo <- factor(data$Sexo, levels = c(1,2), labels = c("Masculino", "Femenino"))
data$actividad <- factor(data$actividad, levels = c(1,2,3), labels = c("Ocupado", "Desocupado","Inactivo"))
data$c9a <- factor(data$c9a, levels = c(1,2), labels = c("Sí", "No"))
data$c9b <- factor(data$c9b, levels = c(1,2), labels = c("Sí", "No"))

# Resumen de los datos
summary(data) 

# Cantidad total de NAs por columna
colSums(is.na(data)) # existen 256 NAs en c9b

# Imputar NAs
data <- hotdeck(data) # imputa datos por similaridad en las otras variables

# Elimina variables extra creadas por hotdeck
data <- data[,c(1:9)]

# Descripción de variables
data_desc <- psych::describe(data[,c(3:6,8:9)], IQR=T) %>% as.data.frame() # Selecciona variables a describir y agrega rango intercuartilico
data_desc <- rownames_to_column(data_desc, var = 'var') # transforma nombres de filas a columna de variables
data_desc <- data_desc[,-2] # elimina variable vars
data_desc$cv <- data_desc$sd/data_desc$mean # calcula coeficiente de variacion

# Calcula outliers (variables numéricas) con distancia de Mahalnobis
dat.center <- colMeans(data[,c(4,5)]) # calcula centroide
dat.cov <- cov(data[,c(4,5)]) # calcula matriz de covarianza

# Calculate Mahalanobis distance and add to data
data$mahalnobis <- mahalanobis(
  x = data[,c(4,5)],
  center = dat.center,
  cov = dat.cov
)

# Grafico de outliers
cutoff <- qchisq(p = 0.95, df = ncol(data[, 4:5])) # punto de corte segun dist. chi2
rad <- sqrt(cutoff) # radio del elipse (raiz cuadrada del punto de corte)
ellipse <- car::ellipse( # obtiene coordenadas del elipse
  center = dat.center,
  shape = dat.cov,
  radius = rad,
  segments = 100,
  draw = FALSE
)
ellipse <- as.data.frame(ellipse) # elipse a data frame
colnames(ellipse) <- colnames(data[, 4:5]) # agrega nombres de columna
data$pvalue <- pchisq(data$mahalnobis, df = ncol(data[, 4:5]), lower.tail = FALSE) # agrega valor p
data <- data %>%
  mutate(outlier = ifelse(mahalnobis > cutoff, 'sí', 'no')) # agrega outliers respecto de la dist de mahalanobis

# crea grafico de dispersion
ggplot(data, aes(x = Edad , y = esc, color = outlier)) +
  geom_point(size = 3) +
  geom_point(aes(dat.center[1], dat.center[2]) , size = 5 , color = 'blue') +
  geom_polygon(data = ellipse, fill = 'grey80', color = 'black', alpha = 0.3) +
  scale_x_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20)) +
  scale_y_continuous(limits = c(0, 25),
                     breaks = seq(0, 25, 5)) +
  scale_color_manual(values = c('black', 'orange3')) +
  labs(title = 'Detección de outliers usando distancia de Mahalanobis',
       x = 'Edad (años)',
       y = 'Escolaridad (años)') +
  theme_bw() +
  theme(plot.title = element_text(margin = margin(b = 3), size = 13,
                                  hjust = 0, color = 'black', face = quote(bold)),
        plot.subtitle = element_text(margin = margin(b = 3), size = 11,
                                     hjust = 0, color = 'black', face = quote(bold)),
        plot.caption = element_text(size = 8, color = 'black'),
        axis.title.y = element_text(size = 11, color = 'black'),
        axis.title.x = element_text(size = 11, color = 'black'),
        axis.text.x = element_text(size = 9, color = 'black'),
        axis.text.y = element_text(size = 9, color = 'black'))

# Elimina outliers de la base de datos
data <- data %>% filter(outlier=='no')

# Descripción de variables actualizada
data_desc2 <- psych::describe(data[,c(3:6,8:9)], IQR=T) %>% as.data.frame() # Selecciona variables a describir y agrega rango intercuartilico
data_desc2 <- rownames_to_column(data_desc2, var = 'var') # transforma nombres de filas a columna de variables
data_desc2 <- data_desc2[,-2] # elimina variable vars
data_desc2$cv <- data_desc2$sd/data_desc2$mean # calcula coeficiente de variacion
data_desc2 # la mediana de sexo indica una mayoria de sexo femenino, la media de edad es de 47 años, la media de escolaridad es de 11 años,
           # la mediana de actividad son ocupados, la mediana de asistencia a exposiciones en el ultimo año y si ha asistido alguna vez en 
           # vida es no
round(prop.table(table(data$Sexo))*100,2) # Sexo 42.08% masculino y 57.92% femenino
round(prop.table(table(data$actividad))*100,2) # Ocupacion 53.38% ocupado, 3.42% desocupado y 43.2% inactivo
round(prop.table(table(data$c9a))*100,2) # Asistencia a expo en ultimos 12 meses 18.44% si y 81.56% no
round(prop.table(table(data$c9b))*100,2) # Asistencia a expo alguna vez en la vida 36.28% si y 63.72% no

# Tabla de contingencia de c1
cultura_tab <- table(data$c1) %>% as.data.frame() # crea tabla y convierte a df
cultura_tab <- cultura_tab %>% filter(Var1 != 'NADA' & Var1 != 'NO SABE') # elimina las cat. nada y no sabe
cultura_tab %>% arrange(desc(Freq)) %>% 
top_n(n=10) # La cultura se asocia principalmente con arte, conocimiento, educación, costumbres y tradiciones y musica

# Calcula normalidad
lillie.test(data$Edad) # Edad no tiene distribucion normal (p<0.05)
lillie.test(data$esc) # Escolaridad no tiene distribucion normal (p<0.05)

# Comparaciones 
data %>% 
  group_by(c9a) %>%
  get_summary_stats(Edad, type = "common") # obtiene descriptivos por nivel en edad para c9a
                                           # la mediana de edad es 40 y la media 41.7 en asistencia
                                           # la mediana de edad es de 49 y la media de 48.4 en sin asistencia

data %>% 
  group_by(c9a) %>%
  get_summary_stats(esc, type = "common") # obtiene descriptivos por nivel en edad para c9a
                                          # la mediana de escolaridad es 13 y la media 13.4 en asistencia  
                                          # la mediana de escolaridad es de 12 y la media de 10.9 en sin asistencia

data %>% 
  group_by(c9b) %>%
  get_summary_stats(Edad, type = "common") # obtiene descriptivos por nivel en edad para c9a
                                           # la mediana de edad es 44 y la media 45.4 en asistencia
                                           # la mediana de edad es de 49 y la media de 48.2 en sin asistencia

data %>% 
  group_by(c9b) %>%
  get_summary_stats(esc, type = "common") # obtiene descriptivos por nivel en edad para c9a
                                          # la mediana de escolaridad es 12 y la media 12.8 en asistencia  
                                          # la mediana de escolaridad es de 12 y la media de 10.5 en sin asistencia

# Visualizacion de diferencias en Edad para c9a
ggboxplot(data = data, x='c9a', y='Edad',
           xlab ='¿Ha asistido a alguna exposición en los últimos 12 meses?',
           ylab = 'Edad en años',
          add = 'mean') 

# Visualizacion de diferencias en Edad para c9b
ggboxplot(data = data, x='c9b', y='Edad',
          xlab ='¿Ha asistido a alguna exposición en su vida?',
          ylab = 'Edad en años',
          add = 'mean') 

# Visualizacion de diferencias en Escolaridad para c9a
ggboxplot(data = data, x='c9a', y='esc',
          xlab ='¿Ha asistido a alguna exposición en los últimos 12 meses?',
          ylab = 'escolaridad en años',
          add = 'mean')

# Visualizacion de diferencias en Escolaridad para c9b
ggboxplot(data = data, x='c9b', y='esc',
          xlab ='¿Ha asistido a alguna exposición en su vida?',
          ylab = 'Escolaridad en años',
          add = 'mean')

# Aplica wilcoxon para variables c9a y c9b segun Edad y Escolaridad

# Calculo wilcoxon en Edad y c9a
data %>% wilcox_test(Edad ~ c9a) # existe diferencia significativa (> edad en quienes no han asistido)
data %>% wilcox_effsize(Edad ~ c9a) # la magnitud del efecto es pequeña

# Calculo wilcoxon en Escolaridad y c9a
data %>% wilcox_test(esc ~ c9a) # existe diferencia significativa (> escolaridad en quienes han asistido)
data %>% wilcox_effsize(esc ~ c9a) # la magnitud del efecto es pequeña

# Calculo wilcoxon en Edad y c9b
data %>% wilcox_test(Edad ~ c9b) # existe diferencia significativa (> edad en quienes no han asistido)
data %>% wilcox_effsize(Edad ~ c9b) # la magnitud del efecto es pequeña

# Calculo wilcoxon en Escolaridad y c9b
data %>% wilcox_test(esc ~ c9b) # existe diferencia significativa (> escolaridad en quienes han asistido)
data %>% wilcox_effsize(esc ~ c9b) # la magnitud del efecto es pequeña

# Calculo Chi cuadrado entre variables nominales

# Crea grafico para Sexo vs Asistencia 12 meses
dt_sexo_c9a <- as.table(as.matrix(table(data[,c(3,8)])))
colnames(dt_sexo_c9a) <- c('Sí', 'No') 
rownames(dt_sexo_c9a) <- c('Masculino', 'Femenino') 
balloonplot(t(dt_sexo_c9a), main ="Sexo vs Asistencia en últimos 12 meses", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Crea grafico para Sexo vs Asistencia en la vida
dt_sexo_c9b <- as.table(as.matrix(table(data[,c(3,9)])))
colnames(dt_sexo_c9b) <- c('Sí', 'No') 
rownames(dt_sexo_c9b) <- c('Masculino', 'Femenino') 
balloonplot(t(dt_sexo_c9b), main ="Sexo vs Asistencia alguna vez en la vida", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Crea grafico para Ocupacion vs Asistencia 12 meses
dt_act_c9a <- as.table(as.matrix(table(data[,c(6,8)])))
colnames(dt_act_c9a) <- c('Sí', 'No') 
rownames(dt_act_c9a) <- c('Ocupado', 'Desocupado','Inactivo') 
balloonplot(t(dt_act_c9a), main ="Ocupación vs Asistencia en últimos 12 meses", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Crea grafico para Ocupacion vs Asistencia en la vida
dt_act_c9b <- as.table(as.matrix(table(data[,c(6,9)])))
colnames(dt_act_c9b) <- c('Sí', 'No') 
rownames(dt_act_c9b) <- c('Ocupado', 'Desocupado','Inactivo')  
balloonplot(t(dt_act_c9b), main ="Ocupación vs Asistencia alguna vez en la vida", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


# Aplica test Chi cuadrado

# Chisq entre c9a y Sexo
chisq_c9a_sexo <- chisq.test(data$c9a, data$Sexo)
chisq_c9a_sexo # la asociacion no es estadisticamente signif. (p>0.05)
chisq_c9a_sexo$observed
round(chisq_c9a_sexo$expected,2) # la diferencia entre recuento observado y esperado es minima

# Chisq entre c9a y Actividad
chisq_c9a_act <- chisq.test(data$c9a, data$actividad)
chisq_c9a_act # la asociacion es estadisticamente signif. (p<0.05)
chisq_c9a_act$observed
round(chisq_c9a_act$expected,2) # existe una leve diferencia en los recuentos obs y esperados
round(chisq_c9a_act$residuals, 3) 
corrplot(chisq_c9a_act$residuals, is.cor = FALSE) # segun los residuos, existe una asociacion positiva entre la asistencia a expos
                                                  #en los ultimos 12 meses y quienes estan ocupados y desocupados, mientras que los
                                                  # inactivos presentan una asociacion negativa. Para quienes no han asistido, una
                                                  # asociacion inversa a la anterior.
# calcula contribucion
round(100*chisq_c9a_act$residuals^2/chisq_c9a_act$statistic,3) # La categoria que mas contribuye en ambas cat es desocupado

# Chisq entre c9b y Sexo
chisq_c9b_sexo <- chisq.test(data$c9b, data$Sexo)
chisq_c9b_sexo # la asociacion no es estadisticamente signif. (p>0.05)
chisq_c9b_sexo$observed
round(chisq_c9b_sexo$expected,2) # existen leves diferencias en los recuentos obs y esperados

# Chisq entre c9b y Actividad
chisq_c9b_act <- chisq.test(data$c9b, data$actividad)
chisq_c9b_act # la asociacion no es estadisticamente signif. (p>0.05)
chisq_c9b_act$observed
round(chisq_c9b_act$expected,2) # existen leves diferencia en los recuentos obs y esperados
