# T4 Parte 1


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)

azul  <- "#223c6a"
oro   <- "#e69b0a"
pesos <- label_currency(big.mark = ".", decimal.mark = ",")



# Gráfico 1 · CASEN: educación e ingreso por género

casen <- read.csv("data/raw/casen_reducido.csv") |> filter(!is.na(ingreso))

g1 <- ggplot(casen, aes(x = educ, y = ingreso, color = genero)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("F" = oro, "M" = azul), labels = c("F" = "Mujeres", "M" = "Hombres")) +
  scale_y_continuous(labels = pesos) +
  labs(
    title    = "El retorno salarial por educación es mayor en mujeres, acortando la brecha en altos niveles",
    subtitle = "Ingreso mensual y años de escolaridad según género (CASEN, n = 55)",
    x        = "Años de educación",
    y        = NULL,
    color    = "Género",
    caption  = "Fuente: Elaboración propia en base a submuestra CASEN (5 NA excluidos)."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = azul), panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("output/figuras/t4_p1_g1.png", g1, width = 8, height = 5, dpi = 300)

#resultados
lm(ingreso ~ educ, data = casen)

lm(ingreso ~ educ, data = casen, subset = genero == "M")

lm(ingreso ~ educ, data = casen, subset = genero == "F")

# RESPUESTA GRÁFICO 1:
# Responde si estudiar rinde lo mismo en sueldo para hombres y mujeres.
# No rinde igual. Con una sola recta se asume que cada año de estudio les suma
# lo mismo a todos ($42.080), tapando la brecha real. Al separar por sexo se ve que 
# los hombres parten con una base mucho más alta en baja escolaridad ($193.555 frente 
# a $32.295 en mujeres), pero a las mujeres cada año extra de estudio les rinde más 
# ($46.862 vs $38.295 en hombres). Las dos rectas muestran que a mayor nivel 
# educativo la diferencia de sueldos va disminiyendo.


# Gráfico 2 · Ingreso mínimo: nominal contra real

salario <- read.csv("data/raw/salario_minimo_chile.csv") |>
  mutate(real = salario_minimo * (100 / ipc)) |>
  pivot_longer(cols = c(salario_minimo, real), names_to = "tipo", values_to = "monto") |>
  mutate(tipo_etiqueta = if_else(tipo == "salario_minimo", "Nominal", "Real (pesos de 2010)"))

etiquetas_2025 <- salario |> filter(anio == max(anio))

g2 <- ggplot(salario, aes(x = anio, y = monto, color = tipo_etiqueta, linetype = tipo_etiqueta)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_text(
    data        = etiquetas_2025,
    aes(label   = paste0(tipo_etiqueta, ":\n", pesos(round(monto)))),
    hjust       = -0.1,
    size        = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Nominal" = azul, "Real (pesos de 2010)" = oro)) +
  scale_linetype_manual(values = c("Nominal" = "solid", "Real (pesos de 2010)" = "dashed")) +
  scale_x_continuous(breaks = 2014:2025, limits = c(2014, 2027.5)) +
  scale_y_continuous(labels = pesos) +
  labs(
    title    = "La inflación absorbió gran parte del alza nominal del salario mínimo entre 2014 y 2025",
    subtitle = "Evolución del ingreso mínimo mensual nominal y real en pesos de 2010",
    x        = "Año",
    y        = NULL,
    caption  = "Fuente: salario_minimo_chile.csv (INE, IPC base 2010 = 100)."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = azul), panel.grid.minor = element_blank(), legend.position = "none")

ggsave("output/figuras/t4_p1_g2.png", g2, width = 8.5, height = 5, dpi = 300)

# RESPUESTAS GRÁFICO 2:
# 1) En el grafico de barras el valor se mide por el largo de la columna desde el piso;
# cortar el eje Y deforma las diferencias y engaña a la vista. En el de líneas se mira 
# la posición y la tendencia; ajustar el rango del eje permite ver cómo sube o baja el salario 
# en el tiempo con claridad, siempre que los valores estén bien rotulados.
#
# 2) ¿Qué ganas usando color Y tipo de línea?
# Aplica redundancia visual: se usan dos canales distintos (color y línea continua 
# o punteada) para la misma serie. Esto permite que personas con daltonismo distingan 
# las líneas sin problemas, asegura que el gráfico se entienda impreso en blanco y negro, 
# y facilita identificar cada línea sin necesitar leyendas aparte.



# Gráfico 3 · ENE: en qué trabajan hombres y mujeres

ene <- read.csv2("data/raw/ene-2026-06-mjj.csv") |>
  filter(activ == 1, !is.na(categoria_ocupacion), !is.na(fact_cal)) |>
  mutate(
    sexo_txt  = if_else(sexo == 1, "Hombres", "Mujeres"),
    categoria = factor(
      categoria_ocupacion,
      levels = 1:7,
      labels = c("Empleador", "Cuenta propia", "Asalariado privado", "Asalariado público", "Serv. doméstico (afuera)", "Serv. doméstico (adentro)", "Familiar no rem.")
    )
  )

g3 <- ggplot(ene, aes(x = sexo_txt, fill = categoria, weight = fact_cal)) +
  geom_bar(position = "fill", width = 0.55, color = "white", linewidth = 0.2) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_viridis_d(option = "D") +  # Paleta accesible viridis
  labs(
    title    = "Las mujeres duplican la proporción de empleo público y servicio doméstico respecto a los hombres",
    subtitle = "Composición de ocupados por sexo según categoría ocupacional (ENE mayo-julio 2026)",
    x        = NULL,
    y        = "Proporción (%)",
    fill     = "Categoría",
    caption  = "Fuente: ENE (MJJ 2026), ocupados ponderados con fact_cal."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = azul), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

ggsave("output/figuras/t4_p1_g3.png", g3, width = 8, height = 5, dpi = 300)

tabla_ponderada <- ene |>
  filter(activ == 1, !is.na(categoria_ocupacion), !is.na(fact_cal)) |>
  count(sexo, categoria_ocupacion, wt = fact_cal) |>
  group_by(sexo) |>
  mutate(pct = round(n / sum(n) * 100, 2)) 
print(tabla_ponderada)

#sin factor
tabla_sin_peso <- ene |>
  filter(activ == 1, !is.na(categoria_ocupacion)) |>
  count(sexo, categoria_ocupacion) |>
  group_by(sexo) |>
  mutate(pct = round(n / sum(n) * 100, 2))

print(tabla_sin_peso)

# RESPUESTAS GRÁFICO 3:
# 1) ¿En qué categoría está la mayor diferencia entre hombres y mujeres, y en cuál casi no hay? Da las cifras.
# R: La mayor diferencia está en asalariados privados, donde los hombres llegan al 
# 64,9 % y las mujeres al 54,0 % (casi 11 puntos de brecha), seguido por el sector 
# público donde las mujeres poseen  un 17,0 % frente al 9,25 %. Por ultimo donde se presenta
# la menor brecha es en cuenta propia, con 21,1 % en hombres y 21,2 % en 
# mujeres, teniendo una diferencia de solo un 0,1%.

# 2) ¿Qué cambiaría si borras weight = fact_cal?
# R: Al borrar el fact_cal, el gráfico solo mostraría a las 41.850 personas que 
# contestaron la encuesta y no a los trabajadores reales de Chile. Al no expandir 
# la muestra, el asalariado privado se subestima (cae a 62,9 % en hombres y 50,4 % 
# en mujeres), y se sobreestima a las mujeres en el sector público (sube a 18,7 %) 
# y a los trabajadores por cuenta propia.

# AUDITORIA DEL CHECKLIST
# GRÁFICO 1 (CASEN: educación e ingreso por género):
# [x] El título dice el hallazgo: afirma que las mujeres tienen mayor retorno salarial.
# [x] Barras y áreas desde 0: eje Y continuo rotulado en pesos para dispersión.
# [x] Rango completo o justificado: usa todas las personas con ingreso informado (n = 55).
# [x] Paleta accesible: combinación legible de azul y oro.
# [x] El color tiene función: separa a mujeres de hombres.
# [x] Etiquetas directas y líneas guía: rectas de ajuste para comparar pendientes.
# [x] Sin chart junk: theme_minimal sin grilla menor.
# [x] Ejes legibles y con unidades: montos con signo peso y separador de miles.
# [x] Fuente en la caption: submuestra CASEN indicando los 5 NA excluidos.
# [x] Exportado con ggsave(): guardado a 300 dpi en 8 x 5 pulgadas.
#
# GRÁFICO 2 (Ingreso mínimo: nominal contra real):
# [x] El título dice el hallazgo: explica que la inflación absorbió el alza nominal.
# [x] Barras y áreas desde 0: rango acotado a los sueldos para ver la tendencia temporal.
# [x] Rango completo o justificado: serie completa de 2014 a 2025.
# [x] Paleta accesible: contraste de azul y oro.
# [x] El color tiene función: codifica con color y tipo de línea (sólida y cortada).
# [x] Etiquetas directas y líneas guía: texto con valor 2025 al final de la línea.
# [x] Sin chart junk: fondo limpio y sin recuadro de leyenda.
# [x] Ejes legibles y con unidades: años discretos en X y pesos chilenos en Y.
# [x] Fuente en la caption: cita al INE e IPC base 2010.
# [x] Exportado con ggsave(): guardado a 300 dpi en 8.5 x 5 pulgadas.
#
# GRÁFICO 3 (ENE: en qué trabajan hombres y mujeres):
# [x] El título dice el hallazgo: destaca mayor presencia femenina en empleo público.
# [x] Barras y áreas desde 0: barras proporcionales que parten en 0 y van al 100 %.
# [x] Rango completo o justificado: cubre las 7 categorías de ocupación.
# [x] Paleta accesible: paleta viridis apta para daltonismo.
# [x] El color tiene función: un color por categoría con bordes blancos de separación.
# [x] Etiquetas directas y líneas guía: eje relativo de 0 a 100 % para comparar grupos.
# [x] Sin chart junk: sin líneas verticales innecesarias de fondo.
# [x] Ejes legibles y con unidades: proporciones con formato de porcentaje.
# [x] Fuente en la caption: ENE MJJ 2026 ponderada con fact_cal.
# [x] Exportado con ggsave(): guardado a 300 dpi en 8 x 5 pulgadas.