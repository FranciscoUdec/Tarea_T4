# T4 Parte 2 — Portafolio y figura compuesta

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)

dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)

azul  <- "#223c6a"
oro   <- "#e69b0a"
porc  <- label_percent(accuracy = 0.1, decimal.mark = ",")

# Tema propio como función (Semana 11, Sesión 2)
tema_fppe <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", color = azul),
      plot.subtitle    = element_text(color = "grey40"),
      plot.caption     = element_text(color = "grey50", hjust = 0),
      legend.position  = "bottom"
    )
}
theme_set(tema_fppe())

# Cargar bases
casen <- read.csv("data/raw/casen_reducido.csv") |> filter(!is.na(ingreso))
ene <- read.csv2("data/raw/ene-2026-06-mjj.csv") |>
  filter(activ == 1, !is.na(ocup_form),ocup_form != 9,!is.na(fact_cal)) |>
  mutate(
    informal = if_else(ocup_form == 2, 1, 0),
    sexo_txt = if_else(sexo == 1, "Hombres", "Mujeres")
  )

regiones_nombres <- c(
  "1" = "Tarapacá", "2" = "Antofagasta", "3" = "Atacama", "4" = "Coquimbo",
  "5" = "Valparaíso", "6" = "O'Higgins", "7" = "Maule", "8" = "Biobío",
  "9" = "La Araucanía", "10" = "Los Lagos", "11" = "Aysén", "12" = "Magallanes",
  "13" = "Metropolitana", "14" = "Los Ríos", "15" = "Arica y Parinacota", "16" = "Ñuble"
)
ene$region_nombre <- regiones_nombres[as.character(ene$region)]



# Gráfico 1 · Scatter CASEN 

g1 <- ggplot(casen, aes(x = educ, y = ingreso, color = genero)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("F" = oro, "M" = azul), labels = c("F" = "Mujeres", "M" = "Hombres")) +
  scale_y_continuous(labels = label_currency(big.mark = ".", decimal.mark = ",")) +
  labs(
    title   = "A mayor educación, mayor ingreso en todas las regiones",
    x       = "Años de educación",
    y       = "Ingreso ($)",
    color   = NULL,
    caption = "Fuente: CASEN (muestra)"
  )

# Gráfico 2 · Barras de informalidad por región (Ñuble resaltada y línea promedio)
prom_nac <- weighted.mean(ene$informal, ene$fact_cal)
inf_reg <- ene |>
  group_by(region_nombre) |>
  summarise(tasa = weighted.mean(informal, w = fact_cal), .groups = "drop") |>
  mutate(resaltar = region_nombre == "Ñuble")

g2 <- ggplot(inf_reg, aes(x = tasa, y = reorder(region_nombre, tasa), fill = resaltar)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = prom_nac, linetype = "dashed", color = azul, linewidth = 0.8) +
  geom_text(aes(label = porc(tasa)), hjust = -0.15, size = 3, fontface = if_else(inf_reg$resaltar, "bold", "plain")) +
  scale_fill_manual(values = c("FALSE" = "grey80", "TRUE" = oro)) +
  scale_x_continuous(labels = label_percent(), limits = c(0, 0.41), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title   = "Ñuble, segunda región más informal",
    x       = "Tasa de informalidad",
    y       = NULL,
    caption = "Fuente: ENE (MJJ 2026), ponderado con fact_cal."
  ) +
  theme(panel.grid.major.y = element_blank())

# Gráfico 3 · Distribución de horas habituales por sexo (sin centinelas 888/999)

ene_horas <- ene |> filter(!habituales %in% c(888, 999), !is.na(habituales))

g3 <- ggplot(ene_horas, aes(x = sexo_txt, y = habituales, fill = sexo_txt)) +
  geom_violin(alpha = 0.6, show.legend = FALSE) +
  stat_summary(fun = median, geom = "crossbar", width = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("Hombres" = azul, "Mujeres" = oro)) +
  coord_cartesian(ylim = c(0, 80)) +
  labs(
    title   = "Mujeres y hombres comparten mediana de 42 h, pero ellas concentran más jornadas parciales",
    x       = NULL,
    y       = "Horas habituales semanales",
    caption = "Fuente: ENE (MJJ 2026)."
  )

# Gráfico 4 · Informalidad hombres vs mujeres por región (con ggrepel y y = x)

inf_sexo <- ene |>
  group_by(region_nombre, sexo_txt) |>
  summarise(tasa = weighted.mean(informal, w = fact_cal), .groups = "drop") |>
  pivot_wider(names_from = sexo_txt, values_from = tasa) |>
  mutate(resaltar = region_nombre == "Ñuble")

g4 <- ggplot(inf_sexo, aes(x = Hombres, y = Mujeres)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_point(aes(color = resaltar), size = 2.5, show.legend = FALSE) +
  geom_text_repel(aes(label = region_nombre, fontface = if_else(resaltar, "bold", "plain")), seed = 2026, size = 3, max.overlaps = Inf) +
  scale_color_manual(values = c("FALSE" = azul, "TRUE" = oro)) +
  scale_x_continuous(labels = label_percent(), limits = c(0.17, 0.38)) +
  scale_y_continuous(labels = label_percent(), limits = c(0.17, 0.38)) +
  labs(
    title   = "En 12 de 16 regiones la informalidad afecta más a mujeres",
    x       = "Informalidad hombres",
    y       = "Informalidad mujeres",
    caption = "Fuente: ENE (MJJ 2026)."
  )


# Gráfico 5 · Gráfico libre (La curva en U de la informalidad por edad)

#¿Cómo varía la precariedad del empleo a lo largo de las distintas etapas de la vida laboral?
inf_edad <- ene |>
  mutate(tramo_edad = cut(edad, breaks = c(14, 24, 34, 44, 54, 64, Inf), labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))) |>
  group_by(tramo_edad) |>
  summarise(tasa = weighted.mean(informal, w = fact_cal), .groups = "drop") |>
  mutate(extremo = tramo_edad %in% c("15-24", "65+"))

g5 <- ggplot(inf_edad, aes(x = tramo_edad, y = tasa, fill = extremo)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = prom_nac, linetype = "dashed", color = "grey30", linewidth = 0.8) +
  geom_text(aes(label = porc(tasa)), vjust = -0.5, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c("FALSE" = "grey75", "TRUE" = oro)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 0.63), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title   = "La U de la edad: la informalidad golpea a jóvenes y adultos mayores",
    x       = "Tramo de edad (años)",
    y       = "Tasa de informalidad",
    caption = "Fuente: ENE (MJJ 2026)."
  ) +
  theme(panel.grid.major.x = element_blank())

# Guardar los 5 gráficos individuales
ggsave("output/figuras/t4_p2_g1.png", g1, width = 8, height = 5, dpi = 300)
ggsave("output/figuras/t4_p2_g2.png", g2, width = 8, height = 6, dpi = 300)
ggsave("output/figuras/t4_p2_g3.png", g3, width = 8, height = 5.5, dpi = 300)
ggsave("output/figuras/t4_p2_g4.png", g4, width = 8, height = 6, dpi = 300)
ggsave("output/figuras/t4_p2_g5.png", g5, width = 8, height = 5, dpi = 300)


# FIGURA COMPUESTA 

figura_compuesta <- (g2 | (g4 / g5)) +
  plot_layout(widths = c(1, 1.3)) +
  plot_annotation(
    title      = "Radiografía de la informalidad en Chile: concentración territorial, brecha de género y vulnerabilidad etaria",
    subtitle   = "Encuesta Nacional de Empleo, mayo-julio 2026 · ponderado con fact_cal",
    caption    = "Fuente: ENE mayo-julio 2026 (INE), ocupados ponderados con fact_cal.",
    tag_levels = "A"
  )

ggsave("output/figuras/t4_p2_figura_compuesta.png", figura_compuesta, width = 14, height = 9, dpi = 300)
ggsave("output/figuras/t4_p2_figura_compuesta.pdf", figura_compuesta, width = 14, height = 9)

# REFLEXIÓN

# 1. De los cinco gráficos, ¿cuál comunica mejor su hallazgo y por qué?

#  R: El Gráfico 2 (barras de informalidad regional) porque transmite su mensaje 
#  de forma directa y sin sobrecarga visual:
#  Ordena las 16 regiones de forma secuencial por su tasa, facilitando la 
#  comparación inmediata entre los extremos.
#  Aplica un uso selectivo del color: mantiene 15 regiones en un gris neutro y 
#  destaca exclusivamente a Ñuble en dorado, guiando la atención del lector hacia 
#  el dato principal.
#  Incorpora la línea de referencia del promedio nacional (26,5 %) y etiquetas 
#  con el porcentaje exacto al final de cada barra, permitiendo entender que 
#  Ñuble es la segunda región con mayor informalidad (34,5 %) sin tener que 
#  estimar visualmente distancias en el eje.
# 
# 2. En el gráfico 3, ¿por qué elegiste ese geom y no otro?

# R: Elegí geom_violin() junto con un stat_summary() que ayudara a marcar la mediana,
# porque con tantas observaciones un gráfico de puntos se vuelve una mancha ilegible y
# un gráfico de cajas oculta la forma real de los datos. El violín permite ver la
# distribución completa: muestra con claridad la concentración en la jornada completa
# típica (en torno a las 42-45 horas) en ambos grupos y, al mismo tiempo, hace evidente
# que las mujeres tienen una presencia mucho mayor en jornadas parciales bajo las 30
# horas semanales. Así se aprecia directamente que, aunque ambos comparten la misma
# mediana de 42 horas, la distribución hacia jornadas reducidas es muy distinta.
# 
# 3. ¿Qué agrega la figura compuesta que no dan los gráficos por separado?
# R: La composición integra en una sola vista tres dimensiones estructurales del mercado 
# laboral: territorio, disparidad de género y ciclo de vida:
# - El Panel A cuantifica el rezago regional, situando a Ñuble como la segunda región con 
#   mayor informalidad del país (34,5 %).
# - El Panel B desagrega la dinámica interna por género, evidenciando que en 12 de las 
#   16 regiones la precariedad afecta predominantemente a las mujeres , mientras que en 
#   zonas del centro-sur como Ñuble y La Araucanía la informalidad masculina 
#   alcanza o supera a la femenina.
# - El Panel C complementa el panorama al identificar los grupos etarios más expuestos, 
#   mostrando una distribución en "U" donde la vulnerabilidad se concentra en los extremos: 
#   el ingreso de los jóvenes de 15 a 24 años (32,8 %) y la permanencia de los adultos mayores 
#   de 65 años (56,2 %).
# En conjunto, la lámina demuestra que la informalidad no responde a un patrón homogéneo, 
# sino a la interacción simultánea de localización geográfica, género y etapa laboral.

# AUDITORIA DEL CHECKLIST
#
# GRÁFICO 1 (Scatter CASEN de referencia):
# [x] El título dice el hallazgo: afirma que a más educación corresponde más ingreso.
# [x] Barras y áreas desde 0: escala continua adaptada al rango de la muestra.
# [x] Rango completo o justificado: base limpia sin valores perdidos en ingreso.
# [x] Paleta accesible: azul y oro institucionales.
# [x] El color tiene función: identifica hombres y mujeres.
# [x] Etiquetas directas y líneas guía: rectas de regresión para cada género.
# [x] Sin chart junk: fondo minimalista estándar.
# [x] Ejes legibles y con unidades: ingresos en pesos con formato chileno.
# [x] Fuente en la caption: submuestra CASEN.
# [x] Exportado con ggsave(): guardado a 300 dpi en 8 x 5 pulgadas.
#
# GRÁFICO 2 (Barras de informalidad por región):
# [x] El título dice el hallazgo: resalta a Ñuble como segunda región más informal.
# [x] Barras y áreas desde 0: barras horizontales con origen estricto en 0 %.
# [x] Rango completo o justificado: incluye las 16 regiones ordenadas por tasa.
# [x] Paleta accesible: contraste entre gris y oro.
# [x] El color tiene función: gris para 15 regiones y oro de acento solo en Ñuble.
# [x] Etiquetas directas y líneas guía: porcentaje por barra y línea del promedio nacional.
# [x] Sin chart junk: sin líneas de grilla horizontal en el fondo.
# [x] Ejes legibles y con unidades: porcentajes con coma decimal.
# [x] Fuente en la caption: ENE MJJ 2026 ponderada con fact_cal.
# [x] Exportado con ggsave(): guardado a 300 dpi en 8 x 6 pulgadas.
#
# GRÁFICO 3 (Distribución de horas habituales por sexo):
# [x] El título dice el hallazgo: señala mediana de 42 h y sesgo femenino a parciales.
# [x] Barras y áreas desde 0: zoom de 0 a 80 h con coord_cartesian() sin cortar datos.
# [x] Rango completo o justificado: filtra códigos centinela 888 y 999.
# [x] Paleta accesible: azul para hombres y oro para mujeres.
# [x] El color tiene función: transparencia para notar concentración de horas.
# [x] Etiquetas directas y líneas guía: barra central con la mediana por sexo.
# [x] Sin chart junk: violines limpios sin leyenda repetida.
# [x] Ejes legibles y con unidades: eje Y rotulado en horas semanales.
# [x] Fuente en la caption: ENE MJJ 2026.
# [x] Exportado con ggsave(): guardado a 300 dpi en 8 x 5.5 pulgadas.
#
# GRÁFICO 4 (Informalidad hombres vs mujeres por región):
# [x] El título dice el hallazgo: indica mayor informalidad femenina en 12 de 16 regiones.
# [x] Barras y áreas desde 0: ejes simétricos de 17 % a 38 % en ambos lados.
# [x] Rango completo o justificado: incluye la totalidad de las 16 regiones.
# [x] Paleta accesible: puntos azules y destaque de Ñuble en oro.
# [x] El color tiene función: resalta la región de Ñuble sobre el resto.
# [x] Etiquetas directas y líneas guía: diagonal de igualdad y nombres con ggrepel.
# [x] Sin chart junk: sin leyendas innecesarias al rotular los puntos.
# [x] Ejes legibles y con unidades: porcentajes iguales en ejes X e Y.
# [x] Fuente en la caption: ENE MJJ 2026 ponderada con fact_cal.
# [x] Exportado con ggsave(): guardado a 300 dpi en 8 x 6 pulgadas.
#
# GRÁFICO 5 (Curva en U de la informalidad por edad):
# [x] El título dice el hallazgo: expone que golpea más a jóvenes y mayores de 65.
# [x] Barras y áreas desde 0: barras verticales que parten en 0 %.
# [x] Rango completo o justificado: 6 grupos etarios cubriendo la vida laboral.
# [x] Paleta accesible: combinación de gris y oro.
# [x] El color tiene función: oro solo en extremos vulnerables y gris al medio.
# [x] Etiquetas directas y líneas guía: porcentaje sobre la barra y promedio nacional.
# [x] Sin chart junk: sin líneas verticales de grilla.
# [x] Ejes legibles y con unidades: porcentajes con coma decimal.
# [x] Fuente en la caption: ENE MJJ 2026 ponderada con fact_cal.
# [x] Exportado con ggsave(): guardado a 300 dpi en 8 x 5 pulgadas.
#
# FIGURA COMPUESTA (Patchwork de 3 paneles):
# [x] El título dice el hallazgo: título general que articula territorio, género y edad.
# [x] Barras y áreas desde 0: proporciones cuidadas en cada panel interno.
# [x] Rango completo o justificado: integra los 3 análisis en una lámina.
# [x] Paleta accesible: paleta homogénea azul, oro y gris en los paneles.
# [x] El color tiene función: el oro guía el foco analítico en toda la figura.
# [x] Etiquetas directas y líneas guía: paneles identificados con letras A, B y C.
# [x] Sin chart junk: paréntesis externos en patchwork para evitar solapamientos.
# [x] Ejes legibles y con unidades: escalas porcentuales consistentes.
# [x] Fuente en la caption: crédito unificado de la ENE al pie.
# [x] Exportado con ggsave(): guardado en PNG a 300 dpi y PDF vectorial.
