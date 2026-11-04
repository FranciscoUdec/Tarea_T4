# T4 — Portafolio de visualización con ggplot2

Portafolio de visualización económica sobre retornos a la educación, salario mínimo e informalidad laboral en Chile.

## Hallazgos principales por script

### `scripts/t4_parte1.R`
- **Gráfico 1:** Estudiar más le sube más el sueldo a las mujeres y acorta la diferencia con los hombres.
- **Gráfico 2:** La inflación se comió casi todo el aumento del sueldo mínimo entre 2014 y 2025.
- **Gráfico 3:** Las mujeres duplican a los hombres en empleo público y trabajo doméstico.

### `scripts/t4_parte2.R`
- **Gráfico 1:** A más estudios hay más sueldo para ambos sexos.
- **Gráfico 2:** Ñuble es la segunda región con más trabajo informal de Chile (34,5 %).
- **Gráfico 3:** Ambos trabajan cerca de 42 horas, pero las mujeres tienen más jornadas cortas.
- **Gráfico 4:** En 12 de 16 regiones la informalidad golpea más a las mujeres.
- **Gráfico 5:** La informalidad pega más fuerte a los jóvenes y a los adultos mayores.
- **Figura compuesta:** Une región, sexo y edad en una sola imagen (también en PDF).

## Datos

- `data/raw/casen_reducido.csv`: Submuestra de la encuesta CASEN 
- `data/raw/salario_minimo_chile.csv`: Serie temporal oficial de salario mínimo nominal e IPC base 2010 (2014–2025).
- `data/raw/ene-2026-06-mjj.csv`: Encuesta Nacional de Empleo (INE, trimestre mayo-julio 2026)

## Cómo correrlo

1. Abrir el proyecto en RStudio desde el archivo `Tarea_t4.Rproj` en la raíz del repositorio.
2. Ejecutar `scripts/t4_parte1.R`
3. Ejecutar `scripts/t4_parte2.R` 
4. Las salidas gráficas quedarán automáticamente exportadas en `output/figuras/`.

## Estructura
```
├── data/
│   └── raw/
│       ├── casen_reducido.csv
│       ├── ene-2026-06-mjj.csv
│       └── salario_minimo_chile.csv
├── output/
│   └── figuras/
│       ├── t4_p1_g1.png
│       ├── t4_p1_g2.png
│       ├── t4_p1_g3.png
│       ├── t4_p2_figura_compuesta.pdf
│       ├── t4_p2_figura_compuesta.png
│       ├── t4_p2_g1.png
│       ├── t4_p2_g2.png
│       ├── t4_p2_g3.png
│       ├── t4_p2_g4.png
│       └── t4_p2_g5.png
├── scripts/
│   ├── t4_parte1.R
│   └── t4_parte2.R
├── README.md
└── tarea_t4.Rproj
```
##Autor

#Francisco Inostroza — Noviembre 2026

## Declaración de autoría y uso de IA
Copia este bloque al final de tu `README.md` y complétalo:
```markdown
## Declaración de autoría y uso de IA
- Herramienta utilizada: Gemini (o «ninguna»)
- Para qué la usé: Le pedi revisar la redaccion de mi codigo y que me ayudara a encontrar errores y me diera ideas.
Tambien le pedi ayuda en corregir la redaccion de mis reflexiones.
- Qué hice yo: Verifique que sus recomendaciones tuvieran sentido con lo que yo sabia, Y no haya modificado lo que yo ya habia planteado. 
- Verificación: confirmo que entiendo y puedo explicar todo lo que entrego.
```
**Uso permitido:** pedir que te expliquen un error, preguntar qué hace una función
o un argumento (`weight`, `position`, `max.overlaps`), pedir alternativas a un
código que ya escribiste, revisar la redacción de la reflexión.
**Uso no permitido:** pegar este enunciado y entregar lo que devuelva; entregar
código que no puedes explicar línea por línea; pedirle que **elija** el mejor
gráfico o que **escriba** la reflexión — esas decisiones son lo que se evalúa.

