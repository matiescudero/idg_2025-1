# idg_2025-1: Inteligencia de Datos Geoespaciales

Este repositorio contiene scripts y ejemplos desarrollados para el curso de Inteligencia de Datos Geoespaciales (IDG_2025-1). 
El objetivo es trabajar con datos territoriales, procesarlos y aplicar análisis estadísticos y de modelado, siguiendo buenas prácticas de programación en R, Python y SQL.

## Estructura del Proyecto

La organización del repositorio es la siguiente:

```
/
├── .gitignore                   # Archivos y directorios ignorados por Git (e.g., datos pesados)
├── README.md                    # Este archivo de documentación
│
├── encuestas/                   # Datos brutos: encuestas y archivos originales
│   └── CASEN/
│       └── Base de datos Casen 2022 SPSS_18 marzo 2024.sav  # Archivo original (ignorado por Git)
│
├── data/                        # Datos procesados y outputs generados por los scripts
│   └── casen_rm.rds             # Ejemplo de dataset filtrado para la Región Metropolitana (RM)
│
├── scripts/                     # Scripts en R para procesamiento y análisis
│   └── filtrar_casen.R          # Script de ejemplo para filtrar la encuesta CASEN para la RM
│
└── docs/                        # Documentación adicional (opcional)
```


