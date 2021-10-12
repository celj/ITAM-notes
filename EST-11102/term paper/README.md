# Term paper

### Estructura de archivos

```console
foo@bar:~/../term paper$ tree
.
├── data
│   └── raw
│       └── data.xlsx      <- Datos
├── README.md
├── reports
│   └── figures            <- Salida de gráficas
│       ├── fig_01.svg
│       ├── fig_02.svg
│       ├── fig_03.svg
│       ├── fig_04.svg
│       ├── fig_05.svg
│       ├── fig_06.svg
│       ├── fig_07.svg
│       ├── fig_08.svg
│       ├── fig_09.svg
│       ├── fig_10.svg
│       ├── fig_11.svg
│       ├── fig_12.svg
│       ├── fig_13.svg
│       ├── fig_14.svg
│       ├── fig_15.svg
│       ├── fig_16.svg
│       ├── fig_17.svg
│       ├── fig_18.svg
│       ├── fig_19.svg
│       ├── fig_20.svg
│       ├── fig_21.svg
│       ├── fig_22.svg
│       ├── fig_23.svg
│       ├── fig_24.svg
│       ├── fig_25.svg
│       ├── fig_26.svg
│       ├── fig_27.svg
│       ├── fig_28.svg
│       ├── fig_29.svg
│       ├── fig_30.svg
│       ├── fig_31.svg
│       └── fig_32.svg
└── src
    ├── data
    │   ├── explore.R      <- Exploración de datos
    │   └── import.R       <- Lectura de datos
    ├── features
    │   ├── functions.R    <- Funciones personalizadas
    │   └── packages.R     <- Librerías
    ├── models
    │   └── summary.R      <- Respuestas a preguntas puntuales
    └── visualization
        └── visualize.R    <- Gráficas
```

### Orden de ejecución

```
packages.R
└── functions.R
    └── import.R
        └── explore.R
            ├── summary.R
            └──visualize.R
```

Basta ejecutar `summary.R` o `visualize.R`.