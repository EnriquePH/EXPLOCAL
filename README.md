# EXPLOCAL

[![Deploy site](https://github.com/EnriquePH/EXPLOCAL/actions/workflows/publish-site.yml/badge.svg)](https://github.com/EnriquePH/EXPLOCAL/actions/workflows/publish-site.yml)
[![Check site](https://github.com/EnriquePH/EXPLOCAL/actions/workflows/check-site.yml/badge.svg)](https://github.com/EnriquePH/EXPLOCAL/actions/workflows/check-site.yml)
[![Website](https://img.shields.io/badge/docs-site-blue)](https://enriqueph.github.io/EXPLOCAL/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Made with Quarto](https://img.shields.io/badge/made%20with-Quarto-3a86ff)](https://quarto.org)
[![Since 1996](https://img.shields.io/badge/since-1996-orange)](CONTRIBUTING.md)
[![Cite this repository](https://img.shields.io/badge/cite-CITATION.cff-informational)](CITATION.cff)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

📖 **[Leer la memoria / libro del proyecto](https://enriqueph.github.io/EXPLOCAL/)**

![](code/EXPLOCAL.PNG)

## Descripción

**Explocal** es una aplicación informática que calcula las principales
características teóricas de una mezcla explosiva (calor y temperatura de
explosión, velocidad y presión de detonación, balance de oxígeno,
composición de los productos de explosión...) siguiendo el método
simplificado de la norma española **UNE 31-002**. Se desarrolló en C++
(Borland C++ 3.1 + Object Windows 1.0) para *Windows 3.1* en 1996, como
Proyecto Fin de Carrera de Enrique Pérez Herrero ([E.T.S.I. de Minas de
Madrid](https://minasyenergia.upm.es/), especialidad Laboreo y Explosivos).

Este repositorio conserva el código y los datos originales de 1996 tal
cual, y reconstruye la memoria completa del proyecto (texto, fórmulas,
figuras, tablas y anexos con el código fuente) como libro digital. Para
la descripción técnica completa, véase el
[Resumen / Abstract](https://enriqueph.github.io/EXPLOCAL/capitulos/02-resumen.html)
del propio libro.

## Historia del proyecto

- **1996** — Proyecto Fin de Carrera original: memoria en papel/Word y
  aplicación `EXPLOCAL.EXE` en C++ para Windows 3.1.
- **2015-2016** — Primer intento de modernización: exploración de los
  ficheros `*.DAT` con R/ggplot (sin llegar a reimplementar el motor de
  cálculo).
- **2026** — Reconstrucción completa: la memoria original (`docs/`) se
  transcribe y corrige como libro Markdown/Quarto (`site/`), publicado en
  GitHub Pages; en paralelo se reimplementa el motor de cálculo en Python
  (`new/`, no versionado). El código C++ de 1996 (`code/`) se conserva sin
  modificar como referencia histórica.

## Estructura del repositorio

- **`code/`** — código fuente C++ original de 1996 (Borland C++ 3.1 +
  Object Windows 1.0), datos, recursos y el ejecutable `EXPLOCAL.EXE`
  compilado. Referencia histórica, no se toca.
- **`site/`** — la memoria (PFC) reconstruida como libro Markdown/Quarto:
  capítulos numerados, índices de figuras/tablas y los anexos originales.
  Se publica como web en GitHub Pages
  (**<https://enriqueph.github.io/EXPLOCAL/>**) y también se compila a
  PDF/DOCX/LaTeX con pandoc — ver «Cómo contribuir» más abajo.
- **`docs/`** — documentos originales sin procesar (`.DOC`/LaTeX/Word de
  1994-2015) de los que parte la reconstrucción de `site/`.
- **`new/`** *(fuera de git)* — reimplementación moderna del motor de
  cálculo en Python más una app Streamlit; regenerable a partir de
  `code/`.
- **`old/`** *(fuera de git)* — software de terceros usado en 1996
  (compilador, editor de ayuda, bibliotecas) conservado localmente por
  motivos históricos; no se redistribuye por ser propietario de terceros
  (ver Anexo H del libro).
- **[`catalog.json`](catalog.json)** — índice machine-readable de todos
  los capítulos y anexos del libro (título, ruta y URL de cada uno) más
  los formatos disponibles (HTML/PDF/DOCX/LaTeX).

## Estado actual

- ✅ Memoria completa reconstruida, corregida y publicada como libro web
  (HTML) y disponible en PDF/DOCX/LaTeX.
- ✅ Código y datos originales de 1996 preservados íntegros en `code/`.
- 🚧 Reimplementación en Python del motor de cálculo (`new/`): en
  desarrollo, no forma parte del repositorio público todavía.

## Cómo contribuir

¿Encontraste una errata en la memoria, un enlace roto o un error en el
código reproducido? Abre un
[issue](https://github.com/EnriquePH/EXPLOCAL/issues). Para proponer
cambios (incluidas las instrucciones para compilar el libro localmente)
y las convenciones del proyecto, ver [CONTRIBUTING.md](CONTRIBUTING.md).
Este proyecto se rige por un [Código de Conducta](CODE_OF_CONDUCT.md).

## Licencia

[MIT](LICENSE) © 1996 Enrique Pérez Herrero.

## Cómo citar

Este repositorio incluye un [`CITATION.cff`](CITATION.cff): GitHub genera
la cita automáticamente (botón **"Cite this repository"** en la barra
lateral de la página del repositorio). En texto plano:

```
Pérez Herrero, Enrique (1996). Proyecto de una aplicación informática
para el cálculo de las principales características teóricas de los
explosivos (Explocal). Proyecto Fin de Carrera, E.T.S.I. de Minas de
Madrid. https://enriqueph.github.io/EXPLOCAL/
```
