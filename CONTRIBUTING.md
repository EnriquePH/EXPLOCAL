# Contribuir a EXPLOCAL

Gracias por el interés en este proyecto. Antes de nada conviene entender
qué es este repositorio: **la reconstrucción digital de un Proyecto Fin
de Carrera de 1996**, no un producto de software en desarrollo activo.
Eso condiciona qué tipo de contribuciones tienen sentido.

Al participar se espera que sigas el [Código de Conducta](CODE_OF_CONDUCT.md)
de este proyecto.

## Qué contribuciones son bienvenidas

- **Erratas** en el texto de la memoria reconstruida (ortografía,
  tipografía, numeración de apartados/ecuaciones/figuras/tablas).
- **Enlaces rotos** o anclas que no resuelven, dentro del libro
  (`site/`) o en los ficheros de nivel raíz.
- **Errores de renderizado**: fórmulas mal maquetadas, imágenes que no
  cargan, tablas desbordadas, etc., tanto en la versión web (HTML) como
  en PDF/DOCX.
- **Correcciones factuales** sobre el propio contenido técnico de la
  memoria (química, termoquímica, el método de la norma UNE 31-002),
  siempre que se indique la fuente.

## Qué NO se debe modificar

- **`code/`** — el código C++ original de 1996 (Borland C++ 3.1 +
  Object Windows 1.0) se conserva tal cual, como referencia histórica.
  No se acepta código «modernizado» aquí.
- **`docs/`** — los documentos originales sin procesar (`.DOC`/Word/
  LaTeX de 1994-2015) de los que parte la reconstrucción. Son la fuente
  de verdad frente a la que se verifica `site/`.
- **`old/`** — software de terceros de 1996 (no versionado, no se
  redistribuye).

Si crees que alguno de estos ficheros contiene un error del propio
autor original, coméntalo en el issue: puede documentarse en el texto
del libro (con una nota editorial, como ya se hace en varios apartados)
sin alterar el original.

## Cómo compilar el libro localmente

Requiere [Quarto](https://quarto.org/docs/get-started/) para la web, y
`pandoc` + una distribución LaTeX con `xelatex` para PDF/DOCX/LaTeX.

```bash
cd site
quarto render               # -> _site/ (HTML, igual que GitHub Pages)
quarto preview               # servidor local con recarga en caliente

./scripts/build.sh          # -> salida/libro.md, .docx, .pdf, .tex
./scripts/build.sh pdf      # solo PDF
./scripts/build.sh docx     # solo Word

python3 scripts/build_indices.py   # regenera los índices general/de
                                    # figuras/de tablas tras editar un capítulo
```

El despliegue en GitHub Pages es automático vía Actions en cada push a
`master`/`main` que toque `site/`; cada pull request que toque `site/`
dispara además un build de comprobación (`.github/workflows/check-site.yml`)
sin publicar nada.

## Convenciones del libro (`site/`)

- **Numeración**: apartados `1.`, `1.1`, `1.1.1`; ecuaciones, figuras y
  tablas numeradas como en el documento original (`N-M`), no con la
  numeración automática de Quarto.
- **Fórmulas**: siempre en LaTeX (`$...$` en línea, `$$...$$` en
  bloque), con el número a la derecha vía `\tag{N-M}` — nunca como
  texto en negrita con el número al final de la línea.
- **Figuras**: `![Figura N-M: pie](ruta){#figura-N-M}` con el ancla
  fusionada en el propio atributo de la imagen (no en una línea aparte).
- **Tablas**: ancla `[]{#tabla-N-M}` en la línea anterior a
  `**Tabla N-M: pie**`.
- **Referencias cruzadas**: enlaza figuras/tablas/ecuaciones/citas
  mencionadas en el texto a su ancla real; no des por hecho el número,
  compruébalo contra el índice generado.
- Tras cualquier cambio de este tipo, regenera los índices
  (`python3 scripts/build_indices.py`) y vuelve a compilar antes de
  proponer el cambio.

## Cómo proponer un cambio

1. Abre un [issue](https://github.com/EnriquePH/EXPLOCAL/issues)
   describiendo el apartado afectado, o
2. Haz un fork, crea una rama, aplica el cambio, compila localmente
   para comprobar que no rompe nada, y abre un pull request describiendo
   qué apartado corriges y por qué.
