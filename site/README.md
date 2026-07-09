# Explocal — Memoria del PFC (reconstrucción en Markdown)

Reconstrucción completa, en Markdown, de la memoria original del Proyecto
Fin de Carrera *"Proyecto de una aplicación informática para el cálculo de
las principales características teóricas de los explosivos" (Explocal)*,
de Enrique Pérez Herrero (E.T.S.I. de Minas de Madrid, 1996).

Fuente original: `Explocal Memoria.docx`, `ECONOM.DOC`, `ANEXOAB.DOC`,
`ANEXOC.DOC`, `ANEXOD.DOC`, `ANEXOE.DOC`, `ANEXOF.DOC` (en `docs/`), con
ortografía y tipografía corregidas, apartados renumerados de forma
jerárquica (`1.`, `1.1`, `1.1.1`...) y todas las imágenes/fórmulas/tablas
originales incluidas.

## Estructura

```
site/
  _quarto.yml               Proyecto Quarto (type: book) → web para GitHub Pages
  index.md             Portada (apartado independiente)
  indice-general.md         Índice general (generado, ver scripts/build_indices.py)
  indice-figuras.md         Índice de figuras (27 figuras)
  indice-tablas.md          Índice de tablas (22 tablas)
  capitulos/                Capítulos 1-10 + front/back matter + estudio económico
  anexos/                   Anexos A-F (código fuente, datos, ejemplos de 1996)
  assets/figuras/           67 imágenes (convertidas de WMF/PNG originales a PNG)
  scripts/build.sh          Compila el libro a Markdown único / DOCX / PDF / LaTeX (pandoc)
  scripts/build_indices.py  Regenera los tres índices a partir de las anclas del contenido
  scripts/preamble.tex      Ajustes LaTeX (fuente Unicode, imágenes, código largo)
  salida/                   Generado por build.sh (no versionado, ver .gitignore)
  _site/                    Generado por `quarto render` (no versionado, ver .gitignore)
```

Los mismos ficheros Markdown alimentan DOS pipelines de salida independientes:

1. **Web (Quarto → GitHub Pages)**: `_quarto.yml` los organiza como libro
   Quarto; `.github/workflows/publish-site.yml` ejecuta `quarto render site`
   y publica `site/_site/` en GitHub Pages en cada push a `master`/`main`
   que toque `site/`.
2. **PDF/DOCX/LaTeX (pandoc)**: `scripts/build.sh` los concatena y compila
   con pandoc, igual que antes de introducir Quarto.

## Compilar

### Web con Quarto

Requiere [Quarto](https://quarto.org/docs/get-started/) instalado
(`quarto --version`).

```bash
cd site
quarto render          # genera _site/ (HTML)
quarto preview          # servidor local con recarga en caliente
```

En GitHub, el despliegue es automático vía Actions — solo hace falta que en
**Settings → Pages → Source** esté puesto **"GitHub Actions"** (ajuste
manual único en la web de GitHub, no se puede hacer por git).

### PDF/DOCX/LaTeX con pandoc

Requiere `pandoc` y, para PDF, una distribución LaTeX con `xelatex`
(el repo ya trae TinyTeX en `~/.TinyTeX`).

```bash
cd site
./scripts/build.sh          # genera salida/libro.md, .docx, .pdf y .tex
./scripts/build.sh pdf      # solo PDF
./scripts/build.sh docx     # solo DOCX (Word)
./scripts/build.sh tex      # solo LaTeX standalone
```

### Regenerar los índices

Si se edita cualquier capítulo/anexo y se añaden o quitan figuras/tablas
numeradas (anclas `[]{#fig-X-Y}` / `[]{#tab-X-Y}`), regenera los índices
antes de compilar (afecta a ambos pipelines):

```bash
cd site
python3 scripts/build_indices.py
```

## Alcance y decisiones de la reconstrucción

- **Incluye todo el contenido original**: memoria técnica completa
  (capítulos 1-10), estudio económico (Documento Nº 2 original) y los
  seis anexos A-F (Documento Nº 3 original: código fuente C++/Borland/OWL
  de 1996, datos de reactivos y constantes, archivos de ejemplo). Esta
  decisión se tomó explícitamente el 2026-07-08 pese a que el `CLAUDE.md`
  del repo sugería excluir la parte de ingeniería del software por estar
  obsoleta (ver ese fichero para más contexto); se mantiene como valor
  histórico/documental del proyecto tal como se presentó originalmente.
- **Fórmulas y diagramas**: las 67 imágenes (fórmulas de *Equation
  Editor* y diagramas) se convirtieron de WMF a PNG con LibreOffice; dos
  figuras del capítulo 6 (6-1, 6-2) y una del capítulo 7 (7-1) no tenían
  imagen asociada ya en el `.docx` original (se perdió en su día) y se
  dejaron solo con el pie de foto. Un objeto de fórmula (cerca de la
  tabla 3-4) estaba dañado/en blanco en el original y se documentó con un
  comentario en el texto en vez de inventarse.
- **Código de los Anexos A-C**: no se pudo recuperar la indentación
  original (perdida en el Word de 1996); se reprodujo el código tal cual,
  sin reformatear, y las líneas muy largas se envuelven automáticamente
  para no desbordar la página impresa.
- **Anexo F**: el documento original (`ANEXOF.DOC`) solo contiene el
  título, sin contenido; se reprodujo fielmente esa ausencia.
- **Tabla 6-4**: el original no conserva el contenido tabular (solo el
  título); se documentó así en vez de inventar datos.
