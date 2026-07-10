#!/usr/bin/env bash
# Compila el libro completo (site/) a Markdown único, PDF, DOCX y LaTeX
# usando pandoc. Requiere: pandoc, y para el PDF una distribución LaTeX
# (p.ej. `sudo apt install texlive-latex-recommended texlive-latex-extra
# texlive-lang-spanish`).
#
# Uso:
#   ./scripts/build.sh            # genera todos los formatos
#   ./scripts/build.sh pdf        # solo PDF
#   ./scripts/build.sh docx       # solo DOCX
#   ./scripts/build.sh tex        # solo LaTeX
#   ./scripts/build.sh md         # solo el Markdown combinado (libro.md)

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."   # site

OUT_DIR="salida"
mkdir -p "$OUT_DIR"

FILES=(
  index.md
  indice-general.md
  indice-figuras.md
  indice-tablas.md
  capitulos/02-resumen.md
  capitulos/03-objetivos-y-alcance.md
  capitulos/04-teoria-detonacion.md
  capitulos/05-metodo-calculo.md
  capitulos/06-ingenieria-software.md
  capitulos/07-analisis-requisitos.md
  capitulos/08-diseno.md
  capitulos/09-codificacion.md
  capitulos/10-comprobacion.md
  capitulos/11-mantenimiento.md
  capitulos/12-conclusiones.md
  capitulos/13-referencias.md
  capitulos/14-bibliografia.md
  capitulos/15-estudio-economico.md
  anexos/a-codigo-interfaz-usuario.md
  anexos/b-codigo-modulo-calculos.md
  anexos/c-datos-recursos.md
  anexos/d-archivos-datos.md
  anexos/e-archivos-ejemplo.md
  anexos/f-manual-usuario.md
  anexos/g-norma-une.md
  anexos/h-software-apoyo.md
)

COMBINED="$OUT_DIR/libro.md"
: > "$COMBINED"
for f in "${FILES[@]}"; do
  if [[ ! -f "$f" ]]; then
    echo "AVISO: falta $f, se omite" >&2
    continue
  fi
  # Los ficheros de capitulos/ y anexos/ referencian imágenes como
  # ../assets/figuras/... (relativo a su propia carpeta); al concatenarlos
  # todos en un único fichero en la raíz de site hay que quitar ese "../".
  sed -E 's#\(\.\./assets/#(assets/#g' "$f" >> "$COMBINED"
  printf '\n\n' >> "$COMBINED"
done
echo "Markdown combinado: $COMBINED"

TARGET="${1:-all}"

PANDOC_COMMON=(
  --from=markdown+footnotes+pipe_tables+grid_tables+fenced_code_blocks
  --resource-path=.
  --metadata lang=es
  # Sin --toc ni --metadata title/author: el libro ya trae su propia
  # portada (index.md) y su propio índice general
  # (indice-general.md) como contenido; --toc duplicaría el índice y
  # metadata title/author generarían una portada extra de LaTeX.
)

if [[ "$TARGET" == "md" || "$TARGET" == "all" ]]; then
  echo "-> libro.md ya generado arriba"
fi

if [[ "$TARGET" == "docx" || "$TARGET" == "all" ]]; then
  pandoc "${PANDOC_COMMON[@]}" "$COMBINED" -o "$OUT_DIR/Explocal-Memoria.docx"
  echo "-> $OUT_DIR/Explocal-Memoria.docx"
fi

if [[ "$TARGET" == "tex" || "$TARGET" == "all" ]]; then
  pandoc "${PANDOC_COMMON[@]}" --standalone -V documentclass=book -V lang=es \
    -V mainfont="DejaVu Serif" -V sansfont="DejaVu Sans" -V monofont="DejaVu Sans Mono" \
    -H scripts/preamble.tex \
    "$COMBINED" -o "$OUT_DIR/Explocal-Memoria.tex"
  echo "-> $OUT_DIR/Explocal-Memoria.tex (compilar con xelatex o lualatex, no pdflatex, por los caracteres Unicode)"
fi

if [[ "$TARGET" == "pdf" || "$TARGET" == "all" ]]; then
  # xelatex maneja UTF-8/Unicode (ρ, φ, Δ, ~, superíndices...) sin
  # configuración extra de paquetes LaTeX de codificación.
  pandoc "${PANDOC_COMMON[@]}" -V documentclass=book -V lang=es \
    -V geometry:margin=2.5cm -V geometry:headsep=1.2cm --pdf-engine=xelatex \
    -V mainfont="DejaVu Serif" -V sansfont="DejaVu Sans" -V monofont="DejaVu Sans Mono" \
    -H scripts/preamble.tex \
    "$COMBINED" -o "$OUT_DIR/Explocal-Memoria.pdf" || \
    echo "AVISO: la generación de PDF falló. Prueba './scripts/build.sh docx' mientras tanto." >&2
  [[ -f "$OUT_DIR/Explocal-Memoria.pdf" ]] && echo "-> $OUT_DIR/Explocal-Memoria.pdf"
fi
