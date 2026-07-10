# EXPLOCAL

[![Deploy site](https://github.com/EnriquePH/EXPLOCAL/actions/workflows/publish-site.yml/badge.svg)](https://github.com/EnriquePH/EXPLOCAL/actions/workflows/publish-site.yml)
[![Website](https://img.shields.io/badge/docs-site-blue)](https://enriqueph.github.io/EXPLOCAL/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

📖 **[Read the memoria / project book](https://enriqueph.github.io/EXPLOCAL/)**

**Explocal** computes the main theoretical characteristics of explosives
(detonation heat/temperature, velocity, pressure, oxygen balance...)
following the Spanish standard **UNE 31-002**. Written in C++ for
*Windows 3.1* in 1996. See the book's
[Resumen / Abstract](https://enriqueph.github.io/EXPLOCAL/capitulos/02-resumen.html)
for the full project description.

![](code/EXPLOCAL.PNG)

## Repository layout

- **`code/`** — the original 1996 C++ source (Borland C++ 3.1 + Object
  Windows 1.0), data files, resources and the compiled `EXPLOCAL.EXE`, kept
  untouched as the historical reference.
- **`site/`** — the project's memoria (PFC) reconstructed as a Markdown/
  Quarto book (corrected, fully numbered, with figure/table indexes and the
  original annexes). Published as a website via GitHub Pages at
  **<https://enriqueph.github.io/EXPLOCAL/>**, and also buildable to
  PDF/DOCX/LaTeX with pandoc — see below.
- **`docs/`** — original raw source documents (1994-2015 `.DOC`/LaTeX/Word
  drafts) that `site/` was reconstructed from.
- **`new/`** *(not versioned)* — a modern Python re-implementation of the
  calculation engine plus a Streamlit app; regenerable from `code/`.

## Building the book

Requires [Quarto](https://quarto.org/docs/get-started/) for the website,
and `pandoc` + a LaTeX distribution with `xelatex` for PDF/DOCX/LaTeX.

```bash
cd site
quarto render               # -> _site/ (HTML, same as GitHub Pages)
quarto preview               # local server with live reload

./scripts/build.sh          # -> salida/libro.md, .docx, .pdf, .tex
./scripts/build.sh pdf      # PDF only
./scripts/build.sh docx     # Word only

python3 scripts/build_indices.py   # regenerate the general/figure/table
                                    # indexes after editing a chapter
```

GitHub Pages deployment is automatic via Actions on every push to
`master`/`main` that touches `site/`; it only requires the one-time
manual setting **Settings → Pages → Source → "GitHub Actions"**.
