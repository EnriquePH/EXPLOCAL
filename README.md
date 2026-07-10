# EXPLOCAL

[![Deploy site](https://github.com/EnriquePH/EXPLOCAL/actions/workflows/publish-site.yml/badge.svg)](https://github.com/EnriquePH/EXPLOCAL/actions/workflows/publish-site.yml)
[![Website](https://img.shields.io/badge/docs-site-blue)](https://enriqueph.github.io/EXPLOCAL/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

📖 **[Read the memoria / project book](https://enriqueph.github.io/EXPLOCAL/)**

The objective of this project is to built computer tool, called __Explocal__, to
compute the main theoretical characteristics of explosives. The calculus method 
used holds the __UNE 31-002__ Spanish standard. This computer application has 
been developed in language __C++__ and it runs under the *Windows 3.1* operative
system. __Explocal__ includes all characteristics of a modern commercial
computer application, as: executable module, data files, hypertext help and
installation code.

![](code/EXPLOCAL.PNG)

## Repository layout

- **`code/`** — the original 1996 C++ source (Borland C++ 3.1 + Object
  Windows 1.0), data files, resources and the compiled `EXPLOCAL.EXE`, kept
  untouched as the historical reference.
- **`site/`** — the project's memoria (PFC) reconstructed as a Markdown/
  Quarto book (corrected, fully numbered, with figure/table indexes and the
  original annexes). Published as a website via GitHub Pages at
  **<https://enriqueph.github.io/EXPLOCAL/>**, and also buildable to
  PDF/DOCX/LaTeX with pandoc — see `site/README.md`.
- **`docs/`** — original raw source documents (1994-2015 `.DOC`/LaTeX/Word
  drafts) that `site/` was reconstructed from.
- **`new/`** *(not versioned)* — a modern Python re-implementation of the
  calculation engine plus a Streamlit app; regenerable from `code/`.
