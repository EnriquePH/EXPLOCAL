#!/usr/bin/env python3
"""Genera índice general, índice de figuras e índice de tablas del libro
PFC escaneando mecánicamente los ficheros finales de capítulos y anexos
(encabezados Markdown y anclas []{#fig-...}/[]{#tab-...}). Evita transcribir
a mano y por tanto evita desincronización con el contenido real.

Uso: python3 scripts/build_indices.py   (ejecutar desde site/)
"""
import re
from pathlib import Path

BASE = Path(__file__).resolve().parent.parent

CHAPTER_FILES = [
    "capitulos/02-resumen.md",
    "capitulos/03-objetivos-y-alcance.md",
    "capitulos/04-teoria-detonacion.md",
    "capitulos/05-metodo-calculo.md",
    "capitulos/06-ingenieria-software.md",
    "capitulos/07-analisis-requisitos.md",
    "capitulos/08-diseno.md",
    "capitulos/09-codificacion.md",
    "capitulos/10-comprobacion.md",
    "capitulos/11-mantenimiento.md",
    "capitulos/12-conclusiones.md",
    "capitulos/13-referencias.md",
    "capitulos/14-bibliografia.md",
    "capitulos/15-estudio-economico.md",
]

ANEXO_FILES = [
    "anexos/a-codigo-interfaz-usuario.md",
    "anexos/b-codigo-modulo-calculos.md",
    "anexos/c-datos-recursos.md",
    "anexos/d-archivos-datos.md",
    "anexos/e-archivos-ejemplo.md",
    "anexos/f-manual-usuario.md",
    "anexos/g-norma-une.md",
    "anexos/h-software-apoyo.md",
]

HEADER_RE = re.compile(r"^(#{1,3})\s+(.+?)\s*$")
# Anclas "figura-"/"tabla-" (NO "fig-"/"tab-": esos prefijos son palabra
# reservada de Quarto y activan su numeración automática de crossref,
# duplicando el número que ya llevamos escrito a mano en el caption).
TAB_ANCHOR_RE = re.compile(r"^\[\]\{#(tabla-[\w.-]+)\}\s*$")
TAB_CAPTION_RE = re.compile(r"^\*\*(Tabla [^*]+)\*\*")
# Figuras: ancla fusionada como atributo del propio ![]() para que pandoc
# la trate como figura flotante real (ver scripts/fix_figures.py).
FIG_INLINE_RE = re.compile(r"^!\[(Figura [^\]]+)\]\([^)]*\)\{#(figura-[\w.-]+)\}")
# Pies de figura sin imagen asociada (huérfanos, en negrita/cursiva triple).
FIG_ORPHAN_RE = re.compile(r"^\*\*\*Figura ([\w.-]+): ([^*]+)\*\*\*$")


# Indentación por nivel usando espacios de no separación (NBSP, U+00A0):
# los espacios normales al inicio de línea se recortan en Markdown, los
# NBSP no. Cada entrada termina en "  " (dos espacios) para forzar un
# salto de línea duro sin dejar viñeta ni línea en blanco entre entradas.
NBSP = " "


def indent_for(level):
    return NBSP * 4 * (level - 1)


def entry(title, relpath, anchor, level=1):
    frag = f"#{anchor}" if anchor else ""
    return f"{indent_for(level)}[{title}]({relpath}{frag})  "


def pandoc_slug(title):
    """Reproduce el algoritmo de pandoc para generar el id de un
    encabezado (extensión auto_identifiers), que Quarto usa tal cual:
    1) minúsculas; 2) se quita todo excepto letras/dígitos/espacios/
    guiones/guiones bajos/puntos; 3) espacios -> guiones; 4) se elimina
    todo lo que haya ANTES de la primera letra (por eso "3.2.1 Título"
    pierde el número: el id empieza en "título", pero "C.1 Título"
    conserva "c.1-título" porque ya empieza por una letra).
    """
    s = title.lower()
    s = re.sub(r"[^\w\s.-]", "", s, flags=re.UNICODE)
    s = re.sub(r"\s+", "-", s.strip())
    m = re.search(r"[a-zà-öø-ÿ]", s)
    s = s[m.start():] if m else "section"
    return s or "section"


def build_general_index():
    lines = ["# Índice general", "", "[Portada](index.md)  "]
    for relpath in CHAPTER_FILES + ANEXO_FILES:
        path = BASE / relpath
        if not path.exists():
            lines.append(f"**[FALTA: {relpath}]**  ")
            continue
        text = path.read_text(encoding="utf-8")
        first_heading_in_file = True
        for line in text.splitlines():
            m = HEADER_RE.match(line)
            if not m:
                continue
            level = len(m.group(1))
            title = m.group(2).strip()
            if first_heading_in_file:
                # Quarto trata el primer encabezado de cada capítulo como
                # el título de la página (sin <section id="">  propio);
                # enlaza al fichero sin fragmento para no romper el enlace.
                anchor = ""
                first_heading_in_file = False
            else:
                anchor = pandoc_slug(title)
            lines.append(entry(title, relpath, anchor, level))
    return "\n".join(lines) + "\n"


def build_fig_index():
    lines = ["# Índice de figuras", ""]
    count = 0
    for relpath in CHAPTER_FILES + ANEXO_FILES:
        path = BASE / relpath
        if not path.exists():
            continue
        for line in path.read_text(encoding="utf-8").splitlines():
            m = FIG_INLINE_RE.match(line)
            if m:
                caption, anchor_id = m.group(1).strip(), m.group(2)
                lines.append(entry(caption, relpath, anchor_id))
                count += 1
                continue
            m = FIG_ORPHAN_RE.match(line)
            if m:
                num, caption = m.group(1), m.group(2).strip()
                # Los huérfanos no tienen imagen: sin ancla propia, se
                # enlaza al fichero (sin #) para no romper el enlace.
                lines.append(f"[Figura {num}: {caption}]({relpath})  ")
                count += 1
    lines.append("")
    lines.append(f"*Total: {count} figuras.*")
    return "\n".join(lines) + "\n"


def build_tab_index():
    lines = ["# Índice de tablas", ""]
    count = 0
    for relpath in CHAPTER_FILES + ANEXO_FILES:
        path = BASE / relpath
        if not path.exists():
            continue
        source_lines = path.read_text(encoding="utf-8").splitlines()
        for i, line in enumerate(source_lines):
            m = TAB_ANCHOR_RE.match(line)
            if not m:
                continue
            anchor_id = m.group(1)
            caption = None
            for j in range(i + 1, min(i + 4, len(source_lines))):
                cm = TAB_CAPTION_RE.match(source_lines[j].strip())
                if cm:
                    caption = cm.group(1).strip()
                    break
            if caption is None:
                caption = f"(sin caption detectado, ancla {anchor_id})"
            lines.append(entry(caption, relpath, anchor_id))
            count += 1
    lines.append("")
    lines.append(f"*Total: {count} tablas.*")
    return "\n".join(lines) + "\n"


def main():
    (BASE / "indice-general.md").write_text(build_general_index(), encoding="utf-8")
    (BASE / "indice-figuras.md").write_text(build_fig_index(), encoding="utf-8")
    (BASE / "indice-tablas.md").write_text(build_tab_index(), encoding="utf-8")
    print("Índices generados.")


if __name__ == "__main__":
    main()
