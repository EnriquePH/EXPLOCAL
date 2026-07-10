# Anexo H: Software de apoyo empleado

*El documento original no incluye una relación formal de las herramientas
de desarrollo empleadas más allá de las menciones dispersas en la
memoria (Capítulos 6 a 9) y en las cabeceras de los ficheros fuente. Este
anexo, añadido en la presente reconstrucción, documenta el software de
apoyo empleado — compilador, entorno de desarrollo, editor de ayuda y
bibliotecas de control en tiempo de ejecución — como referencia técnica.
Se trata en todos los casos de software propietario de terceros (Microsoft,
Borland, y el autor de Help Edit), ajeno a Explocal: no se reproduce ni
redistribuye aquí, ni se incluye en el repositorio de GitHub. Cuando el
propio proyecto conserva localmente alguna copia histórica de estas
herramientas (en `old/software/`), esa carpeta está excluida del control
de versiones mediante `.gitignore` y por tanto nunca se publica; este
anexo se limita a documentar, en texto, qué herramientas fueron y qué
papel cumplieron.*

## H.1 Compilador de ayuda de Windows

El sistema de ayuda hipertexto de Explocal (`EXPLOCAL.HLP`, ver
[Anexo F](f-manual-usuario.md)) se generó compilando el fichero fuente
`docs/hlp/AYUDA.RTF` y su proyecto `docs/hlp/AYUDA.HPJ` con el **Microsoft
Help Compiler**, la herramienta oficial de Microsoft para producir
ficheros `.HLP` de Windows 3.1 a partir de RTF. Se ha identificado el uso
de dos versiones del compilador, con las siguientes cabeceras de
identificación:

```
Microsoft (R) Help Compiler Version 3.10.445
Copyright (c) Microsoft Corp 1990 - 1992. All rights reserved.
```

```
Microsoft (R) Help Compiler HCRTF 4.01.0950
Copyright (c) Microsoft Corp 1990 - 1995. All rights reserved.
```

La primera corresponde a `HC.EXE` (versión de 16 bits, contemporánea de
Windows 3.1), invocada normalmente a través de un script `HC.BAT`; la
segunda a `HCRTF.EXE` ("Help Compiler Rich Text Format"), la revisión
posterior del mismo compilador. Ambas son propiedad de Microsoft
Corporation; no se redistribuyen en ningún caso, ni como binario ni en
el repositorio de GitHub. Lo único que forma parte del proyecto y sí se
publica son los ficheros fuente propios (`.RTF`/`.HPJ`, en `docs/hlp/`)
que servían de entrada al compilador.

## H.2 Editor de ficheros de ayuda: Help Edit

Para escribir el `.RTF` con las marcas de hipertexto (saltos, contextos,
palabras clave, ventanas emergentes) y el `.HPJ` de proyecto sin
editarlos a mano, se empleó **Help Edit for Windows**, un editor
especializado en la redacción de ficheros de ayuda de Windows 3.1. Su
pantalla de presentación (`LOGO.BMP`, no reproducida aquí por tratarse
de material con derechos de autor de un tercero) identifica el programa
con el texto *"Help Edit for Windows — Version 1.5 — Copyright © 1993
Aciran Software Systems — All Rights Reserved"*:

- **Nombre:** Help Edit for Windows, versión 1.5
- **Autor:** James Herron, Aciran Software Systems (Bishopbriggs,
  Glasgow, Escocia, Reino Unido)
- **Año:** 1993
- **Distribución:** *shareware* ("pruébelo antes de comprarlo"); la copia
  conservada es una versión de evaluación sin registrar

Help Edit no es software propio de este proyecto: es una herramienta de
terceros de propósito general, shareware y sin registrar, por lo que sus
ficheros (`HELPEDIT.EXE`, `HELPEDIT.HLP`, `README.DOC`, `HLPDEFS.DOC`,
`INVOICE.DOC`, `LOGO.BMP`, etc.) no se incluyen en el repositorio de
GitHub; se mantienen, cuando existe copia local, fuera del control de
versiones y separados de los ficheros de ayuda propios de Explocal
(`docs/hlp/`), para no confundir la herramienta de autor con el
contenido que se escribió con ella. `README.DOC` describe el programa
como *"a Windows Text Editor that you can use to produce Windows Help
files (...) Help Edit manages all aspects of automatically generating
jump labels, and it even creates the HPJ project file required by the
Help Compiler"*; `INVOICE.DOC` es el boletín de registro (pago) del
programa.

## H.3 Entorno de desarrollo: Borland Turbo C++ 3.1 for Windows

El motor de cálculo y la interfaz de Explocal (Capítulos 6 a 9, Anexos A
y B) se desarrollaron con **Borland Turbo C++ 3.1 for Windows** (Borland
International, 1992), usando la biblioteca **Object Windows Library
(OWL) 1.0** para la parte gráfica, y compilando para el entorno de 16
bits de Windows 3.1. Es software propietario de Borland International;
el paquete de instalación no se incluye ni se distribuye en el
repositorio de GitHub.

## H.4 Bibliotecas de controles en tiempo de ejecución

Junto al ejecutable `EXPLOCAL.EXE` (ver [Anexo D](d-archivos-datos.md))
se distribuían dos bibliotecas dinámicas (`.DLL`) de terceros,
necesarias para el aspecto y funcionamiento de los diálogos de la
aplicación:

- **`BWCC.DLL`** (*Borland Windows Custom Controls*): biblioteca de
  Borland International (copyright 1989-1992) que implementa los
  controles con aspecto "Borland" (botones y agrupaciones con relieve
  gris característico) usados en los cuadros de diálogo de Object
  Windows. Es una dependencia directa y obligatoria de Explocal: el
  código fuente la carga explícitamente con `LoadLibrary("BWCC.DLL")`
  (ver [Anexo A](a-codigo-interfaz-usuario.md)) y el propio ejecutable
  incluye el mensaje de error *"No se puede cargar la librería de
  controles: BWCC.DLL"* (ver [Anexo C](c-datos-recursos.md)) para el
  caso de que no esté presente en el sistema.
- **`CTL3D.DLL`**: biblioteca de controles con efecto de relieve 3D para
  Windows 3.1 (cuyo aspecto por defecto era plano), habitual en el
  conjunto de bibliotecas redistribuibles de la generación de
  herramientas de Borland/Microsoft de la época.

Ambas bibliotecas son software propietario de terceros; ninguna de las
dos se incluye ni se distribuye en el repositorio de GitHub.
