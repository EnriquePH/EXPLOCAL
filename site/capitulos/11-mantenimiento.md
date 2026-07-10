# 9. Mantenimiento del software

El mantenimiento de una aplicación informática conlleva: corregir los
fallos de programación después de estudiar la fase de comprobación,
ajustar los requisitos y mejorar las funciones del programa.

**Nunca se termina un proyecto de programación definitivamente**,
ninguno de los principales sistemas de software está **totalmente**
libre de fallos.

Además, el *software* está en constante evolución y debe adaptarse tanto
a las nuevas tecnologías (ordenadores más potentes, progresos en los
gráficos, nuevos sistemas operativos, dispositivos de entrada
alternativos, etc.) como a las nuevas necesidades de los usuarios.

Aunque ***Explocal*** (versión Beta 1.0) es un programa completo y libre
de fallos, antes de ser comercializado necesita un replanteamiento de
los requisitos y una posterior comprobación exhaustiva de todas las
funciones de la aplicación informática.

Entre las posibles mejoras, ampliaciones y añadidos se pueden considerar
las siguientes:

- **Traducción a otros idiomas**: Se puede conseguir sin ni siquiera
  tener una copia del código C++ de la aplicación, puesto que todas las
  cadenas de caracteres se almacenan en los recursos ([EXPLOCAL.RC](../anexos/c-datos-recursos.md#c.1-código-del-archivo-de-recursos-principal-explocal.rc) y sus
  módulos de inclusión) y se pueden modificar con la aplicación *Resource
  Workshop* accediendo directamente al archivo ejecutable (EXPLOCAL.EXE).

- **Actualización a *Windows* 95:** Se necesita cambiar el interfaz de
  usuario ([EXPLOCAL.CPP](../anexos/a-codigo-interfaz-usuario.md#a.1-código-del-módulo-principal-explocal.cpp)), dejando intacto el módulo de los cálculos.

  La actualización está facilitada por la modularidad del programa y
  dificultada por la necesidad de emplear una nueva biblioteca C++ para
  *Windows* 95.

  Las nuevas bibliotecas para la programación en *Windows* 95, no
  coinciden en líneas generales con las de *Windows* 3.1, por lo que la
  actualización obliga a cargar con: los costes de adquisición de una
  nueva versión del compilador, los costes de aprendizaje de las
  funciones de la nueva biblioteca y los costes de codificación.

- **Actualización de los datos:** (previa consulta de otras fuentes de
  información): Sólo es necesario modificar adecuadamente los archivos
  de texto con los datos (\*.DAT) con cualquier procesador de textos.

- **Mejora del procesador de textos:** de modo que se permita emplear
  texto con formato (con subíndices, negrita, cursiva, cambio de
  tamaño...).

  Cualquier cambio, como este, del procesador de textos que no se pueda
  realizar modificando la funcionalidad de la clase *TEditWindow* de la
  biblioteca *Object Windows*, requiere un remodelado completo del
  bloque del interfaz de usuario. Sólo se podría reutilizar el módulo de
  cálculos.

- **Mejoras en el proceso de impresión**: La impresión de textos en
  ***Explocal*** funciona al más bajo nivel posible. Se pueden utilizar
  las clases *TPrintout, TPrinter, TPrinterSetupDlg* y *PrinterAbortDlg*
  (proporcionadas en uno de los ejemplos que se proporcionan con el
  compilador de *Borland C++*), para poder detener el proceso de
  impresión y configurar la impresora.
