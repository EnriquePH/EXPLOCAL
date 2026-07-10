# 8. Comprobación

La fase de comprobación dentro de la Ingeniería del *software* implica
asegurarse de que cada función trabaja como se espera que se haga
(*depuración*) y que todo el sistema de *software* cumple con las
especificaciones dadas en los documentos de requisitos (*cumplimiento de
objetivos*).

La fase de comprobación comienza con la corrección de los errores
sintácticos en la escritura del código y termina con el ajuste de las
características de la aplicación en su conjunto.

En la versión Beta de ***Explocal***, incluida en los discos anexos al
proyecto, se ha realizado una primera comprobación, tanto de los
resultados del cálculo como del *interfaz de usuario*, suficiente para
garantizar un funcionamiento libre de errores.

## 8.1 Depuración de errores

Para aproximarse a la depuración, es necesario en primer lugar, evitar
errores utilizando disciplinas de programación apropiadas y tener
conocimiento de los errores potenciales.

En la **tabla [8-1](10-comprobacion.md#tabla-8-1)** se muestran algunos errores comunes, que se pueden
evitar fácilmente mientras se escribe el código; prestando un poco de
atención se puede ahorrar mucho tiempo en el proceso de comprobación:

[]{#tabla-8-1}
**Tabla 8-1: Errores comunes**

| Errores comunes |
|---|
| Uso del signo de asignación igual ('=') en lugar del signo lógico de comparación ('=='). |
| Llamada a una función con argumentos que no sean enteros antes de haberla definido o declarado, u olvidando el uso necesario de los ficheros de cabecera (.H). |
| Uso de los tipos **char** (con signo) con códigos extendidos de pantalla o teclado. Por ejemplo, el código de byte auxiliar de IBM para **Alt-0** es 129, pero este valor se interpreta como -127 por ser de tipo **char** (con signo). Hay que usar unsigned char o int siempre que esté implicado un código de pantalla o del teclado. |
| Uso de un punto y coma al final de la sentencia **for**, **do...while** o **while**. |
| Uso de punto y coma al final de una directiva **#define**. El punto y coma se convierte en parte de la cadena a sustituir. |
| Intento de asignar una cadena constante a un *array* de cadena empleando una sentencia de asignación en vez de **strcpy**. |
| Confusión entre las cabeceras STDIO.H y STDLIB.H. |
| Confusión entre el operador Y bit a bit '&' y el operador lógico Y '&&'. |
| Asignación de un valor a una expresión.<br>`var++=15; // **Error**` |
| Uso del índice 1 en el primer elemento de un matriz o vector (*array*). En C todas las matrices comienzan en el elemento 0. |

## 8.2 Archivos ejemplo

En los anexos del proyecto se incluyen una serie de resultados que han
sido obtenidos con la versión final de ***Explocal***.

Todos los ejemplos se incluyen, como archivo de ***Explocal*** (\*.XPL),
en el disco de instalación de la versión Beta 1.0.

En los ejemplos, se ha procurado incluir por lo menos un ejemplo de cada
tipo de explosivo industrial, como:

- ANFOs.

- ALNAFOs.

- Dinamitas.

- Explosivos de seguridad de intercambio iónico.

- Gelatinas.

- Emulsiones.

Los archivos ejemplo cumplen dos importantes misiones: ayudar a la
comprobación exhaustiva del programa y servir de herramienta de aprender
a manejar ***Explocal***.
