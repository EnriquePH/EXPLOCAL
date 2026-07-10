# 1. Objetivos y alcance del proyecto

El objeto del presente proyecto es desarrollar una *aplicación
informática profesional* denominada ***Explocal***, que permita
facilitar y sistematizar el *cálculo de las principales características
teóricas de los explosivos.*

El método de cálculo empleado por la aplicación informática es el
recogido en la norma *UNE 31-002-94 "Cálculo de las principales
características teóricas de los explosivos*" [[1]](13-referencias.md#referencia-1).

La herramienta informática, además permitirá *comparar*, con una cierta
comodidad, el efecto que produce en los resultados, cualquier variación
en la composición de la mezcla explosiva.

Es de hacer notar que, aunque la aplicación informática esté destinada a
cualquier persona con conocimientos técnicos en la materia e interesado
en el tema, no se ha descuidado el interfaz de usuario, procurando que
la aplicación informática sea de fácil manejo y que la introducción de
datos sea lo más simple posible.

La informatización del método de cálculo, según la norma *UNE 31-002*
[[1]](13-referencias.md#referencia-1)*,* permitirá desarrollar una herramienta de cálculo, con que se
puedan obtener las características de cualquier mezcla explosiva, de una
forma rápida, ágil y eficaz. Esto es debido a que se conseguirá reducir
el tiempo necesario para efectuar el cálculo de forma manual, de
aproximadamente media hora, a décimas de segundo que es lo que se tarda
en llevarlo a cabo de forma automática.

## 1.1 Características y limitaciones del método de cálculo descrito en la norma UNE 31-002-94

En el proceso de detonación de una sustancia explosiva influyen un gran
número de parámetros.

Debido a la gran complejidad del fenómeno, cualquier enfoque teórico
admisible, implica la necesidad de establecer un gran número de
hipótesis de partida y simplificaciones. Estas simplificaciones
introducirán inevitablemente un sesgo en los resultados.

El método consiste, en esencia, en efectuar un estudio del *estado de
detonación ideal a volumen constante* (de una mezcla explosiva a partir
del estado normal) y aproximar el *estado de detonación CJ* mediante
unas *fórmulas empíricas.*

El análisis del estado de detonación a volumen constante proporcionará
una primera aproximación a una serie de parámetros termoquímicos como
pueden ser:

a\) La temperatura de explosión.

b\) El calor de explosión.

c\) La composición de los productos de explosión.

El resto de los parámetros de detonación: (presión, velocidad y densidad
de detonación), se aproximarán mediante unas *fórmulas empíricas* que
recreen el estado de detonación CJ.

El método de cálculo, aunque mucho menos exacto que otros métodos que
parten de un enfoque teórico mucho más genérico, es suficiente para
obtener una *primera estimación* de los parámetros de detonación de
cualquier mezcla explosiva.

Las principales **limitaciones** de los resultados obtenidos por el
método de cálculo UNE 31-002 [[1]](13-referencias.md#referencia-1), son las siguientes:

a\) Las *temperaturas de explosión* calculadas son excesivamente altas
en comparación con las obtenidas mediante códigos de detonación más
complejos.

b\) En las *detonaciones que se apartan del régimen ideal* de
detonación, el método sólo proporciona unos límites máximos de presión y
velocidad.

c\) La composición de los productos de detonación obtenida no tendrá en
consideración los componentes menores como pueden ser: el metano
(CH~4~), el amoniaco (NH~3~), el cianhídrico (CNH) y los óxidos de
nitrógeno entre otros.

Por lo que el método no es útil para cuantificar la toxicidad de los
humos producidos en la detonación, aunque sí determine el contenido de
monóxido de carbono (CO).

d\) El método no es aplicable a deflagraciones, puesto que el método se
fundamenta en un análisis a volumen constante y no a presión constante y
tampoco se tienen en cuenta los efectos de difusión de materia y
transmisión del calor.

## 1.2 Características y limitaciones de la aplicación informática comercial Explocal

Una aplicación informática comercial se destaca del resto por un
conjunto de características, que cuando confluyen en un mismo producto,
proporcionan a este un toque de calidad profesional, siempre
beneficioso, y que a menudo se convierte en condición necesaria para
conseguir el éxito comercial de la aplicación, e incluso para permitir
la viabilidad de su distribución.

Para garantizar una aplicación informática de alta calidad y bajo coste
se debe emplear una metodología que aborde el diseño, la escritura y el
mantenimiento eficiente de los programas.

Esta metodología constituye la ***Ingeniería del software**.*

La *Ingeniería del software* implica una serie de fases generales en el
desarrollo de la aplicación informática (véase ***figura [4-1](06-ingenieria-software.md#figura-4-1)***). Estas
fases incluyen unas reglas que evitan realizar elecciones caprichosas y
que constituyen lo que se denomina *disciplina de programación*.

El proyecto sigue todas las fases de la Ingeniería del software, en las
que se justifican todas las decisiones que se han tomado, hasta obtener
una primera versión ejecutable de la aplicación informática (conocida
como **versión Beta**), cuyo código y datos se incluyen, tanto en forma
de listado como en soporte electromagnético, en los anexos del proyecto.

La versión Beta incluida se puede ejecutar (bajo el entorno *Windows*)
en cualquiera de sus versiones más extendidas como son*: 3.1, 3.11 para
Trabajo en Grupo ó Windows 95*, puesto que todas las aplicaciones que
funcionan con la versión 3.1 de *Windows* son perfectamente compatibles
con versiones más modernas.

La aplicación informática es ejecutable, por lo tanto, en máquinas tipo
*PC y compatibles* que tengan instalada una versión de *Windows*
superior a la 3.1.

La *versión Beta* de la aplicación informática incluye, además, todos
los archivos necesarios para su funcionamiento, esto es:

- Archivos de datos: (\*.DAT)

- Programa de instalación.

- Datos de la instalación.

- Archivos ejemplo: (\*.XPL)

- Bibliotecas de enlace dinámico: (\*.DLL)

- Archivo de ayuda, en hipertexto: (\*.HLP)

- Datos de inicialización: (\*.INI)

El código ejecutable de la *versión Beta* está realizado en *lenguaje
C++,* y utiliza la jerarquía de clases para el interfaz de *Windows* de
la biblioteca ***Object Windows 1.0*** (OWL) incluida con la versión 3.1
del entorno integrado (IDE) del compilador de *Borland*.

La biblioteca *Object Windows* es una implementación de la *Programación
Orientada a Objetos (OOP)*, combinada con los procesos de programación
basada en eventos, (que constituyen la filosofía de la programación en
*Windows*), y ejemplifica lo bien que estas dos metodologías pueden
coexistir.

*Object Windows* proporciona una alternativa a la programación directa
en *Windows* y simplifica enormemente la tarea de crear una aplicación
informática para *Windows*.
