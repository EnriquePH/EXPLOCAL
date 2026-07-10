# 5. Análisis de requisitos

La fase de *análisis de requisitos*, dentro de la Ingeniería del
*software* persigue como fin describir de forma exhaustiva las
características de la aplicación informática que se pretende
desarrollar, para ello se elabora una lista, con su explicación
correspondiente, de las propiedades (y restricciones) a las que la
aplicación informática debe ajustarse.

El análisis de requisitos determina las metas que debe cumplir el
proyecto de *software* y qué se debe hacer para conseguirlas.

Esta fase es la que tiene mayor peso en el proyecto, puesto que fija
tanto el punto de partida como los objetivos que se tendrán que cumplir.

En general, para la mayoría de los proyectos de *software*, la
documentación que acompaña a los requisitos consiste en algo que está
entre una lista corta y una descripción detallada.

Para los aficionados, por ejemplo, la documentación de los requisitos
puede consistir sólo en una lista breve de los atributos del programa y
una breve descripción de su comportamiento.

Pero, en general, cualquier proyecto profesional (realizado por un
equipo profesional) en el que los programadores, los diseñadores y
analistas son personas distintas, los documentos que se incluirán son:

- a) Una descripción detallada de la *necesidad* o propósito del
  programa.

- b) Las especificaciones completas del **sistema**, (tanto del sistema
  operativo como del soporte físico)

- c) Explicación de los **servicios** que va a proporcionar la aplicación
  informática.

- d) Una descripción de todas las **funciones** del *software*.
  (Descripción de los cálculos)

- e) Los requisitos de una **base de datos** de información externa.

- f) Una descripción de los **requisitos de mantenimiento**.

- g) Un **glosario**.

- h) Un **índice** (En este caso incluido al comienzo del proyecto)

## 5.1 Necesidad, propósito y características de la aplicación informática comercial

El primer paso que debe dar cualquier análisis de requisitos es
*justificar* la necesidad de la herramienta informática que se pretende
construir.

Esta cuestión es de gran importancia puesto que si después de realizar
un gran esfuerzo y dedicar recursos, tanto humanos como materiales, para
obtener un material informático determinado, el resultado final no
supone una mejora sustancial respecto a la utilización de otras
herramientas informáticas e incluso no supera el empleo de una técnica
manual, se habrá estado tirando el tiempo y el dinero.

En el *proceso de detonación* de un explosivo se libera energía mediante
una reacción química exotérmica muy rápida. Los productos de explosión
de esta reacción, mayoritariamente gaseosos, se encuentran en el
instante en que se completa la reacción, en unas condiciones de presión,
temperatura y densidad que es necesario conocer para llevar a cabo una
*aplicación racional de los explosivos* a cada uso en particular.

Por ejemplo: en barrenos húmedos se descarta el empleo de explosivos con
nitrato amónico entre sus componentes. En casos como este la elección
del explosivo se puede realizar a partir de su composición, pero en
general es necesario conocer las características de la detonación. Como
en el caso de que se desee obtener grandes bloques (baja fragmentación),
es aconsejable el empleo de explosivos con preponderancia del volumen de
gases respecto a su poder rompedor.

Pero si además de evaluar las características de un explosivo, es
posible predecir la influencia que va a tener la composición del
explosivo en estas características, será posible alterar la composición
para conseguir el efecto deseado. Esta es, sin duda, la utilidad
práctica que persigue cualquier método de cálculo de las características
teóricas de los explosivos.

La herramienta informática que se va a desarrollar debe, por lo tanto,
poder permitir *comparar*, con una cierta comodidad, el efecto que
produce, en los resultados, cualquier variación en la composición de la
mezcla explosiva.

El principal propósito de la informatización del método de cálculo de
las principales características teóricas de los explosivos es
desarrollar una herramienta fiable con la que se puedan obtener los
parámetros de detonación de cualquier mezcla explosiva de una forma
rápida, ágil y eficaz, puesto que permitirá reducir el tiempo necesario
empleado en realizar el cálculo de forma manual, de aproximadamente
media hora, a segundos, que es el tiempo que lleva introducir los datos.

De todos los métodos de cálculo que existen, se va a trabajar con el que
se encuentra recogido en la norma UNE 31-002-94. Esta elección se ve
respaldada tanto por la difusión que tiene la norma como por la
sencillez y generalidad del método. Además se trata de un método que
resuelve un problema bien definido y con una solución algorítmica
conocida lo que asegura la viabilidad del diseño del programa.

La aplicación informática va a revalorizar, en cierto modo, el método de
cálculo puesto que permitirá estudiar con facilidad la influencia que
tiene cualquier variación de un componente (o componentes) en una
familia mezclas explosivas.

## 5.2 Estudio de los costes de una aplicación informática

Cuando se pretende desarrollar una aplicación informática que sustituye
a otra (o a un método manual), hay que sopesar las ventajas e
inconvenientes que presentará la creación de una nueva herramienta
informática antes de incurrir en unos costes superiores a la alternativa
de continuar empleando la ya existente.

Los costes del método automático de resolución, son los que acarrea
tanto la construcción como la utilización de la aplicación informática y
se pueden clasificar según la siguiente división de *costes
interdependientes:*

(véase **tabla 5-1**)

[]{#tabla-5-1}
**Tabla 5-1: Costes del método automático**

| ***Costes del método de Cálculo Automático (CA)*** |
|---|
| Costes de desarrollo: (CA~Desarrollo~) |
| Costes del sistema: (CA~Sistema~) |
| Costes del tiempo de ejecución: (CA~Ejecución~=f(n)) |
| Costes de aprendizaje del manejo: (CA~Aprendizaje~) |

Fuente: Elaboración propia.

Nota: n es el número de veces que se aplica el método.

Los conceptos anteriores se pueden expresar tanto en tiempo como en
dinero, si se consideran los honorarios de analistas, diseñadores,
programadores y usuarios.

En lo que sigue se considera que todos los costes están expresados en
unidades uniformes.

Los ***costes del desarrollo*** **(CA~Desarrollo~):** incluyen los
costes de análisis, diseño, codificación, comprobación y mantenimiento.

No es difícil darse cuenta que la etapa desarrollo influye notablemente
en los demás conceptos: Por ejemplo los costes del sistema dependen de
las decisiones tomadas en la fase de análisis, los costes de aprendizaje
dependen también tanto del tipo de aplicación considerada en el análisis
como de la eficiencia de las etapas de diseño y comprobación.

También cuenta, notablemente, las herramientas de programación de las
que se disponga.

Si se desea minimizar en lo posible el tiempo y esfuerzo dedicado al
desarrollo de una aplicación informática se debe emplear la metodología
que marca la Ingeniería del *software*.

Con los sistemas operativos actuales, que constituyen un entorno común a
todas las aplicaciones, suele ser práctica común hoy en día en las
empresas profesionales del sector **aprovechar** la estructura de una
aplicación para desarrollar otra distinta (puesto que el *interfaz de
usuario* es siempre similar).

En efecto, es mucho más económico partir de un código robusto y ya
comprobado y dedicar el esfuerzo de desarrollo sólo a las
características particulares de cada aplicación que empezar desde cero.

Últimamente se ha producido un abuso de esta práctica que ha originado
innumerables versiones ligeramente diferentes de la misma aplicación
informática que en ocasiones lo único que ha conseguido ha sido
confundir e irritar al usuario.

Desgraciadamente en la aplicación objetivo de este proyecto no se ha
podido contar con estas facilidades.

Los **costes del sistema: (CA~Sistema~)** engloban tanto el soporte
físico *(hardware)* como el conjunto de programas necesarios que hacen
posible el funcionamiento de la aplicación (*software*).

Esto incluye: los costes del procesador, sus sensores y transductores
(las memorias en particular) y los programas de utilidad con los que
vaya ocupado, como por ejemplo el sistema operativo.

El procesador se caracteriza por el número y naturaleza del conjunto de
instrucciones elementales, el tiempo de ejecución de las mismas, el
tamaño de la memoria interna y de los programas de utilidad (sistema
operativo) y se elige de acuerdo con criterios de organización ajenos al
campo de los diseñadores de *software*.

El diseñador sólo puede limitar la cantidad de memoria interna necesaria
si reduce el número de funciones que desarrolla la aplicación
informática.

Sin embargo, es frecuentemente inútil gastar tiempo en reducir el uso de
la memoria interna, ya que, además del incremento de coste de diseño del
programa, se tiende a reducir tanto la comprensibilidad como la
versatilidad del diseño.

Cuanto más complejo y extenso sea el programa que se necesite más caro
será el sistema que consiga hacerlo funcionar.

Aunque lo normal es que no se decida la compra de un sistema informático
para ejecutar, en exclusiva, un único programa, precisamente el éxito de
los ordenadores personales se basa en la capacidad de poder llevar a
cabo distintas tareas.

Esta es la razón que implica que el *coste del sistema* se **reparta**
entre todas las aplicaciones que el usuario utilice en un sistema en
particular.

Teniendo en cuenta esta aseveración, se debe intentar elegir, en la fase
de análisis del proyecto, por un sistema ampliamente difundido y no por
otro *sui generis* más limitado.

Los **Costes de ejecución: (CA~Ejecución~=f(n))** dependen tanto del
proceso que automatizan como de la potencia del sistema.

La rapidez es la principal baza con la que juegan los ordenadores al
competir con un método no automático.

Si se quiere reducir el tiempo de ejecución, en lo posible, hay que
invertir en un sistema más potente (y caro) o intentar optimizar el
algoritmo del programa.

Sin embargo, la opción de optimizar puede ocasionar problemas similares
a los que puede causar un intento de reducir las necesidades de memoria
del programa.

Los **Costes de aprendizaje: (CA~Aprendizaje~)** junto con los costes
de ejecución y sistema son los que recaen en el usuario final (y por lo
tanto en el consumidor o cliente).

El fin último que persigue cualquier aplicación informática es realizar
una tarea determinada con la mayor facilidad y sencillez posibles.

No es extraño, por lo tanto, que las tendencias actuales en lo que se
refiere a sistemas operativos se hayan dirigido a facilitar la vida de
los usuarios permitiendo el desarrollo de utilidades de fácil manejo.

En este contexto se han establecido ciertos estándares como el CUA
*(Common User Interface)* definido por IBM y adoptado por el sistema
operativo *Windows* que reducen la curva de aprendizaje mediante el uso
de unos interfaces de usuario comunes.

Con esto se consigue que el usuario sólo gaste su tiempo en aprender
cómo manejar las funciones que ofrece la aplicación informática en sí, y
no en el manejo del sistema.

Cuando se maneja un programa informático, es frecuente que todos los
pormenores del cálculo queden ocultos al usuario, por lo que no es
necesario poseer un conocimiento exhaustivo del método de cálculo para
utilizar correctamente el programa.

Como consecuencia de lo anterior se consigue disminuir el tiempo de
aprendizaje.

Si no empleamos el ordenador incurriremos en los siguientes costes
reflejados en la **tabla 5-2**:

[]{#tabla-5-2}
**Tabla 5-2: Costes del método manual**

| ***Costes del empleo de un método de cálculo manual (CM)*** |
|---|
| Coste de aprendizaje: (CM~Aprendizaje~) |
| Coste de ejecución del método: (CM~Ejecución~=f(n)) |

Fuente: Elaboración propia.

Nota: n es el número de veces que se aplica el método.

Los **costes de aprendizaje** del método manual incluyen el conocimiento
exhaustivo tanto de las generalidades como de los detalles de todas las
etapas del cálculo.

En general se puede decir que el coste de aprendizaje del método manual
es superior al automático. Aunque aprender a utilizar el cálculo manual
tiene la ventaja de proporcionar una visión más profunda del problema,
no asegura la comprensión del método (al igual que el método
automático).

Las principales ventajas que posee el método automático (bien diseñado y
depurado) sobre el manual son: la mayor comodidad, la rapidez, y la
ausencia de errores.

Es decir, menor **coste de ejecución.**

Cuando el método de cálculo se aplica gran cantidad de veces es siempre
más ventajoso su automatización que la insistencia en su forma manual.

A partir de este razonamiento se puede concluir como, una vez más, con
una mayor inversión (desarrollando una aplicación informática) se puede
obtener un menor coste operativo (coste de ejecución).

De este modo se puede transformar un esfuerzo eminentemente repetitivo
(como realizar cien veces el mismo cálculo), en uno creativo (como
programar).

Hay que recordar que aunque el ordenador siempre lleve las de ganar
cuando se trata de realizar tareas repetitivas, utilizar el ordenador
sin justificación es una pérdida de tiempo.

La cuantificación *a priori* de los costes de una aplicación
informática, antes de desarrollarla, es una tarea que entraña gran
dificultad.

Sólo si se posee experiencia en desarrollar aplicaciones similares es
posible realizar una estimación previa de lo que puede costar.

## 5.3 Especificaciones del sistema

La aplicación informática ***Explocal*** está pensada para poder ser
utilizada por cualquier usuario de ordenadores personales.

Debido a la gran difusión que han tenido los ordenadores personales tipo
**IBM - PC y compatibles**, se ha optado por ellos como plataforma de
desarrollo de la aplicación ***Explocal***.

La evolución de los ordenadores personales ha hecho imprescindible el
uso de sistemas operativos avanzados para poder aprovechar al máximo las
posibilidades de los procesadores disponibles en la actualidad.

De todos los sistemas operativos de amplia difusión para IBM - PC son
sin duda el *Windows* y el MS-DOS, (ambos desarrollados por Microsoft)
los más utilizados en la actualidad por usuarios de todo el mundo.

La aparición de la versión *Windows* 95 del sistema operativo
*Microsoft* *Windows* no ha roto la compatibilidad con las versiones
anteriores del *Windows* (*3.0, 3.1, 3.11 para Trabajo en Grupo*), ni
con el sistema operativo *MS-DOS*.

Esta circunstancia convierte a cualquiera de las versiones de *Windows*
3.1 ó 3.11 en las más compatibles, a excepción del MS-DOS, puesto que
también lo son con los sistemas operativos *OS/2* y *OS/2 Warp* de
IBM*.*

### 5.3.1 Características del entorno *Windows*. Comparación con el DOS

Se puede decir que *Windows* es más que un sistema operativo, es un
entorno gráfico.

*Microsoft Windows* hace más sencilla la vida del usuario.

Las aplicaciones informáticas para *Windows* reducen la curva de
aprendizaje mediante el empleo de interfaces de usuario familiares: Una
vez que el usuario ha configurado *Windows* para un monitor y una
impresora particulares desaparecen los persistentes problemas de
compatibilidad que antes se producían con la instalación de cada nuevo
paquete de *software* en *MS-DOS*.

El usuario también se beneficia de algunas características implícitas en
*Windows* como: la transferencia de datos entre distintos programas
(mediante el portapapeles), la ejecución de más de un programa a la vez,
la posibilidad de establecer enlaces dinámicos entre programas en
ejecución, la mezcla entre texto y gráficos y un interfaz de pulsar y
soltar común.

Estas características son las que inclinan la balanza a favor de
*Windows* en el desarrollo de una aplicación como ***Explocal*** puesto
que se desea una aplicación versátil y de fácil manejo.

### 5.3.2 Soporte necesario mínimo y mínimo recomendado

El soporte mínimo necesario, para ejecutar una aplicación informática
determinada, viene condicionado por las características que un sistema
(o equipo) debe poseer para poder utilizar aplicaciones *Windows*.

Tanto el *hardware* como el *software* necesario para ejecutar
***Explocal*** es el que viene reflejado en la **tabla 5-3**.

En la **tabla 5-3** también se incluyen las características del sistema
que se va a emplear en la codificación de ***Explocal***.

[]{#tabla-5-3}
**Tabla 5-3: Soporte necesario y plataforma de desarrollo**

| | Soporte mínimo | Soporte mínimo recomendado | Soporte de desarrollo |
|---|---|---|---|
| **Sistema operativo** | *Windows* 3.1 | *Windows* 3.1 | *Windows* 3.11 para trabajo en grupo. |
| **Procesador** | Intel 80286 | Intel 80386 | Intel 80486 |
| **Velocidad** | 12 MHz | 33 MHz | 66 MHz |
| **Monitor** | B/N | Color | Color |
| **Resolución** | EGA | VGA | SVGA |
| **RAM** | 1 Mb | 2 Mb | 8 Mb |

Fuente: Elaboración propia, basada en datos de Microsoft.

## 5.4 Funciones a implementar

Los datos y resultados del problema constituyen una lista compleja de
información e incluyen:

- a) **La composición de la mezcla:** Nombre de los reactivos,
  porcentajes, fórmulas, energías de formación.

- b) **Las características de la mezcla:** Nombre del explosivo, densidad
  de encartuchado, energía interna, fórmula para 1 kg de explosivo,
  balance de oxígeno.

- c) **La composición de los productos de explosión**.

- d) **Los parámetros de la reacción a volumen constante:** Calor de
  explosión, temperatura de explosión, moles de productos gaseosos,
  volumen de gases en condiciones normales, energía específica, masa
  molecular media de productos gaseosos.

- e) **Los parámetros de detonación:** Presión de detonación, velocidad
  de detonación, densidad de detonación, coeficiente adiabático.

La manera más versátil de presentar todos los datos anteriores es, sin
duda, un *procesador de textos*.

En consecuencia ***Explocal*** debe poseer todas las funciones de un
procesador de textos, y si queremos acceder a datos de diferentes
explosivos a la vez la aplicación debe poseer un interfaz tipo MDI.

### 5.4.1 Funciones de un procesador de textos

Las funciones de un procesador de textos se pueden clasificar en:

- a) Escritura de texto por teclado.

- b) Posibilidad de seleccionar todo o una parte del texto.

- c) Acceso al portapapeles de *Windows*: Esta característica permite
  compartir datos con otras aplicaciones y es, sin duda, una de las
  ventajas más sobresalientes de *Windows* respecto a MS-DOS. Debe
  incorporar las funciones de "*cortar*" y "*pegar*".

- d) Escritura y lectura de datos en disco: indiferentemente si se trata
  de disco duro o de *disquete*.

- e) Buscar y reemplazar un texto.

- f) Salida del texto por impresora.

### 5.4.2 Funciones de una aplicación MDI

Muchas aplicaciones de *Windows* (como el *Administrador de programas*,
y el *Administrador de archivos)* implementan un interfaz especial de
*Windows* con múltiples ventanas.

Se trata de un interfaz estándar de *Windows* denominado *Multiple
Document Interface (**MDI**)*. El estándar MDI forma parte del estándar
CUA *(Common User Access)* definido por IBM.

Cada aplicación que cumple las especificaciones MDI posibilita la
apertura de ventanas hijas para tareas específicas, tales como edición
de textos, manejo de bases de datos o trabajo con una hoja de cálculo.

Las funciones genéricas que incorpora una aplicación MDI son:

- a) **Creación y cierre individual de ventanas hijas.**

- b) **Dimensionamiento** de cada una de las ventanas hijas.

- c) **Organización de iconos.**

- d) **Organización de ventanas** abiertas en mosaico o cascada.

Las funciones anteriores van a permitir que utilizando ***Explocal*** se
pueda comparar de un sólo vistazo diferentes mezclas explosivas.

### 5.4.3 Funciones del cálculo de una mezcla explosiva

El programa debe incorporar todo el proceso de cálculo descrito en la
norma UNE 31-002 [[1]](13-referencias.md#referencia-1).

Esto incluye:

- a) **Introducción de datos** de una mezcla explosiva.

- b) Manejo de **fórmulas químicas**: Cálculo de pesos moleculares.

- c) Cálculo del **balance de oxígeno**.

- d) **Discusión del tipo de explosivo**, excedentario o deficitario en
  oxígeno.

- e) Cálculo de la **composición de los productos** de detonación y de la
  temperatura de explosión: Si el explosivo es deficitario en oxígeno el
  programa debe incorporar una función que resuelva una ecuación
  polinómica de tercer grado y discuta la solución obtenida. En
  cualquier caso se necesita la resolución de la ecuación en temperatura
  mediante un proceso iterativo.

- f) Cálculo del número de moles gaseosos, masa molecular media de
  productos gaseosos y volumen de gases.

- g) Aplicación de las **fórmulas empíricas** de *Kamlet y Jacobs* [[3]](13-referencias.md#referencia-3).

- h) Detección de **errores** en el proceso de cálculo y discusión de los
  resultados obtenidos.

- i) **Visualización de los resultados** en pantalla.

- j) Posibilidad de **cambiar las unidades** de los resultados.

### 5.4.4 Funciones de una aplicación informática comercial

Aunque no son intrínsecamente necesarias para el funcionamiento básico
del programa, existe una serie de características que proporcionan un
toque de calidad a cualquier aplicación informática.

Las más importantes son:

- a) **Iconos descriptivos** de la aplicación y de las ventanas hijas.

- b) **Programa de instalación**.

- c) **Acceso** a la **aplicación mediante el Administrador de
  Programas**: Debe permitir iniciar la aplicación con el archivo
  seleccionado.

- d) Archivo de **ayuda en hipertexto:** con acceso mediante menú y
  mediante botones. (*.HLP)

- e) Archivo de **inicialización** que almacene las preferencias elegidas
  por el usuario. (*.INI)

- f) Incorporación de **información** sobre la versión del programa.

### 5.4.5 Funciones de un programa de instalación

El programa de instalación soporta la responsabilidad de ser la primera
impresión que cualquier usuario se va a llevar de la aplicación
comercial; y dado que muchas veces la primera impresión es la que
cuenta: es de sentido común cuidar al máximo sus prestaciones y su
estética.

Los usuarios finales esperan un conjunto de características de los
programas de instalación. Añadir estas características puede asegurar
que la utilidad de la instalación produzca una buena impresión.

Estas características pueden resumirse en:

- a) **Ejecutable en *Windows*:** El software de instalación debería
  ejecutarse desde *Windows* puesto que la aplicación informática está
  diseñada para este sistema operativo.

- b) **Unidades de destino y origen modificables:** Después de mostrar un
  pequeño mensaje, el programa de instalación debería solicitar al
  usuario confirmación tanto de la unidad de origen como de la unidad de
  destino y del directorio de instalación. Se debería sugerir un valor
  por defecto para cada uno.

- c) **Opciones de instalación:** A continuación se puede preguntar al
  usuario qué opciones se van a instalar, (no todos los usuarios querrán
  instalar los ejemplos, o los archivos de ayuda). En este caso, como en
  los demás, también se debería sugerir opciones por defecto.

- d) **Control del espacio disponible:** Tan pronto como el programa
  conozca cuántos y qué archivos se van a instalar, se debería comprobar
  que en la unidad de destino hay espacio suficiente antes de continuar.
  Si no hay espacio suficiente es mejor avisar al usuario antes de
  comenzar la copia, en vez de agotar el espacio del disco después de
  haber copiado casi todos los archivos de distribución. El programa de
  instalación puede sugerir al usuario que elimine algunos archivos que
  use con poca frecuencia o que no usa para hacer un hueco en el disco.

- e) **Indicador de progreso:** Cuando el programa de instalación esté
  preparado para empezar a copiar archivos de los discos de distribución
  al disco duro del usuario final, el cursor del ratón debe cambiar al
  cursor de espera (símbolo del reloj de arena). Durante la secuencia de
  copia debe aparecer en pantalla un indicador de progreso que mantenga
  completamente informado al usuario. (Con frecuencia se usa una barra
  que se expande y un texto con el porcentaje realizado de instalación.)

- f) **Discos de distribución:** Si la aplicación informática se
  distribuye en más de un disquete, se debería asegurar que no es
  necesario introducir un disco más de una vez en la unidad, durante la
  instalación.

- g) **Nuevo grupo de programas:** Después de que el programa de
  instalación haya terminado de copiar archivos en el disco del usuario
  final, se debería añadir un nuevo grupo de programas en el
  Administrador de programas de *Windows*. El grupo de programas debería
  contener un icono por cada ejecutable de la aplicación informática.

Escribir un buen programa de instalación no es una tarea fácil, ya que:
crear y actualizar el indicador de progreso y usar el intercambio
dinámico de datos (DDE) para ordenar al *Administrador de archivos* que
cree un nuevo grupo presenta una elevada complicación técnica.

Aunque el desarrollo de un programa de instalación no esté entre los
objetivos de este proyecto se incluye uno, de calidad más que aceptable,
en el disco que se adjunta en los anexos de proyecto, que permite la
instalación de todos los archivos que forman parte de la aplicación
informática ***Explocal***.

Estos archivos son:

- h) Archivo ejecutable.

- i) Ayuda en hipertexto.

- j) Ejemplos.

- k) Archivos de datos.

- l) Bibliotecas enlace dinámico.

- m) Archivo de inicialización.

## 5.5 Bases de datos de información externa

Se pretende organizar toda la información que necesita ***Explocal***
para su correcto funcionamiento en archivos de datos (extensión *.DAT).

Un criterio, de gran importancia, que se debe tener en cuenta para
facilitar la vida del usuario es: procurar disminuir (al mínimo) el
número de datos que se deben introducir para conseguir hacer funcionar
correctamente el programa en cuestión.

En ***Explocal*** se debe, por lo tanto, conseguir que una vez que se ha
seleccionado un compuesto determinado, el programa se encargue de buscar
sus datos adicionales (como pueden ser su fórmula, nombre completo,
energía de formación, etc.) y de calcular otros (como su peso molecular,
y su balance de oxígeno).

Esto obliga a crear un archivo de datos (que se denominará REACTIVO.DAT)
conteniendo información con datos de diversos componentes de explosivos.

En el *ANEXO B* de la norma UNE 31-002 [[1]](13-referencias.md#referencia-1) se puede encontrar la
información necesaria para crear REACTIVO.DAT.

También se necesita incorporar información sobre los productos de
detonación con datos sobre: átomo asociado al producto de detonación
(símbolo y masa atómica), fórmula del producto de explosión, fórmula del
producto para el cálculo del balance de oxígeno, incremento de entalpía
específica en un intervalo amplio de temperaturas, temperatura de
vaporización y energía de formación. Todos estos datos se almacenan en
el archivo TABLPROD.DAT.

Los datos sobre los valores de las constantes de equilibrio se
encuentran en el archivo CONSTANT.DAT y son los que incluye la norma UNE
31-002 [[1]](13-referencias.md#referencia-1) en su ANEXO C.

Como se puede deducir de todo lo anterior se intenta colocar todos los
datos químicos y termoquímicos en archivos de texto (de fácil edición)
para que se puedan modificar sin esfuerzo. Esto es interesante debido a
que los datos pueden variar dependiendo de la fuente de donde se tomen.

Por último se considera un archivo con las descripciones de los errores
que pueden ocurrir en la aplicación de los cálculos: ERROR.DAT.

## 5.6 Requisitos del mantenimiento

Se consideran las siguientes posibilidades de cambio en el código del
programa:

- a) **Traducción del programa a otros idiomas:** La codificación debe
  garantizar un fácil acceso a los textos.

- b) **Cambio en los datos:** Sólo es necesario editar y cambiar los
  archivos de datos.

- c) **Actualización de la aplicación a *Windows* *95*:** Se debe cambiar
  el *interfaz de usuario*, por lo que puede resultar muy útil separar
  en el código donde se implementa el funcionamiento del interfaz, del
  código donde se incluyen los cálculos.
