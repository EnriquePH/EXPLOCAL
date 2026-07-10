# Anexo F: Manual del usuario

*El manual de usuario original de Explocal (1996) se distribuía como
sistema de ayuda hipertexto de Windows 3.1 (`EXPLOCAL.HLP`, compilado a
partir del código fuente `docs/hlp/AYUDA.RTF`, incluido también en el
repositorio en `docs/hlp/`, con el Microsoft Help Compiler — ver
[Anexo H](h-software-apoyo.md) para los detalles de versión de esta
herramienta y del editor empleado para redactar el RTF). Al no poder
reproducirse aquí la navegación por hipervínculos e iconos propia de la
ayuda de Windows, este anexo presenta linealmente el contenido de
`AYUDA.RTF`: la descripción de la aplicación y las instrucciones de uso
completas. Se omiten los apartados de la ayuda que explican la teoría de
la detonación y el método de cálculo, ya que reproducen literalmente el
contenido de los Capítulos 2 y 3 de esta memoria.*

## F.1 Descripción de Explocal

**Explocal** es una aplicación informática, basada en la norma UNE
31-002 que permite obtener las principales características
teóricas de cualquier explosivo, a partir de su
composición porcentual en peso y su densidad inicial.

El programa trabaja como un **procesador de textos** e incorpora todas
las funciones básicas de manejo de textos y del calculo de un
explosivo, como:

-Manejo del portapapeles (Cortar y Pegar)

-Manejo de múltiples ventanas.

-Impresión de resultados

-Cambio de unidades energéticas

-Buscar, y repetir búsqueda.

-Selección de texto.

-Abrir y Guardar ficheros de texto.

-Abrir y Guardar ficheros con datos de un explosivo.

-Ayuda en hipertexto.

-Cálculo de un explosivo.

-Añadir datos de sustancias explosivas.

## F.2 Uso de Explocal
### F.2.1 El menú


*La tabla detallada de opciones del menú (equivalente a esta sección de la ayuda) se reproduce como **Tabla [6-7](../capitulos/08-diseno.md#tabla-6-7): Opciones del menú** en el apartado [6.3.3 Menú. Funciones respuesta](../capitulos/08-diseno.md#menú.-funciones-respuesta) de esta memoria; se omite aquí para no duplicar contenido.*

### F.2.2 Introducir datos adicionales

Desde este cuadro de diálogo, se pueden introducir el nombre del
explosivo (opcional) y la

densidad inicial. de la mezcla en g/cm^3^.

Si no se introduce ningún valor el programa usa uno por defecto (1,2
g/cm^3^ )

Para conseguir este objetivo, se deben emplear los siguientes controles:

**EDICIÓNES DE TEXTO:**

Uno para el nombre del explosivo y otro para la densidad inicial o de
encartuchado.

**TECLA INTRO:**

Introduce el dato del *control de edición de texto* en la memoria.

**BOTÓN ACEPTAR:**

**Muestra los resultados del explosivo en una ventana. **

**BOTÓN CANCELAR:**

**Vuelve al diálogo anterior: composición cuantitativa.**

**BOTÓN AYUDA:**

Muesta esta pantalla de ayuda.

**Modificar lista de reactivos:**

**(Explosivo\\Modificar lista de reactivos\...)**

Desde este cuadro de diálogo, se pueden añadir nuevos reactivos a la
base de datos que se encuentra en el archivo REACTIVO.DAT.

Otra posibilidad es editar el archivo en modo texto e introducir los
datos manualmente.

Para conseguir este objetivo, se deben emplear los siguientes controles:

**LISTA DE REACTIVOS:**

Muestra todos los nombres de los reactivos de los que se dispone
información ( en el archivo REACTIVO.DAT..)

-Para buscar un reactivo:

Pulse la inicial del nombre del reactivo y selecciónelo con el ratón.

**LISTA DE DATOS DEL REACTIVO:**

Muestra todos los datos del reactivo que tiene el cursor.

**Nombre, Fórmula, Peso molecular, Energía de formación**

**y balance de oxígeno**.

**BOTÓN AÑADIR:**

Cambia a introducir datos del reactivo.

**BOTÓN ELIMINAR:**

Elimina el compuesto seleccionado de la lista de reactivos.

¡ USELO CON PRECAUCIÓN !

Haga copias de REACTIVO.DAT antes.

**BOTÓN ACEPTAR:**

Vuelve al menú .

**BOTÓN CANCELAR:**

Vuelve al menú .

**BOTÓN AYUDA:**

Muesta esta pantalla de ayuda.

### F.2.3 Introducir datos del reactivo

Desde este cuadro de diálogo, se pueden introducir el *nombre del
reactivo,* *la fórmula* y la *energía interna* a **298 K** ( que
coincide con la de formación al considerar el estado de referencia a esa
temperatura.)

La fórmula no debe contener paréntesis y debe estar escrita con los
elementos en la correcta combinación de minúsculas y mayúsculas.

Para conseguir este objetivo, se deben emplear los siguientes controles:

**EDICIÓNES DE TEXTO:**

Uno para el nombre del reactivo, otro para la fórmula y otro para la
energía interna a 298 K.

**TECLA INTRO:**

Introduce el dato del *control de edición de texto* en la memoria.

**BOTÓN ACEPTAR:**

Guarda los datos de los controles en
REACTIVO.DAT.**.**

**BOTÓN CANCELAR:**

Vuelve al diálogo anterior: **modificar la lista de
reactivos**.

**BOTÓN AYUDA:**

Muesta esta pantalla de ayuda.

### F.2.4 Preferencias

**(Archivo\\Preferencias\...)**

Desde este cuadro de diálogo, se puede configurar el programa para que
muestre el resultado en Julios (J) o en calorías (cal) o elegir si se
ajustan los valores del calor, temperatura y velocidad de detonación (
al multiplo más próximo a 5 ó 10, según el caso.)

También se puede configurar en directorio de datos, que es dónde se
deben encontrar los archivos de datos**.**

Si en la instalación sustituyó en directorio de instalación por otro
distinto a C:\\EXPLOCAL

debe cambiar este por el nuevo directorio, si no el programa no
funcionará.

Si quiere cambiar los resultados de un explosivo de unidades, seleccione
en Preferencias las unidades que desee y luego vaya a
**(Explosivo\\Modificar
composición\....)** y pulse dos
veces **Aceptar**.

**EDICIÓN DE TEXTO:**

Para almacenar el directorio de datos.

**CONTROL JULIOS - CALORÍAS:**

Pulse sobre la unidad que desee.

**CONTROL DE AJUSTES:**

Si se encuentra tachado, se ajustan los resultados.

**TECLA INTRO:**

Introduce el dato del *control de edición de texto* en la memoria.

**BOTÓN ACEPTAR:**

Guarda los datos de configuración de **Explocal** en:

C:\\WINDOWS\\EXPLOCAL.INI

y vuelve al menú .

**BOTÓN CANCELAR:**

Vuelve al menú .

**BOTÓN AYUDA:**

Muesta esta pantalla de ayuda.

### F.2.5 Archivos de datos

Los archivos con extensión \*.DAT se deben encontrar directorio que
señala el diálogo Preferencias

EXPLOCAL.INI:

Archivo de inicialización, almacena las Preferencias y se
encuentra en C:\\WINDOWS

REACTIVOS.DAT:

Almacena los datos de los reactivos que forman parte de los explosivos.

ERRORES.DAT:

Guarda la descripción de los errores que se pueden producir en el
proceso de cálculo.

CONSTANT.DAT:

Almacena los datos de las constantes K1 y K2 para el intervalo de
temperaturas 298 K - 6000 K.

PRODUCTO.DAT:

Contiene los datos de los productos de explosión que considera que se
forma el método simplificado.



El método de cálculo que emplea **Explocal** está descrito en la norma **UNE 31-002-94: *Cálculo de las principales características teóricas de los explosivos***. Para más información sobre la norma, dirigirse a **AENOR** (c/ Fernández de la Hoz, 52 — 28010 Madrid — Tlf: 310 49 61).

## F.3 Glosario de términos

**balance de oxígeno:** Cantidad de oxígeno que
sobra o falta a una mezcla explosiva para oxidar *completamente* todos
los elementos químicos que la componen, salvo el nitrógeno que se supone
inerte, expresada en tanto por ciento en peso. Es negativo (deficitario)
si falta y positivo (excedentario) si sobra.

**calor de explosión:** Es la *máxima energía* que
se puede liberar en la reacción de un explosivo. Se trata de la
característica más importante de un explosivo, puesto que representa la
capacidad del explosivo para generar choques e impartir movimiento al
medio en el que está confinado, es decir su poder energético.

**coeficiente adiabático:** disminución de la
presión , respecto a un aumento de volumen manteniendo la entropía
constante. En los gases ideales es igual a la relación entre calores
específicos.

**densidad inicial:** densidad de la mezcla
explosiva intacta, es decir, antes de sufrir el proceso de explosión. Se
denomina a veces densidad del encartuchado (o del granel).

**detonación:** Proceso de explosión en el cual se
produce flujo de productos de reacción, inducido por la propagación de
la onda de reacción de velocidad supersónica respecto al material
intacto

**detonación ideal:** detonación con un frente de
onda plano (diámetro infinito) , en el que la reacción se produce
instantáneamente en el frente de discontinuidad (entre reactivos y
productos), y se mueve a velocidad constante (de forma estacionaria).

**deflagración:** Proceso de explosión en el cual
la onda de reacción se propaga a una velocidad inferior a la del sonido
en el material sin reaccionar o intacto.

**estado Champman-Jouguet o estado C-J:** estado
que alcanzan los productos en una detonación en régimen estacionario, en
el instante de completarse la reacción.

**explosivo:** Sustancia susceptible de sufrir una
explosión.

**explosión:** Proceso por el cual una sustancia se
transforma bruscamente en productos, en su mayor parte en estado
gaseoso, con una velocidad de transformación suficientemente alta, de
forma que los productos se encuentran a presiones y temperaturas
elevadas.

**presión de detonación:** Presión en el estado C-J
(Champman - Jouguet). Difiere de la presión a volumen constante.

**temperatura de explosión:** Es la temperatura que
alcanzan los productos de explosión considerando que la energía liberada
(calor de explosión) se invierte exclusivamente en calentar los
productos de reacción.

**velocidad de detonación:** Velocidad de
propagación de la onda de detonación.

**volumen de gases en condiciones normales:** Es el
volumen que ocuparían los productos gaseosos de la explosión ( de un
kilo de explosivo) a una temperatura de 273.15 K (0 ºC) y a una presión
de 1,013 · 10 ^5^ Pa ( 1 atm)
