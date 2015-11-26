/*_____________________________________________________________________
	Archivo:   		EXPLOCAL.H
	Proyecto:  		EXPLOCAL.PRJ
	Autor:     		Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
				  		Proyecto de Fin de Carrera.
	Especialidad: 	Laboreo y Explosivos.
	Descripción:   Archivo de cabecera que declara todas las clases
						,y estructuras necesarias para compilar el código
						de EXPLOFAB.CPP así como la definición de todos
						las macros.
_____________________________________________________________________*/



// Macro para la compilación condicional.


#define __EXPLOCAL_H

#if !defined(ID_INPUT)
#include "owlrc.h"
#endif

//************  Definiciones de los macros de los controles **********


// CADENAS:____________________________________________________________

#define IDS_TITULO	10
#define IDS_ERROR1	20
#define IDS_ERROR2	30
#define IDS_ERROR3   40
#define IDS_ERROR4	50
#define IDS_ERROR5	60
#define IDS_ERROR6	70
#define IDS_ERROR7   75

#define IDS_NOMBRE   80
#define IDS_FORMUL   90
#define IDS_ENEFOR  100
#define IDS_PSOMOL  110
#define IDS_BALOXI  115
#define IDS_PRNOMB  120
#define IDS_PRCOMP  130
#define IDS_PRDENS  140
#define IDS_PRENER  150
#define IDS_PRFORM  160
#define IDS_PRFOR1  163
#define IDS_PRFOR2  165
#define IDS_PRBALO  170
#define IDS_PREXCE  180
#define IDS_PREQUI  190
#define IDS_PRDEFI  200
#define IDS_PRQEXP  210
#define IDS_PRTEXP  220
#define IDS_PRMOLE  230
#define IDS_PRMASA  240
#define IDS_PRVOLG  250
#define IDS_PRESPE  260
#define IDS_PRPRFI  270
#define IDS_PRPRES  280
#define IDS_PRVELD  290
#define IDS_PRDECJ  300
#define IDS_PRCOAD  310
#define IDS_CARACT  320
#define IDS_REACIO  340
#define IDS_ESTIMA  350
#define IDS_NUMERO  360
#define IDS_RNOMBR  370
#define IDS_RFORMU  380
#define IDS_RENERG  390
#define IDS_RPSOMO  400
#define IDS_RPORCE  410
#define IDS_RBALOX  420
#define IDS_PMOLES  430
#define IDS_EFORMA  440
#define IDS_PRODUC  450
#define IDS_PRODEX  460
#define IDS_ELIMIN  470
#define IDS_ESTECP  480
#define IDS_ERRRRR  490

// MENUS:______________________________________________________________


#define CM_IMPRIMIR 111
#define CM_NUEVOEXP 131
#define CM_ABRIREXP 141
#define CM_GUARDEXP 151
#define CM_GCOMOEXP 161
#define CM_COMPOSIC 171
#define CM_MODIFICA 181
#define CM_INDICEAY 191
#define CM_ACERCADE 211
#define CM_EDITSELT 221
#define CM_PREFEREN 241
#define CM_RECALCUL 251


// LISTAS:_____________________________________________________________
#define ID1_LISTA1 180
#define ID1_LISTA2 190
#define ID1_LISTA3 200
#define ID2_LISTA1 210
#define ID2_LISTA2 220
#define ID2_LISTA3 230
#define ID4_LISTA1 240
#define ID4_LISTA2 250

// CUADROS DE EDICION:_________________________________________________
#define ID2_EDITA1 261

#define ID3_EDITA1 270
#define ID3_EDITA2 280

#define ID5_EDITA1 290
#define ID5_EDITA2 300
#define ID5_EDITA3 310
#define ID6_EDITA1 104

// BOTONES:_________________________________________________________________
#define ID1_BTPONE 400 // Diálogo 1
#define ID1_BTQUIT 410
#define ID1_BTINIC 420
#define ID1_BTEXAM 440

#define ID2_BTCAMB 450 // Diálogo 2
#define ID2_BTINIC 470
#define ID2_BTINTR 480

#define ID3_BTINTR 490 // Diálogo 3
#define ID4_BTANAD 500 // Diálogo 4
#define ID4_BTELIM 510

#define ID5_BTINTR 10  // Diálogo 5


// BOTONES DE RADIO:________________________________________________________
#define ID6_BRJULS 101 // Diálogo 6
#define ID6_BRCALS 102
#define ID6_BRAJUS 106
                 
//CADENAS:_________________________________________________________________
#define CAD_JULIOS "julios"
#define CAD_CALORS "calorías"
#define CAD_SI	"si"
#define CAD_NO "no"

//ARCHIVOS:_________________________________________________________________

#define __ERROR_DAT    "error.dat"     // Archivo de errores.
#define __TABLPROD_DAT "tablprod.dat"  // Archivo con la tabla de productos.
#define __CONSTANT_DAT "constant.dat"	// Cte. de equilibrio (K1) y (K2)
#define __EXPLOCAL_INI "explocal.ini"	// Archivo de inicialización.
#define __REACTIVO_DAT "reactivo.dat"	// Datos de los reactivos.
#define __REACTIVO_BAK "reactivo.bak"	// Seguridad del archivo de datos.
#define __EXPLOCAL_HLP "explocal.hlp"	// Archivo de ayuda para Windows 3.1
													// Archivo temporal,  ¡¡falta!!
#define ARCH_CABECERO "[Explocal]"  	// Cabecero de los archivos *.xpl

//OTROS:____________________________________________________________________

//	Emplazamiento de la ventana principal:
#define ORGX_VENTANA	 0
#define ORGY_VENTANA	 0
// Posición del menú de acceso a la ventana hija.
#define VENTANA_MENU_POS 4

// Caracteres en el nombre de la ventana hija.
#define MAX_NOMBRE_HIJA 14

#define SI TRUE
#define NO FALSE

#define MAXCAD	 50  		  //Tamaño de las cadenas alfanuméricas.
#define MAXTEX 250        //Tamaño de los textos en caracteres.


#define TIPO_TEXTO TRUE
#define TIPO_EXPLO FALSE
#define TIPO_INACCESIBLE -1

// Estados del menú.
#define MF_ACTIVO   MF_ENABLED
#define MF_INACTIVO MF_GRAYED

/*_____________________________________________________________________
  Fin del Archivo:   		EXPLOCAL.H
_____________________________________________________________________*/
