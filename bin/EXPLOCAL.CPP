
/*_____________________________________________________________________
	Archivo:			EXPLOCAL.CPP
	Proyecto:  		EXPLOCAL.PRJ
	Autor:     		Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
	Objetivo:		Proyecto Fin de Carrera.
   Fecha:         Septiembre 1996
	Especialidad: 	Laboreo y Explosivos.
   Departamento:	Ingeniería  química y  combustibles.
	Descripción:   Listado del programa principal que incorpora la in-
						terfaz con el usuario y el manejo de las funciones
						y datos para el cálculo de las princiapales carac-
						terísticas de los explosivos, según la norma UNE 31-002
   Aplicación:    Interfaz de Múltiple Documento (MDI) para Windows 3.1
	Compilador:		Borland C++ 3.1 con la librería Object Windows 1.0
_____________________________________________________________________*/


// DEFINICIONES:

#define STRICT   				// Comprobación estricta de conversión de tipos.
#define WIN31    				// Versión 3.1 de Windows.

//	ARCHIVOS DE CABECERA DEL C++:

#include <owl.h>		      // Librería Object Windows 1.0
#include <commdlg.h>       // Diálogos comunes.

#include <mdi.h>           // Clases (MDI)

#ifndef __IO_H
#include <io.h>     			// Función access
#endif

#include <bwcc.h>          // Borland Custom Controls
#include <alloc.h>
#include <editwnd.h>       // Editor de textos.

#ifndef __FILEDIAL_H
#include <filedial.h>      // Diálogos de apertura y cierre de archivos.
#endif


// ARCHIVOS DE CABECERA CREADOS:

#include "lockds.h"        // Funciones aux. segmento DS.

#if !defined (__EXPLOCAL_H)
#include "explocal.h"      // Inclusión de los macros.
#endif

#include "calculos.h"      // Cálculo de explosivos.


/*__________________________________________________________________________
  Clase explosivo, soporta todas las funciones necesarias para poder
  emplear el método de cálculo.
__________________________________________________________________________*/

Explosivo Xpl;  				
 
/*__________________________________________________________________________
	Clase:			AplicaciónMDI
   Derivada de:  	TApplication
   Descripción:	Definición  de la aplicación.
   Datos miembro:	XXXX_Aplic
	Nota I: 			Todas las aplicaciones OWL deben tener una descenciente
   					de TApplication.
	Nota II: 		El  macro _CLASSDEF se emplea para declarar tipos de
						referencia y puntero  con relación a las  nuevas clases                  
__________________________________________________________________________*/

_CLASSDEF(AplicacionMDI)

class _CLASSTYPE AplicacionMDI : public TApplication
{
LPSTR ArgLinea_Aplic;               	// Argumentos de línea.
public:
													// Constructor.
	 AplicacionMDI( LPSTR NombreApl,		// Nombre de la aplicación.
					HINSTANCE hEstaInst,    // Instancia de la aplicación.
					HINSTANCE hPreviaInst,  // Instancia previa.
					 	 LPSTR ArgLinea,     // Argumentos de la línea de comandos.
						   int CmdPres):    	// Comandos de presentación.
	 TApplication(NombreApl,hEstaInst,hPreviaInst,ArgLinea,CmdPres)
				{
            //Pasa los argumentos de línea.
				ArgLinea_Aplic=ArgLinea;
				};

virtual void InitMainWindow();   
virtual void InitInstance();
};

/*__________________________________________________________________________
	Clase:			VentanaMarcoMDI.
   Derivada de:  	TMDIFrame.
	Descripción:	Definición del tipo y características del marco MDI.
   					Realiza la gestión de las ventanas hijas.
	Datos miembro:	XXXX_Marco
	Nota   I: 		La gestión de los mensajes MDI envía los mensajes de tipo
						comando primero a la ventana hija activa y si esta no
						responde se envía a la ventana marco.
	Nota  II: 		La ventana marco es la que tiene el Menú.
	Nota III:	   Las funciones de respuesta a los mensajes del menú,
						se consideran como una característica de la ventana.
__________________________________________________________________________*/

_CLASSDEF(VentanaMarcoMDI)

class VentanaMarcoMDI : public TMDIFrame
{
public:
	int   NumHija_Marco;            // Númeración de la última ventana hija.
	int   CuentaHijas_Marco;        // Contador del número total de hijas.
	char  DirDatos_Marco[MAXPATH];  // Directorio de los archivos *.DAT.
	LPSTR ArgLinea_Marco;           // Argumentos de la línea de comandos.
	BOOL  Unidades_Marco;			  // Julios o calorías.
	BOOL  Ajuste_Marco;				  // Ajuste de resultados (s/n);
   // Función constructora.
	VentanaMarcoMDI(LPSTR Titulo,LPSTR NombreMenu,LPSTR ArgLinea);
   // Función destructora de la  clase.
	~VentanaMarcoMDI();
	// tomar el handle de la impresora.
	HDC GetPrtDC();    
	void CambiarMenu(UINT Tipo_Menu);
	BOOL TestArchExplo(char *Archivo);
   BOOL TestArchDat(char *Archivo);
protected:
	virtual void GetWindowClass(WNDCLASS _FAR & ClaseMarco);
	virtual void SetupWindow();
	void CrearHija(LPSTR Archivo,BOOL TipoHija);

	// Declaración de las funciones de respuesta a los mensajes del menú.
	void  CMNewFile(RTMessage Msg)=[CM_FIRST+CM_FILENEW];
	void CMOpenFile(RTMessage Msg)=[CM_FIRST+CM_FILEOPEN];
	void CMNuevoExp(RTMessage Msg)=[CM_FIRST+CM_NUEVOEXP];
	void CMAbrirExp(RTMessage Msg)=[CM_FIRST+CM_ABRIREXP];
	void CMModifica(RTMessage Msg)=[CM_FIRST+CM_MODIFICA];
	void CMIndiceAy(RTMessage Msg)=[CM_FIRST+CM_INDICEAY];
	void CMAcercaDe(RTMessage Msg)=[CM_FIRST+CM_ACERCADE];
	void CMPreferen(RTMessage Msg)=[CM_FIRST+CM_PREFEREN];
};

/*__________________________________________________________________________
	Clase:			VentanaHijaMDI.
   Derivada de:  	TEditWindow.
	Descripción:	Definición y características de las ventanas hijas.
						Incorpora toda las funciones de un editor de textos
						,que se emplea para mostrar los resultados de los cál-
						culos, y también el manejo de archivos.
	Datos miembro:	XXXX_Hija
__________________________________________________________________________*/

_CLASSDEF(VentanaHijaMDI)

class VentanaHijaMDI : public TEditWindow
{
//Datos privados necesarios para calcular un explosivo.
int 		 NumReact_Hija;
REACTIVO  Reactivo_Hija[MAX_REACTIVOS];
double 	 d0_Hija;
char 	    NombreExp_Hija[MAXCAD];
 
LPSTR     Archivo_Hija; 	// Nombre del archivo asociado a la ventana hija.
BOOL      DatModif_Hija;	// ¿Se han modificado los datos? 
BOOL      ArchNuevo_Hija; 	// ¿Es nuevo el archivo?
BOOL      Tipo_Hija;   		// El archivo ¿Es de texto o explosivo?
    								// Puntero a la ventana marco.
PVentanaMarcoMDI VMarco_Hija;

public:
    // Constructor.
	 VentanaHijaMDI(PVentanaMarcoMDI Marco,     // Puntero al marco MDI.
						 PTWindowsObject AParent,    // Puntero al precursor.
						 LPSTR NombreHija,           // Nombre de la ventana.
						 LPSTR Archivo,              // Archivo asociado.
						 BOOL TipoHija);             // ¿Texto o explosivo?
	 // Destructor.
	 virtual ~VentanaHijaMDI();

private:
	 virtual void GetWindowClass(WNDCLASS _FAR & ClaseHija);
    virtual BOOL CanClear();
	 virtual BOOL CanClose();
	 void ReplaceWith(LPSTR Archivo);
	 void PonerArchivoHija(LPSTR Archivo);
	 
	 void PonerDatos();
	 void TomarDatos();
    BOOL Modificacion();
    BOOL Leer(BOOL TipoDatos);
    BOOL Read();
    BOOL Save(BOOL TipoDatos);
	 BOOL SaveAs(BOOL TipoDatos);
    BOOL Grabar(BOOL TipoDatos);
    BOOL Write();

protected:
	 void PonerEnEditor(int IDSCadena,double Valor,BOOL Unidades=EN_DEFECTO);
	 void PonerEnEditor(int IDSCadena,char *Texto="");
	 void PonerResultados();
	 void PrintPage(HDC hPrtDC, char* Texto);  // imprimir las páginas
	 virtual void SetupWindow();

	 // Funciones respuesta a las opciones del menu:
    virtual void CMFileSave(RTMessage Msg)=[CM_FIRST+CM_FILESAVE];
	 virtual void CMFileSaveAs(RTMessage Msg)=[CM_FIRST+CM_FILESAVEAS];
	 void CMEditSelT(RTMessage Msg)=[CM_FIRST+CM_EDITSELT];
	 void CMGuardExp(RTMessage Msg)=[CM_FIRST+CM_GUARDEXP];
	 void CMGComoExp(RTMessage Msg)=[CM_FIRST+CM_GCOMOEXP];
	 void CMComposic(RTMessage Msg)=[CM_FIRST+CM_COMPOSIC];
	 void CMFilePrint(RTMessage Msg)=[CM_FIRST+CM_IMPRIMIR];
	 void CMRecalcul(RTMessage Msg)=[CM_FIRST+CM_RECALCUL];
	 
};

/*__________________________________________________________________________
	Definición y características del cuadro de diálogo: DIALOGO_BASE
	que sirve de clase base del resto de los diálogos de la aplicación
	que utilicen las funciones del manejo de reactivos de la mezcla
	explosiva.
__________________________________________________________________________*/

class Dialogo_Base:public TDialog
{
public:
	PVentanaMarcoMDI VMarco_DBase;
	// función constructora.
	Dialogo_Base(PTWindowsObject Propiedades,PVentanaMarcoMDI Marco,
								  LPSTR Nombre_Dialogo):
		TDialog(Propiedades,Nombre_Dialogo)
				{ VMarco_DBase=Marco;	}; 
protected:

	void Examinar(RTMessage Msg,int IDLstDestino,int IDLstDatos);
   void PonerEnLista(int IDLstDestino,int IDSCadena,char *Cadena1);
};


/*__________________________________________________________________________
	Definición y características del cuadro de diálogo: DIALOGO1
	(Composición cuantitativa de la mezcla explosiva).
	
   Nota	I: 	Cancelar no necesita que se declare una función de respuesta
					ya que existe una función miembro de TDialog que responde a
					un mensaje IDCANCEL al Windows.
__________________________________________________________________________*/

class Dialogo1:public Dialogo_Base
{
public:
	// función constructora.
	Dialogo1(PTWindowsObject Propiedades,PVentanaMarcoMDI Marco,
			LPSTR Nombre_Dialogo):
				Dialogo_Base(Propiedades,Marco,Nombre_Dialogo){};

protected:
	virtual void SetupWindow();
	// Declaración de las funciones respuesta a los botones del cuadro.
	void BTInic(RTMessage Msg)=[ID_FIRST+ID1_BTINIC];
   void BTPone(RTMessage Msg)=[ID_FIRST+ID1_BTPONE];
   void BTQuit(RTMessage Msg)=[ID_FIRST+ID1_BTQUIT];
   void Lista1(RTMessage Msg)=[ID_FIRST+ID1_LISTA1];
   void Lista2(RTMessage Msg)=[ID_FIRST+ID1_LISTA2];
	virtual void Ok(RTMessage Msg)=[ID_FIRST+IDOK];
	virtual void IDHelp(RTMessage Msg)=[ID_FIRST+IDHELP];

};


/*__________________________________________________________________________
	Definición y características del cuadro de diálogo: DIALOGO2
	 (Composición cualitativa de la mezcla explosiva)
__________________________________________________________________________*/

class Dialogo2:public TDialog
{
public:
	Dialogo2(PTWindowsObject Propiedades,LPSTR Nombre_Dialogo):
		TDialog(Propiedades,Nombre_Dialogo){};
protected:
	virtual void  SetupWindow();
   void PonerTotal();
	void BTIntr(RTMessage Msg)=[ID_FIRST+ID2_BTINTR];
	void Lista1(RTMessage Msg)=[ID_FIRST+ID2_LISTA1];
   void Lista2(RTMessage Msg)=[ID_FIRST+ID2_LISTA2];
	virtual void Ok(RTMessage Msg)=[ID_FIRST+IDOK];
	virtual void IDHelp(RTMessage Msg)=[ID_FIRST+IDHELP];
};


/*__________________________________________________________________________
	Definición y características del cuadro de diálogo: DIALOGO3
	 (Datos adicionales)
__________________________________________________________________________*/
class Dialogo3:public TDialog
{             
public:
	Dialogo3(PTWindowsObject Propiedades,LPSTR Nombre_Dialogo):
		TDialog(Propiedades,Nombre_Dialogo){};
protected:
	 virtual void SetupWindow();
	 void  BTIntr(RTMessage Msg)=[ID_FIRST+ID3_BTINTR];
	 virtual void  Ok(RTMessage Msg)=[ID_FIRST+IDOK];
	 virtual void IDHelp(RTMessage Msg)=[ID_FIRST+IDHELP];
};


/*__________________________________________________________________________
	Definición y características del cuadro de diálogo: DIALOGO4
	 (Modificar lista de compuestos)
__________________________________________________________________________*/
class Dialogo4:public Dialogo_Base
{
public:
	Dialogo4(PTWindowsObject Propiedades,PVentanaMarcoMDI Marco,
			LPSTR Nombre_Dialogo):
				Dialogo_Base(Propiedades,Marco,Nombre_Dialogo){};

protected:
	virtual void SetupWindow();
   void Lista1(RTMessage Msg)=[ID_FIRST+ID4_LISTA1];
   void BTAnad(RTMessage Msg)=[ID_FIRST+ID4_BTANAD];
	void BTElim(RTMessage Msg)=[ID_FIRST+ID4_BTELIM];
	virtual void IDHelp(RTMessage Msg)=[ID_FIRST+IDHELP];
};


/*__________________________________________________________________________
	Definición y características del cuadro de diálogo: DIALOGO5
	 (Introducir datos del reactivo)
__________________________________________________________________________*/
class Dialogo5:public TDialog
{
PVentanaMarcoMDI VMarco_Dial5;
public:
	Dialogo5(PTWindowsObject Propiedades ,PVentanaMarcoMDI Marco
					,LPSTR Nombre_Dialogo):
		TDialog(Propiedades,Nombre_Dialogo)
			{	VMarco_Dial5=Marco;	};
protected:
	virtual void  Ok(RTMessage Msg)=[ID_FIRST+IDOK];
	void BTIntr(RTMessage Msg)=[ID_FIRST+ID5_BTINTR];
	virtual void IDHelp(RTMessage Msg)=[ID_FIRST+IDHELP];
};

/*__________________________________________________________________________
	Definición y características del cuadro de diálogo: DIALOGO6
	 (Preferencias)
__________________________________________________________________________*/
class Dialogo6:public TDialog
{
// Datos miembro privados:
PVentanaMarcoMDI VMarco_Dial6; 	// Puntero a la ventana Marco
											// Permite el acceso a los datos públicos
public:

   // Función constructora declarada 'in line'
	Dialogo6(PTWindowsObject Propiedades,
				PVentanaMarcoMDI Marco,
				LPSTR Nombre_Dialogo)
				:TDialog(Propiedades,Nombre_Dialogo)
						{
						 // Pasa el puntero al Marco.
						VMarco_Dial6=Marco;  
						};

protected:
	virtual void SetupWindow();
	virtual void     Ok(RTMessage Msg)=[ID_FIRST+IDOK];
	virtual void IDHelp(RTMessage Msg)=[ID_FIRST+IDHELP];
};



/*__________________________________________________________________________
	Declaración de las funciones de respuesta a los elementos del
	 cuadro de diálogo:	DIALOGO_BASE
__________________________________________________________________________*/


// PONERENLISTA: Función auxiliar empleada para simplificar EXAMINAR.
void Dialogo_Base::PonerEnLista(int IDLstDestino,
										  int IDSCadena,char *Cadena1)
{
char Cadena2[MAXCAD];
LoadString(GetApplication()->hInstance,IDSCadena,Cadena2,MAXCAD);
strcat(Cadena2,Cadena1);
SendDlgItemMsg(IDLstDestino,LB_ADDSTRING,0,(LONG)Cadena2);
};


/* EXAMINAR: Muestra los datos del reactivo que tiene el rectángulo foco.
Nota I: 	No es necesario comprobar si está disponible el archivo de datos
		  	puesto que de faltar, el programa no pasa la opción "Iniciar". */
void Dialogo_Base::Examinar(RTMessage Msg,int IDLstDestino,int IDLstDatos)
{
double PMol;            // Peso molecular.
double BO;              // Balance de oxígeno.
double Energia;			// Energía de formación.
char Cadena1[MAXCAD];   // Cadenas auxiliares.
char Cadena2[MAXCAD];
char DirecDatos[MAXPATH];  // Directorio de datos.

sprintf(DirecDatos,"%s\\%s",VMarco_DBase->DirDatos_Marco,__REACTIVO_DAT);

FILE *f;					   // Archivo de datos.
LRESULT i;

if(Msg.LP.Hi==LBN_SELCHANGE) // Detecta cambios en la selección.
	{
	SendDlgItemMsg(IDLstDestino,LB_RESETCONTENT,0,0L); // Borra L. destino.

	// Si hay por lo menos un elemento en la lista 2.
		if(SendDlgItemMsg(IDLstDatos, LB_GETCOUNT, 0, 0L )!=0)
		{
		// Obtiene el índice del elemento de la lista 2 que tiene el rectán-
		// gulo foco,carga el elemento y lo envía a la lista 3
		i=SendDlgItemMsg(IDLstDatos,LB_GETCARETINDEX,0,0L);
		SendDlgItemMsg(IDLstDatos,LB_GETTEXT,i,(LONG)Cadena1);
		PonerEnLista(IDLstDestino,IDS_NOMBRE,Cadena1);

		
		// Abrir sólo de lectura.
		f=fopen(DirecDatos,"r");
		do	{
			fgets(Cadena2,MAXCAD,f);
			}

		/* Usa la longitud de la cadena para poder realizar la comparación.
		y la decrementa en una unidad para evitar problemas con el último
		 caracter que se ha cambiado en la opción de iniciar. */
		while(strncmp(Cadena2,Cadena1,strlen(Cadena1)-1));

		// Si las cadenas coinciden, se ha encontrado el compuesto.
		fgets(Cadena1,MAXCAD,f);            	// Carga la fórmula.
		Cadena1[(strlen(Cadena1))-1]='\0';  	// Elimina el carácter intro.
		BO=Xpl.CalcBO(BO_PRODBO,Cadena1);      // Calcula el Balance de Ox.
		PMol=Xpl.CalcPMol(Cadena1);            // Calcula el Peso molecular.
		PonerEnLista(IDLstDestino,IDS_FORMUL,Cadena1);
		sprintf(Cadena1,"%.3f g/mol",PMol); // Peso molecular con 3 decimales
		PonerEnLista(IDLstDestino,IDS_PSOMOL,Cadena1);
		fgets(Cadena2,MAXCAD,f);            // Carga la energía de formación
      // Conversión de unidades.
		Energia=CalAJul(VMarco_DBase->Unidades_Marco)*strtod(Cadena2,NULL);
		//Cadena2[(strlen(Cadena2))-1]='\0';  // Elimina el carácter intro.
		switch(VMarco_DBase->Unidades_Marco)
			{
			case EN_JULIOS:
				sprintf(Cadena1,"%.3f kJ/kg",Energia);
				break;
			case EN_CALORIAS:        	
				sprintf(Cadena1,"%.3f kcal/kg",Energia);
				break;
         }
		PonerEnLista(IDLstDestino,IDS_ENEFOR,Cadena1);
		sprintf(Cadena1,"%+.3f %%",BO);     // Balance de ox. con 3 decimales
		PonerEnLista(IDLstDestino,IDS_BALOXI,Cadena1);
		fclose(f);
   	}
	}
}


/*__________________________________________________________________________
	Declaración de las funciones de respuesta a los elementos del
	 cuadro de diálogo:	DIALOGO1
    (Composición cualitativa de la mezcla explosiva)
__________________________________________________________________________*/


/* La función SetupWindow() es siempre llamada inmediatamente después de
	haber sido construido el cuadro de dialogo, se ha redefinido para
  poder inicializar las listas. */
void Dialogo1::SetupWindow()
{
char Cadena1[MAXCAD];   	// Cadenas auxiliares.
char Cadena2[MAXCAD];
char DirecDatos[MAXPATH];  // Directorio de datos.
FILE *f;					   	// Archivo de datos.
sprintf(DirecDatos,"%s\\%s",VMarco_DBase->DirDatos_Marco,__REACTIVO_DAT);
if (VMarco_DBase->TestArchDat(DirecDatos))//Comprueba la presencia del archivo de datos.
		{
      Xpl.BorrarDatos();
		// Borra todos los cuadros de lista del cuadro de diálogo.
		SendDlgItemMsg(ID1_LISTA1,LB_RESETCONTENT,0,0L);
		SendDlgItemMsg(ID1_LISTA2,LB_RESETCONTENT,0,0L);
		SendDlgItemMsg(ID1_LISTA3,LB_RESETCONTENT,0,0L); 
		 	// Abre el archivo de las sustancias explosivas.
		f=fopen(DirecDatos,"r");
		while((fgets(Cadena1,MAXCAD,f))!=NULL) //Hasta el final del archivo.
			{
			fgets(Cadena2,MAXCAD,f); // Salta el campo de fórmulas.
			fgets(Cadena2,MAXCAD,f); // Salta el campo de energías de form.
				// Elimina el caracter intro: de no hacerlo los nombres de los
     		 	// compuestos acabarán con un cuadrado al salir en la lista.
			Cadena1[(strlen(Cadena1))-1]='\0';
			SendDlgItemMsg(ID1_LISTA1,LB_ADDSTRING,0,(LONG)Cadena1);
			}    
		fclose(f);
		}
  else EndDialog(HWindow,IDCANCEL);//Termina el diálogo antes de mostrarlo.
}


// LISTA1: Muestra los datos del compuesto de la lista 1 que tiene
// el rectángulo foco.
void Dialogo1::Lista1(RTMessage Msg)
{
Examinar(Msg,ID1_LISTA3,ID1_LISTA1);
}

// LISTA2: Muestra los datos del compuesto de la lista 2 que tiene
// el rectángulo foco.
void Dialogo1::Lista2(RTMessage Msg)
{
Examinar(Msg,ID1_LISTA3,ID1_LISTA2);
}

/* INICIAR: El botón de Iniciar tiene la misión de reinicializar el cuadro,
   funciona a modo de 'RESET'. */
void Dialogo1::BTInic(RTMessage)
{
EnableWindow (GetDlgItem(HWindow,IDOK),NO); // Desabilita botón.
SetupWindow();
}

// PONER: >> Pasa los compuestos seleccionados de la lista 1 a la 2.
void Dialogo1::BTPone(RTMessage)
{
char Cadena1[MAXCAD];   // Cadenas auxiliares.
LRESULT Estado;
LRESULT NumLista=SendDlgItemMsg(ID1_LISTA1,LB_GETCOUNT   ,0,0L);
LRESULT NumSelec=SendDlgItemMsg(ID1_LISTA1,LB_GETSELCOUNT,0,0L);

while(NumSelec)
	{
	EnableWindow(GetDlgItem(HWindow,IDOK),SI); //Habilita el botón: Aceptar
	for(LRESULT i=0;i<NumLista;i++)
		{
		Estado=SendDlgItemMsg(ID1_LISTA1,LB_GETSEL,i,0L);
		if(Estado!=0 && Estado!=LB_ERR)
			{
			SendDlgItemMsg(ID1_LISTA1,LB_GETTEXT,i,(LONG)Cadena1);
			SendDlgItemMsg(ID1_LISTA1,LB_DELETESTRING,i,0L);
			SendDlgItemMsg(ID1_LISTA2,LB_ADDSTRING,0,(LONG)Cadena1);
			NumSelec=SendDlgItemMsg(ID1_LISTA1,LB_GETSELCOUNT,0,0L);
			}
		}     
	}
}

// QUITAR: << Pasa los compuestos seleccionados de la lista 2 a la 1.
void Dialogo1::BTQuit(RTMessage)
{
char Cadena1[MAXCAD];
LRESULT 	NumLista, // Nº de reactivos en la lista 2.
			NumSelec, // Nº de reactivos seleccionados en la lista 2.
			Estado;   // de un elemento de la lista.
NumLista=SendDlgItemMsg(ID1_LISTA2,LB_GETCOUNT,0,0L);
NumSelec=SendDlgItemMsg(ID1_LISTA2,LB_GETSELCOUNT,0,0L);
while(NumSelec)
	{
	for(LRESULT i=0;i<NumLista;i++)
		{
		Estado=SendDlgItemMsg(ID1_LISTA2,LB_GETSEL,i,0L);
		if(Estado!=0 && Estado!=LB_ERR) // ¿Está seleccionado?
			{
			SendDlgItemMsg(ID1_LISTA2,LB_GETTEXT,i,(LONG)Cadena1);
			SendDlgItemMsg(ID1_LISTA2,LB_DELETESTRING,i,0L);
			SendDlgItemMsg(ID1_LISTA1,LB_ADDSTRING,0,(LONG)Cadena1);
			NumSelec=SendDlgItemMsg(ID1_LISTA2,LB_GETSELCOUNT,0,0L);
			}
		}     
	}
// Si no queda ningún reactivo en la lista 2, deshabilita el botón: Aceptar. 
if (!SendDlgItemMsg(ID1_LISTA2,LB_GETCOUNT,0,0L))
	{
	EnableWindow (GetDlgItem(HWindow,IDOK),NO);
   }
}


// OK: (Aceptar) Carga todos los reactivos de la lista 2 en REACTIVO. 
void Dialogo1::Ok(RTMessage)
{
char Cadena1[MAXCAD];
char Texto[MAXTEX];
// Anula todos los datos de los reactivos.

Xpl.BorrarDatos(); 

Xpl.NumReact=(int)SendDlgItemMsg(ID1_LISTA2,LB_GETCOUNT,0,0L);

if (Xpl.NumReact>MAX_REACTIVOS) // Si hay demasiados seleccionados.
	 {
	 // Mensaje de error: Demasiados Compuestos.
	 LoadString(GetApplication()->hInstance,IDS_ERROR3,Cadena1,MAXCAD);
	 LoadString(GetApplication()->hInstance,IDS_ERROR4,Texto,MAXTEX);
	 BWCCMessageBox(HWindow,Texto,Cadena1,MB_OK|MB_ICONEXCLAMATION);
	 }
else{
	 for(int i=0;i<Xpl.NumReact;i++)
	 	{
		SendDlgItemMsg(ID1_LISTA2,LB_GETTEXT,i,(LONG)Cadena1);	
		strcpy(Xpl.Reactivo[i].Nombre,Cadena1);
	   }
	 // En el caso de aceptar carga el diálogo 2.
	 if(GetApplication()->ExecDialog(new Dialogo2(this,"DIALOGO_2"))==IDOK)
	 	{
		EndDialog(HWindow,IDOK);
      }
	}  
}

// IDHELP: Llama archivo de ayuda.
void Dialogo1::IDHelp(RTMessage)
{
WinHelp(HWindow,__EXPLOCAL_HLP,HELP_CONTEXT,0x0007);
}


/*__________________________________________________________________________
	Declaración de las funciones de respuesta a los elementos del
	cuadro de diálogo: DIALOGO2
   (Composición cuantitativa de la mezcla)
__________________________________________________________________________*/

//PONERTOTAL: Coloca el porcentaje total en la lista y activa o desactiva
//el botón de aceptar sólo si se alcanza el 100%
void Dialogo2::PonerTotal()
{
char Cadena1[MAXCAD];
double Total=Xpl.CalcTotal();    // Cálculo del porcentaje total:
	
if(Total==100.0)
	{
  	EnableWindow(GetDlgItem(HWindow,IDOK),SI);
	}
else
	{
  	EnableWindow(GetDlgItem(HWindow,IDOK),NO);
   }
// Impresión del total en la lista.
sprintf(Cadena1,"%.3f",Total);
SendDlgItemMsg(ID2_LISTA3,LB_RESETCONTENT,0,0L);
SendDlgItemMsg(ID2_LISTA3,LB_ADDSTRING,0,(LONG)Cadena1);
}
 



// SETUPWINDOW: Carga los compuestos y pone a cero los porcentajes.
void Dialogo2::SetupWindow()
{
char Cadena1[MAXCAD];
// Borra los cuadros de lista.
SendDlgItemMsg(ID2_LISTA1,LB_RESETCONTENT,0,0L);
SendDlgItemMsg(ID2_LISTA2,LB_RESETCONTENT,0,0L);
SendDlgItemMsg(ID2_LISTA3,LB_RESETCONTENT,0,0L);
for(int i=0;i<Xpl.NumReact;i++)
	{

	SendDlgItemMsg(ID2_LISTA1,LB_ADDSTRING,0,(LONG)Xpl.Reactivo[i].Nombre);
	sprintf(Cadena1,"%.3f",Xpl.Reactivo[i].Porcentaje);
	SendDlgItemMsg(ID2_LISTA2,LB_ADDSTRING,0,(LONG)Cadena1);
	SendDlgItemMsg(ID2_LISTA1,LB_SETCURSEL,0,0L);
	SendDlgItemMsg(ID2_LISTA2,LB_SETCURSEL,0,0L);
	}
PonerTotal();
}

// LISTA1: Sincroniza la selección de la lista2 con la lista1.
void Dialogo2::Lista1(RTMessage Msg)
{
LRESULT i;
if(Msg.LP.Hi==LBN_SELCHANGE)	// Detecta cambios en la selección
	{
	 i=SendDlgItemMsg(ID2_LISTA1,LB_GETCURSEL,0,0L);
	   SendDlgItemMsg(ID2_LISTA2,LB_SETCURSEL,i,0L);
	}
}
// LISTA2: Sincroniza la selección de la lista1 con la lista2.
void Dialogo2::Lista2(RTMessage Msg)
{
LRESULT i;
if(Msg.LP.Hi==LBN_SELCHANGE)	// Detecta cambios en la selección
	{
	 i=SendDlgItemMsg(ID2_LISTA2,LB_GETCURSEL,0,0L);
	   SendDlgItemMsg(ID2_LISTA1,LB_SETCURSEL,i,0L);
	}
}

/* BTINTR: Respuesta a la pulsación de la tecla "Intro",se emplea un botón
   invisible  de tipo BS_DEFPUSHBUTTON. (Botón Fantasma)
  	Comprueba si hay algo en la lista: si es así, lee el porcentaje del
 	reactivo seleccionado, baja la barra de selección al reactivo inmediata-
 	mente inferior tanto en la lista de porcentajes de cada
 	reactivo como en la de porcentajes totales.*/

void Dialogo2::BTIntr(RTMessage)
{
char Cadena1[MAXCAD];
double Lectura;
WORD i;// Índice del elemento seleccionado.


// Es importante que la cadena leida termine en '\0'. Si no error...
Cadena1[(WORD)SendDlgItemMessage(HWindow,ID2_EDITA1,EM_GETLINE,0,
			(DWORD)(LPSTR)Cadena1)]='\0';
if(strlen(Cadena1)) // Cuando haya algo en el Cuadro de edición...
	{
	i=SendDlgItemMsg(ID2_LISTA1,LB_GETCURSEL,0,0L);
	//Borra el cuadro de edición.
	SetDlgItemText(HWindow,ID2_EDITA1,"");
	Lectura=strtod(Cadena1,NULL);
	if(Lectura>=0.0 && Lectura<=100.0)
   	{
		Xpl.Reactivo[i].Porcentaje=Lectura;
		sprintf(Cadena1,"%.3f",Lectura);

    	// Cambia el porcentaje antiguo por el nuevo.
		SendDlgItemMsg(ID2_LISTA2,LB_DELETESTRING,i,(LONG)Cadena1);
		SendDlgItemMsg(ID2_LISTA2,LB_INSERTSTRING,i,(LONG)Cadena1);

		i++;  // Siguiente elemento...

		if(i==Xpl.NumReact)
      	{
			i=0;
         }
		/* Si se llega al final, ir al principio.
		Selecciona el siguiente elemento, sin romper la
		sincronización entre ambas listas.   */
		SendDlgItemMsg(ID2_LISTA1,LB_SETCURSEL,i,0L);
		SendDlgItemMsg(ID2_LISTA2,LB_SETCURSEL,i,0L);
		PonerTotal();
      }
	}
}

// OK:
void Dialogo2::Ok(RTMessage)
{
if(GetApplication()->ExecDialog(new Dialogo3(this,"DIALOGO_3"))==IDOK)
	{
	EndDialog(HWindow,IDOK);
   }
}

// IDHELP: Llama archivo de ayuda.
void Dialogo2::IDHelp(RTMessage)
{
WinHelp(HWindow,__EXPLOCAL_HLP,HELP_CONTEXT,0x0008);
}


/*__________________________________________________________________________
	Declaración de las funciones de respuesta a los elementos del
	cuadro de diálogo:	DIALOGO3
   (Datos adicionales)
__________________________________________________________________________*/

//SETUPWINDOW:
void Dialogo3::SetupWindow()
{
char Cadena[MAXCAD];
SendDlgItemMsg(ID3_EDITA1,EM_SETSEL,0,0L);
SendDlgItemMsg(ID3_EDITA1,EM_REPLACESEL,0
						,(LPARAM)((LPSTR)Xpl.Resultado.Nombre));

if(Xpl.Resultado.d0)
	{
	sprintf(Cadena,"%.3f",Xpl.Resultado.d0);
	SendDlgItemMsg(ID3_EDITA2,EM_SETSEL,0,0L);
	SendDlgItemMsg(ID3_EDITA2,EM_REPLACESEL,0
							,(LPARAM)((LPSTR)Cadena));
   }
}

// OK:
void Dialogo3::Ok(RTMessage)
{
char Cadena1[MAXCAD];

Cadena1[(WORD)SendDlgItemMsg(ID3_EDITA1,EM_GETLINE,0,
		  (DWORD)(LPSTR)Cadena1)]='\0';
strcpy(Xpl.Resultado.Nombre,Cadena1);

Cadena1[(WORD)SendDlgItemMsg(ID3_EDITA2,EM_GETLINE,0,
											(DWORD)(LPSTR)Cadena1)]='\0';
Xpl.Resultado.d0=strtod(Cadena1,NULL);
EndDialog(HWindow,IDOK);
}    


/* BTINTR: Al Pulsar la tecla INTRO: El foco se mueve al siguiente elemento
    Esto se tiene en cuenta para facilitar la entrada de datos  */
void Dialogo3::BTIntr(RTMessage)
{
char Cadena1[MAXCAD];

Cadena1[(WORD)SendDlgItemMsg(ID3_EDITA1,EM_GETLINE,0,
		  (DWORD)(LPSTR)Cadena1)]='\0';
strcpy(Xpl.Resultado.Nombre,Cadena1);

Cadena1[(WORD)SendDlgItemMsg(ID3_EDITA2,EM_GETLINE,0,
											(DWORD)(LPSTR)Cadena1)]='\0';
Xpl.Resultado.d0=strtod(Cadena1,NULL);

SendMessage(HWindow,WM_NEXTDLGCTL,0,0);
}

// IDHELP: Llama archivo de ayuda.
void Dialogo3::IDHelp(RTMessage)
{
WinHelp(HWindow,__EXPLOCAL_HLP,HELP_CONTEXT,0x0009);
}


/*__________________________________________________________________________
	Declaración de las funciones de respuesta a los elementos del
	cuadro de diálogo:	DIALOGO4
   (Modificar lista de reactivos)
__________________________________________________________________________*/

// SETUPWINDOW:

void Dialogo4::SetupWindow()
{
char Cadena1[MAXCAD];   // Cadenas auxiliares.
char Cadena2[MAXCAD];
char DirecDatos[MAXPATH];     // Directorio de datos.
FILE *f;					   		// Archivo de datos.
sprintf(DirecDatos,"%s\\%s",VMarco_DBase->DirDatos_Marco,__REACTIVO_DAT);
// Desactiva el botón de eliminar
EnableWindow (GetDlgItem(HWindow,ID4_BTELIM),NO);
if (VMarco_DBase->TestArchDat(DirecDatos)) //Comprueba la presencia del archivo de datos.
		{ // Borra todos los cuadros de lista del cuadro de diálogo.
		SendDlgItemMsg(ID4_LISTA1,LB_RESETCONTENT,0,0L);
		SendDlgItemMsg(ID4_LISTA2,LB_RESETCONTENT,0,0L);
		f=fopen(DirecDatos,"r");
		while((fgets(Cadena1,MAXCAD,f))!=NULL)
			{
			fgets(Cadena2,MAXCAD,f);
			fgets(Cadena2,MAXCAD,f);
			Cadena1[(strlen(Cadena1))-1]='\0';
			SendDlgItemMsg(ID4_LISTA1,LB_ADDSTRING,0,(LONG)Cadena1);
			}    
  		fclose(f);
		}
//Termina el diálogo antes de mostrarlo.
else EndDialog(HWindow,IDCANCEL);
}  


/* LISTA1: Muestra los datos del compuesto de la lista 1 que tiene
	el rectángulo foco. */
void Dialogo4::Lista1(RTMessage Msg)
{
//Habilita el botón: Eliminar
EnableWindow(GetDlgItem(HWindow,ID4_BTELIM),SI);
Examinar(Msg,ID4_LISTA2,ID4_LISTA1);
}


// BTANAD: Muestra el Dialogo5, (Añadir reactivo).
void Dialogo4::BTAnad(RTMessage)
{
if(GetApplication()->
	ExecDialog(new Dialogo5(this,VMarco_DBase,"DIALOGO_5"))==IDOK)
	{
	SetupWindow();
   }
}


//BTELIM: Eliminar datos del reactivo, del archivo de datos.

void Dialogo4::BTElim(RTMessage)        
{
LRESULT i;              // Índice de la lista.
FILE *f;   					// Archivo de datos.
FILE *b;   					// Archivo 'Back-up'.
char Cadena1[MAXCAD];   // Cadenas auxiliares.
char Cadena2[MAXCAD];
char Texto[MAXTEX];
char DirecDatos1[MAXPATH];  // Directorio de datos *.DAT
char DirecDatos2[MAXPATH];  // Directorio de datos *.BAK

sprintf(DirecDatos1,"%s\\%s",VMarco_DBase->DirDatos_Marco,__REACTIVO_DAT);
sprintf(DirecDatos2,"%s\\%s",VMarco_DBase->DirDatos_Marco,__REACTIVO_BAK);

i=SendDlgItemMsg(ID4_LISTA1,LB_GETCURSEL,0,0L);
if(i!=LB_ERR)   // Si hay algún compuesto seleccionado...
	{
	SendDlgItemMsg(ID4_LISTA1,LB_GETTEXT,i,(LONG)Cadena1);
	LoadString(GetApplication()->hInstance,IDS_ESTECP,Cadena2,MAXCAD);
   sprintf(Texto,Cadena2,Cadena1);
	LoadString(GetApplication()->hInstance,IDS_ELIMIN,Cadena2,MAXCAD);
	if(BWCCMessageBox
		(HWindow,Texto,Cadena2,MB_YESNO|MB_ICONQUESTION)==IDYES)
		{
		b=fopen(DirecDatos1,"r");   // sólo lectura.
		strcpy(Cadena2,"\0");
		f=fopen(DirecDatos2,"w"); 	// escritura.
		while (!feof(b))  	// mientras no se llege al final del archivo...
			{ 
			if(strncmp(Cadena2,Cadena1,strlen(Cadena1)-1))
         	{
				fputs(Cadena2,f);
            }
			else
				{
				fgets(Cadena2,MAXCAD,b);
				fgets(Cadena2,MAXCAD,b);
				}
			fgets(Cadena2,MAXCAD,b);
			}
		fcloseall();
		remove(DirecDatos1);
		rename(DirecDatos2,DirecDatos1);
		SetupWindow();  // Inicializa las listas del diálogo.
		}
   }
}

// IDHELP: Llama archivo de ayuda.
void Dialogo4::IDHelp(RTMessage)
{
WinHelp(HWindow,__EXPLOCAL_HLP,HELP_CONTEXT,0x0010);
}

/*__________________________________________________________________________
							*** Dialogo5 ***
	Declaración de las funciones de respuesta a los elementos del cuadro
	de diálogo.
__________________________________________________________________________*/

// OK:  Respusta a la pulsación de la tecla aceptar del diálogo 5. 

void Dialogo5::Ok(RTMessage)
{
char Cadena1[MAXCAD];   // Cadenas auxiliares.
char Cadena2[MAXCAD];
char DirecDatos[MAXPATH];
FILE *f;					   // Archivo de datos.
sprintf(DirecDatos,"%s\\%s",VMarco_Dial5->DirDatos_Marco,__REACTIVO_DAT);

f=fopen(DirecDatos,"a+t"); // Abrir lectura y escritura.

Cadena1[(WORD)SendDlgItemMessage(HWindow,ID5_EDITA1,EM_GETLINE,0,
			(DWORD)(LPSTR)Cadena1)]='\0';
sprintf(Cadena2,"%s\r\n\0",Cadena1);
fputs(Cadena2,f);

Cadena1[(WORD)SendDlgItemMessage(HWindow,ID5_EDITA2,EM_GETLINE,0,
			(DWORD)(LPSTR)Cadena1)]='\0';
sprintf(Cadena2,"%s\r\n\0",Cadena1);
fputs(Cadena2,f);

Cadena1[(WORD)SendDlgItemMessage(HWindow,ID5_EDITA3,EM_GETLINE,0,
			(DWORD)(LPSTR)Cadena1)]='\0';
sprintf(Cadena2,"%s\r\n\0",Cadena1);
fputs(Cadena2,f);

EndDialog(HWindow,IDOK);
fclose(f);
}

/* BTINTR: Al Pulsar la tecla INTRO: El foco se mueve al siguiente elemento
	del DIALOGO5, es decir consigue que INTRO realice la misma misión que el
   tabulador. Esto se tiene en cuenta para facilitar la entrada de datos  */
void Dialogo5::BTIntr(RTMessage)
{
SendMessage(HWindow,WM_NEXTDLGCTL,0,0);
}

// IDHELP: Llama archivo de ayuda.
void Dialogo5::IDHelp(RTMessage)
{
WinHelp(HWindow,__EXPLOCAL_HLP,HELP_CONTEXT,0x0011);
}


/*__________________________________________________________________________
							*** Dialogo6 ***
	Declaración de las funciones de respuesta a los elementos del cuadro
	de diálogo.
__________________________________________________________________________*/


/* SETUPWINDOW:	Inicializa el diálogo activando el botón de radio,
	que corresponde a las unidades preferidas en la actualidad. */  
void Dialogo6::SetupWindow()
{
SendDlgItemMsg(ID6_EDITA1,EM_SETSEL,0,0L);
SendDlgItemMsg(ID6_EDITA1,EM_REPLACESEL,0
						,(LPARAM)((LPSTR)VMarco_Dial6->DirDatos_Marco));

switch(VMarco_Dial6->Unidades_Marco)
	{
	case EN_JULIOS:
		SendDlgItemMsg(ID6_BRJULS,BM_SETCHECK,SI,0L);
		break;
	case EN_CALORIAS:
		SendDlgItemMsg(ID6_BRCALS,BM_SETCHECK,SI,0L);
   	break;
	}
SendDlgItemMsg(ID6_BRAJUS,BM_SETCHECK,VMarco_Dial6->Ajuste_Marco,0L);
}

/* OK:  Termina el diálogo y cambia la preferencia de unidades según
	la selección de los botones de radio								*/
void Dialogo6::Ok(RTMessage)
{
char directorio[MAXPATH];

VMarco_Dial6->Unidades_Marco=
				(BOOL)SendDlgItemMsg(ID6_BRJULS,BM_GETCHECK,0,0L);
VMarco_Dial6->Ajuste_Marco=
				(BOOL)SendDlgItemMsg(ID6_BRAJUS,BM_GETCHECK,0,0L);

directorio[(WORD)SendDlgItemMessage(HWindow,ID6_EDITA1,EM_GETLINE,0,
			(DWORD)(LPSTR)directorio)]='\0';

strcpy(VMarco_Dial6->DirDatos_Marco,directorio);

// Finaliza el diálogo.
EndDialog(HWindow,IDOK); 		
}

// IDHELP: Llama archivo de ayuda.
void Dialogo6::IDHelp(RTMessage)
{
WinHelp(HWindow,__EXPLOCAL_HLP,HELP_CONTEXT,0x0012);
}


/*__________________________________________________________________________
						*** VentanaHijaMDI ***
				Declaración de las funciones miembro.		
__________________________________________________________________________*/


/* TOMARDATOS: Carga la ventana hija activa con los datos del explosivo
contenidos en la clase Explosivo. */

void VentanaHijaMDI::TomarDatos()
{
if (Tipo_Hija==TIPO_EXPLO)
	{
	NumReact_Hija=Xpl.NumReact;
	d0_Hija=Xpl.Resultado.d0;
	strcpy(NombreExp_Hija,Xpl.Resultado.Nombre);
	for(int i=0;i<Xpl.NumReact;i++)
   	{
		Reactivo_Hija[i].Porcentaje=Xpl.Reactivo[i].Porcentaje;
		strcpy(Reactivo_Hija[i].Nombre,Xpl.Reactivo[i].Nombre);
		}
	}
}


/* PONERDATOS: Carga la clase Explosivo con los datos del explosivo
 contenidos en la ventana hija activa*/
 
void VentanaHijaMDI::PonerDatos()
{
if (Tipo_Hija==TIPO_EXPLO)
	{
   Xpl.BorrarDatos();		//
	Xpl.NumReact=NumReact_Hija;
	Xpl.Resultado.d0=d0_Hija;
	strcpy(Xpl.Resultado.Nombre,NombreExp_Hija);
	for(int i=0;i<Xpl.NumReact;i++)
   	{
		Xpl.Reactivo[i].Porcentaje=Reactivo_Hija[i].Porcentaje;
		strcpy(Xpl.Reactivo[i].Nombre,Reactivo_Hija[i].Nombre);
		}
	}
}


/* PONERENEDITOR: Coloca un resultado en el editor, leyendo la cadena
  del formato en el  archivo de recursos, es una función polimórfica */

void VentanaHijaMDI::PonerEnEditor
				(int IDSCadena,double Valor,BOOL Unidades)
{
char Cadena1[MAXTEX];
char Cadena2[MAXTEX];

LoadString(GetApplication()->hInstance,IDSCadena,Cadena1,MAXTEX);
switch(Unidades)
	{
	case EN_JULIOS:
		sprintf(Cadena2,Cadena1,Valor*CalAJul(Unidades),"kJ");
		break;
	case EN_CALORIAS:
		sprintf(Cadena2,Cadena1,Valor,"kcal");
		break;
	case EN_DEFECTO:
		sprintf(Cadena2,Cadena1,Valor);
		break;
   }
Editor->Insert(Cadena2);
};

void VentanaHijaMDI::PonerEnEditor(int IDSCadena,char *Texto)
{
char Cadena1[MAXTEX];
char Cadena2[MAXTEX];
LoadString(GetApplication()->hInstance,IDSCadena,Cadena1,MAXTEX);
sprintf(Cadena2,Cadena1,Texto);
Editor->Insert(Cadena2);
};



// RESULTADOS: Carga los resultados del problema en el editor de textos. 
void VentanaHijaMDI::PonerResultados()
{
int i,j=1;
double NumAt,Qexplosion,Texplosion,Dcj;
BOOL Unidades=VMarco_Hija->Unidades_Marco;


switch (VMarco_Hija->Ajuste_Marco)
	{
	case SI:
		Qexplosion=Ajuste(Xpl.Resultado.Qexplosion,5.0);
		Texplosion=Ajuste(Xpl.Resultado.Texplosion,5.0);
		Dcj=Ajuste(Xpl.Resultado.Dcj,10.0);
		break;
	case NO:
		Qexplosion=Xpl.Resultado.Qexplosion;
		Texplosion=Xpl.Resultado.Texplosion;
		Dcj=Xpl.Resultado.Dcj;
		break;
   }


PonerEnEditor(IDS_PRCOMP);

for(i=0;i<Xpl.NumReact;i++)
	{	
	// Datos de los reactivos que forman la mezcla.
	PonerEnEditor(IDS_NUMERO,(double)(i+1));
	PonerEnEditor(IDS_RNOMBR,Xpl.Reactivo[i].Nombre);
	PonerEnEditor(IDS_RFORMU,Xpl.Reactivo[i].Formula);
	PonerEnEditor(IDS_RENERG,Xpl.Reactivo[i].Energia,Unidades); 	
	PonerEnEditor(IDS_RPSOMO,Xpl.Reactivo[i].PesoMol);
	PonerEnEditor(IDS_RPORCE,Xpl.Reactivo[i].Porcentaje);
	PonerEnEditor(IDS_RBALOX,Xpl.Reactivo[i].BO);
	}
// Características de la mezcla explosiva.
PonerEnEditor(IDS_CARACT);
PonerEnEditor(IDS_PRNOMB,Xpl.Resultado.Nombre);
PonerEnEditor(IDS_PRDENS,Xpl.Resultado.d0);
PonerEnEditor(IDS_PRENER,Xpl.Resultado.Eo,Unidades);  				

// Fórmula para 1kg de explosivo.
PonerEnEditor(IDS_PRFORM);
for(int k=0;k<MAX_ELEMENTOS;k++)
	{
	NumAt=Xpl.CalcNumAt(Xpl.Resultado.Formula_1kg,k);
	if(NumAt)
		{
		PonerEnEditor(IDS_PRFOR1,Xpl.TablaProd[k].Simbolo);
		PonerEnEditor(IDS_PRFOR2,NumAt);
		}
	}
// Balance de oxígeno.    if(fabs(respuesta)<=PRECISION_BO) respuesta=0.0;
PonerEnEditor(IDS_PRBALO,Xpl.Resultado.BO);
switch((fabs(Xpl.Resultado.BO)<=PRECISION_BO)? IGUAL
								:(Xpl.Resultado.BO>0.0)? MAYOR:MENOR)
	{ 
	case MENOR:
		// Xpl.Resultado deficitario en oxígeno.
		PonerEnEditor(IDS_PRDEFI);
		break;
	case IGUAL:
   	// Explosivo equilibrado.
		PonerEnEditor(IDS_PREQUI);
		break;
	case MAYOR:
   	// Explosivo excedentario.
		PonerEnEditor(IDS_PREXCE);
		break;
	}

// Productos de explosión.
if(!Xpl.TomarError(AVI_NO_PRO))
	{
	PonerEnEditor(IDS_PRODEX);
	for (i=0;i<MAX_PRODUCTOS;i++)
		{
		if (Xpl.TablaProd[i].Moles)
			{
			PonerEnEditor(IDS_NUMERO,(double)j++);
			PonerEnEditor(IDS_PRODUC,Xpl.TablaProd[i].Formula);
			PonerEnEditor(IDS_PMOLES,Xpl.TablaProd[i].Moles);
			PonerEnEditor(IDS_EFORMA,Xpl.TablaProd[i].Eformacion,Unidades); 
			}
		}
   }
// Resto de parámetros.
PonerEnEditor(IDS_REACIO);
PonerEnEditor(IDS_PRQEXP,Qexplosion,Unidades);
	
PonerEnEditor(IDS_PRTEXP,Texplosion);

if(!Xpl.TomarError(ERR_ENDOTE) && !Xpl.TomarError(AVI_NO_GAS))
	{
	PonerEnEditor(IDS_PRMOLE,Xpl.Resultado.Ng);
	PonerEnEditor(IDS_PRMASA,Xpl.Resultado.M);
	PonerEnEditor(IDS_PRVOLG,Xpl.Resultado.Vcn);
	PonerEnEditor(IDS_PRESPE,Xpl.Resultado.f,Unidades);

	PonerEnEditor(IDS_ESTIMA);
	PonerEnEditor(IDS_PRPRFI,Xpl.Resultado.Fi);
	PonerEnEditor(IDS_PRPRES,Xpl.Resultado.Pcj);
	PonerEnEditor(IDS_PRVELD,Dcj);
	PonerEnEditor(IDS_PRDECJ,Xpl.Resultado.dcj);
	PonerEnEditor(IDS_PRCOAD,Xpl.Resultado.CoefAdiabat);
   }

// Errores de los cálculos.
if(Xpl.TomarError(ERR_EX_UNO))
	{	
	PonerEnEditor(IDS_ERRRRR);
	for(i=1;i<MAX_ERROR;i++)
		{
		if(Xpl.Error[i].ind)
  		 	{
			Editor->Insert(Xpl.Error[i].des);
			}
		}
	}

// Coloca el cursor al principio del editor.
Editor->SetSelection(0,0);
}



// Modificación de los valores por defecto de la ventana.
void VentanaHijaMDI::GetWindowClass(WNDCLASS _FAR & ClaseHija)
{
TWindow::GetWindowClass(ClaseHija);
ClaseHija.hIcon=LoadIcon(GetApplication()->hInstance,"ICONO_HIJAS");
};


// SETUPWINDOW:  Inicializa el contenido de la ventana hija.
void VentanaHijaMDI::SetupWindow()
{
TEditWindow::SetupWindow();
PonerArchivoHija(Archivo_Hija);
char Cadena[MAXCAD];
char Texto[MAXTEX];
switch(Tipo_Hija)
	{
   case TIPO_EXPLO:
	  if (Archivo_Hija[0] != '\0')
  	  	{
		if (Leer(TIPO_EXPLO) )
			{
			Xpl.CalcResultado();// Calcular todas las carácterísticas del explosivo.
			PonerResultados();   // Envía los resultados al editor de texto.
			}
		else
			{
			// Error: El archivo no tiene datos de ningún explosivo.
			LoadString(GetApplication()->hInstance,IDS_ERROR1,Cadena,MAXCAD);
			LoadString(GetApplication()->hInstance,IDS_ERROR7,Texto,MAXTEX);
			BWCCMessageBox(HWindow,Texto,Cadena,MB_OK|MB_ICONEXCLAMATION);
			// Abrir como archivo de texto.
			Tipo_Hija=TIPO_TEXTO;
			if(!Leer(TIPO_TEXTO))
         	{
				PonerArchivoHija("");
            }
         }
      }
	  else
		{
		PonerArchivoHija("");
		Xpl.CalcResultado();// Calcular todas las carácterísticas del explosivo.
		PonerResultados();   // Envía los resultados al editor de texto.
		}     
	  break;

	case TIPO_TEXTO:
		if ( Archivo_Hija[0] != '\0' && !Leer(TIPO_TEXTO))
  			{	
			PonerArchivoHija("");		
			}
		break;
	default:
   	// Error en los datos de la ventana hija.
   	break;
   }

}


/* Contructor de VentanaHijaMDI:  Inicializa los datos miembros con
 los parámetros que se proporcionan y los valores por defecto */
VentanaHijaMDI::VentanaHijaMDI
		(PVentanaMarcoMDI Marco,
		 PTWindowsObject AParent,
		 LPSTR NombreHija,
		 LPSTR Archivo,
		 BOOL TipoHija):TEditWindow(AParent,NombreHija)
{
VMarco_Hija=Marco;
Tipo_Hija=TipoHija;
if(VMarco_Hija->CuentaHijas_Marco==1)
	{
	VMarco_Hija->CambiarMenu(MF_ACTIVO);
	}
VMarco_Hija->CuentaHijas_Marco++;

Archivo_Hija=_fstrdup(Archivo ? Archivo : "");

ArchNuevo_Hija=((Archivo_Hija[0]== '\0') ? SI:NO);
DatModif_Hija = ArchNuevo_Hija;

if(Tipo_Hija==TIPO_EXPLO)
	{
	TomarDatos(); 
   }

}


/* ~VENTANAHIJAMDI: Función destructora;
		 Libera la memoria ocupada por el archivo */
VentanaHijaMDI::~VentanaHijaMDI()
{
VMarco_Hija->CuentaHijas_Marco--;
if(VMarco_Hija->CuentaHijas_Marco==1)
	{
   // Si al destruir esta ventana hija ya no queda ninguna.
	VMarco_Hija->NumHija_Marco=1;
   // Desactivo el menú.
	VMarco_Hija->CambiarMenu(MF_INACTIVO);
   }

if ( Archivo_Hija )
	{
   // Libera la memoria ocupada por la cadena de texto.
	farfree(Archivo_Hija);
   }

}


// PRINTPAGE: Imprimir página.
void VentanaHijaMDI::PrintPage(HDC hPrtDC, char* Texto)
{
const MRG_HORZ_MM = 25;
const ALTO_LINEA_PX = 50;
const LINEAS_MAX = 60;
const TAB_PX = 100;
char *p1 = Texto;
char *p2 = Texto;
int ancho_mm=GetDeviceCaps(hPrtDC,HORZSIZE); // Medidas del dispositivo.
int ancho_px=GetDeviceCaps(hPrtDC,HORZRES);
int MRG_horz_px=MRG_HORZ_MM*(ancho_px/ancho_mm);
int aTab[2]={MRG_horz_px*2,MRG_horz_px*3};
int Numlineas = 0;

  while (*p2 != '\0') {
    if (*p2 != '\r')
      p2++;
	 else {
		TabbedTextOut(hPrtDC,MRG_horz_px, Numlineas++ * ALTO_LINEA_PX,
				p1,(int)(p2 - p1),sizeof(aTab)/sizeof(int),aTab,TAB_PX);
		if (Numlineas >= LINEAS_MAX) {
        // expulsar página
        Escape(hPrtDC, NEWFRAME, 0, NULL, NULL);
		  Numlineas = 0;
      }
      // reinicializar punteros
      p2 += 2;
      p1 = p2;
    }
  }

  if (p1 < p2) {
		if (Numlineas >= LINEAS_MAX) {
        // expulsar página
        Escape(hPrtDC, NEWFRAME, 0, NULL, NULL);
		  Numlineas = 0;
		}
		TabbedTextOut(hPrtDC, MRG_horz_px, Numlineas++ * ALTO_LINEA_PX,
				p1,(int)(p2 - p1),sizeof(aTab)/sizeof(int),aTab,TAB_PX);
  }
};


/* WRITE: Guarda el contenido del editor de texto en un archivo
 	previamente especificado. */

BOOL VentanaHijaMDI::Write()
{
  const BufferSize = 1024;
  long CharsToWrite, CharsWritten = 0;
  int BlockSize;
  Pchar ABuffer;
  char S[MAXPATH + 33];
  int NumLines;
  int AFile;

  AFile = _lcreat(Archivo_Hija, 0);
  if ( AFile == -1 )
  {
	 wsprintf(S, "No se puede escribir el archivo: '%s' en el disco."
				, Archivo_Hija);
	BWCCMessageBox(HWindow, S, GetModule()->Name, MB_ICONEXCLAMATION | MB_OK);
	 
	 return NO;
  }
  else
  {
    NumLines = Editor->GetNumLines();
    CharsToWrite = Editor->GetLineIndex(NumLines-1) +
                   Editor->GetLineLength(NumLines-1);
    ABuffer = new char[BufferSize+1 + 2*NumLines];
    if ( !ABuffer )
		return NO;
    else
    {
      while ( CharsWritten < CharsToWrite )
      {
        BlockSize = (int)Min((CharsToWrite - CharsWritten), BufferSize);
        Editor->GetSubText(ABuffer, (int)CharsWritten,
          (int)(CharsWritten + BlockSize));
        if (_lwrite(AFile, ABuffer, strlen(ABuffer)) == (WORD)-1)
        {
	    delete ABuffer;
            _lclose(AFile);
				return NO;
        }
        CharsWritten += BlockSize;
      }
		ArchNuevo_Hija = NO;
      Editor->ClearModify();
      delete ABuffer;
    }
    _lclose(AFile);
  }
  return SI;
}


/* READ: Lee el contenido de un archivo previamente especificado en el
editor de texto*/

BOOL VentanaHijaMDI::Read()
{ 
long CharsToRead;
UINT HEditorBuffer;
LPSTR EditorBuffer;
WORD EditorsDS;
char S[MAXPATH + 33];
int AFile;
BOOL Success = NO;

  AFile = _lopen(Archivo_Hija, OF_READ);
  if ( AFile != -1 )
  {
    CharsToRead = _llseek(AFile, 0L, 2);
    _llseek(AFile, 0L, 0);
    if ( CharsToRead < MaxInt && CharsToRead > 0 )
    {
      Editor->Clear();

      // attempt to reallocate Editor's buffer to the size of the file
      HEditorBuffer =(UINT) SendMessage(Editor->HWindow, EM_GETHANDLE, 0, 0L);
      EditorsDS = FP_SEG(GlobalLock((Editor->GetModule())->hInstance));
      if ( LocalReAllocDS((HANDLE)HEditorBuffer, (WORD)(CharsToRead+1),
                          LHND, EditorsDS) != NULL )
      {
        // read the file into EditorBuffer
	EditorBuffer = (LPSTR)LocalLockDS((HANDLE)HEditorBuffer, EditorsDS);
        if ( _lread(AFile, EditorBuffer, (WORD)CharsToRead) == CharsToRead )
        {
          // NULL terminate Editor's buffer
          EditorBuffer[(WORD)CharsToRead] = '\0';
	  LocalUnlockDS((HANDLE)HEditorBuffer, EditorsDS);

	  SendMessage(Editor->HWindow, EM_SETHANDLE, HEditorBuffer, 0L);
			 Success = SI;

			 ArchNuevo_Hija = NO;
          Editor->ClearModify();
          Editor->SetSelection(0,0);
        }
      }
      GlobalUnlock((Editor->GetModule())->hInstance);
    }
    _lclose(AFile);
  }
  if ( !Success )
  {
	wsprintf(S, "No se puede leer el archivo:\r\n '%s' del disco."
					, Archivo_Hija);
	BWCCMessageBox(HWindow, S, GetModule()->Name, MB_ICONEXCLAMATION | MB_OK);
  }
  return Success;
}



/*PONERARCHIVOHIJA: Cambia el archivo de la ventana y actualiza el título.*/

void VentanaHijaMDI::PonerArchivoHija(LPSTR Archivo)
{
  char TituloNuevo[81];
  LPSTR P[2];

  if ( Archivo_Hija != Archivo )
  {
	 farfree(Archivo_Hija);
	 Archivo_Hija = _fstrdup(Archivo ? Archivo : "");
  }
  P[1] = Title;              // Title apunta al título de la ventana.
  if ( Archivo_Hija[0] == '\0' )
    P[0] = "Sin título";
  else
	 P[0] = Archivo;
  if ( Title == NULL || Title[0] == '\0' )
    SetWindowText(HWindow, P[0]);
  else
  {
	 wvsprintf(TituloNuevo, "%s:%s", (LPSTR)P);
    SetWindowText(HWindow, TituloNuevo);
  }
}


/* REPLACEWITH: Reeplaza el archivo actual con el que se proporciona. */
void VentanaHijaMDI::ReplaceWith(LPSTR Archivo)
{
  char Nombre_viejo[MAXPATH];

  _fstrcpy(Nombre_viejo, Archivo_Hija);
  PonerArchivoHija(Archivo);
  if ( Leer(Tipo_Hija) )
  	 {
	 InvalidateRect(Editor->HWindow, NULL, NO);
  	 }
  else
  	{
	PonerArchivoHija(Nombre_viejo);
	}
}



/* LEER: Lee los datos del problema de un archivo previamente especificado */
BOOL VentanaHijaMDI::Leer(BOOL TipoDatos)
{
BOOL Respuesta=NO;
char Cadena[MAXCAD];
FILE *f;   // Archivo de explosivo

switch(TipoDatos)
	{
	case TIPO_EXPLO:
		if(VMarco_Hija->TestArchExplo(Archivo_Hija)==TIPO_EXPLO)
      	{
			if((f=fopen(Archivo_Hija,"r"))!=NULL)
				{
			
				// Lee los datos del problema en el archivo Archivo_Hija.
				fgets(Cadena,MAXCAD,f);    // No tiene en cuenta el cabecero.
				fgets(Cadena,MAXCAD,f);
				NumReact_Hija=(int)strtod(Cadena,NULL);
				fgets(Cadena,MAXCAD,f);            
				Cadena[(strlen(Cadena))-1]='\0';  // Elimina el carácter intro.
				strcpy(NombreExp_Hija,Cadena);
				fgets(Cadena,MAXCAD,f);
				d0_Hija=strtod(Cadena,NULL);
				for(int i=0;i<NumReact_Hija;i++)
					{
					fgets(Cadena,MAXCAD,f);
					Cadena[(strlen(Cadena))-1]='\0';  // Elimina el carácter intro.
					strcpy(Reactivo_Hija[i].Nombre,Cadena);
					fgets(Cadena,MAXCAD,f);
					Reactivo_Hija[i].Porcentaje=strtod(Cadena,NULL);
					}
				PonerDatos();
				Respuesta=SI;
				}
			fclose(f);
			}
		else
			{
			Respuesta=NO;
         }
		break;

	case TIPO_TEXTO:
		Respuesta=Read();
		break;

	default:
		// No hacer nada.
      break;
	} 
return Respuesta;
}


/* SAVE: Graba en disco el contenido del editor de texto o los datos del
explosivo.*/

BOOL VentanaHijaMDI::Save(BOOL TipoDatos)
{
if (Modificacion())
	{
	if(ArchNuevo_Hija)
  		{
		return SaveAs(TipoDatos);
   	}
   else
    {
	 if ( Grabar(TipoDatos) )
    	{
		return SI;
      }
	 else
    	{
		return NO;
      }
    }
  }
else
	{
  //El contenido del Editor no ha cambiado No es necesario cambiar
	 return SI;
    }
}


/* SAVEAS:	Graba los contenidos de control TEdit (editor de texto) en un
	archivo cuyo nombre es proporcionado por el usuario, mediante un diálogo
   de grabar archivo. Devuelve SI si se pudo grabar el archivo. */

BOOL VentanaHijaMDI::SaveAs(BOOL TipoDatos)
{
  char Nombre_Tmp[MAXPATH];
  char Nombre_viejo[MAXPATH];
  OFSTRUCT TmpOfStruct;
  char S[MAXPATH+20];

  _fstrcpy(Nombre_viejo, Archivo_Hija);
  _fstrcpy(Nombre_Tmp, Archivo_Hija);
 // if (Nombre_Tmp[0] == '\0')    
  	{
	switch(TipoDatos)
		{
		case TIPO_EXPLO:
		_fstrcpy(Nombre_Tmp, "*.XPL");
		break;

		case TIPO_TEXTO:
		_fstrcpy(Nombre_Tmp, "*.TXT");
      break;
      }
   }                                               
  if ( GetModule()->ExecDialog( new TFileDialog
				(this, SD_FILESAVE, Nombre_Tmp , GetModule())) == IDOK )
  {
	 if ( OpenFile(Nombre_Tmp, &TmpOfStruct, OF_EXIST) != -1 )
    {
		wsprintf(S, "¿Reemplazar el actual: '%s'?", (LPSTR)Nombre_Tmp);
		if(BWCCMessageBox(HWindow, S, "Cambio en el archivo:",
								  MB_YESNO | MB_ICONQUESTION) == IDNO)
			
      {
		  PonerArchivoHija(Nombre_viejo);
		  return NO;
      }
    }
	 PonerArchivoHija(Nombre_Tmp);
	 if ( Grabar(TipoDatos) )
		return SI;
    else
    {
      PonerArchivoHija(Nombre_viejo);
		return NO;
    }
  }
  return NO;
}


/* GRABAR: Escribe el contenido del editor en un archivo predeterminado */

BOOL VentanaHijaMDI::Grabar(BOOL TipoDatos)
{
BOOL Respuesta=NO;
switch(TipoDatos)
	{
	case TIPO_EXPLO:
		// Archivo de explosivo.
		FILE *f; 
		DatModif_Hija=NO;
		if((f=fopen(Archivo_Hija,"w"))!=NULL && Xpl.NumReact)
			{
			// Graba los datos del problema en el archivo Archivo_Hija.
         fprintf(f,"%s\n",ARCH_CABECERO);
			fprintf(f,"%i\n",NumReact_Hija);
			fprintf(f,"%s\n",NombreExp_Hija);
			fprintf(f,"%.3f\n",d0_Hija);
         // Graba los datos de los reactivos.
			for(int i=0;i<NumReact_Hija;i++)
				{
				fprintf(f,"%s\n",Reactivo_Hija[i].Nombre);
				fprintf(f,"%.3f\n",Reactivo_Hija[i].Porcentaje);
				}
			fclose(f);			
			Respuesta=SI;
         }
		break;

	case TIPO_TEXTO:
   	// Archivo de Texto.
		Respuesta=Write();
		break;

	default:
		// Error en los datos de la ventana hija.
      Respuesta=NO;
   	break;
	}
return Respuesta;
}


// MODIFICACION:	¿Se ha modificado el contenido de la ventana? (SI/NO)
BOOL VentanaHijaMDI::Modificacion()
{
BOOL Respuesta;
switch(Tipo_Hija)
	{
	case TIPO_EXPLO:
		Respuesta=DatModif_Hija;
		break;
	case TIPO_TEXTO:
		Respuesta=Editor->IsModified();
		break;
	default:
   	// Error en los datos de la ventana hija.
		Respuesta=NO;
		break;
   }
return Respuesta;
}

/* CANCLEAR: Devuelve un valor tipo BOOL indicando si se puede borrar
	el control TEdit. Devuelve SI si el texto no ha cambiado o el usuario
   está conforme con borrar el texto.*/

BOOL VentanaHijaMDI::CanClear()   
{
char S[MAXPATH+28];
int Respuesta;

if (Modificacion())
	{
	if ( !Archivo_Hija || (Archivo_Hija[0]=='\0'))
		_fstrcpy(S,"El archivo: 'Sin título' ha cambiado\n ¿Desea grabarlo?");
    else
		wsprintf(S, "El archivo: '%s' ha cambiado\n ¿Desea grabarlo?"
		,Archivo_Hija);
		
		Respuesta = BWCCMessageBox(HWindow, S, "Cambio en el archivo:",
								  MB_YESNOCANCEL | MB_ICONQUESTION);
	
	 if ( Respuesta == IDYES )
    	{
		return Save(Tipo_Hija);
      }
	 else
    	{
		return Respuesta != IDCANCEL;
      }
  }
return SI;
}

/* CANCLOSE: Devuelve un valor tipo BOOL, indicando se se puede  o no
	cerrar el texto de TEdit. El resultado es una llamada a CanClear. */
BOOL VentanaHijaMDI::CanClose()
{
return CanClear();
}


/*__________________________________________________________________________
						*** VentanaHijaMDI ***
Declaración de las funciones miembro que Procesan de los mensajes del menú
__________________________________________________________________________*/


// CMFILESAVE: Grabar archivo de texto.
void VentanaHijaMDI::CMFileSave(RTMessage)
{
Save(TIPO_TEXTO);
}


// CMFILESAVEAS: Grabar archivo de texto como.
void VentanaHijaMDI::CMFileSaveAs(RTMessage)
{
SaveAs(TIPO_TEXTO);
}


//CMGUARDEXP:	Guardar explosivo.
void VentanaHijaMDI::CMGuardExp(RTMessage)
{
Save(TIPO_EXPLO);
}


//CMGCOMOEXP: Grabar explosivo como.	
void VentanaHijaMDI::CMGComoExp(RTMessage)
{
SaveAs(TIPO_EXPLO);
}


// CMEDITSEL:	Selecciona todo el texto de la ventana.

void VentanaHijaMDI::CMEditSelT(RTMessage)
{
Editor->SetSelection(0,Editor->GetTextLen()+1);
}


// CMComposic: Modificación de los datos del explosivo.

void VentanaHijaMDI::CMComposic(RTMessage)
{
if(Tipo_Hija==TIPO_EXPLO)
	{
	PonerDatos();

	if((GetApplication()->ExecDialog(new Dialogo2(this,"DIALOGO_2")))==IDOK);
		{
		DatModif_Hija=SI;
	   Editor->Clear();
      Xpl.CalcResultado();
		PonerResultados();
   	TomarDatos();
		}
	}
}

// CMRECALCUL: Recalcula el explosivo.
void VentanaHijaMDI::CMRecalcul(RTMessage)
{
if(Tipo_Hija==TIPO_EXPLO)
	{
	PonerDatos();
	DatModif_Hija=NO;
	Editor->Clear();
	Xpl.CalcResultado();
	PonerResultados();
   TomarDatos();
	}
}


// CMFILEPRINT: Imprime el contenido de la ventana hija activa.

void VentanaHijaMDI::CMFilePrint(RTMessage)
{
char PrtMsg[] = "Explocal:";   				// Mensaje de impresión
HDC hPrtDC=VMarco_Hija->GetPrtDC(); 		// tomar el handle de la impresora.
int textLen = Editor->GetTextLen() + 1;   // Imprime todo el texto.
char *textStr = new char[textLen];

// tomar el texto.
Editor->GetText(textStr, textLen); 
  if (hPrtDC)
	{
	// comenzar el proceso de impresión
   if (Escape(hPrtDC, STARTDOC, sizeof(PrtMsg)-1, PrtMsg, NULL) > 0)
    	{
		// imprimir las páginas
      PrintPage(hPrtDC, textStr);
		if (Escape(hPrtDC, NEWFRAME, 0, NULL, NULL) > 0)
      	{
			Escape(hPrtDC, ENDDOC, 0, NULL,NULL);
         }
    	}
  	}
  	// borrar los handles de la impresora y de la cadena dinámica
  	DeleteDC(hPrtDC);
  	delete textStr;
}


/*__________________________________________________________________________
						*** VentanaMarcoMDI ***
				Declaración de las funciones miembro.		
__________________________________________________________________________*/

/* VENTANAMARCOMDI: Constructor de la clase, inicializa los atributos de
la ventana y carga el menú desde los recursos.*/

void VentanaMarcoMDI::VentanaMarcoMDI
	(LPSTR Titulo,LPSTR NombreMenu,LPSTR ArgLinea)
							:TMDIFrame(Titulo,NombreMenu)
{
NumHija_Marco=CuentaHijas_Marco=1;     // Inicialización de contadores;
ArgLinea_Marco=ArgLinea;               // Pasa los argumentos de línea.
//Unidades_Marco=EN_JULIOS;              // Todo a Julios.
//Ajuste_Marco=SI;
													// Características de la ventana.
Attr.Style|=WS_MAXIMIZE;
Attr.X=ORGX_VENTANA; 				  		// Coordenada (X) del origen.
Attr.Y=ORGY_VENTANA;                   // Coordenada (Y) del origen.
Attr.W=GetSystemMetrics(SM_CXSCREEN);  // Ancho de la pantalla.
Attr.H=GetSystemMetrics(SM_CYSCREEN);  // Alto de la pantalla.
AssignMenu("MENU_1"); 						// Inclusión del menú.
};

/* VENTANAMARCOMDI: Destructor de la clase, graba las preferencias
en el archivo de inicialización: EXPLOCAL.INI*/

VentanaMarcoMDI::~VentanaMarcoMDI()
{
char Unidad[2][MAXCAD]={ CAD_JULIOS, CAD_CALORS };
char Ajuste[2][MAXCAD]={ CAD_NO,CAD_SI };
int i,j;

i=((Unidades_Marco==EN_JULIOS) ? 0 : 1 );
j=((Ajuste_Marco==NO) ? 0 : 1);

WritePrivateProfileString
	("datos","unidad",Unidad[i],__EXPLOCAL_INI);

WritePrivateProfileString
	("datos","ajustes",Ajuste[j],__EXPLOCAL_INI);

WritePrivateProfileString
	("datos","directorio",DirDatos_Marco,__EXPLOCAL_INI);
}


/* GETWINDOWCLASS:  Modificación de los valores por defecto de la ventana.
Asigna el icono de la aplicación a la ventana y cambia el fondo a gris.*/

void VentanaMarcoMDI::GetWindowClass(WNDCLASS _FAR & ClaseMarco)
{
TMDIFrame::GetWindowClass(ClaseMarco);
ClaseMarco.hIcon=LoadIcon(GetApplication()->hInstance,"ICONO_APLIC");
ClaseMarco.hbrBackground=(HBRUSH)GetStockObject(LTGRAY_BRUSH);
};



/* TESTDATARCH	Función que comprueba si el archivo de datos:
	REACTIVO.DAT es accesible.*/
BOOL VentanaMarcoMDI::TestArchDat(char *Archivo)
{
char Cadena1[MAXCAD];
char Texto[MAXTEX];
BOOL Respuesta=SI;
if(access(Archivo,0))
	{
	// Error: No se encuentra el archivo de datos.
	LoadString(GetApplication()->hInstance,IDS_ERROR1,Cadena1,MAXCAD);
	LoadString(GetApplication()->hInstance,IDS_ERROR2,Texto,MAXTEX);
	BWCCMessageBox(HWindow,Texto,Cadena1,MB_OK|MB_ICONEXCLAMATION);
	Respuesta=NO;
	}
return Respuesta;
}

/* TESTARCHEXPLO: Comprueba si el archivo contiene datos de un explosivo.*/
BOOL VentanaMarcoMDI::TestArchExplo(char *Archivo)
{
FILE *f;
BOOL Respuesta;
char Cadena[MAXCAD];

if((f=fopen(Archivo,"r"))!=NULL)
	{
   // Toma el Cabecero.
	fgets(Cadena,MAXCAD,f);
	Respuesta=
		((strncmp(Cadena,ARCH_CABECERO,
		 strlen(ARCH_CABECERO)-1)) ? TIPO_TEXTO : TIPO_EXPLO);
	}
else
	{
	Respuesta=TIPO_INACCESIBLE;
 	}
return Respuesta;
} 


/* CAMBIARMENU:	Cambio del estado de los elementos del menu*/
void VentanaMarcoMDI::CambiarMenu(UINT Tipo_Menu)
{
HMENU hMenu=GetMenu(HWindow); 

if((Tipo_Menu==MF_ACTIVO) || (Tipo_Menu==MF_INACTIVO))
	{
	//	Archivo.
	EnableMenuItem(hMenu,CM_FILESAVE,Tipo_Menu);
	EnableMenuItem(hMenu,CM_FILESAVEAS,Tipo_Menu);
	EnableMenuItem(hMenu,CM_IMPRIMIR,Tipo_Menu);
	// Edición.
	EnableMenuItem(hMenu,CM_EDITUNDO,Tipo_Menu);
	EnableMenuItem(hMenu,CM_EDITCUT,Tipo_Menu);
	EnableMenuItem(hMenu,CM_EDITCOPY,Tipo_Menu);
	EnableMenuItem(hMenu,CM_EDITPASTE,Tipo_Menu);
	EnableMenuItem(hMenu,CM_EDITDELETE,Tipo_Menu);
	EnableMenuItem(hMenu,CM_EDITCLEAR,Tipo_Menu);
	EnableMenuItem(hMenu,CM_EDITSELT,Tipo_Menu);
	//Buscar
	EnableMenuItem(hMenu,CM_EDITFIND,Tipo_Menu);
	EnableMenuItem(hMenu,CM_EDITFINDNEXT,Tipo_Menu);
	EnableMenuItem(hMenu,CM_EDITREPLACE,Tipo_Menu);
	// Explosivo.
	EnableMenuItem(hMenu,CM_GUARDEXP,Tipo_Menu);
	EnableMenuItem(hMenu,CM_GCOMOEXP,Tipo_Menu);
	EnableMenuItem(hMenu,CM_COMPOSIC,Tipo_Menu);
	EnableMenuItem(hMenu,CM_RECALCUL,Tipo_Menu);
	// Ventana.
	EnableMenuItem(hMenu,CM_TILECHILDREN,Tipo_Menu);
	EnableMenuItem(hMenu,CM_CASCADECHILDREN,Tipo_Menu);
	EnableMenuItem(hMenu,CM_ARRANGEICONS,Tipo_Menu);
	EnableMenuItem(hMenu,CM_CLOSECHILDREN,Tipo_Menu);
	}
}


/* GETPRTDC:	Obtiene el contexto de dispositivo de la impresora.*/

HDC VentanaMarcoMDI::GetPrtDC()
{
  const int PrinterDataSize = 80;
  char PrinterData[PrinterDataSize];
  char *DevicePtr, *DriverPtr, *OutputPtr;

  // Accede a los datos sobre la impresora del archivo WIN.INI
  GetProfileString("windows","device",",,,",PrinterData,PrinterDataSize);
  DevicePtr = strtok(PrinterData, ",");
  DriverPtr = strtok(NULL, ",");
  OutputPtr = strtok(NULL, ",");
  if (DevicePtr && DriverPtr && OutputPtr)
  	{
	return CreateDC(DriverPtr, DevicePtr, OutputPtr, NULL);
   }
  else
  	{
	return NULL;
   }
}

/* CREARHIJA:	Construye un ventana hija MDI y le añade el título */

void VentanaMarcoMDI::CrearHija(LPSTR Archivo,BOOL TipoHija)
{
char NombreHija[MAX_NOMBRE_HIJA];
wsprintf(NombreHija,"%d",NumHija_Marco++);
GetApplication()->MakeWindow(new
		VentanaHijaMDI(this,this,NombreHija,Archivo,TipoHija));
}


/* SETUPWINDOW: Inicializa la ventana marco,  cargando las tablas de
					datos. Muestra en  pantalla el logotipo en mapa de bits  */

void VentanaMarcoMDI::SetupWindow()
{
int dib_x,dib_y;
char Cadena[MAXCAD];
HDC hdc, hdcMemory;
HBITMAP hbmpMyBitmap, hbmpOld;
BITMAP bm;
HCURSOR hCursor;

// Cambia a cursor de reloj de arena y guarda en cursor anterior.
hCursor = SetCursor(LoadCursor(NULL, IDC_WAIT));

hbmpMyBitmap = LoadBitmap(GetApplication()->hInstance, "MAPABITS_1");
GetObject(hbmpMyBitmap, sizeof(BITMAP), &bm);

hdc = GetDC(NULL);
hdcMemory = CreateCompatibleDC(hdc);
hbmpOld =(HBITMAP)SelectObject(hdcMemory, hbmpMyBitmap);

// Coloca el dibujo de presentación en mitad de la pantalla.
dib_x=(GetSystemMetrics(SM_CXSCREEN)-bm.bmWidth)/2;
dib_y=(GetSystemMetrics(SM_CYSCREEN)-bm.bmHeight)/2;
  
BitBlt(hdc,dib_x, dib_y, bm.bmWidth, bm.bmHeight, hdcMemory, 0, 0, SRCCOPY);
SelectObject(hdcMemory, hbmpOld);

// Libera el contexto de dispositivo asociado al gráfico.
DeleteDC(hdcMemory);
ReleaseDC(NULL, hdc);

TMDIFrame::SetupWindow();

// Carga la configuración, almacenada en EXPLOCAL.INI
GetPrivateProfileString
	("datos","unidad",CAD_JULIOS,Cadena,MAXCAD,__EXPLOCAL_INI);

if(strncmp(CAD_JULIOS,Cadena,strlen(Cadena)-1))
	{
	Unidades_Marco=EN_CALORIAS;
	}
else
	{
	Unidades_Marco=EN_JULIOS;
	} 

GetPrivateProfileString
	("datos","ajustes",CAD_SI,Cadena,MAXCAD,__EXPLOCAL_INI);

if(strncmp(CAD_NO,Cadena,strlen(Cadena)-1))
	{
	Ajuste_Marco=SI;   
	}
else
	{
	Ajuste_Marco=NO;
	}

GetPrivateProfileString
			("datos","directorio","c:\explocal",DirDatos_Marco,
				MAXPATH,__EXPLOCAL_INI);


// Carga las tablas de datos.
Xpl.CargarTablaProd(DirDatos_Marco);
Xpl.CargarTablaK12(DirDatos_Marco);

switch(TestArchExplo(ArgLinea_Marco))
	{
	case TIPO_EXPLO:
		CrearHija(ArgLinea_Marco,TIPO_EXPLO);
		break;
	case TIPO_TEXTO:
		CrearHija(ArgLinea_Marco,TIPO_TEXTO);
      break;
	default:
   	// No hacer nada.
		break;
	}
// Restaura el anterior cursor.
SetCursor(hCursor);
}


/*__________________________________________________________________________
						*** VentanaMarcoMDI ***  (MENÚ)
Declaración de las funciones miembro que procesan los mensajes del menú
__________________________________________________________________________*/

/* CMNEWFILE:	(Archivo/Nuevo): Nuevo archivo en modo texto */

void VentanaMarcoMDI::CMNewFile(RTMessage)
{
// Ventana hija sin nombre y con texto.
CrearHija("",TIPO_TEXTO);
}


/* CMOPENFILE:	(Archivo/Abrir): Abre un archivo en modo texto. */

void VentanaMarcoMDI::CMOpenFile(RTMessage)
{
char Archivo[MAXPATH];
if (GetApplication()->ExecDialog(new
			TFileDialog(this,SD_FILEOPEN,_fstrcpy(Archivo, "*.*")))==IDOK)
	{
	CrearHija(Archivo,TIPO_TEXTO); 
	}
}



/* CMNUEVOEXP: (Explosivo/Nuevo): contruye una ventana hija con el resultado
	del problema cuyos datos se especifican por teclado*/

void VentanaMarcoMDI::CMNuevoExp(RTMessage)
{
if(GetApplication()->ExecDialog
	(new Dialogo1(this,this,"DIALOGO_1"))==IDOK)
	{
	CrearHija("",TIPO_EXPLO);
	}
}


/* CMABRIREXP:	(Explosivo/Abrir) contruye una ventana hija con el resultado
	del problema cuyos datos se especifican, mediante archivo*/

void VentanaMarcoMDI::CMAbrirExp(RTMessage)
{
char Archivo[MAXPATH];

if(GetApplication()->ExecDialog
	(new TFileDialog(this,SD_FILEOPEN,_fstrcpy(Archivo, "*.XPL")))==IDOK)
 	{
	CrearHija(Archivo,TIPO_EXPLO);
	}
}


/* CMMODIFICAR:(Explosivo/Modificar...):	Modifica la lista de reactivos */

void VentanaMarcoMDI::CMModifica(RTMessage)
{
GetApplication()->ExecDialog(new Dialogo4(this,this,"DIALOGO_4"));
}


/*CMINDICEAY:(Ayuda/Índice) Muestra el contenido del archivo de ayuda
   EXPLOCAL.HLP */

void VentanaMarcoMDI::CMIndiceAy(RTMessage)
{
WinHelp(HWindow,__EXPLOCAL_HLP,HELP_CONTENTS,0L);
}


/* CMACERCADE:(Ayuda/Acerca de...): Muestra un cuadro de diálogo con
	información sobre el programa. */

void VentanaMarcoMDI::CMAcercaDe(RTMessage)
{
GetApplication()->ExecDialog(new TDialog(this,"DIALOGO_ACERCA"));
}

/* CMPREFEREN:(Archivo/Preferencias...) Preferencias sobre las unidades
	de los resultados. */
void VentanaMarcoMDI::CMPreferen(RTMessage)
{
GetApplication()->ExecDialog(new Dialogo6(this,this,"DIALOGO_6"));
}

/*_________________________________________________________________________
       		*** AplicacionMDI ***
	
	La ventana principal de AplicacionMDI es del tipo VentanaMarcoMDI.
_________________________________________________________________________

/* INITMAINWINDOW: 	Crea una instancia de la aplicación.
	Coloca la posición del acceso por menu a la ventana hija en
  	la posición que ocupa 'Ventana' en el menú.  				*/
void AplicacionMDI::InitMainWindow()
{
MainWindow = new VentanaMarcoMDI(Name,NULL,ArgLinea_Aplic);


((PVentanaMarcoMDI)MainWindow)->ChildMenuPos = VENTANA_MENU_POS;
}


/* INITINSTANCE:	Inicializa cada instancia de la aplicación Windows
	cargado la tabla de teclas aceleradoras asociadas al menú 		 */

void AplicacionMDI::InitInstance()
{
TApplication::InitInstance();

if (!Status)
	{
   /* Si la inicialización ha tenido éxito...
      Carga las teclas aceleradoras.       */
	HAccTable = LoadAccelerators(hInstance, "MENU_1");
   }
}


/*__________________________________________________________________________
WINMAIN:    		*** Función principal ***
				Punto de entrada del programa para Windows.
				Se debe cargar en memoria la librería BWCC.DLL para los
				controles tipo Borland.
__________________________________________________________________________*/


int PASCAL WinMain(HINSTANCE hEstaInst, HINSTANCE hPreviaInst,
			LPSTR ArgLinea, int CmdPres)
{
char Cadena[MAXCAD]; 			// Buffers para recursos cadenas.
HINSTANCE hControles; 			// Instancia a la librería

/* Carga la librería de enlace dinámico: Controles para Windows
   al estilo Borland: (Borland Windows Custom Controls). */
hControles=LoadLibrary("BWCC.DLL");

LoadString(hEstaInst,IDS_TITULO,Cadena,MAXCAD);
AplicacionMDI MDIApl(Cadena, hEstaInst, hPreviaInst,ArgLinea, CmdPres);
MDIApl.Run();

/*	Libera la memoria ocupada por la libería de los controles en caso de
	  que se hubiera podido cargar.*/
if((int)hControles>32) FreeLibrary(hControles);

return MDIApl.Status;
}

/*_____________________________________________________________________
  Fin del Archivo:   	  EXPLOCAL.CPP
_____________________________________________________________________*/
