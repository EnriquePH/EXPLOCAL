/*-------------------------------------------------------------------------
	Archivo:   		CALCULOS.H
	Proyecto:  		EXPLOCAL.PRJ
	Autor:     		Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
				  		Proyecto de Fin de Carrera.
	Especialidad: 	Laboreo y Explosivos.
	Descripción:   Archivo de cabecera que declara todas las funciones
						macros y estructuras de datos necesarias para calcu-
						lar las características de un explosivo.
	Compilador:		Borland C++ 3.1 con la librería Object Windows 1.0
-------------------------------------------------------------------------*/



/*-------------------------------------------------------------------------
								Archivos de cabecera.
-------------------------------------------------------------------------*/

#define __CALCULOS_H  		// Macro de identificación de la librería.

#include <math.h> 			// Librería de las funciones matemáticas.
#include <ctype.h>         //
#include <stdio.h>         // Entrada y salida.
#include <string.h>        // Manipulación de cadenas.
#include <stdlib.h>        //

#ifndef __IO_H
#include <io.h>     			// Función access
#endif

#if !defined (__WINDOWS_H)
#include "windows.h"      // Inclusión funciones windows.
#endif


#if !defined (__EXPLOCAL_H)
#include "explocal.h"      // Inclusión de los macros.
#endif

/*-------------------------------------------------------------------------
						Declaración de las MACROS.
-------------------------------------------------------------------------*/


#define MAX_ERROR 17

#define ERR_EX_UNO 0   // Hay errores

#define AVI_NOMBRE 1   // No se ha introducido el nombre del explosivo.
#define AVI_DENSID 2   // Densidad de encartuchado errónea. Usando por def.
#define AVI_NO_GAS 3   // No hay productos gaseosos.
#define AVI_NO_PRO 4   // No hay productos de explosión.
#define AVI_MAXITE 5   // Número de iteraciones máximo rebasado.

#define ERR_REACTI 6   // No se encuentran los datos de algún reactivo.
#define ERR_DT_TAB 7   // No se ha cargado los datos de los productos.
#define ERR_DT_CON 8	  // No se ha cargado los datos de la ctes. de equil.
#define ERR_DT_REA 9   // No se ha cargado los datos de los reactivos.
#define ERR_ENDOTE 10  // Reacción endotérmica. No hay detonación.

#define ERR_SOLUCI 11  // Sistema sin solución.
#define ERR_GRADOS 12  // Error grado del sistema.
#define ERR_IND_EXC 13 // Índice de la tablas de datos fuera de rango.
#define ERR_IND_DEF 14 // Índice de la tablas de datos fuera de rango.
#define ERR_DIV_CER 15 // División por cero.
#define ERR_ERROR_ 16  // Error en la lista de errores.


#define MAX_REACTIVOS 15  //Número máximo de reactivos en una mezcla exp.

#define MAX_ELEMENTOS 32  /* Número máximo de elementos químicos distintos
								     que forman parte de una mezcla explosiva.  */
#define MAX_PRODUCTOS 35  /* Número máximo de productos de
								     explosión distintos. */

/* Características de la tabla de entalpías de los productos de explosión.*/
#define TBL_DATOS 58  // Número de datos de la tabla de entalpías.


// Posiciones de los elementos fundamentales en la Estructura ELEMENTO.
#define CARBONO     5  	 
#define HIDROGENO  12  	 
#define OXIGENO    22

// Posiciones de los productos fundamentales en la estructura ELEMENTO.
#define      AGUA_H 12           
#define   GRAFITO_C 32
#define  MONOXIDO_C 33
#define   DIOXIDO_C 5
#define DIATOMICO_H 34
   	 
// Aproximación de la temperatura de explosión

   
#define PRECISION_T 10.0      // Detención del proceso iterativo. 10 K
#define MAX_ITER 20            // Número máximo de iteraciones.

#define DENS_DEFECT 1.2   // Densidad de encartuchado por defecto [g/cm3].
#define DENS_MAXIMA 7.0   // Densidad de encartuchado máxima [g/cm3].
#define DENS_MINIMA 0.1	  // Densidad de encartuchado mínima [g/cm3].


// Tabla...

#define MAX_TEMP  6000.00   // Máxima temperatura [K].
#define MIN_TEMP   300.00   // Mínima temperatura [K].
#define INC_TEMP   100.00   // Incremento de temperatura [K].
#define INI_TEMP	3000.00	 // Tª inicio de iteraciones.


// Mácros de control.

#define CON_GRAFITO SI
#define SIN_GRAFITO NO

#define EN_JULIOS SI  
#define EN_CALORIAS NO
#define EN_DEFECTO -1

#define MENOR 1
#define MAYOR 2
#define IGUAL 3

// Macros necesarias en los cálculos.

#define EC_CUBICA 3       // Tercer grado.

// Debe ser cualquier valor negativo (sin significado físico.)
#define RAIZ_IMAGINARIA -1.0 

#define NRM_TEMP 298   	  	 // Tª del estado normal.   [K]
#define A_DCJ 22.33 	 		 // Ctes K. Jacobs
#define B_DCJ 1.30        	 // [g/cm³]
#define K_PCJ 0.0007617
#define S_dCJ 1.470
#define T_dCJ 0.05625

#define R_KCAL 0.0019872   // Cte. de los gases ideales en [kcal/(K·mol)]
#define R_JUL 8.31441	    // Cte. de los gases ideales en [J/(K·mol)]
#define GCM3_A_KGM3 1000    // 1 g/cm³ = 1000 kg/m³
#define CAL_A_JUL 4.184     // 1 cal = 4.184 J
#define JUL_A_CAL 0.2390057 // 1 J = 0.239 cal
#define K_VCN 0.0224138     // [m³/mol]
#define PRECISION_BO 0.01

#define Q_A_TEMP 2.0			 // Aumento de la Tª en el proceso iterativo.

 
/*-------------------------------------------------------------------------
   		          DECLARACIÓN DE FUNCIONES AUXILIARES 
-------------------------------------------------------------------------*/

// CUADRADO:------------------------------------------------------------
double Cuadrado(double x)
{
return x*x;
}

// CUBO:----------------------------------------------------------------
double Cubo(double x)
{
return x*Cuadrado(x);
}

// RAIZCUB:-------------------------------------------------------------
double RaizCub(double x)
{
return ( x ? (x/fabs(x)) : 0.0)*pow(fabs(x),1.0/3.0);
}

/* CALAJUL:----------------------------------------------------------------
	 Factor de conversión de calorías a julios.
 	 Si se pide en calorías el factor de conversión es 1.
-------------------------------------------------------------------------*/
double CalAJul(BOOL Unidades)
{
return ((Unidades==EN_JULIOS) ? CAL_A_JUL:1.0);
}

// AJUSTE:-----------------------------------------------------------------
double Ajuste(double x,double n)
{
double m=floor(x/n);
double i=(((x-m*n)<(n/2.0)) ? 0.0:1.0);

return (n*(m+i));
}

/*-------------------------------------------------------------------------
					Definición de las ESTRUCTURAS de datos. 
-------------------------------------------------------------------------*/

struct REACTIVO
{
  char Nombre[MAXCAD];        // Nombre del reactivo.
  char Formula[MAXCAD];       // Fórmula del reactivo.
double Porcentaje;            // Peso del react. en la mezcla.[%]
double PesoMol;               // Peso molecular del compuesto [g/mol]
double Energia;               // Incr. Energía formación 298 K [kcal/mol]
double BO;                    // Balance de Oxígeno [%]
};



struct RESULTADO
{
  char Nombre[MAXCAD];
  char Formula_1kg[MAXTEX];
double d0,	 		 		 // Densidad del encartuchado [g/cm³]
		 Eo, 			 		 // Energía interna de la mezcla exp.[kcal/kg]
		 BO,         		 // Balance de oxígeno [%]
		 Qexplosion, 		 // Calor de explosión [kcal/kg]
       Texplosion, 		 // Temperatura de explosión [K]
		 Ng,         		 // Total de moles gaseosos [mol/kg]
		 M,          		 // Masa molecular media de gases [g/mol]
		 Vcn,			 		 // Volumen	de gases en C.N [m³/kg]
       f,			 		 // Energía específica [kcal/kg]
		 Fi,         		 // Fi=Ng*(M*Qexplosion)^1/2
		 Pcj,        		 // Presión de detonación [GPa]
		 Dcj,        		 // Velocidad de detonación [m/s]
		 dcj,			 		 // Densidad de detonación [g/cm³]
       CoefAdiabat;		 // Coeficiente adiabático [-]
}; 

struct PRODUCTO
{
char Simbolo[3];   	// 2+1 caracteres forman el símbolo de un elemento
							// químico.
double Masa_atomica; // Masa atómica del elemento químico.
char   Formula[7];  /* Fórmula química del producto de explosión asociado
							   al elemento dado.
							   6+1 caracteres, máximo para los carbonatos Na2CO3.*/
char   FormulaBO[7];  /* Producto para el cálculo del balance de oxígeno:
							   a diferencia del anterior debe estar COMPLETAMENTE
							   oxidado.*/			 
double Moles;        // [mol/kg]  formados en la explosión.
double Eformacion;   // a 298 K en kcal/mol.
double Tvapor; 		// Temperatura de vaporización.

/* Entalpias de formación de los productos de explosión H(T)-H(298 K)
 en kcal/mol datos desde 300 K hasta 6000 K de 100 en 100 K. 			*/
double HT_H298[TBL_DATOS];
};


struct ERRORES_
{
BOOL ind;				// Estado del indicador del error: SI ó NO.  
char des[MAXTEX];    // Descripción del error.
};


/*-------------------------------------------------------------------------
									Clase EXPLOSIVO.
-------------------------------------------------------------------------*/


class	Explosivo
{
public:
int 			NumReact;     //Nº de Reactivos que forman parte de la mezcla.
REACTIVO 	Reactivo[MAX_REACTIVOS];	 // Datos de los reactivos.
PRODUCTO 	TablaProd[MAX_PRODUCTOS];   // Tabla de los prod. de detonación.
RESULTADO 	Resultado;                  // Resultados del problema.
ERRORES_ 	Error[MAX_ERROR];				 // Errores del problema.
private:
double 		Tabla_K1[TBL_DATOS];	 // Cte. de equilibrio K1[-]
double 		Tabla_K2[TBL_DATOS];	 // Cte. de equilibrio K2[Pa]

public:
	Explosivo();                            	// Función constuctora.

	double CalcNg(double temperatura);			// Moles gaseosos.
	double CalcTotal();								// Porcentaje total.
	double CalcPMol(char *formula);           // Peso molecular [g/mol]
	double CalcBO(int i,char *formula);			// Balance de oxígeno [%].
	double CalcNumAt(char *formula, int i);	// Número de átomos.

   void  BorrarMoles();                      // Moles de los productos.
	void  BorrarDatos();                      // Datos del problema a 0.
   void  BorrarError();							   // Errores a 0.
	void  PonerError(int i);						// Poner error en la lista.
	BOOL  TomarError(int i);                  // Valor del error.
	void CalcResultado();                     // Result. del problema.
   BOOL  CargarTablaError(char *directorio); // Tabla de errores.
	BOOL  CargarTablaProd(char *directorio);  // Tabla productos explosión.
	BOOL  CargarTablaK12(char *directorio);	// Tabla ctes. de equilibrio.

private:	
	BOOL Gas(double temperatura, int i);		// ¿ Está gaseoso?
	double IndiceTbl(double temperatura);		// Indice acceso tablas. 


	double HT_H298(double temperatura,int j); // Entalpía productos. 
	double K1(double temperatura);				// Cte. de equilibrio K1[-].
	double K2_(double temperatura);				// Cte. de equil. K2_[kg/mol].
	

	void VerificarDensidad();						// Verifica dens. encartuchado.
   void VerificarResultados();               // Verifica la solución.
	void CalcFormula_1kg();							// Fórmula para un kilo de exp.
	void CalcEnergia_Interna();               // [kcal/kg]
	void CalcM();										// Masa molecular media.
   void CalcParamAdic();
	void CalcKamletJacobs();                  // Fómulas empíricas.

	void CalcTexplosion();							// Temperatura exp [K].
	void CalcTexpMolesProductos();				// Temp. y prod de explosión.
	void CalcQexplosion();							// Calor de explosión [kcal/kg]
	void CalcMolesProductos();						// Composición de los productos.
	void CargarDatosReactivos();

	double Calcf_nCO(double f_nCO[],double nCO);
	void CalcCoef(double temperatura,double Bh,double Bo,
				  double Bc,double f_nCO[],BOOL Grafito);

	double CalcTermIndep(int j);
   double CalcQsensible(double temperatura);
	double CalcSolucion(double f_nCO[],double CotaInf,double CotaSup);
	BOOL CalcRemonte(double temperatura,double Bh,double Bo,   
					  double Bc, double nCO, BOOL Grafito);
};

/*-------------------------------------------------------------------------
EXPLOSIVO:        Función constructora de la clase.
	Parámetros:		Ninguno
	Resultado:		Ninguno
	Comentarios:   Inicializa las estructuras de la clase. 
-------------------------------------------------------------------------*/
Explosivo::Explosivo()
{
BorrarDatos();
}

/*-------------------------------------------------------------------------
BORRARERROR:
	Parámetros:		Ninguno
	Resultado:		Ninguno
	Comentarios:	Reinicializa los moles de los productos.
-------------------------------------------------------------------------*/

void Explosivo::BorrarMoles()
{
for(int i=0;i<MAX_PRODUCTOS;i++)
	{
   // Borra los moles de los productos.
	TablaProd[i].Moles=0.0;
	}
}
/*-------------------------------------------------------------------------
BORRARERROR:
	Parámetros:		Ninguno
	Resultado:		Ninguno
	Comentarios:	Reinicializa la lista de errores.
-------------------------------------------------------------------------*/

void Explosivo::BorrarError()
{
for(int i=0;i<MAX_ERROR;i++)
	{
	Error[i].ind=NO;
	}
}

/*-------------------------------------------------------------------------
BORRARDATOS:
	Parámetros:		Ninguno
	Resultado:		Ninguno
	Comentarios:	Reinicializa los datos del problema.
-------------------------------------------------------------------------*/
void Explosivo::BorrarDatos()
{
NumReact=0;
memset(&Resultado,0,sizeof(Resultado));
BorrarError();
BorrarMoles();
for(int i=0;i<MAX_REACTIVOS;i++)
	{
	memset(&Reactivo[i],0,sizeof(Reactivo[i]));
	}

}


/*-------------------------------------------------------------------------
PONERERROR:
	Parámetros:		int i:  identificador del tipo de error
	Resultado:		Ninguno
	Comentarios:	Activa el indicador del error.
-------------------------------------------------------------------------*/

void Explosivo::PonerError(int i)
{
switch((i<MAX_ERROR)? SI:NO)
	{
	case SI:
		Error[i].ind=SI;
		Error[ERR_EX_UNO].ind=SI;  // Al menos hay un error.
		break;

	case NO:
		Error[ERR_ERROR_].ind=Error[ERR_EX_UNO].ind=SI;
		break;
	}
}


/*-------------------------------------------------------------------------
TOMARERROR:
	Parámetros:		int i:  identificador del tipo de error
	Resultado:		BOOL:   Estado del error.
	Comentarios:	Ninguna.
-------------------------------------------------------------------------*/

BOOL Explosivo::TomarError(int i)
{
BOOL respuesta=NO;
if (i<MAX_ERROR)
	{
	respuesta=Error[i].ind;
	}
return respuesta;
}

/*-------------------------------------------------------------------------
CARGARTABLAERROR:       
	Parámetros:		char *directorio:  Directorio de datos.
	Resultado:		BOOL :  Cierto si se puede acceder al archivo de datos.
	Comentarios:	Carga el archivo de datos TABLPROD.DAT y almacena
   					los datos en las estructuras.
-------------------------------------------------------------------------*/


BOOL Explosivo::CargarTablaError(char *directorio)
{
char posicion[MAXCAD];
char Cadena1[MAXTEX];
char Cadena2[MAXCAD];
char DirecDatos[MAXPATH];// Directorio de datos.
BOOL respuesta;
int i;

sprintf(DirecDatos,"%s\\%s",directorio,__ERROR_DAT);

respuesta=(access(DirecDatos,0)? NO:SI);
switch(respuesta)
	{
	case SI:  	
		for(i=0;i<MAX_ERROR;i++)
			{
			sprintf(posicion,"%i",i);
         sprintf(Cadena2,"\tError:(%i)\r\n"); 
			GetPrivateProfileString
					("Error",posicion,Cadena2,Cadena1,MAXTEX,DirecDatos);
			sprintf(Error[i].des,"\t%s\r\n",Cadena1);
         }
	case NO:
		// Error: No se encuentra 'tablprod.dat'
		PonerError(ERR_ERROR_);
   	break;
	}
return respuesta;
}


/*-------------------------------------------------------------------
CARGARTABLAPROD:       
	Parámetros:		char *directorio:  Directorio de datos.
	Resultado:		BOOL :  Cierto si se puede acceder al archivo de datos.
	Comentarios:	Carga el archivo de datos TABLPROD.DAT y almacena
   					los datos en las estructuras.
-------------------------------------------------------------------*/


BOOL Explosivo::CargarTablaProd(char *directorio)
{
char posicion[MAXCAD];
char Cadena1[MAXCAD];
char Cadena2[MAXCAD];
char DirecDatos[MAXPATH];// Directorio de datos.
BOOL respuesta;
int i;

// Tablad de errores.
CargarTablaError(directorio);
sprintf(DirecDatos,"%s\\%s",directorio,__TABLPROD_DAT);
BorrarDatos();  				// Pone los miembros de la estructura a 0.

respuesta=(access(DirecDatos,0)? NO:SI);
switch(respuesta)
	{
	case SI:  	
		for(i=0;i<MAX_PRODUCTOS;i++)
			{
			sprintf(posicion,"%i",i);
			GetPrivateProfileString
				(posicion,"simbolo","?",TablaProd[i].Simbolo
				,sizeof(TablaProd[0].Simbolo),DirecDatos);

			GetPrivateProfileString
				(posicion,"masa_atom","0.0",Cadena1,MAXCAD,DirecDatos);
			TablaProd[i].Masa_atomica=strtod(Cadena1,NULL);

			GetPrivateProfileString
				(posicion,"prod_exp","?",TablaProd[i].Formula
				,MAXCAD,DirecDatos);

			GetPrivateProfileString
				(posicion,"prod_bo","?",TablaProd[i].FormulaBO
				,MAXCAD,DirecDatos);

			GetPrivateProfileString
				(posicion,"ener_form","0.0",Cadena1,MAXCAD,DirecDatos);
			TablaProd[i].Eformacion=strtod(Cadena1,NULL);

			GetPrivateProfileString
				(posicion,"temp_vapor","0.0",Cadena1,MAXCAD,DirecDatos);
			TablaProd[i].Tvapor=strtod(Cadena1,NULL);

    	  // Carga la tabla de entalpías.
			for(int j=0;j<TBL_DATOS;j++)
				{	
				sprintf(Cadena1,"t%i00",(j+3));
				GetPrivateProfileString
					(posicion,Cadena1,"0.0",Cadena2,MAXCAD,DirecDatos);
				TablaProd[i].HT_H298[j]=strtod(Cadena2,NULL);
				}
			}     
		break;

	case NO:
		// Error: No se encuentra 'tablprod.dat'
		PonerError(ERR_DT_TAB);
   	break;
	}
return respuesta;
}

/*-------------------------------------------------------------------
CARGARTABLAK12:    
	Parámetros:		char *directorio:  Directorio de datos.
	Resultado:		BOOL :  Cierto si se puede acceder al archivo de datos.
	Comentarios:	Carga el archivo de datos CONSTANT.DAT y almacena
   					los datos en las estructuras.
-------------------------------------------------------------------*/

BOOL Explosivo::CargarTablaK12(char *directorio)
{
char Cadena1[MAXCAD];
char Cadena2[MAXCAD];
char DirecDatos[MAXPATH];  // Directorio de datos.
int i;
BOOL respuesta;
sprintf(DirecDatos,"%s\\%s",directorio,__CONSTANT_DAT);


respuesta=(access(DirecDatos,0) ? NO:SI);

switch(respuesta)
	{
	case SI: 	
		for(i=0;i<TBL_DATOS;i++)
			{
			sprintf(Cadena1,"t%i00",(i+3));
      	// Constante K1(-)
			GetPrivateProfileString
				("K1",Cadena1,"0.0",Cadena2,MAXCAD,DirecDatos);
			Tabla_K1[i]=strtod(Cadena2,NULL);
			// Constante K2(Pa)
			GetPrivateProfileString
				("K2",Cadena1,"0.0",Cadena2,MAXCAD,DirecDatos);
			Tabla_K2[i]=strtod(Cadena2,NULL);
			}	
		break;

	case NO:  	
		// Error: No se encuentra 'constant.dat'
		PonerError(ERR_DT_CON);
      break;
	}
return respuesta;
}



/*-------------------------------------------------------------------
GAS:
	Parámetros:	   double: temperatura (K)
						int:   Índice de acceso a la tabla de productos.   
	Resultado:		BOOL:  ¿El producto 'i' es gaseoso a la Tº
						'temperatura'
	Comentarios:	temperatura puede tomar cualquier valor
-------------------------------------------------------------------*/

BOOL Explosivo::Gas(double temperatura, int i)
{
return ((temperatura>=TablaProd[i].Tvapor) ? SI:NO); 
}

/*-------------------------------------------------------------------
INDICETBL:
	Parámetros:	   double: temperatura (K) 
	Resultado:		double: Aprox. del índice de acceso a las tablas.	
	Comentarios:	El dato buscado en las tabla se interpola entre
   					los valores enteros más próximos.
-------------------------------------------------------------------*/

double Explosivo::IndiceTbl(double temperatura)
{
double temptabla=temperatura;
if (temperatura>MAX_TEMP)
	{
	temptabla=MAX_TEMP;
   // Temperatura excesiva para acceder a las tablas.
	PonerError(ERR_IND_EXC);
   }
if (temperatura<NRM_TEMP)
	{
	temptabla=MIN_TEMP;
   // Temperatura demasiado baja para acceder a las tablas.
	PonerError(ERR_IND_DEF);
   }
return (temptabla-MIN_TEMP)/INC_TEMP;
}


/*-------------------------------------------------------------------
CALCTOTAL:
	Parámetros:		Ninguno
	Resultado:		Porcentaje total de la mezcla (%).
	Comentarios:	Se emplea en la introducción de los datos de la
   					mezcla explosiva.
-------------------------------------------------------------------*/
double Explosivo::CalcTotal()
{
double total=0.0;
for(int i=0;i<NumReact;i++)
	{
	total+=Reactivo[i].Porcentaje;
	}
return total;
}

/*--------------------------------------------------------------------
 CALCNUMAT: 
	 Parámetros:
			 char *formula: Fórmula del compuesto.
			 int  i      :Posición del elemento en la estructura ELEMENTO.
	 Resultado:
			double 	     : Número de átomos del elemento en la fórmula.
	 Comentarios:
			La función tiene en cuenta que la fórmula puede tener coeficientes
			decimales,  pero no comprueba ni la presencia de paréntesis ni
			 la repetición del mismo elemento en la misma fórmula.
			Además los símbolos de los elementos deben presentarse en la
         correcta combinación de mayúsculas y minúsculas.
---------------------------------------------------------------------*/

double Explosivo::CalcNumAt(char *formula,int i)
{
unsigned char Posicion;
char NumAtomos[MAXCAD];
char digito[]=" ",
	  *Encuentra_at;

strcpy(NumAtomos,"\0");
// Busca en la fórmula el símbolo del elemento en cuestión.
Encuentra_at=strstr(formula,TablaProd[i].Simbolo);
if(Encuentra_at!=NULL) 
  {
  // En caso de que lo encuentre...
  // Avanza en la fórmula tantos caracteres como longitud tenga el
  // símbolo del elemento, pues a partir de aquí estará lo que buscamos.
  Posicion=((Encuentra_at-formula)+strlen(TablaProd[i].Simbolo));
  // Si lo que encuentra es una letra o el final de la cadena...
  if(isupper(formula[Posicion])!=0 || formula[Posicion]==NULL)
	  {
	  // Un sólo átomo de ese elemento.
	  strcpy(NumAtomos,"1\0");
	  }
  else{ // Si no irá pasando caracteres hasta que no encuentre números.
		 while(isdigit(formula[Posicion])!=0 || formula[Posicion]=='.')
	    	{
			digito[0]=formula[Posicion];
			strcat(NumAtomos,digito);
			Posicion++;
			}
       }
  return strtod(NumAtomos,NULL);
  }
else return 0.0;
}


/* CALCPMOL:-----------------------------------------------------------  
Parámetros:
	char *formula: Fórmula del compuesto.
Resultado:
	double	  :Peso molecular de la fórmula.
Comentarios:
	La función sólo tiene en cuenta los elementos definidos en
	la estructura 'TablaProd'.
	 Basada en 'CalcNumAt' ,tiene las mismas limitaciones que esta.
---------------------------------------------------------------------*/

double Explosivo::CalcPMol(char *formula)
{
double acumulador=0; 			// Inicializa el acumulador a 0.
for(int i=0;i<MAX_ELEMENTOS;i++)
	{
	acumulador+=(CalcNumAt(formula,i)*TablaProd[i].Masa_atomica);
   }	
return acumulador;
}


/* HT_H298:-----------------------------------------------------------
Parámetros:
	double temperatura: En [K]
   int 	j:  Índice del producto de explosión en la tabla de datos. 
Resultado:
	double	  : [kcal/mol] Incremento de entalpía desde 298 K hasta
					temperatura del producto j de la tabla TablaProd.
Comentarios:
	La función realiza una interpolación lineal cuando se pide la ental-
	pía a una temperatura entre dos valores de la tabla.
	Trabaja en el intervalo NRM_TEMP y MAX_TEMP.
	 El acceso a las tablas tanto de entalpías como de ctes se debe reali-
  	zar mediante un índice y no directamente en temperaturas por lo que es
  	necesario un cambio de variable.
---------------------------------------------------------------------*/

double Explosivo::HT_H298(double temperatura,int j)
{
double respuesta; 					  // Respuesta de la función.
double i=IndiceTbl(temperatura);   // Cambio de variable para acceder tablas.
int	 int_i=i;        				  // Parte entera de i.


switch((temperatura>=MIN_TEMP)? SI:NO)
	{
	case SI:
		/* Se accede a la tabla mediante interpolación lineal entre dos
			miembros de la tabla.  */
		respuesta=(i-int_i)*(TablaProd[j].HT_H298[int_i+1]
			-TablaProd[j].HT_H298[int_i])+TablaProd[j].HT_H298[int_i];
		break;

	case NO:
		/* Interpolación entre 0 y MIN_TEMP
			Para temperaturas entre 298 K y 300 K. */
		respuesta=(temperatura-NRM_TEMP)
			*TablaProd[j].HT_H298[0]/(MIN_TEMP-NRM_TEMP);
      break;
	}
return respuesta;
}


/*-------------------------------------------------------------------
K1:
	Parámetros:		temperatura (K) 
	Resultado:		Valor de la cte de equilibrio para la temperatura dada.
	Comentarios:	La temperatura debe estar entre unos límites.
-------------------------------------------------------------------*/

double Explosivo::K1(double temperatura)
{
double i=IndiceTbl(temperatura);		//Índice de la tabla.
int int_i=i;                        // Parte entera por trucadura de i.

// Interpolación lineal entre miembros de la tabla.
return (i-int_i)*Tabla_K1[int_i+1]-(i-int_i-1)*Tabla_K1[int_i];
}


/*-------------------------------------------------------------------
K2_:
	Parámetros:		double temperatura: En [K] 
	Resultado:	   double: Valor de la cte de equilibrio: [kg/mol]
	 					para la	temperatura dada.
	Comentarios:	La temperatura debe estar entre unos límites.
	Nota I:			La constante de la tabla (Tabla_K2) debe estar
						en Pa.
-------------------------------------------------------------------*/

double Explosivo::K2_(double temperatura)
{
double i=IndiceTbl(temperatura);		//Índice de la tabla.
int int_i=i;                        // Parte entera por trucadura de i.

return ((i-int_i)*Tabla_K2[int_i+1]-(i-int_i-1)*Tabla_K2[int_i])
					/(Resultado.d0*temperatura*R_JUL*GCM3_A_KGM3);
}


/*-------------------------------------------------------------------
CALCNG:
	Parámetros:		double temperatura: En [K] 
	Resultado:	   double: Cantidad total de productos gaseosos [mol/kg]
	Comentarios:	Ninguno.
-------------------------------------------------------------------*/

double Explosivo::CalcNg(double temperatura)
{
double respuesta=0.0;
for (int i=0;i<MAX_PRODUCTOS;i++)
	{
	if((Gas(temperatura,i) && TablaProd[i].Moles))
		{
		// Si el producto de explosión está en estado gaseoso...
		respuesta+=TablaProd[i].Moles;
		}
	}
return respuesta;
}


/*--------------------------------------------------------------------------
CALCQSENSIBLE:
	Parámetros:		double temperatura:
	Resultado:		Calor sensible los productos de explosión en [kcal/kg].
	Comentarios:   Si Calor sensible es negativo devuelve cero.
--------------------------------------------------------------------------*/

double Explosivo::CalcQsensible(double temperatura)
{
double EnHT_H298=0.0;  // 	[kcal/kg]
double respuesta;
for (int j=0;j<MAX_PRODUCTOS;j++)
	{
	EnHT_H298+=TablaProd[j].Moles*HT_H298(temperatura,j);
	}

respuesta=(EnHT_H298-CalcNg(temperatura)*R_KCAL*(temperatura-NRM_TEMP));

return ((respuesta>0.0) ? respuesta : 0.0);
}


/*-------------------------------------------------------------------------
CALCBO:
	Parámetros:		Fórmula química de un compuesto.
	Resultado:		Balance de oxígeno (%).
						int i: Tipo de balance de oxígeno.

	Comentarios:
		 Caso: i=BO_PROBO:
						Calcula el balance de oxígeno según la definición,
						es decir, supone que los productos de explosión
						están en su mayor estado de oxidación.
						Se emplea en todos los resultados que se muestran
						en pantalla.
		 Caso: i=BO_PRODEX:
		 				Calcula el balance de oxígeno teniendo en cuenta
						los productos de explosión que se van a formar.
						Se emplea en decidir si el explosivo es de verdad
						excedentario o deficitario en oxígeno y para calcular
                  los moles de oxígeno entre los productos de explosión.

	Nota I:	Esta diferenciación es necesaria por la divergencia de
				criterios.
	Nota II: Si el B.O. se aproxima lo suficiente a cero la función
				devuelve 0 , si no sería casi imposible encontrar explosivo
				perfectamente equilibrados. 
-------------------------------------------------------------------------*/

#define BO_PRODBO 1
#define BO_PRODEX 2

double Explosivo::CalcBO(int i,char *formula)
{
char formProd[MAXCAD];
double Ox_Necesario=0.0
		,Ox_Presente=0.0
		,Consumo_Ox 		// Peso del elemento en el B.O.
		,PesoMol=CalcPMol(formula)
		,respuesta=0.0;
if (PesoMol)
	{
	for (int j=0;j<MAX_ELEMENTOS;j++)
		{
		switch(i)
			{
			case BO_PRODBO:
				strcpy(formProd,TablaProd[j].FormulaBO);
				break;
			case BO_PRODEX:
				strcpy(formProd,TablaProd[j].Formula);
				break;
         }
      // Caso general.
		Consumo_Ox=CalcNumAt(formProd,OXIGENO)/CalcNumAt(formProd,j);
		// Excepciones:
		if (CalcNumAt(formProd,CARBONO) && j!=CARBONO)
			{
			// Alcalinos: Forman carbonatos.
			Consumo_Ox=0.5;
         }	
		if (CalcNumAt(formProd,HIDROGENO) && j!=HIDROGENO)
			{
			// Halógenos: Forman haluros de hidrógeno.
			Consumo_Ox=-0.5;
         }
		if (j==OXIGENO)
			{
			// Oxígeno:  ¡ El Oxígeno no cuenta en el Ox_Necesario !
			Consumo_Ox=0.0;
         }

		Ox_Necesario+=(Consumo_Ox*CalcNumAt(formula,j));
		}
	Ox_Presente=CalcNumAt(formula,OXIGENO);
	respuesta=((Ox_Presente-Ox_Necesario)
					 *TablaProd[OXIGENO].Masa_atomica*100.0/PesoMol);
	}
return respuesta;
}


/**************** DECLARACIÓN DE FUNCIONES PRINCIPALES ***************/

/* CALCFORMULA_1KG()*/

void Explosivo::CalcFormula_1kg()
{
char formula[MAXTEX]="\0";
char Cadena[MAXCAD]="\0";
double acumulador;

for (int i=0;i<MAX_ELEMENTOS;i++)
	{
	acumulador=0.0;
	for (int j=0;j<NumReact;j++)
		{
		if (Reactivo[j].Porcentaje && Reactivo[j].PesoMol)
			{		
			acumulador+=(10.0*Reactivo[j].Porcentaje*
			CalcNumAt(Reactivo[j].Formula,i)/Reactivo[j].PesoMol);	           
			}
		}
   if (acumulador)
		{
		sprintf(Cadena,"%s%.4f\0",TablaProd[i].Simbolo,acumulador);
		strcat(formula,Cadena);
		}
	}
strcpy(Resultado.Formula_1kg,formula);
}


/* CALCENERGIA_INTERNA-----------------------------------------------*/
void Explosivo::CalcEnergia_Interna()
{
 // Energía interna de la mezcla explosiva.
double acumulador=0.0;
for(int i=0;i<NumReact;i++)
	{
	acumulador+=(Reactivo[i].Energia*Reactivo[i].Porcentaje);
	}
Resultado.Eo=(acumulador/100.0);
}

/* CALCMOLESPRODUCTOS------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción:
		-La función calcula los moles de los productos de explosión
		suponiendo que tanto el carbono como el hidrógeno se oxidan
		completamente a CO2 y H2O, y que el resto de los elementos
		se oxidan a óxido o carbonato, excepto los halógenos que
		forman el haluro correspondiente y el Hg que se volatiliza.
	Limitaciones:
   	-No calcula los moles de oxígeno.
		-Para mezclas deficitarias en oxígeno no proporciona los
		moles de productos: CO2,CO,C,H2O,H2 que dependen de la Tª.
--------------------------------------------------------------------*/

void Explosivo::CalcMolesProductos()
{
double   NumAt,
			Carbonatos=0.0,
			Haluros=0.0,
			oxigeno;
	
for (int i=0;i<MAX_ELEMENTOS;i++)
	{
	NumAt=CalcNumAt(TablaProd[i].Formula,i);
  	if(NumAt)  // Evita el error si se anula los denominadores.
   	{
		TablaProd[i].Moles=CalcNumAt(Resultado.Formula_1kg,i)/NumAt;

		// Excepciones:
		//Alcalinos: Forman carbonatos.
		if (CalcNumAt(TablaProd[i].Formula,CARBONO) && i!=CARBONO)
  		 	{
			Carbonatos+=TablaProd[i].Moles;
			}
		//Halógenos: Forman haluros de hidrógeno.
		if (CalcNumAt(TablaProd[i].Formula,HIDROGENO) && i!=HIDROGENO)
			{
			Haluros+=TablaProd[i].Moles;
			}
	
		}
	}
// Moles de oxígeno.

oxigeno=10*CalcBO(BO_PRODEX,Resultado.Formula_1kg)/CalcPMol("O2");

// Si el balance de oxígeno es negativo no hay oxígeno libre.
TablaProd[OXIGENO].Moles=((oxigeno>0.0) ? oxigeno : 0.0);
TablaProd[CARBONO].Moles-=Carbonatos;
TablaProd[HIDROGENO].Moles-=Haluros*0.5;

}

/* CALCQEXPLOSION-----------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción: Calcula el calor de explosión [kcal/kg] apartir de la
					energía interna y los productos de explosión.
--------------------------------------------------------------------*/

void Explosivo::CalcQexplosion()
{
double acumulador=0.0;
for (int i=0;i<MAX_PRODUCTOS;i++)  
	{
	acumulador+=TablaProd[i].Moles*TablaProd[i].Eformacion;
	}
Resultado.Qexplosion=(Resultado.Eo-acumulador);
if (Resultado.Qexplosion<=0.0)
	{
	// Reacción endotérmica imposible la detonación.
	PonerError(ERR_ENDOTE);
	}
}




/* CALCM-------------------------------------------------------------
	Parámetros:		Ninguno.
	Resultado:		Ninguno.
	Descripción:  	Calcula la masa molecular media de los productos
					 	gaseosos.
	Nota I:			Si los moles de gas Ng[g/mol] son nulos, M lo debe ser
						también.
	Nota II:			M(g/mol) = Sum(n(i)[mol/kg]*Pm(i)[g/mol])/ ng[mol/kg]
--------------------------------------------------------------------*/
void Explosivo::CalcM()
{
double acumulador=0.0;
double molesGas=CalcNg(Resultado.Texplosion);

for (int i=0;i<MAX_PRODUCTOS;i++)
	{
	if(Gas(Resultado.Texplosion,i) && TablaProd[i].Moles)
		{
		// Si el producto de explosión es un gas...
		acumulador+=TablaProd[i].Moles*CalcPMol(TablaProd[i].Formula);
		}
	}
Resultado.M=((molesGas) ? (acumulador/molesGas) : 0.0);
}

/* CALTEXPLOSION ----------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción:
		Resuelve la ecuación: Q(T)=Qexplosión, mediante un
		proceso iterativo utilizando el método de la secante.
	Limitaciones:
		El margen de temperaturas en el que trabaja es de
		NRM_TEMP (298 K), hasta el máximo de datos que de los que se
		dispone en la tabla de entalpías MAX_TEMP (6000 K).
		En el intervalo NRM_TEMP,MIN_TEMP que no está recogido en la tabla
		se realiza una interpolación lineal.
		La función no calcula la composición de los productos de explo-
		sión, por lo que sólo es útil para mezclas con B.O positivo.
		La ecuación sólo tiene solución si la reacción es exotérmica,
		es decir Qexplosión>0. 
   
--------------------------------------------------------------------*/

void Explosivo::CalcTexplosion()
{
int iteracion=0;            	  // Iteraciones del proceso.

double T0=NRM_TEMP,     	 		  // Temperatura del estado normal 298 K
		 T1=MAX_TEMP,        		  // Máxima temp. recagistrada en la tabla.
		 T2=INI_TEMP,					  // Punto medio del intervalo de Tª.        
		 Q0=CalcQsensible(T0),
		 Q1=CalcQsensible(T1),
		 Q2=0.0;
 
/* Condición de detención del proceso iterativo:
	Por aproximarse lo suficiente a la solución menor
	o por rebasar el número admisible de iteraciones.    */

while((fabs(Q2-Resultado.Qexplosion)>0.001) && (iteracion<MAX_ITER)) 
	{
	if(Q1-Q0)
		{
		// Interpolación lineal.
		T2=((T1-T0)*Resultado.Qexplosion-T1*Q0+T0*Q1)/(Q1-Q0);
		}
	else
   	{
		PonerError(ERR_DIV_CER); // Error división por cero...
    	}
	Q2=CalcQsensible(T2);
	iteracion++;

	if(iteracion==MAX_ITER)
		{
      // Se ha rebasado el límite de iteraciones.
		PonerError(AVI_MAXITE);
		}

	if(Q2>Resultado.Qexplosion)
		{
		T1=T2; // Disminuimos la Tª de búsqueda.
		Q1=Q2;
		}
	else
		{
		T0=T2; // Aumentamos la temperatura.
		Q0=Q2;
		} 
	}
Resultado.Texplosion=T2;
}


/* CALCTERMINDEP: -----------------------------------------------------------
	Parámetros:
   				int j:  Término independiente que queremos.
	Resultado:
					double: valor del término indep. deseado.

	Descripción: Calcula el termino indep del sistema formado por las
   				 ecuaciones del H,O ó C.

   Nota I:		La función solo funciona con mezclas con B.O. negativo.
-------------------------------------------------------------------------*/


double Explosivo::CalcTermIndep(int j)
{
double Haluros=0.0,
		 Oxidos=0.0,
		 Carbonatos=0.0,
		 H,O,C;
double respuesta;

for (int i=0;i<MAX_ELEMENTOS;i++)
	{
	H=CalcNumAt(TablaProd[i].Formula,HIDROGENO);
	O=CalcNumAt(TablaProd[i].Formula,OXIGENO);
	C=CalcNumAt(TablaProd[i].Formula,CARBONO);

	if (H && i!=AGUA_H)
   	{
		Haluros+=(H*TablaProd[i].Moles);
      }
	if (O && (i!=OXIGENO) && (i!=AGUA_H) && (i!=CARBONO))
   	{
		Oxidos+=(O*TablaProd[i].Moles);
		}
	if (C && (i!=CARBONO))
   	{
		Carbonatos+=(C*TablaProd[i].Moles);
      }
	}
switch(j)
	{
   case HIDROGENO:
		respuesta=0.5*(CalcNumAt(Resultado.Formula_1kg,HIDROGENO)-Haluros);
      break;
   case OXIGENO:
		respuesta=CalcNumAt(Resultado.Formula_1kg,OXIGENO)-Oxidos;
      break;
   case CARBONO:
		respuesta=CalcNumAt(Resultado.Formula_1kg,CARBONO)-Carbonatos;
		break;
	default:
		//Error...
      // PonerError();
      respuesta=0.0;
      break;
	}
return respuesta;
}


/* CALCCOEF: -----------------------------------------------------------
	Parámetros:
				double temperatura:	
				double Bh:        	Term. Indep ec. balance Hidrógeno.
				double Bo:        	Term. Indep ec. balance Oxígeno.
				double Bc:        	Term. Indep ec. balance Carbono.
				double f_nCO:        Coeficientes.
				BOOL Grafito:    		¿ Se produce grafitización ?
		
	Resultado: Ninguno.

	Descripción: Calcula los coeficientes de la ec. en moles de CO.

   Nota I:		La función solo funciona con mezclas con B.O. negativo.
-------------------------------------------------------------------------*/

void Explosivo::CalcCoef(double temperatura,double Bh,double Bo
						,double Bc,double f_nCO[],BOOL Grafito)
{
double K_1=K1(temperatura);
double K_2;
switch(Grafito)
	{
	case CON_GRAFITO:
		K_2=K2_(temperatura);
		if(K_2) 
      	{
			f_nCO[0]=2.0*K_1/Cuadrado(K_2);
			f_nCO[1]=(K_1+2.0)/K_2;
			f_nCO[2]=(1.0+(Bh-Bo)*K_1/K_2);
			f_nCO[3]=-Bo;
			}
		else
			{
         //PonerError();
         }
      break;
	case SIN_GRAFITO:	
		f_nCO[0]=(1.0-K_1);
		f_nCO[1]=K_1*(Bh-Bo+3.0*Bc)+Bo-2.0*Bc;
		f_nCO[2]=K_1*Bc*(Bo-Bh-2.0*Bc);
		f_nCO[3]=0.0;
      break;
	}
}

/* CALCF_NCO----------------------------------------------------------------
	Parámetros:  double f_nCO[]:  coef. del polinomio.
					 double nCO:      valor. 
	Resultado:   double:        Valor de la función polinómica para nCO.
		Ninguno.
	Descripción:  
--------------------------------------------------------------------------*/

double Explosivo::Calcf_nCO(double f_nCO[],double nCO)
{
double acumulador=0.0;

for(int i=0;i<EC_CUBICA;i++)
	{
	acumulador+=f_nCO[i];
   acumulador*=nCO;
	}
acumulador+=f_nCO[EC_CUBICA];
return acumulador;
}


/* CALCSOLUCION------------------------------------------------------------
	Parámetros:  double f_nCO[]:  Coef. del polinomio.
					 double CotaInf:  Cota inferior:  restricción de la sol.  
                double CotaSup:	Cota superior:  restricción de la sol.
	Resultado:
						double:  moles de CO [mol/kg] 
	Descripción:  Solución de la ecuación polinómica de tercer grado.
					  Resuelve la ecuación en moles de CO.   
--------------------------------------------------------------------------*/

double Explosivo::CalcSolucion(double f_nCO[],double CotaInf,double CotaSup)
{

double nCO=0.0;			 // Moles de CO: resultado de la función.(incógnita)
double raiz[EC_CUBICA];  // Raíces de la ecuación.
double p,q,s;            // Cambio a:  y^3+3py+2q=0
							    //            x=y-s
double	K;              // Parámetro para la discusión de la ec.
double   alfa;           // Parámetro para la solución trigonométrica.

if(f_nCO[0])    			 // Si la ecuación es de verdad de tercer grado.
	{
	// Realiza el cambio de variable.

	s=(f_nCO[1]/(3.0*f_nCO[0]));
	p=(f_nCO[2]/(3.0*f_nCO[0]))-Cuadrado(s);
	q=Cubo(s)+(-f_nCO[2]*s+f_nCO[3])*0.5/f_nCO[0];

	// Cálcula el parámetro de discusión.

	K=Cubo(p)+Cuadrado(q);

	switch((K==0.0)? IGUAL:(K>0.0)? MAYOR:MENOR)
		{ 
		case MENOR:
			// Tres raíces reales distintas.
			alfa=acos(-q/sqrt(Cubo(-p)));     
			for(int i=-1;i<=1;i++)
				{
				raiz[i+1]=2.0*pow(-1.0,i)*sqrt(-p)
							*cos((alfa+M_PI*(double)i)/3.0)-s;
				}
			break;
		case IGUAL:
      	// Una raíz real y dos imaginarias.
			raiz[0]=RaizCub(-q)-RaizCub(q)-s;
			raiz[2]=raiz[1]=RAIZ_IMAGINARIA;
			break;
		case MAYOR:
      	// Una raiz real doble y otra simple.
			raiz[0]=RaizCub(-q+sqrt(K))-RaizCub(q+sqrt(K))-s;
			raiz[2]=raiz[1]=-0.5*(raiz[0]+3.0*s);
			break;
		}	  	
/*Elección entre las raíces, cual tiene significado físico.
  los moles de monóxido de carbono no pueden ser negativos ni superar
  al carbono presente en la mezcla explosiva intacta. */
	for(int i=0;i<EC_CUBICA;i++)
		{
		if(raiz[i]<CotaSup && raiz[i]>CotaInf)
			{
			nCO=raiz[i];
			}
		}
	}
else
	{
	//ERROR en el grado del sistema.
	PonerError(ERR_GRADOS);
   // PonerError();
   }
return nCO;
}


/* CALCREMONTE: -----------------------------------------------------------
	Parámetros:
				double temperatura:	
				double Bh:        	Term. Indep ec. balance Hidrógeno.
				double Bo:        	Term. Indep ec. balance Oxígeno.
				double Bc:        	Term. Indep ec. balance Carbono.
				double nCO:       	Monóxido de carbono. [mol/kg] de partida.
				BOOL Grafito:    		¿ Se produce grafitización ?
		
	Resultado:
				BOOL: Depende si la solución es válida es o no admisible.

	Descripción: Realiza el remonte del sistema, calculando los moles de
					 CO2,CO,C,H2O y H2. Además verifica si la solución obtenida
					 es admisible (valores positivos)
   Nota I:		La función solo funciona con mezclas con B.O. negativo.

-------------------------------------------------------------------------*/

BOOL Explosivo::CalcRemonte(double temperatura,double Bh,double Bo,      
									 double Bc,double nCO,BOOL Grafito)
{
switch(Grafito)
	{
	case CON_GRAFITO:
		switch((K2_(temperatura)) ? SI : NO)
			{
         case SI:
				TablaProd[DIOXIDO_C].Moles=Cuadrado(nCO)/K2_(temperatura);
				TablaProd[GRAFITO_C].Moles=Bc-nCO-TablaProd[DIOXIDO_C].Moles;
				break;
			case NO:
				// ERROR constante de equilibrio K2.
            // PonerError();
				break;
          }
		break;
	case SIN_GRAFITO:
		TablaProd[DIOXIDO_C].Moles=Bc-nCO;
		TablaProd[GRAFITO_C].Moles=0.0;
      break;
	 }
TablaProd[MONOXIDO_C].Moles=nCO;
TablaProd[AGUA_H].Moles=Bo-nCO-2.0*TablaProd[DIOXIDO_C].Moles;
TablaProd[DIATOMICO_H].Moles=Bh-TablaProd[AGUA_H].Moles;
 

return ((									nCO<0.0 ||
				 TablaProd[DIOXIDO_C].Moles<0.0 ||
					 TablaProd[AGUA_H].Moles<0.0 ||
			  TablaProd[DIATOMICO_H].Moles<0.0 ||
				 TablaProd[GRAFITO_C].Moles<0.0) ? NO:SI);
}


/* CALCTEXPMOLESPRODUCTOS: ------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción:  Calcula la temperatura de explosión y los moles de
					  C(grafito),	CO, CO2, H2 , H2O.   
-------------------------------------------------------------------------*/

void Explosivo::CalcTexpMolesProductos()
{
double Bh,Bo,Bc;                  
double temperatura=INI_TEMP;
double temperatura2;
double f_nCO[EC_CUBICA];
BOOL 	 Grafito;
BOOL   Es_solucion;                  // El sistema tiene solución.
double nCO;                          // Moles de monóxido de carbono.
double CotaInf=0.0;
double CotaSup=CalcNumAt(Resultado.Formula_1kg,CARBONO);
double Q1,Q2;
int iteracion=0;

Bh=CalcTermIndep(HIDROGENO);  // Term. Indep ec. balance Hidrógeno.
Bo=CalcTermIndep(OXIGENO);    // Term. Indep ec. balance Oxígeno.
Bc=CalcTermIndep(CARBONO);    // Term. Indep ec. balance Carbono.

do	{
	temperatura2=temperatura;
	Grafito=CON_GRAFITO;
	CalcCoef(temperatura,Bh,Bo,Bc,f_nCO,Grafito);
	nCO=CalcSolucion(f_nCO,CotaInf,CotaSup);
	Es_solucion=CalcRemonte(temperatura,Bh,Bo,Bc,nCO,Grafito);
	if(Es_solucion==NO)
		{
		Grafito=SIN_GRAFITO;
		CalcCoef(temperatura,Bh,Bo,Bc,f_nCO,Grafito);
		nCO=CalcSolucion(f_nCO,CotaInf,CotaSup);
		Es_solucion=CalcRemonte(temperatura,Bh,Bo,Bc,nCO,Grafito);
		}
	Q1=CalcQsensible(temperatura);
	CalcQexplosion();
	
	Q2=((Resultado.Qexplosion>0.0) ? Resultado.Qexplosion : 0.0);

   // Incremento o disminución de la Tª en función de la aproximación. 
	temperatura+=Q_A_TEMP*(Q2-Q1);   
	iteracion++;

	if(iteracion==MAX_ITER)
		{
      // Se ha rebasado el límite de iteraciones.
		PonerError(AVI_MAXITE);
		}
	}
	while(fabs(temperatura-temperatura2)>PRECISION_T && iteracion<MAX_ITER);

if(Es_solucion==NO)
	{
	//Error: Sistema sin solución.
   BorrarMoles();
   PonerError(ERR_SOLUCI);
	}
Resultado.Qexplosion=CalcQsensible(temperatura);
Resultado.Texplosion=temperatura;  
}

/* CALCPARAMADIC----------------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción:	Calcula el volumen de gases [m3/kg] y la energía
						específica   
-------------------------------------------------------------------------*/

void Explosivo::CalcParamAdic()
{
// Parámetros adicionales:
// Volumen de gases en condiciones normales [m3/kg].
Resultado.Vcn=K_VCN*Resultado.Ng;

// Fuerza o energía específica [kcal/kg].
Resultado.f=R_KCAL*Resultado.Ng*Resultado.Texplosion;
}


/* CALCKAMLETJACOBS---------------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción:	Calcula la aproximación al estado de detonación CJ
						mediante las fórmulas de Kamlet-Jacobs.

 	Nota I:  El calor de explosión debe ir en julios.
 	Nota II: Fi  no  tiene sentido en  reacciones endotérmicas.
	Nota III: El Coeficiente Adiabático toma valores infinitos si d0=8.355555

   					Fi=ng[mol/kg]*(M[g/mol]*Q[kJ/kg])^1/2
--------------------------------------------------------------------------*/


void Explosivo::CalcKamletJacobs()
{

if(!TomarError(ERR_ENDOTE))
	{
   // Parámetro Fi.
	Resultado.Fi=Resultado.Ng*sqrt(Resultado.M*Resultado.Qexplosion*CAL_A_JUL);

	// Presión de detonación.
	Resultado.Pcj=K_PCJ*Resultado.Fi*Cuadrado(Resultado.d0);

	// Velocidad de detonación.
	Resultado.Dcj=A_DCJ*sqrt(Resultado.Fi)*(1.0+B_DCJ*Resultado.d0);
   }
// Densidad de detonación.
Resultado.dcj=S_dCJ*Resultado.d0/(1.0+T_dCJ*Resultado.d0);

// Coeficiente Adiabático.
Resultado.CoefAdiabat=Resultado.d0/(Resultado.dcj-Resultado.d0);
// 
}



/* CargarDatosReactivos: --------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción:   
-------------------------------------------------------------------------*/

void Explosivo::CargarDatosReactivos()
{
char Cadena[MAXCAD];

FILE *f;

f=fopen(__REACTIVO_DAT,"r");        // Error
for(int i=0;i<NumReact;i++)
	{
	do
		{
		fgets(Cadena,MAXCAD,f);   
		}
	while(!feof(f) &&
			strncmp(Cadena,Reactivo[i].Nombre,(strlen(Reactivo[i].Nombre)-1)));

	switch((!feof(f)) ? SI:NO)
		{
		case SI:
      	/* Si no se ha llegado al final del archivo.
		  	 y las cadenas coinciden se ha encontrado el compuesto.*/

			// Carga la fórmula.
			fgets(Cadena,MAXCAD,f);   			
			// Elimina el caracter intro.
			Cadena[(strlen(Cadena))-1]='\0'; 
			strcpy(Reactivo[i].Formula,Cadena);
			Reactivo[i].PesoMol=CalcPMol(Reactivo[i].Formula);
			Reactivo[i].BO=CalcBO(BO_PRODBO,Reactivo[i].Formula);
			// Carga la energía de formación.
			fgets(Cadena,MAXCAD,f);   		
			Reactivo[i].Energia=strtod(Cadena,NULL);
			break;

		case NO:
         // Error no se ha encontrado el compuesto.
			PonerError(ERR_REACTI);
			strcpy(Reactivo[i].Formula,"?????");
			Reactivo[i].PesoMol=0.0;
			Reactivo[i].BO=0.0;		
			Reactivo[i].Energia=0.0;
			break;
         }
	rewind(f);
	}
fclose(f);
}      

/* VERFICARDENSIDAD: ------------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción:   Si la densidad de encartuchado [g/cm3] no está entre
						ciertos límites o no se ha introducido ningún valor
						se consigna el error y se proporciona una densidad
						por defecto. 
-------------------------------------------------------------------------*/


void Explosivo::VerificarDensidad()
{
if(Resultado.d0<DENS_MINIMA || Resultado.d0>DENS_MAXIMA)
	{
	// Mensaje de error se ha tomado una densidad por defecto.
	PonerError(AVI_DENSID);
   // Usa la densidad por defecto.
	Resultado.d0=DENS_DEFECT;
	}

}


/* VERFICARESULTADOS: -----------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción:  Analiza los resultados obtenidos en busca de posibles
				     errores.  
-------------------------------------------------------------------------*/

void Explosivo::VerificarResultados()
{
double acumulador=0.0;



if(Resultado.Qexplosion<=0.0)
	{
	PonerError(ERR_ENDOTE);
	Resultado.Texplosion=NRM_TEMP;
   Resultado.Ng=CalcNg(NRM_TEMP);
	}

if(!strlen(Resultado.Nombre)) PonerError(AVI_NOMBRE);
	
if(!Resultado.Ng)	PonerError(AVI_NO_GAS);

for (int i=0;i<MAX_PRODUCTOS;i++)
	{
	acumulador+=TablaProd[i].Moles;
	}

if(!acumulador) PonerError(AVI_NO_PRO);
}



/* CALCEXPLOSIVO -----------------------------------------------------
	Parámetros:
		Ninguno.
	Resultado:
		Ninguno.
	Descripción:    Enlaza todas las funciones anteriores para resolver
						el problema completo, e constituye el algoritmo de
						resolución.  
--------------------------------------------------------------------*/


void Explosivo::CalcResultado()
{
// Previa...
BorrarError();
VerificarDensidad();
CargarDatosReactivos();
if(!TomarError(ERR_REACTI))
	{
	// Si se han encontrado todos los reactivos...
	CalcFormula_1kg();
	CalcEnergia_Interna();
	Resultado.BO=CalcBO(BO_PRODBO,Resultado.Formula_1kg);
	CalcMolesProductos();

	switch((CalcBO(BO_PRODBO,Resultado.Formula_1kg)>=0.0)? SI : NO)
   	{
		case SI:
			/* Explosivo excedentario o equilibrado en oxígeno.
            Se produce oxígeno (si no es equilibrado).    */
			CalcQexplosion();
			CalcTexplosion();
			break;

		case NO:
			/* Explosivo deficitario en oxígeno.
			   No hay oxígeno libre.            */ 
			CalcTexpMolesProductos();
			break;
      }     	
	Resultado.Ng=CalcNg(Resultado.Texplosion);
	CalcM();
   CalcParamAdic();
	CalcKamletJacobs();
	}
VerificarResultados();
}

/*--------------------------------------------------------------------------
  Fin del Archivo:   	   CALCULOS.H
--------------------------------------------------------------------------*/
