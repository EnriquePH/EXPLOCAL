# Anexo C: Datos de los recursos



*Listados de los ficheros de recursos originales de Explocal (1996): menús, cadenas de texto, diálogos y versión de la aplicación para Windows 3.1.*

## C.1 CÓDIGO DEL ARCHIVO DE RECURSOS PRINCIPAL (EXPLOCAL.RC)

```c
/*_____________________________________________________________________
Archivo: EXPLOCAL.RC
Proyecto: EXPLOCAL.PRJ
Autor: Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
Proyecto de Fin de Carrera.
Especialidad: Laboreo y Explosivos.
Descripción: Archivo de recursos del proyecto.
_____________________________________________________________________*/
// Inclusión de las macros.
#if !defined (__EXPLOCAL_H)
#include "explocal.h"
#endif
// Icono de la aplicación: Explocal.
ICONO_APLIC ICON "aplic.ico"
// Icono de las ventanas hija de la aplicación MDI.
ICONO_HIJAS ICON "hijas.ico"
#if !defined (__DISCO_ICO)
ICONO_DISCO ICON "disco.ico"
#define __DISCO_ICO
#endif
MAPABITS_1 BITMAP "explocal.bmp"
// Inclusión de diálogos.
#include "kfiledia.dlg"
#include "kstdwnds.dlg"
#include "kinputdi.dlg"
#include "dialogos.dlg"
// Inclusión del menú y teclas aceleradoras asociadas.
#include "menu.rc"
// Inclusión de la tabla de cadenas.
#include "tblcaden.rc"
// Inclusión de la versión.
#include "version.rc"
/*_____________________________________________________________________
Fin del Archivo: EXPLOCAL.RC
_____________________________________________________________________*/
```

## C.2 ARCHIVOS DE DIÁLOGOS

```c

```

### C.2.1 Archivo kfiledia.dlg

```c
/*__________________________________________________________________________
Archivo: KFILEDIA.DLG
Proyecto: EXPLOCAL.PRJ
Autor: Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
Proyecto de Fin de Carrera.
Especialidad: Laboreo y Explosivos.
Descripción: Versión personalizada y traducida del archivo
FILEDIAL.DLG de la librería ObjectWindows. Introduce los recursos para cargar los diálogos de a
    pertura y cierre de archivos
__________________________________________________________________________*/
#if !defined (__EXPLOCAL_H)
#include "explocal.h"
#endif
#if !defined (__DISCO_ICO)
ICONO_DISCO ICON "disco.ico"
#define __DISCO_ICO
#endif
SD_FILEOPEN DIALOG 28, 28, 154, 214
STYLE WS_TILED | WS_CAPTION | WS_SYSMENU | DS_SETFONT | DS_MODALFRAME
CLASS "bordlg"
CAPTION "Abrir"
FONT 8, "Helv"
BEGIN
LTEXT "&Nombre del archivo:", -1, 44, 12, 66, 8
CONTROL "", ID_FNAME, "EDIT", ES_OEMCONVERT | WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP | 
    ES_AUTOHSCROLL, 41, 22, 101, 12
CONTROL "", ID_FLIST, "LISTBOX", WS_CHILD | WS_VISIBLE | WS_TABSTOP | LBS_STANDARD, 9, 84, 64, 
    82
CONTROL "", ID_DLIST, "LISTBOX", WS_CHILD | WS_VISIBLE | WS_TABSTOP | LBS_STANDARD, 81, 84, 64,
     82
CONTROL "", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 98, 183, 36
    , 24
CONTROL "", IDOK, "BorBtn", BS_DEFPUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 20, 183, 36,
     24
LTEXT "Directorio:", -1, 7, 50, 33, 8
LTEXT "", ID_FPATH, 40, 50, 106, 8
LTEXT "&Archivos:", -1, 9, 72, 30, 10
LTEXT "&Directorios:", -1, 79, 72, 40, 10
CONTROL "", 4096, "BorShade", 2 | WS_CHILD | WS_VISIBLE, 1, 174, 152, 2
ICON "ICONO_DISCO", -1, 7, 12, 18, 20, WS_CHILD | WS_VISIBLE
CONTROL "", 4097, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 5, 71, 144, 99
CONTROL "", 4098, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 35, 10, 113, 29
END
SD_FILESAVE DIALOG 51, 43, 139, 148
STYLE WS_TILED | WS_CAPTION | WS_SYSMENU | DS_SETFONT | DS_MODALFRAME
CLASS "bordlg"
CAPTION "Guardar como..."
FONT 8, "Helv"
BEGIN
CONTROL "", ID_FNAME, "EDIT", ES_OEMCONVERT | WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP | 
    ES_AUTOHSCROLL, 6, 23, 101, 12
CONTROL "", ID_DLIST, "LISTBOX", WS_CHILD | WS_VISIBLE | WS_TABSTOP | LBS_STANDARD, 6, 62, 64, 
    82
CONTROL "", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 90, 65, 36,
     24
CONTROL "", IDOK, "BorBtn", BS_DEFPUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 90, 111, 36,
     24
LTEXT "&Nombre de archivo:", -1, 8, 10, 64, 8
LTEXT "Directorio:", -1, 6, 38, 34, 8
LTEXT "", ID_FPATH, 40, 38, 92, 8
LTEXT "&Directorios:", -1, 6, 51, 64, 8
ICON "ICONO_DISCO", -1, 115, 11, 18, 20, WS_CHILD | WS_VISIBLE
CONTROL "", 4093, "BorShade", 3 | WS_CHILD | WS_VISIBLE, 76, 51, 3, 92
END
/*_____________________________________________________________________
Fin del Archivo: KFILEDIA.DLG
_____________________________________________________________________*/
```

### C.2.2 Archivo kstdwnd.dlg

```c
/*__________________________________________________________________________
Archivo: KSTWNDS.DLG
Proyecto: EXPLOCAL.PRJ
Autor: Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
Proyecto de Fin de Carrera.
Especialidad: Laboreo y Explosivos.
Descripción: Versión personalizada y traducida del archivo STWNDS.DLG de la librería ObjectWind
    ows. Introduce los recursos para cargar los diálogos de edición de textos.
__________________________________________________________________________*/
#if !defined (__EXPLOCAL_H)
#include "explocal.h"
#endif
SD_SEARCH DIALOG 50, 65, 205, 60
STYLE WS_POPUP | WS_CLIPSIBLINGS | WS_CAPTION | WS_SYSMENU | DS_SETFONT | DS_MODALFRAME
CLASS "bordlg"
CAPTION "Buscar"
FONT 8, "Helv"
BEGIN
CONTROL "&Buscar:", 205, "STATIC", WS_CHILD | WS_VISIBLE, 17, 11, 27, 9
CONTROL "", ID_SEARCHTEXT, "EDIT", WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP | ES_AUTOHSCR
    OLL, 49, 9, 149, 12
CONTROL "Mayúsculas/Minúsculas.", ID_CASESENSITIVE, "BorCheck", BS_AUTOCHECKBOX | WS_CHILD | WS
    _VISIBLE | WS_TABSTOP, 11, 35, 89, 10
CONTROL "", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 113, 28, 36
    , 24
CONTROL "", IDOK, "BorBtn", BS_DEFPUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 160, 28, 36,
     24
CONTROL "", 206, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 8, 32, 96, 15
END
SD_REPLACE DIALOG 58, 64, 208, 86
STYLE WS_POPUP | WS_CLIPSIBLINGS | WS_CAPTION | WS_SYSMENU | DS_SETFONT | DS_MODALFRAME
CLASS "bordlg"
CAPTION "Reemplazar"
FONT 8, "Helv"
BEGIN
CONTROL "&Buscar:", 205, "STATIC", WS_CHILD | WS_VISIBLE, 11, 9, 53, 7
CONTROL "", ID_SEARCHTEXT, "EDIT", WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP | ES_AUTOHSCR
    OLL, 65, 6, 137, 12
CONTROL "Reemplazar por:", 206, "STATIC", WS_CHILD | WS_VISIBLE, 10, 27, 55,8
CONTROL "", ID_REPLACETEXT, "EDIT", WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP | ES_AUTOHSC
    ROLL, 65, 25, 137, 12
CONTROL "Sensible a Máy/Mín.", ID_CASESENSITIVE, "BUTTON", WS_CHILD | WS_VISIBLE | WS_GROUP | W
    S_TABSTOP | BS_AUTOCHECKBOX, 15, 46, 77, 8
CONTROL "&Todas las apariciones.", ID_REPLACEALL, "BUTTON", WS_CHILD | WS_VISIBLE | WS_TABSTOP 
    | BS_AUTOCHECKBOX, 15, 57, 84, 8
CONTROL "&Cursor en el reemplazo.", ID_PROMPTONREPLACE, "BUTTON", WS_CHILD | WS_VISIBLE | WS_TA
    BSTOP | BS_AUTOCHECKBOX, 15, 68, 89, 8
CONTROL "Button", IDOK, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 162, 50, 
    36, 24
CONTROL "Button", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 120, 
    50, 36, 24
CONTROL "", 207, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 11, 44, 96, 35
CONTROL "", 207, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 114, 45, 91, 35
END
/*_____________________________________________________________________
Fin del Archivo: KSTDWNDS.DLG
_____________________________________________________________________*/
```

### C.2.3 Archivo kinputdi.dlg

```c
/*__________________________________________________________________________
Archivo: KINPUT.DLG
Versión personalizada y traducida del archivo INPUTDIA.DLG de ObjectWindows
__________________________________________________________________________*/
SD_INPUTDIALOG DIALOG 22, 27, 149, 76
STYLE WS_POPUP | WS_CAPTION | DS_SETFONT
CLASS "bordlg"
FONT 8, "Helv"
BEGIN
CONTROL "", ID_INPUT, "EDIT", WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP | ES_AUTOHSCROLL, 
    11, 19, 126, 12
CONTROL "", IDOK, "BorBtn", BS_DEFPUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 19, 45, 36, 
    24
CONTROL "", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 82, 44, 36,
     24
LTEXT "", ID_PROMPT, 9, 7, 130, 8
CONTROL "", 4092, "BorShade", 2 | WS_CHILD | WS_VISIBLE, 4, 36, 140, 3
END
/*_____________________________________________________________________
Fin del Archivo: KINPUTDI.DLG
_____________________________________________________________________*/
```

### C.2.4 Archivo dialogos.dlg

```c
/*__________________________________________________________________________
Archivo: DIALOGOS.DLG
Proyecto: EXPLOCAL.PRJ
Autor: Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
Proyecto de Fin de Carrera.
Especialidad: Laboreo y Explosivos.
Descripción: Características, dimensiones y aspecto de los diálogos de entrada de datos y manej
    o del cálculo de explosivos.
__________________________________________________________________________*/
#if !defined (EXPLOCAL_H_DEFINIDO)
#include "explocal.h"
#endif
DIALOGO_1 DIALOG PRELOAD FIXED DISCARDABLE IMPURE 7, 14, 352, 184
STYLE DS_MODALFRAME | WS_POPUP | WS_CAPTION | WS_SYSMENU
CLASS "bordlg"
CAPTION "Composición cualitativa de la mezcla explosiva"
FONT 9, "Arial"
BEGIN
CONTROL "", ID1_LISTA1, "LISTBOX", LBS_STANDARD | LBS_MULTIPLESEL | LBS_HASSTRINGS | LBS_USETAB
    STOPS | LBS_DISABLENOSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 10, 20, 150, 106
PUSHBUTTON ">>", ID1_BTPONE, 169, 22, 14, 28, BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTO
    P
PUSHBUTTON "", ID1_BTINIC, 169, 60, 14, 14
PUSHBUTTON "<<", ID1_BTQUIT, 169, 87, 14, 28, BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTO
    P
CONTROL "", ID1_LISTA2, "LISTBOX", LBS_STANDARD | LBS_MULTIPLESEL | LBS_HASSTRINGS | WS_CHILD |
     WS_VISIBLE | WS_TABSTOP, 190, 20, 150, 104
CONTROL "", IDOK, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_DISABLED | WS_TABSTOP, 2
    16, 146, 36, 21
CONTROL "", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 257, 146, 3
    6, 22
CONTROL "Button", IDHELP, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 299, 14
    6, 36, 21
CONTROL "", 241, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 209, 141, 132, 33
CONTROL "", ID1_LISTA3, "LISTBOX", LBS_NOTIFY | LBS_HASSTRINGS | WS_CHILD | WS_VISIBLE | WS_BOR
    DER | WS_VSCROLL, 10, 135, 181, 45
LTEXT "Lista de reactivos:", -1, 15, 8, 60, 8
LTEXT "Reactivos seleccionados:", -1, 196, 8, 85, 8
LTEXT "Datos del reactivo:", -1, 15, 123, 62, 8
END
DIALOGO_2 DIALOG 89, 1, 179, 193
STYLE DS_MODALFRAME | WS_POPUP | WS_CAPTION | WS_SYSMENU | DS_SETFONT
CLASS "bordlg"
CAPTION "Composición cuantitativa de la mezcla"
FONT 9, "Arial"
BEGIN
CONTROL "", ID2_EDITA1, "EDIT", ES_LEFT | ES_OEMCONVERT | WS_CHILD | WS_VISIBLE | WS_BORDER | W
    S_TABSTOP, 11, 132, 116, 10
CONTROL "", IDOK, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_DISABLED | WS_TABSTOP, 1
    0, 165, 36, 21
CONTROL "", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 71, 165, 36
    , 21
CONTROL "Button", IDHELP, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 135, 16
    5, 36, 21
CONTROL "", ID2_LISTA1, "LISTBOX", LBS_STANDARD | LBS_HASSTRINGS | LBS_USETABSTOPS | WS_CHILD |
     WS_VISIBLE | WS_TABSTOP, 9, 19, 118, 102
CONTROL "", ID2_LISTA2, "LISTBOX", LBS_NOTIFY | LBS_HASSTRINGS | LBS_USETABSTOPS | WS_CHILD | W
    S_VISIBLE | WS_BORDER | WS_VSCROLL | WS_TABSTOP, 133, 19, 36, 104
CONTROL "", ID2_LISTA3, "LISTBOX", LBS_NOTIFY | LBS_USETABSTOPS | WS_CHILD | WS_VISIBLE | WS_BO
    RDER | WS_VSCROLL, 132, 133, 37, 10
LTEXT "Reactivo:", -1, 24, 9, 30, 8
LTEXT "(%) ", -1, 145, 9, 9, 8
LTEXT "Total: (%)", -1, 137, 123, 30, 8
CONTROL "", 272, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 6, 122, 166, 30
CONTROL "", 273, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 6, 8, 166, 110
CONTROL "", 271, "BorShade", 2 | WS_CHILD | WS_VISIBLE, 2, 156, 175, 2
CONTROL "INTRO:Invisible", ID2_BTINTR, "BUTTON", BS_DEFPUSHBUTTON | WS_CHILD | NOT WS_VISIBLE, 
    66, -1, 65, 14
END
DIALOGO_3 DIALOG 8, 17, 192, 128
STYLE DS_MODALFRAME | WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU
CLASS "bordlg"
CAPTION "Introducir datos adicionales"
FONT 9, "Arial"
BEGIN
EDITTEXT ID3_EDITA1, 26, 30, 140, 12, ES_LEFT | WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP
EDITTEXT ID3_EDITA2, 26, 61, 140, 12, ES_LEFT | WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP
CONTROL "Button", IDOK, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 15, 100, 
    32, 20
CONTROL "Button", IDCANCEL, "BorBtn", WS_CHILD | WS_VISIBLE | WS_GROUP | WS_TABSTOP, 80, 100, 3
    2, 20
CONTROL "Button", IDHELP, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 140, 10
    0, 36, 21
CONTROL "INTRO:Invisible", ID3_BTINTR, "BUTTON", BS_DEFPUSHBUTTON | WS_CHILD | NOT WS_VISIBLE |
     WS_TABSTOP, 110, 17, 57, 9
CONTROL "", 103, "BorShade", 2 | WS_CHILD | WS_VISIBLE, 3, 90, 185, 2
CONTROL "Nombre del explosivo:", -1, "STATIC", SS_LEFT | WS_CHILD | WS_VISIBLE, 30, 20, 75, 8
CONTROL "Densidad inicial: do(g/cm3)", -1, "STATIC", SS_LEFT | WS_CHILD | WS_VISIBLE, 30, 50, 9
    7, 10
CONTROL "", 106, "BorShade", 32769 | WS_CHILD | WS_VISIBLE | WS_GROUP, 11, 13, 170, 70
END
DIALOGO_4 DIALOG 35, 18, 236, 204
STYLE DS_MODALFRAME | WS_POPUP | WS_CAPTION | WS_SYSMENU
CLASS "Bordlg"
CAPTION "Modificar la lista de reactivos"
FONT 9, "Arial"
BEGIN
CONTROL "", ID4_LISTA1, "LISTBOX", LBS_STANDARD | LBS_HASSTRINGS | LBS_DISABLENOSCROLL | WS_CHI
    LD | WS_VISIBLE, 10, 19, 160, 110
CONTROL "", IDOK, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 192, 10, 32, 20
CONTROL "Button", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 192, 
    45, 32, 20
CONTROL "Button", IDHELP, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 191, 80
    , 36, 21
CONTROL "Aña&dir", ID4_BTANAD, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 19
    2, 120, 36, 24
CONTROL "&Eliminar", ID4_BTELIM, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_DISABLED 
    | WS_TABSTOP, 192, 158, 37, 23
LISTBOX ID4_LISTA2, 10, 149, 160, 43
LTEXT "&Lista de reactivos:", -1, 20, 7, 60, 8
LTEXT "&Datos del reactivo:", -1, 18, 137, 60, 8
CONTROL "", 104, "BorShade", 3 | WS_CHILD | WS_VISIBLE, 180, 0, 1, 199
END
DIALOGO_5 DIALOG 48, 21, 186, 155
STYLE DS_MODALFRAME | WS_POPUP | WS_CAPTION | WS_SYSMENU
CLASS "bordlg"
CAPTION "Introducir datos del reactivo"
FONT 9, "Arial"
BEGIN
CONTROL "", ID5_EDITA1, "EDIT", ES_LEFT | ES_OEMCONVERT | WS_CHILD | WS_VISIBLE | WS_BORDER | W
    S_TABSTOP, 11, 24, 164, 12
CONTROL "", ID5_EDITA2, "EDIT", ES_LEFT | ES_OEMCONVERT | WS_CHILD | WS_VISIBLE | WS_BORDER | W
    S_TABSTOP, 11, 54, 164, 12
CONTROL "", ID5_EDITA3, "EDIT", ES_LEFT | ES_OEMCONVERT | WS_CHILD | WS_VISIBLE | WS_BORDER | W
    S_TABSTOP, 11, 86, 164, 12
CONTROL "", IDOK, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 13, 121, 36, 24
CONTROL "", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 73, 121, 40
    , 25
CONTROL "", IDHELP, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 131, 121, 36,
     21
LTEXT "&Nombre:", -1, 15, 15, 27, 8
CONTROL "", 104, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 6, 11, 173, 94
LTEXT "&Fórmula:", -1, 15, 43, 30, 8
LTEXT "&Energía interna (kcal/mol):", -1, 16, 75, 90, 8
CONTROL "", 106, "BorShade", 2 | WS_CHILD | WS_VISIBLE, 1, 113, 184, 2
CONTROL "INTRO:Invisible", ID5_BTINTR, "BUTTON", BS_DEFPUSHBUTTON | WS_CHILD | NOT WS_VISIBLE, 
    105, 5, 65, 14
END
DIALOGO_6 DIALOG 36, 35, 168, 116
STYLE DS_MODALFRAME | WS_POPUP | WS_CAPTION | WS_SYSMENU
CLASS "bordlg"
CAPTION "Preferencias"
BEGIN
EDITTEXT ID6_EDITA1, 11, 21, 143, 12, ES_LEFT | WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP
CONTROL "Julios (J)", ID6_BRJULS, "BorRadio", BS_AUTORADIOBUTTON | WS_CHILD | WS_VISIBLE, 78, 4
    9, 48, 10
CONTROL "Calorías (cal)", ID6_BRCALS, "BorRadio", BS_AUTORADIOBUTTON | WS_CHILD | WS_VISIBLE | 
    WS_TABSTOP, 78, 59, 52, 10
CONTROL "", IDOK, "BorBtn", BS_DEFPUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 14, 87, 32, 
    20
CONTROL "Button", IDCANCEL, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 68, 8
    7, 32, 20
CONTROL "Button", IDHELP, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 120, 87
    , 32, 20
CONTROL "", 103, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 8, 10, 153, 28
LTEXT "Directorio de datos:", -1, 16, 12, 65, 7
LTEXT "Unidades:", -1, 40, 49, 38, 8
CONTROL "", 103, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 34, 46, 100, 26
CONTROL "", 105, "BorShade", 2 | WS_CHILD | WS_VISIBLE, 2, 79, 163, 1
END
DIALOGO_ACERCA DIALOG 90, 17, 178, 187
STYLE DS_MODALFRAME | WS_POPUP | WS_CAPTION | WS_SYSMENU
CLASS "bordlg"
CAPTION "Acerca de Explocal"
FONT 9, "Arial"
BEGIN
CONTROL "", IDOK, "BorBtn", BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 71, 146, 35, 20
ICON "ICONO_APLIC", -1, 21, 19, 18, 17
CONTROL "", 101, "BorShade", 32769 | WS_CHILD | WS_VISIBLE, 64, 140, 50, 35
LTEXT "Explocal 1.0", -1, 68, 25, 41, 8
CONTROL "- Cálculo de las principales características teóricas de los explosivos.", -1, "STATIC
    ", SS_LEFT | WS_CHILD | WS_VISIBLE, 16, 41, 140, 17
CONTROL "", 102, "BorShade", 32774 | WS_CHILD | WS_VISIBLE, 14, 12, 149, 122
LTEXT "- E.T.S.I de Minas de Madrid.", -1, 16, 70, 100, 10
CONTROL "- Según la norma UNE 31-002-94.", -1, "STATIC", SS_LEFT | WS_CHILD | WS_VISIBLE, 15, 5
    9, 109, 8
LTEXT "- Proyecto Fin de Carrera.", -1, 16, 81, 85, 8
CONTROL "- Departamento de Ingeniería Química y Combustibles.", -1, "STATIC", SS_LEFT | WS_CHIL
    D | WS_VISIBLE | WS_GROUP, 17, 93, 137, 17
LTEXT "Enrique Pérez Herrero Noviembre - 1996", -1, 17, 122, 143, 8
END
/*_____________________________________________________________________
Fin del Archivo: DIALOGOS.DLG
_____________________________________________________________________*/
```

## C.3 Archivo de menú y teclas aceleradoras ( MENU.RC )

```c
/*__________________________________________________________________________
Archivo: MENU.RC
Proyecto: EXPLOCAL.PRJ
Autor: Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
Proyecto de Fin de Carrera.
Especialidad: Laboreo y Explosivos.
Descripción: Menú de la aplicación y teclas aceleradoras.
__________________________________________________________________________*/
#if !defined (EXPLOCAL_H_DEFINIDO)
#include "explocal.h"
#endif
MENU_1 MENU
BEGIN
POPUP "&Archivo"
BEGIN
MENUITEM "&Nuevo", CM_FILENEW
MENUITEM "&Abrir...", CM_FILEOPEN
MENUITEM "&Guardar", CM_FILESAVE, GRAYED
MENUITEM "G&uardar como...", CM_FILESAVEAS, GRAYED
MENUITEM SEPARATOR
MENUITEM "&Imprimir...", CM_IMPRIMIR, GRAYED
MENUITEM SEPARATOR
MENUITEM "&Preferencias...", CM_PREFEREN
MENUITEM SEPARATOR
MENUITEM "&Salir\aAlt+F4", CM_EXIT
END
POPUP "&Edición"
BEGIN
MENUITEM "&Deshacer\aCtrl+Z", CM_EDITUNDO, GRAYED
MENUITEM SEPARATOR
MENUITEM "&Cortar\aCtrl+X", CM_EDITCUT, GRAYED
MENUITEM "C&opiar\aCtrl+C", CM_EDITCOPY, GRAYED
MENUITEM "&Pegar\aCtrl+V", CM_EDITPASTE, GRAYED
MENUITEM SEPARATOR
MENUITEM "&Borrar\aSupr", CM_EDITDELETE, GRAYED
MENUITEM "Bor&rar todo\aCtrl+Supr", CM_EDITCLEAR, GRAYED
MENUITEM SEPARATOR
MENUITEM "&Seleccionar todo\aCtrl+E", CM_EDITSELT, GRAYED
END
POPUP "&Buscar"
BEGIN
MENUITEM "&Buscar...", CM_EDITFIND, GRAYED
MENUITEM "&Repetir búsqueda\aF3", CM_EDITFINDNEXT, GRAYED
MENUITEM "R&eemplazar...", CM_EDITREPLACE, GRAYED
END
POPUP "&Explosivo"
BEGIN
MENUITEM "&Nuevo explosivo...\aF5", CM_NUEVOEXP
MENUITEM "&Abrir explosivo...\aF6", CM_ABRIREXP
MENUITEM "&Guardar explosivo", CM_GUARDEXP, GRAYED
MENUITEM "G&uardar explosivo como...", CM_GCOMOEXP, GRAYED
MENUITEM "&Modificar composición...\aF7", CM_COMPOSIC, GRAYED
MENUITEM SEPARATOR
MENUITEM "M&odificar lista de reactivos...", CM_MODIFICA
END
POPUP "&Ventana"
BEGIN
MENUITEM "&Cascada\aMayús+F5", CM_CASCADECHILDREN, GRAYED
MENUITEM "&Mosaico\aMayús+F4", CM_TILECHILDREN, GRAYED
MENUITEM "&Organizar iconos", CM_ARRANGEICONS, GRAYED
MENUITEM SEPARATOR
MENUITEM "C&errar todo", CM_CLOSECHILDREN, GRAYED
END
POPUP "Ay&uda", HELP
BEGIN
MENUITEM "&Índice\aF1", CM_INDICEAY, HELP
MENUITEM SEPARATOR
MENUITEM "&Acerca de Explocal...", CM_ACERCADE, HELP
END
END
//Teclas aceleradoras asociadas al menú: MENU_1
MENU_1 ACCELERATORS LOADONCALL MOVEABLE DISCARDABLE
BEGIN
"Z", CM_EDITUNDO, VIRTKEY, CONTROL
"X", CM_EDITCUT, VIRTKEY, CONTROL
"C", CM_EDITCOPY, VIRTKEY, CONTROL
"V", CM_EDITPASTE, VIRTKEY, CONTROL
"E", CM_EDITSELT, VIRTKEY, CONTROL
VK_F1, CM_INDICEAY, VIRTKEY
VK_F3, CM_EDITFINDNEXT, VIRTKEY
VK_F4, CM_TILECHILDREN, VIRTKEY, SHIFT
VK_F5, CM_NUEVOEXP, VIRTKEY
VK_F5, CM_CASCADECHILDREN, VIRTKEY, SHIFT
VK_F6, CM_ABRIREXP, VIRTKEY
VK_F7, CM_COMPOSIC, VIRTKEY
VK_DELETE, CM_EDITDELETE, VIRTKEY
VK_DELETE, CM_EDITCLEAR, VIRTKEY, CONTROL
END
/*_____________________________________________________________________
Fin del Archivo: MENU.RC
_____________________________________________________________________*/
```

## C.4 Archivo DE TABLA DE CADENAS DE TEXTO (TBLCADEN.RC)

```c
/*__________________________________________________________________________
Archivo: TBLCADEN.RC
Proyecto: EXPLOCAL.PRJ
Autor: Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
Proyecto de Fin de Carrera.
Especialidad: Laboreo y Explosivos.
Descripción: Tablas de cadenas empleadas por EXPLOCAL.CPP se agrupan y se cargan como recurso p
    ara facilitar la traducción
__________________________________________________________________________*/
#if !defined (__EXPLOCAL_H)
#include "explocal.h"
#endif
STRINGTABLE
BEGIN
IDS_TITULO, "Explocal 1.0"
IDS_ERROR1, "Error de Archivo:"
IDS_ERROR2, "No se ha encontrado el Archivo:\n\n REACTIVOS.DAT."
IDS_ERROR3, "Demasiados Reactivos."
IDS_ERROR4, "Elimine algunos de la Caja de Selección."
IDS_ERROR5, "Error de Sistema"
IDS_ERROR6, "No se puede cargar la libería\n de controles: BWCC.DLL"
IDS_ERROR7, "El archivo no contiene\n datos de ningún explosivo."
END
STRINGTABLE
BEGIN
IDS_NOMBRE, "Nombre: "
IDS_FORMUL, "Fórmula: "
IDS_ENEFOR, "Energía de formación (298K): "
IDS_PSOMOL, "Peso molecular: "
IDS_BALOXI, "Balance de oxígeno: "
END
STRINGTABLE
BEGIN
IDS_CARACT, "\r\n-CARACTERÍSTICAS DE LA MEZCLA EXPLOSIVA:\r\n\r\n"
IDS_PRNOMB, "\t-Nombre del explosivo:\r\n\t\t %s\r\n"
IDS_PRCOMP, "\r\n-COMPOSICIÓN DE LA MEZCLA EXPLOSIVA:\r\n"
IDS_NUMERO, "\r\n (%.0f)"
IDS_RNOMBR, "\t- %s:"
IDS_RFORMU, " %s\r\n"
IDS_RENERG, "\t- Eo=%.3f %s/mol\r\n"
IDS_RPSOMO, "\t- Pm=%.3f g/mol\r\n"
IDS_RPORCE, "\t- Peso=%.3f %%\r\n"
IDS_RBALOX, "\t- BO=%+.3f %%\r\n"
IDS_PRDENS, "\t-Densidad de encartuchado:\r\n\t\t do=%.3f g/cm³\r\n"
IDS_PRODEX, "\r\n-PRODUCTOS DE EXPLOSIÓN:\r\n"
IDS_REACIO, "\r\n-REACCIÓN A VOLUMEN CONSTANTE:\r\n\r\n"
IDS_PRODUC, "\t- %s: "
IDS_PMOLES, "%.4f mol/kg\r\n"
IDS_EFORMA, "\t- Ef=%.3f %s/mol\r\n"
IDS_PRENER, "\t-Energía interna:\r\n\t\t Eo=%.3f %s/kg\r\n"
IDS_PRFORM, "\t-Fórmula para 1kg de explosivo:\r\n"
IDS_PRFOR1, "\t\t%2s"
IDS_PRFOR2, "=%.4f mol/kg \r\n"
IDS_PRBALO, "\t-Balance de Oxígeno:\r\n\t\t BO=%+.3f %%\r\n"
IDS_PREXCE, "\t-El explosivo es excedentario en Oxígeno.\r\n"
IDS_PREQUI, "\t-El explosivo está equilibrado en Oxígeno.\r\n"
IDS_PRDEFI, "\t-El explosivo es deficitario en Oxígeno.\r\n"
IDS_PRQEXP, "\t-Calor de explosión:\r\n\t\t Q=%.0f %s/kg\r\n"
IDS_PRTEXP, "\t-Temperatura de explosión:\r\n\t\t T=%.0f K\r\n"
IDS_PRMOLE, "\t-Moles de productos gaseosos:\r\n\t\t ng=%.2f mol/kg\r\n"
IDS_PRMASA, "\t-Masa molecular media gases:\r\n\t\t M=%.3f g/mol\r\n"
IDS_PRVOLG, "\t-Volumen de gases en C.N.:\r\n\t\t Vcn=%.3f m³/kg\r\n"
IDS_PRESPE, "\t-Energía específica.:\r\n\t\t f=%.3f %s/kg\r\n"
IDS_ESTIMA, "\r\n-PARÁMETROS DE DETONACIÓN: (Fórmulas de Kamlet)\r\n\r\n"
IDS_PRPRFI, "\t-Parámetro Fi.:\r\n\t\t Ø=%.0f J·(mol/g)½ \r\n"
IDS_PRPRES, "\t-Presión de detonación:\r\n\t\t P=%.0f GPa\r\n"
IDS_PRVELD, "\t-Velocidad de detonación:\r\n\t\t D=%.0f m/s\r\n"
IDS_PRDECJ, "\t-Densidad de detonación:\r\n\t\t dcj=%.3f g/cm³\r\n"
IDS_PRCOAD, "\t-Coeficiente Adiabático:\r\n\t\t L=%.3f \r\n"
IDS_ERRRRR, "\r\n-ERRORES EN EL PROCESO DE CÁLCULO\r\n\r\n"
END
STRINGTABLE
BEGIN
IDS_ELIMIN,"Eliminar"
IDS_ESTECP,"¿Está seguro de querer eliminar el reactivo? %s"
END
/*_____________________________________________________________________
Fin del Archivo: TBLCADEN.RC
_____________________________________________________________________*/
```

## C.5 ARCHIVO DE LA VERSIÓN (VERSION.RC)

```c
/*_____________________________________________________________________
Archivo: VERSION.RC
Proyecto: EXPLOCAL.PRJ
Autor: Enrique Pérez Herrero, E.T.S.I de Minas de Madrid.
Proyecto de Fin de Carrera.
Especialidad: Laboreo y Explosivos.
Descripción: Versión de la aplicación.
Archivo de recursos del proyecto.
_____________________________________________________________________*/
VERSION VERSIONINFO LOADONCALL MOVEABLE
FILEVERSION 1, 0, 0, 4055
PRODUCTVERSION 1, 0, 0, 4055
FILEFLAGSMASK VS_FF_DEBUG | VS_FF_PRERELEASE | VS_FF_PATCHED | VS_FF_PRIVATEBUILD | VS_FF_INFOI
    NFERRED | VS_FF_SPECIALBUILD
FILEOS VOS__WINDOWS16
FILETYPE VFT_APP
BEGIN
BLOCK "StringFileInfo"
BEGIN
BLOCK "040904E4"
BEGIN
VALUE "Comments", 29264, 31087, 25445, 28532, 25632, 8293, 26950, 8302, 25956, 17184, 29281, 25
    970, 24946, 8250, 28229, 26994, 30065, 8293, 59728, 25970, 8314, 25928, 29298, 29285, 111
VALUE "CompanyName", "Kikesoft® - PROTEvS\000"
VALUE "FileDescription", 57667, 25452, 27765, 8303, 25956, 27680, 29537, 28704, 26994, 25454, 2
    8777, 27745, 29541, 25376, 29281, 25441, 25972, 60786, 29811, 25449, 29537, 29728, 62309, 2
    6994, 24931, 8307, 25956, 29984, 8302, 30821, 27760, 29551, 30313, 11887
VALUE "FileVersion", "1.00\000"
VALUE "InternalName", "xplcal\000"
VALUE "LegalCopyright", "Copyright© E.T.S.I de Minas de Madrid 1996.\000"
VALUE "OriginalFilename", "explocal.exe\000"
VALUE "ProductName", "Explocal\000"
VALUE "ProductVersion", "1.00\000"
END
END
END
/*_____________________________________________________________________
Fin del Archivo: VERSION.RC
_____________________________________________________________________*/
```
