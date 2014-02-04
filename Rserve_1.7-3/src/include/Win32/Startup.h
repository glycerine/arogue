/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2003  The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef STARTUP_H_
#define STARTUP_H_

#include <R_ext/Boolean.h>	/* TRUE/FALSE */

#ifdef Win32
typedef int (*blah1) (char *, char *, int, int);
typedef void (*blah2) (char *, int);
typedef void (*blah3) ();
typedef void (*blah4) (char *);
typedef int (*blah5) (char *);
typedef void (*blah6) (int);
typedef enum {RGui, RTerm, LinkDLL} UImode;
#endif

/* Startup Actions */
typedef enum {
    SA_NORESTORE,/* = 0 */
    SA_RESTORE,
    SA_DEFAULT,/* was === SA_RESTORE */
    SA_NOSAVE,
    SA_SAVE,
    SA_SAVEASK,
    SA_SUICIDE
} SA_TYPE;

typedef struct
{
    Rboolean R_Quiet;
    Rboolean R_Slave;
    Rboolean R_Interactive;
    Rboolean R_Verbose;
    Rboolean LoadSiteFile;
    Rboolean LoadInitFile;
    Rboolean DebugInitFile;
    SA_TYPE RestoreAction;
    SA_TYPE SaveAction;
    unsigned long vsize;
    unsigned long nsize;
    unsigned long max_vsize;
    unsigned long max_nsize;
    unsigned long ppsize;
    int NoRenviron;

      /* Permanent copy of the command line arguments and the number
         of them passed to the application.
         These are populated via the routine R_set_command_line_arguments()
         called from R_common_command_line().
       */
    int    NumCommandLineArgs;
    char **CommandLineArgs;

#ifdef Win32
    char *rhome;               /* R_HOME */
    char *home;                /* HOME  */
    blah1 ReadConsole;
    blah2 WriteConsole;
    blah3 CallBack;
    blah4 message;
    blah5 yesnocancel;
    blah6 busy;
    UImode CharacterMode;
    Rboolean DebugMenuitem;
#endif
} structRstart;

typedef structRstart *Rstart;

void R_DefParams(Rstart);
void R_SetParams(Rstart);
void R_SetWin32(Rstart);
void R_SizeFromEnv(Rstart);
void R_common_command_line(int *, char **, Rstart);

void R_set_command_line_arguments(int argc, char **argv, Rstart Rp);

void setup_Rmainloop(void);

/* originally from Defn.h : */

void R_CleanUp(SA_TYPE, int, int);
void R_StartUp(void);

FILE *R_OpenInitFile(void);
FILE *R_OpenSysInitFile(void);
FILE *R_OpenSiteFile(void);

#endif
