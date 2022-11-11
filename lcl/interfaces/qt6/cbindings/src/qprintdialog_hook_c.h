//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTDIALOG_HOOK_C_H
#define QPRINTDIALOG_HOOK_C_H

#include "qprintdialog_hook.h"

C_EXPORT QPrintDialog_hookH QPrintDialog_hook_Create(QObjectH handle);
C_EXPORT void QPrintDialog_hook_Destroy(QPrintDialog_hookH handle);
C_EXPORT void QPrintDialog_hook_hook_accepted(QPrintDialog_hookH handle, QHookH hook);

#endif
