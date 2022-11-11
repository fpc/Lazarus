//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTPRINTDIALOG_HOOK_C_H
#define QABSTRACTPRINTDIALOG_HOOK_C_H

#include "qabstractprintdialog_hook.h"

C_EXPORT QAbstractPrintDialog_hookH QAbstractPrintDialog_hook_Create(QObjectH handle);
C_EXPORT void QAbstractPrintDialog_hook_Destroy(QAbstractPrintDialog_hookH handle);

#endif
