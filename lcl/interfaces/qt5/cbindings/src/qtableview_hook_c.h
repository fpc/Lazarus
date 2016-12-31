//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABLEVIEW_HOOK_C_H
#define QTABLEVIEW_HOOK_C_H

#include "qtableview_hook.h"

C_EXPORT QTableView_hookH QTableView_hook_Create(QObjectH handle);
C_EXPORT void QTableView_hook_Destroy(QTableView_hookH handle);

#endif
