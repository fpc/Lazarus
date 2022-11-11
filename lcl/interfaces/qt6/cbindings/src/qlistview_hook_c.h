//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLISTVIEW_HOOK_C_H
#define QLISTVIEW_HOOK_C_H

#include "qlistview_hook.h"

C_EXPORT QListView_hookH QListView_hook_Create(QObjectH handle);
C_EXPORT void QListView_hook_Destroy(QListView_hookH handle);

#endif
