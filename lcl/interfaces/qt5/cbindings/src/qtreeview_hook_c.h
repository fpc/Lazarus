//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTREEVIEW_HOOK_C_H
#define QTREEVIEW_HOOK_C_H

#include "qtreeview_hook.h"

C_EXPORT QTreeView_hookH QTreeView_hook_Create(QObjectH handle);
C_EXPORT void QTreeView_hook_Destroy(QTreeView_hookH handle);
C_EXPORT void QTreeView_hook_hook_expanded(QTreeView_hookH handle, QHookH hook);
C_EXPORT void QTreeView_hook_hook_collapsed(QTreeView_hookH handle, QHookH hook);

#endif
