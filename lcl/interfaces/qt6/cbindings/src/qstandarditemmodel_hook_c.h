//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTANDARDITEMMODEL_HOOK_C_H
#define QSTANDARDITEMMODEL_HOOK_C_H

#include "qstandarditemmodel_hook.h"

C_EXPORT QStandardItem_hookH QStandardItem_hook_Create(QObjectH handle);
C_EXPORT void QStandardItem_hook_Destroy(QStandardItem_hookH handle);
C_EXPORT QStandardItemModel_hookH QStandardItemModel_hook_Create(QObjectH handle);
C_EXPORT void QStandardItemModel_hook_Destroy(QStandardItemModel_hookH handle);
C_EXPORT void QStandardItemModel_hook_hook_itemChanged(QStandardItemModel_hookH handle, QHookH hook);

#endif
