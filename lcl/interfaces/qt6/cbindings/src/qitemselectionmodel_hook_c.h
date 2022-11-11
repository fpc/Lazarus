//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QITEMSELECTIONMODEL_HOOK_C_H
#define QITEMSELECTIONMODEL_HOOK_C_H

#include "qitemselectionmodel_hook.h"

C_EXPORT QItemSelectionRange_hookH QItemSelectionRange_hook_Create(QObjectH handle);
C_EXPORT void QItemSelectionRange_hook_Destroy(QItemSelectionRange_hookH handle);
C_EXPORT QItemSelectionModel_hookH QItemSelectionModel_hook_Create(QObjectH handle);
C_EXPORT void QItemSelectionModel_hook_Destroy(QItemSelectionModel_hookH handle);
C_EXPORT void QItemSelectionModel_hook_hook_currentChanged(QItemSelectionModel_hookH handle, QHookH hook);
C_EXPORT void QItemSelectionModel_hook_hook_currentRowChanged(QItemSelectionModel_hookH handle, QHookH hook);
C_EXPORT void QItemSelectionModel_hook_hook_currentColumnChanged(QItemSelectionModel_hookH handle, QHookH hook);

#endif
