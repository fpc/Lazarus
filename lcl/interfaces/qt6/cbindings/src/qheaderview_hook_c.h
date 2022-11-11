//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QHEADERVIEW_HOOK_C_H
#define QHEADERVIEW_HOOK_C_H

#include "qheaderview_hook.h"

C_EXPORT QHeaderView_hookH QHeaderView_hook_Create(QObjectH handle);
C_EXPORT void QHeaderView_hook_Destroy(QHeaderView_hookH handle);
C_EXPORT void QHeaderView_hook_hook_sectionMoved(QHeaderView_hookH handle, QHookH hook);
C_EXPORT void QHeaderView_hook_hook_sectionResized(QHeaderView_hookH handle, QHookH hook);
C_EXPORT void QHeaderView_hook_hook_sectionPressed(QHeaderView_hookH handle, QHookH hook);
C_EXPORT void QHeaderView_hook_hook_sectionClicked(QHeaderView_hookH handle, QHookH hook);
C_EXPORT void QHeaderView_hook_hook_sectionEntered(QHeaderView_hookH handle, QHookH hook);
C_EXPORT void QHeaderView_hook_hook_sectionDoubleClicked(QHeaderView_hookH handle, QHookH hook);
C_EXPORT void QHeaderView_hook_hook_sectionCountChanged(QHeaderView_hookH handle, QHookH hook);
C_EXPORT void QHeaderView_hook_hook_sectionHandleDoubleClicked(QHeaderView_hookH handle, QHookH hook);
C_EXPORT void QHeaderView_hook_hook_geometriesChanged(QHeaderView_hookH handle, QHookH hook);
C_EXPORT void QHeaderView_hook_hook_sortIndicatorChanged(QHeaderView_hookH handle, QHookH hook);

#endif
