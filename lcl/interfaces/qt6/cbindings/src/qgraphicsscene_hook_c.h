//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGRAPHICSSCENE_HOOK_C_H
#define QGRAPHICSSCENE_HOOK_C_H

#include "qgraphicsscene_hook.h"

C_EXPORT QGraphicsScene_hookH QGraphicsScene_hook_Create(QObjectH handle);
C_EXPORT void QGraphicsScene_hook_Destroy(QGraphicsScene_hookH handle);
C_EXPORT void QGraphicsScene_hook_hook_sceneRectChanged(QGraphicsScene_hookH handle, QHookH hook);
C_EXPORT void QGraphicsScene_hook_hook_selectionChanged(QGraphicsScene_hookH handle, QHookH hook);
C_EXPORT void QGraphicsScene_hook_hook_focusItemChanged(QGraphicsScene_hookH handle, QHookH hook);

#endif
