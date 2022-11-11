//******************************************************************************
//  Copyright (c) 2017-2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSCREEN_HOOK_C_H
#define QSCREEN_HOOK_C_H

#include "qscreen_hook.h"

C_EXPORT QScreen_hookH QScreen_hook_Create(QObjectH handle);
C_EXPORT void QScreen_hook_Destroy(QScreen_hookH handle);
C_EXPORT void QScreen_hook_hook_geometryChanged(QScreen_hookH handle, QHookH hook);
C_EXPORT void QScreen_hook_hook_availableGeometryChanged(QScreen_hookH handle, QHookH hook);
C_EXPORT void QScreen_hook_hook_physicalSizeChanged(QScreen_hookH handle, QHookH hook);
C_EXPORT void QScreen_hook_hook_physicalDotsPerInchChanged(QScreen_hookH handle, QHookH hook);
C_EXPORT void QScreen_hook_hook_logicalDotsPerInchChanged(QScreen_hookH handle, QHookH hook);
C_EXPORT void QScreen_hook_hook_virtualGeometryChanged(QScreen_hookH handle, QHookH hook);
C_EXPORT void QScreen_hook_hook_primaryOrientationChanged(QScreen_hookH handle, QHookH hook);
C_EXPORT void QScreen_hook_hook_orientationChanged(QScreen_hookH handle, QHookH hook);
C_EXPORT void QScreen_hook_hook_refreshRateChanged(QScreen_hookH handle, QHookH hook);

#endif

