//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSLIDER_HOOK_C_H
#define QABSTRACTSLIDER_HOOK_C_H

#include "qabstractslider_hook.h"

C_EXPORT QAbstractSlider_hookH QAbstractSlider_hook_Create(QObjectH handle);
C_EXPORT void QAbstractSlider_hook_Destroy(QAbstractSlider_hookH handle);
C_EXPORT void QAbstractSlider_hook_hook_valueChanged(QAbstractSlider_hookH handle, QHookH hook);
C_EXPORT void QAbstractSlider_hook_hook_sliderPressed(QAbstractSlider_hookH handle, QHookH hook);
C_EXPORT void QAbstractSlider_hook_hook_sliderMoved(QAbstractSlider_hookH handle, QHookH hook);
C_EXPORT void QAbstractSlider_hook_hook_sliderReleased(QAbstractSlider_hookH handle, QHookH hook);
C_EXPORT void QAbstractSlider_hook_hook_rangeChanged(QAbstractSlider_hookH handle, QHookH hook);
C_EXPORT void QAbstractSlider_hook_hook_actionTriggered(QAbstractSlider_hookH handle, QHookH hook);

#endif
