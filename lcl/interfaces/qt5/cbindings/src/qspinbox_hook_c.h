//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSPINBOX_HOOK_C_H
#define QSPINBOX_HOOK_C_H

#include "qspinbox_hook.h"

C_EXPORT QSpinBox_hookH QSpinBox_hook_Create(QObjectH handle);
C_EXPORT void QSpinBox_hook_Destroy(QSpinBox_hookH handle);
C_EXPORT void QSpinBox_hook_hook_valueChanged(QSpinBox_hookH handle, QHookH hook);
C_EXPORT void QSpinBox_hook_hook_valueChanged2(QSpinBox_hookH handle, QHookH hook);
C_EXPORT QDoubleSpinBox_hookH QDoubleSpinBox_hook_Create(QObjectH handle);
C_EXPORT void QDoubleSpinBox_hook_Destroy(QDoubleSpinBox_hookH handle);
C_EXPORT void QDoubleSpinBox_hook_hook_valueChanged(QDoubleSpinBox_hookH handle, QHookH hook);
C_EXPORT void QDoubleSpinBox_hook_hook_valueChanged2(QDoubleSpinBox_hookH handle, QHookH hook);

#endif
