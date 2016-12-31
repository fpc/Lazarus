//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSPINBOX_HOOK_C_H
#define QABSTRACTSPINBOX_HOOK_C_H

#include "qabstractspinbox_hook.h"

C_EXPORT QAbstractSpinBox_hookH QAbstractSpinBox_hook_Create(QObjectH handle);
C_EXPORT void QAbstractSpinBox_hook_Destroy(QAbstractSpinBox_hookH handle);
C_EXPORT void QAbstractSpinBox_hook_hook_editingFinished(QAbstractSpinBox_hookH handle, QHookH hook);

#endif
