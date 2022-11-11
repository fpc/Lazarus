//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOMBOBOX_HOOK_C_H
#define QCOMBOBOX_HOOK_C_H

#include "qcombobox_hook.h"

C_EXPORT QComboBox_hookH QComboBox_hook_Create(QObjectH handle);
C_EXPORT void QComboBox_hook_Destroy(QComboBox_hookH handle);
C_EXPORT void QComboBox_hook_hook_editTextChanged(QComboBox_hookH handle, QHookH hook);
C_EXPORT void QComboBox_hook_hook_activated(QComboBox_hookH handle, QHookH hook);
C_EXPORT void QComboBox_hook_hook_activated2(QComboBox_hookH handle, QHookH hook);
C_EXPORT void QComboBox_hook_hook_highlighted(QComboBox_hookH handle, QHookH hook);
C_EXPORT void QComboBox_hook_hook_highlighted2(QComboBox_hookH handle, QHookH hook);
C_EXPORT void QComboBox_hook_hook_currentIndexChanged(QComboBox_hookH handle, QHookH hook);
C_EXPORT void QComboBox_hook_hook_currentIndexChanged2(QComboBox_hookH handle, QHookH hook);
C_EXPORT void QComboBox_hook_hook_currentTextChanged(QComboBox_hookH handle, QHookH hook);

#endif
