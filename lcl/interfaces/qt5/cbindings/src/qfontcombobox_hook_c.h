//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFONTCOMBOBOX_HOOK_C_H
#define QFONTCOMBOBOX_HOOK_C_H

#include "qfontcombobox_hook.h"

C_EXPORT QFontComboBox_hookH QFontComboBox_hook_Create(QObjectH handle);
C_EXPORT void QFontComboBox_hook_Destroy(QFontComboBox_hookH handle);
C_EXPORT void QFontComboBox_hook_hook_currentFontChanged(QFontComboBox_hookH handle, QHookH hook);

#endif
