//******************************************************************************
//  Copyright (c) 2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOMPLETER_HOOK_C_H
#define QCOMPLETER_HOOK_C_H

#include "qcompleter_hook.h"

C_EXPORT QCompleter_hookH QCompleter_hook_Create(QObjectH handle);
C_EXPORT void QCompleter_hook_Destroy(QCompleter_hookH handle);
C_EXPORT void QCompleter_hook_hook_activated(QCompleter_hookH handle, QHookH hook);
C_EXPORT void QCompleter_hook_hook_activated2(QCompleter_hookH handle, QHookH hook);
C_EXPORT void QCompleter_hook_hook_highlighted(QCompleter_hookH handle, QHookH hook);
C_EXPORT void QCompleter_hook_hook_highlighted2(QCompleter_hookH handle, QHookH hook);

#endif
