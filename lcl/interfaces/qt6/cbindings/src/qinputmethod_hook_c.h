//******************************************************************************
//  Copyright (c) 2024 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QINPUTMETHOD_HOOK_C_H
#define QINPUTMETHOD_HOOK_C_H

#include "qinputmethod_hook.h"

C_EXPORT QInputMethod_hookH QInputMethod_hook_Create(QObjectH handle);
C_EXPORT void QInputMethod_hook_Destroy(QInputMethod_hookH handle);
C_EXPORT void QInputMethod_hook_hook_actionRectangleChanged(QInputMethod_hookH handle, QHookH hook);
C_EXPORT void QInputMethod_hook_hook_animatingChanged(QInputMethod_hookH handle, QHookH hook);
C_EXPORT void QInputMethod_hook_hook_cursorRectangleChanged(QInputMethod_hookH handle, QHookH hook);
C_EXPORT void QInputMethod_hook_hook_inputDirectionChanged(QInputMethod_hookH handle, QHookH hook);
C_EXPORT void QInputMethod_hook_hook_inputItemClipRectangleChanged(QInputMethod_hookH handle, QHookH hook);
C_EXPORT void QInputMethod_hook_hook_keyboardRectangleChanged(QInputMethod_hookH handle, QHookH hook);
C_EXPORT void QInputMethod_hook_hook_localeChanged(QInputMethod_hookH handle, QHookH hook);
C_EXPORT void QInputMethod_hook_hook_visibleChanged(QInputMethod_hookH handle, QHookH hook);


#endif
