//******************************************************************************
//  Copyright (c) 2024 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qinputmethod_hook_c.h"

QInputMethod_hookH QInputMethod_hook_Create(QObjectH handle)
{
	return (QInputMethod_hookH) new QInputMethod_hook((QObject*)handle);
}

void QInputMethod_hook_Destroy(QInputMethod_hookH handle)
{
	delete (QInputMethod_hook *)handle;
}

void QInputMethod_hook_hook_actionRectangleChanged(QInputMethod_hookH handle, QHookH hook)
{
  ((QInputMethod_hook *)handle)->hook_actionRectangleChanged(hook);
}

void QInputMethod_hook_hook_animatingChanged(QInputMethod_hookH handle, QHookH hook)
{
  ((QInputMethod_hook *)handle)->hook_animatingChanged(hook);
}

void QInputMethod_hook_hook_cursorRectangleChanged(QInputMethod_hookH handle, QHookH hook)
{
  ((QInputMethod_hook *)handle)->hook_cursorRectangleChanged(hook);
}

void QInputMethod_hook_hook_inputDirectionChanged(QInputMethod_hookH handle, QHookH hook)
{
  ((QInputMethod_hook *)handle)->hook_inputDirectionChanged(hook);
}

void QInputMethod_hook_hook_inputItemClipRectangleChanged(QInputMethod_hookH handle, QHookH hook)
{
  ((QInputMethod_hook *)handle)->hook_inputItemClipRectangleChanged(hook);
}

void QInputMethod_hook_hook_keyboardRectangleChanged(QInputMethod_hookH handle, QHookH hook)
{
  ((QInputMethod_hook *)handle)->hook_keyboardRectangleChanged(hook);
}

void QInputMethod_hook_hook_localeChanged(QInputMethod_hookH handle, QHookH hook)
{
  ((QInputMethod_hook *)handle)->hook_localeChanged(hook);
}

void QInputMethod_hook_hook_visibleChanged(QInputMethod_hookH handle, QHookH hook)
{
  ((QInputMethod_hook *)handle)->hook_visibleChanged(hook);
}


