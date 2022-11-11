//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTBROWSER_HOOK_C_H
#define QTEXTBROWSER_HOOK_C_H

#include "qtextbrowser_hook.h"

C_EXPORT QTextBrowser_hookH QTextBrowser_hook_Create(QObjectH handle);
C_EXPORT void QTextBrowser_hook_Destroy(QTextBrowser_hookH handle);
C_EXPORT void QTextBrowser_hook_hook_backwardAvailable(QTextBrowser_hookH handle, QHookH hook);
C_EXPORT void QTextBrowser_hook_hook_forwardAvailable(QTextBrowser_hookH handle, QHookH hook);
C_EXPORT void QTextBrowser_hook_hook_historyChanged(QTextBrowser_hookH handle, QHookH hook);
C_EXPORT void QTextBrowser_hook_hook_sourceChanged(QTextBrowser_hookH handle, QHookH hook);
C_EXPORT void QTextBrowser_hook_hook_highlighted(QTextBrowser_hookH handle, QHookH hook);
C_EXPORT void QTextBrowser_hook_hook_highlighted2(QTextBrowser_hookH handle, QHookH hook);
C_EXPORT void QTextBrowser_hook_hook_anchorClicked(QTextBrowser_hookH handle, QHookH hook);

#endif
