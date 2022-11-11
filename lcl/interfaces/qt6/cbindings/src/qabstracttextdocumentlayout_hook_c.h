//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTTEXTDOCUMENTLAYOUT_HOOK_C_H
#define QABSTRACTTEXTDOCUMENTLAYOUT_HOOK_C_H

#include "qabstracttextdocumentlayout_hook.h"

C_EXPORT QAbstractTextDocumentLayout_hookH QAbstractTextDocumentLayout_hook_Create(QObjectH handle);
C_EXPORT void QAbstractTextDocumentLayout_hook_Destroy(QAbstractTextDocumentLayout_hookH handle);
C_EXPORT void QAbstractTextDocumentLayout_hook_hook_update(QAbstractTextDocumentLayout_hookH handle, QHookH hook);
C_EXPORT void QAbstractTextDocumentLayout_hook_hook_update2(QAbstractTextDocumentLayout_hookH handle, QHookH hook);
C_EXPORT void QAbstractTextDocumentLayout_hook_hook_updateBlock(QAbstractTextDocumentLayout_hookH handle, QHookH hook);
C_EXPORT void QAbstractTextDocumentLayout_hook_hook_documentSizeChanged(QAbstractTextDocumentLayout_hookH handle, QHookH hook);
C_EXPORT void QAbstractTextDocumentLayout_hook_hook_pageCountChanged(QAbstractTextDocumentLayout_hookH handle, QHookH hook);
C_EXPORT QTextObjectInterface_hookH QTextObjectInterface_hook_Create(QObjectH handle);
C_EXPORT void QTextObjectInterface_hook_Destroy(QTextObjectInterface_hookH handle);

#endif
