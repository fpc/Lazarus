//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMENUBAR_C_H
#define QMENUBAR_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QMenuBarH QMenuBar_Create(QWidgetH parent);
C_EXPORT void QMenuBar_Destroy(QMenuBarH handle);
C_EXPORT QActionH QMenuBar_addAction(QMenuBarH handle, PWideString text);
C_EXPORT QActionH QMenuBar_addAction2(QMenuBarH handle, PWideString text, const QObjectH receiver, const char* member);
C_EXPORT QActionH QMenuBar_addMenu(QMenuBarH handle, QMenuH menu);
C_EXPORT QMenuH QMenuBar_addMenu2(QMenuBarH handle, PWideString title);
C_EXPORT QMenuH QMenuBar_addMenu3(QMenuBarH handle, const QIconH icon, PWideString title);
C_EXPORT QActionH QMenuBar_addSeparator(QMenuBarH handle);
C_EXPORT QActionH QMenuBar_insertSeparator(QMenuBarH handle, QActionH before);
C_EXPORT QActionH QMenuBar_insertMenu(QMenuBarH handle, QActionH before, QMenuH menu);
C_EXPORT void QMenuBar_clear(QMenuBarH handle);
C_EXPORT QActionH QMenuBar_activeAction(QMenuBarH handle);
C_EXPORT void QMenuBar_setActiveAction(QMenuBarH handle, QActionH action);
C_EXPORT void QMenuBar_setDefaultUp(QMenuBarH handle, bool AnonParam1);
C_EXPORT bool QMenuBar_isDefaultUp(QMenuBarH handle);
C_EXPORT void QMenuBar_sizeHint(QMenuBarH handle, PSize retval);
C_EXPORT void QMenuBar_minimumSizeHint(QMenuBarH handle, PSize retval);
C_EXPORT int QMenuBar_heightForWidth(QMenuBarH handle, int AnonParam1);
C_EXPORT void QMenuBar_actionGeometry(QMenuBarH handle, PRect retval, QActionH AnonParam1);
C_EXPORT QActionH QMenuBar_actionAt(QMenuBarH handle, const QPointH AnonParam1);
C_EXPORT void QMenuBar_setCornerWidget(QMenuBarH handle, QWidgetH w, Qt::Corner corner);
C_EXPORT QWidgetH QMenuBar_cornerWidget(QMenuBarH handle, Qt::Corner corner);
C_EXPORT bool QMenuBar_isNativeMenuBar(QMenuBarH handle);
C_EXPORT void QMenuBar_setNativeMenuBar(QMenuBarH handle, bool nativeMenuBar);
C_EXPORT void QMenuBar_setVisible(QMenuBarH handle, bool visible);

#endif
