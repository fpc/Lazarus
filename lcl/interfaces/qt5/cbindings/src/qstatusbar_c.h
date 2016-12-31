//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTATUSBAR_C_H
#define QSTATUSBAR_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QStatusBarH QStatusBar_Create(QWidgetH parent);
C_EXPORT void QStatusBar_Destroy(QStatusBarH handle);
C_EXPORT void QStatusBar_addWidget(QStatusBarH handle, QWidgetH widget, int stretch);
C_EXPORT int QStatusBar_insertWidget(QStatusBarH handle, int index, QWidgetH widget, int stretch);
C_EXPORT void QStatusBar_addPermanentWidget(QStatusBarH handle, QWidgetH widget, int stretch);
C_EXPORT int QStatusBar_insertPermanentWidget(QStatusBarH handle, int index, QWidgetH widget, int stretch);
C_EXPORT void QStatusBar_removeWidget(QStatusBarH handle, QWidgetH widget);
C_EXPORT void QStatusBar_setSizeGripEnabled(QStatusBarH handle, bool AnonParam1);
C_EXPORT bool QStatusBar_isSizeGripEnabled(QStatusBarH handle);
C_EXPORT void QStatusBar_currentMessage(QStatusBarH handle, PWideString retval);
C_EXPORT void QStatusBar_showMessage(QStatusBarH handle, PWideString text, int timeout);
C_EXPORT void QStatusBar_clearMessage(QStatusBarH handle);

#endif
