//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMDISUBWINDOW_C_H
#define QMDISUBWINDOW_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QMdiSubWindowH QMdiSubWindow_Create(QWidgetH parent, unsigned int flags);
C_EXPORT void QMdiSubWindow_Destroy(QMdiSubWindowH handle);
C_EXPORT void QMdiSubWindow_sizeHint(QMdiSubWindowH handle, PSize retval);
C_EXPORT void QMdiSubWindow_minimumSizeHint(QMdiSubWindowH handle, PSize retval);
C_EXPORT void QMdiSubWindow_setWidget(QMdiSubWindowH handle, QWidgetH widget);
C_EXPORT QWidgetH QMdiSubWindow_widget(QMdiSubWindowH handle);
C_EXPORT QWidgetH QMdiSubWindow_maximizedButtonsWidget(QMdiSubWindowH handle);
C_EXPORT QWidgetH QMdiSubWindow_maximizedSystemMenuIconWidget(QMdiSubWindowH handle);
C_EXPORT bool QMdiSubWindow_isShaded(QMdiSubWindowH handle);
C_EXPORT void QMdiSubWindow_setOption(QMdiSubWindowH handle, QMdiSubWindow::SubWindowOption option, bool on);
C_EXPORT bool QMdiSubWindow_testOption(QMdiSubWindowH handle, QMdiSubWindow::SubWindowOption AnonParam1);
C_EXPORT void QMdiSubWindow_setKeyboardSingleStep(QMdiSubWindowH handle, int step);
C_EXPORT int QMdiSubWindow_keyboardSingleStep(QMdiSubWindowH handle);
C_EXPORT void QMdiSubWindow_setKeyboardPageStep(QMdiSubWindowH handle, int step);
C_EXPORT int QMdiSubWindow_keyboardPageStep(QMdiSubWindowH handle);
C_EXPORT void QMdiSubWindow_setSystemMenu(QMdiSubWindowH handle, QMenuH systemMenu);
C_EXPORT QMenuH QMdiSubWindow_systemMenu(QMdiSubWindowH handle);
C_EXPORT QMdiAreaH QMdiSubWindow_mdiArea(QMdiSubWindowH handle);
C_EXPORT void QMdiSubWindow_showSystemMenu(QMdiSubWindowH handle);
C_EXPORT void QMdiSubWindow_showShaded(QMdiSubWindowH handle);

#endif
