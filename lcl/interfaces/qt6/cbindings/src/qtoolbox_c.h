//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTOOLBOX_C_H
#define QTOOLBOX_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QToolBoxH QToolBox_Create(QWidgetH parent, unsigned int f);
C_EXPORT void QToolBox_Destroy(QToolBoxH handle);
C_EXPORT int QToolBox_addItem(QToolBoxH handle, QWidgetH widget, PWideString text);
C_EXPORT int QToolBox_addItem2(QToolBoxH handle, QWidgetH widget, const QIconH icon, PWideString text);
C_EXPORT int QToolBox_insertItem(QToolBoxH handle, int index, QWidgetH widget, PWideString text);
C_EXPORT int QToolBox_insertItem2(QToolBoxH handle, int index, QWidgetH widget, const QIconH icon, PWideString text);
C_EXPORT void QToolBox_removeItem(QToolBoxH handle, int index);
C_EXPORT void QToolBox_setItemEnabled(QToolBoxH handle, int index, bool enabled);
C_EXPORT bool QToolBox_isItemEnabled(QToolBoxH handle, int index);
C_EXPORT void QToolBox_setItemText(QToolBoxH handle, int index, PWideString text);
C_EXPORT void QToolBox_itemText(QToolBoxH handle, PWideString retval, int index);
C_EXPORT void QToolBox_setItemIcon(QToolBoxH handle, int index, const QIconH icon);
C_EXPORT void QToolBox_itemIcon(QToolBoxH handle, QIconH retval, int index);
C_EXPORT void QToolBox_setItemToolTip(QToolBoxH handle, int index, PWideString toolTip);
C_EXPORT void QToolBox_itemToolTip(QToolBoxH handle, PWideString retval, int index);
C_EXPORT int QToolBox_currentIndex(QToolBoxH handle);
C_EXPORT QWidgetH QToolBox_currentWidget(QToolBoxH handle);
C_EXPORT QWidgetH QToolBox_widget(QToolBoxH handle, int index);
C_EXPORT int QToolBox_indexOf(QToolBoxH handle, QWidgetH widget);
C_EXPORT int QToolBox_count(QToolBoxH handle);
C_EXPORT void QToolBox_setCurrentIndex(QToolBoxH handle, int index);
C_EXPORT void QToolBox_setCurrentWidget(QToolBoxH handle, QWidgetH widget);

#endif
