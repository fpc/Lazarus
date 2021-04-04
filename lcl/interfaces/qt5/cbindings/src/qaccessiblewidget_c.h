//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QACCESSIBLEWIDGET_C_H
#define QACCESSIBLEWIDGET_C_H

// QAccessible::Role
// - 
// QAccessible::State
// - 
// QAccessible::Text
// - 

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QAccessibleWidgetH QAccessibleWidget_Create(QWidgetH o, QAccessible::Role r, PWideString name);
C_EXPORT bool QAccessibleWidget_isValid(QAccessibleWidgetH handle);
C_EXPORT QWindowH QAccessibleWidget_window(QAccessibleWidgetH handle);
C_EXPORT int QAccessibleWidget_childCount(QAccessibleWidgetH handle);
C_EXPORT int QAccessibleWidget_indexOfChild(QAccessibleWidgetH handle, const QAccessibleInterfaceH child);
C_EXPORT QAccessibleInterfaceH QAccessibleWidget_focusChild(QAccessibleWidgetH handle);
C_EXPORT void QAccessibleWidget_rect(QAccessibleWidgetH handle, PRect retval);
C_EXPORT QAccessibleInterfaceH QAccessibleWidget_parent(QAccessibleWidgetH handle);
C_EXPORT QAccessibleInterfaceH QAccessibleWidget_child(QAccessibleWidgetH handle, int index);
C_EXPORT void QAccessibleWidget_text(QAccessibleWidgetH handle, PWideString retval, QAccessible::Text t);
C_EXPORT QAccessible::Role QAccessibleWidget_role(QAccessibleWidgetH handle);
C_EXPORT QAccessible::State QAccessibleWidget_state(QAccessibleWidgetH handle);
C_EXPORT void QAccessibleWidget_actionNames(QAccessibleWidgetH handle, QStringListH retval);
C_EXPORT void QAccessibleWidget_doAction(QAccessibleWidgetH handle, PWideString actionName);


#endif
