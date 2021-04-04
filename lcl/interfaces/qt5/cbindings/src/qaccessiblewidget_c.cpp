//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qaccessiblewidget_c.h"



QAccessibleWidgetH QAccessibleWidget_Create(QWidgetH o, QAccessible::Role r, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (QAccessibleWidgetH) new QAccessibleWidget((QWidget*)o, r, t_name);
}

bool QAccessibleWidget_isValid(QAccessibleWidgetH handle)
{
	return (bool) ((QAccessibleWidget *)handle)->isValid();
}

QWindowH QAccessibleWidget_window(QAccessibleWidgetH handle)
{
	return (QWindowH) ((QAccessibleWidget *)handle)->window();
}

int QAccessibleWidget_childCount(QAccessibleWidgetH handle)
{
	return (int) ((QAccessibleWidget *)handle)->childCount();
}

int QAccessibleWidget_indexOfChild(QAccessibleWidgetH handle, const QAccessibleInterfaceH child)
{
	return (int) ((QAccessibleWidget *)handle)->indexOfChild((QAccessibleInterface*)child);
}

QAccessibleInterfaceH QAccessibleWidget_focusChild(QAccessibleWidgetH handle)
{
	return (QAccessibleInterfaceH) ((QAccessibleWidget *)handle)->focusChild();
}

void QAccessibleWidget_rect(QAccessibleWidgetH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QAccessibleWidget *)handle)->rect();
	copyQRectToPRect(t_retval, retval);
}

QAccessibleInterfaceH QAccessibleWidget_parent(QAccessibleWidgetH handle)
{
	return (QAccessibleInterfaceH) ((QAccessibleWidget *)handle)->parent();
}

QAccessibleInterfaceH QAccessibleWidget_child(QAccessibleWidgetH handle, int index)
{
	return (QAccessibleInterfaceH) ((QAccessibleWidget *)handle)->child(index);
}

void QAccessibleWidget_text(QAccessibleWidgetH handle, PWideString retval, QAccessible::Text t)
{
	QString t_retval;
	t_retval = ((QAccessibleWidget *)handle)->text(t);
	copyQStringToPWideString(t_retval, retval);
}

QAccessible::Role QAccessibleWidget_role(QAccessibleWidgetH handle)
{
	return (QAccessible::Role) ((QAccessibleWidget *)handle)->role();
}

QAccessible::State QAccessibleWidget_state(QAccessibleWidgetH handle)
{
	return (QAccessible::State) ((QAccessibleWidget *)handle)->state();
}

void QAccessibleWidget_actionNames(QAccessibleWidgetH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QAccessibleWidget *)handle)->actionNames();
}

void QAccessibleWidget_doAction(QAccessibleWidgetH handle, PWideString actionName)
{
	QString t_actionName;
	copyPWideStringToQString(actionName, t_actionName);
	((QAccessibleWidget *)handle)->doAction(t_actionName);
}
