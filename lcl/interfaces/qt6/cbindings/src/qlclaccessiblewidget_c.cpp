//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlclaccessiblewidget_c.h"


QLCLAccessibleWidgetH QLCLAccessibleWidget_Create(QWidgetH o, QAccessible::Role r, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	// const QString& name = QString()
	return (QLCLAccessibleWidgetH) new QLCLAccessibleWidget((QWidget *) o, (QAccessible::Role)r, t_name);
}

void QLCLAccessibleWidget_Destroy(QLCLAccessibleWidgetH handle)
{
	delete (QLCLAccessibleWidget *)handle;
}

void QLCLAccessibleWidget_override_actionNames(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_actionNames(hook);
}


void QLCLAccessibleWidget_override_child(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_child(hook);
}

void QLCLAccessibleWidget_override_childAt(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_childAt(hook);
}

void QLCLAccessibleWidget_override_childCount(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_childCount(hook);
}

void QLCLAccessibleWidget_override_doAction(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_doAction(hook);
}

void QLCLAccessibleWidget_override_indexOfChild(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_indexOfChild(hook);
}

void QLCLAccessibleWidget_override_parent(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_parent(hook);
}

void QLCLAccessibleWidget_override_rect(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_rect(hook);
}

void QLCLAccessibleWidget_override_role(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_role(hook);
}

void QLCLAccessibleWidget_override_state(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_state(hook);
}

void QLCLAccessibleWidget_override_text(QLCLAccessibleWidgetH handle, const QOverrideHook hook)
{
	((QLCLAccessibleWidget *)handle)->override_text(hook);
}
