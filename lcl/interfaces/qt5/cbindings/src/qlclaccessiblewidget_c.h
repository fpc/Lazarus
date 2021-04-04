//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCLACCESSIBLEWIDGET_C_H
#define QLCLACCESSIBLEWIDGET_C_H

#include "qlclaccessiblewidget.h"
#include "pascalbind.h"

C_EXPORT QLCLAccessibleWidgetH QLCLAccessibleWidget_Create(QWidgetH o, QAccessible::Role r, PWideString name);
C_EXPORT void QLCLAccessibleWidget_Destroy(QLCLAccessibleWidgetH handle);
C_EXPORT void QLCLAccessibleWidget_override_actionNames(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_child(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_childAt(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_childCount(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_doAction(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_indexOfChild(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_parent(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_rect(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_role(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_state(QLCLAccessibleWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLAccessibleWidget_override_text(QLCLAccessibleWidgetH handle, const QOverrideHook hook);

#endif
