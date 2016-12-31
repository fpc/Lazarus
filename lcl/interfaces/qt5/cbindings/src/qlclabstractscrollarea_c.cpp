//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlclabstractscrollarea_c.h"

QLCLAbstractScrollAreaH QLCLAbstractScrollArea_Create(QWidgetH parent)
{
	return (QLCLAbstractScrollAreaH) new QLCLAbstractScrollArea((QWidget*)parent);
}

void QLCLAbstractScrollArea_Destroy(QLCLAbstractScrollAreaH handle)
{
	delete (QLCLAbstractScrollArea *)handle;
}

void QLCLAbstractScrollArea_override_viewportEvent(QLCLAbstractScrollAreaH handle, const QOverrideHook hook)
{
	((QLCLAbstractScrollArea *)handle)->override_viewportEvent(hook);
}

bool QLCLAbstractScrollArea_InheritedViewportEvent(QLCLAbstractScrollAreaH handle, QEventH event)
{
	return (bool) ((QLCLAbstractScrollArea *)handle)->InheritedViewportEvent((QEvent*)event);
}

