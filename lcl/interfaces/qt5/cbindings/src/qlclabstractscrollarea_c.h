//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCLABSTRACTSCROLLAREA_C_H
#define QLCLABSTRACTSCROLLAREA_C_H

#include "qlclabstractscrollarea.h"
#include "pascalbind.h"

C_EXPORT QLCLAbstractScrollAreaH QLCLAbstractScrollArea_Create(QWidgetH parent);
C_EXPORT void QLCLAbstractScrollArea_Destroy(QLCLAbstractScrollAreaH handle);
C_EXPORT void QLCLAbstractScrollArea_override_viewportEvent(QLCLAbstractScrollAreaH handle, const QOverrideHook hook);
C_EXPORT bool QLCLAbstractScrollArea_InheritedViewportEvent(QLCLAbstractScrollAreaH handle, QEventH event);

#endif
