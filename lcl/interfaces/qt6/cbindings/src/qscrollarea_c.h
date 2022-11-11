//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSCROLLAREA_C_H
#define QSCROLLAREA_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QScrollAreaH QScrollArea_Create(QWidgetH parent);
C_EXPORT void QScrollArea_Destroy(QScrollAreaH handle);
C_EXPORT QWidgetH QScrollArea_widget(QScrollAreaH handle);
C_EXPORT void QScrollArea_setWidget(QScrollAreaH handle, QWidgetH widget);
C_EXPORT QWidgetH QScrollArea_takeWidget(QScrollAreaH handle);
C_EXPORT bool QScrollArea_widgetResizable(QScrollAreaH handle);
C_EXPORT void QScrollArea_setWidgetResizable(QScrollAreaH handle, bool resizable);
C_EXPORT void QScrollArea_sizeHint(QScrollAreaH handle, PSize retval);
C_EXPORT bool QScrollArea_focusNextPrevChild(QScrollAreaH handle, bool next);
C_EXPORT unsigned int QScrollArea_alignment(QScrollAreaH handle);
C_EXPORT void QScrollArea_setAlignment(QScrollAreaH handle, unsigned int AnonParam1);
C_EXPORT void QScrollArea_ensureVisible(QScrollAreaH handle, int x, int y, int xmargin, int ymargin);
C_EXPORT void QScrollArea_ensureWidgetVisible(QScrollAreaH handle, QWidgetH childWidget, int xmargin, int ymargin);

#endif
