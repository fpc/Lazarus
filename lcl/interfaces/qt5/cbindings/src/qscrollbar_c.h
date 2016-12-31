//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSCROLLBAR_C_H
#define QSCROLLBAR_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QScrollBarH QScrollBar_Create(QWidgetH parent);
C_EXPORT void QScrollBar_Destroy(QScrollBarH handle);
C_EXPORT QScrollBarH QScrollBar_Create2(Qt::Orientation AnonParam1, QWidgetH parent);
C_EXPORT void QScrollBar_sizeHint(QScrollBarH handle, PSize retval);
C_EXPORT bool QScrollBar_event(QScrollBarH handle, QEventH event);

#endif
