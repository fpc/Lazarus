//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFRAME_C_H
#define QFRAME_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QFrameH QFrame_Create(QWidgetH parent, unsigned int f);
C_EXPORT void QFrame_Destroy(QFrameH handle);
C_EXPORT int QFrame_frameStyle(QFrameH handle);
C_EXPORT void QFrame_setFrameStyle(QFrameH handle, int AnonParam1);
C_EXPORT int QFrame_frameWidth(QFrameH handle);
C_EXPORT void QFrame_sizeHint(QFrameH handle, PSize retval);
C_EXPORT QFrame::Shape QFrame_frameShape(QFrameH handle);
C_EXPORT void QFrame_setFrameShape(QFrameH handle, QFrame::Shape AnonParam1);
C_EXPORT QFrame::Shadow QFrame_frameShadow(QFrameH handle);
C_EXPORT void QFrame_setFrameShadow(QFrameH handle, QFrame::Shadow AnonParam1);
C_EXPORT int QFrame_lineWidth(QFrameH handle);
C_EXPORT void QFrame_setLineWidth(QFrameH handle, int AnonParam1);
C_EXPORT int QFrame_midLineWidth(QFrameH handle);
C_EXPORT void QFrame_setMidLineWidth(QFrameH handle, int AnonParam1);
C_EXPORT void QFrame_frameRect(QFrameH handle, PRect retval);
C_EXPORT void QFrame_setFrameRect(QFrameH handle, PRect AnonParam1);

#endif
