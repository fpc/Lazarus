//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPEN_C_H
#define QPEN_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QPenH QPen_Create();
C_EXPORT void QPen_Destroy(QPenH handle);
C_EXPORT QPenH QPen_Create2(Qt::PenStyle AnonParam1);
C_EXPORT QPenH QPen_Create3(const QColorH color);
C_EXPORT QPenH QPen_Create4(const QBrushH brush, qreal width, Qt::PenStyle s, Qt::PenCapStyle c, Qt::PenJoinStyle j);
C_EXPORT QPenH QPen_Create5(const QPenH pen);
C_EXPORT void QPen_swap(QPenH handle, QPenH other);
C_EXPORT Qt::PenStyle QPen_style(QPenH handle);
C_EXPORT void QPen_setStyle(QPenH handle, Qt::PenStyle AnonParam1);
C_EXPORT void QPen_dashPattern(QPenH handle, PQRealArray retval);
C_EXPORT void QPen_setDashPattern(QPenH handle, PQRealArray pattern);
C_EXPORT qreal QPen_dashOffset(QPenH handle);
C_EXPORT void QPen_setDashOffset(QPenH handle, qreal doffset);
C_EXPORT qreal QPen_miterLimit(QPenH handle);
C_EXPORT void QPen_setMiterLimit(QPenH handle, qreal limit);
C_EXPORT qreal QPen_widthF(QPenH handle);
C_EXPORT void QPen_setWidthF(QPenH handle, qreal width);
C_EXPORT int QPen_width(QPenH handle);
C_EXPORT void QPen_setWidth(QPenH handle, int width);
C_EXPORT void QPen_color(QPenH handle, PQColor retval);
C_EXPORT void QPen_setColor(QPenH handle, const QColorH color);
C_EXPORT void QPen_brush(QPenH handle, QBrushH retval);
C_EXPORT void QPen_setBrush(QPenH handle, const QBrushH brush);
C_EXPORT bool QPen_isSolid(QPenH handle);
C_EXPORT Qt::PenCapStyle QPen_capStyle(QPenH handle);
C_EXPORT void QPen_setCapStyle(QPenH handle, Qt::PenCapStyle pcs);
C_EXPORT Qt::PenJoinStyle QPen_joinStyle(QPenH handle);
C_EXPORT void QPen_setJoinStyle(QPenH handle, Qt::PenJoinStyle pcs);
C_EXPORT bool QPen_isCosmetic(QPenH handle);
C_EXPORT void QPen_setCosmetic(QPenH handle, bool cosmetic);
C_EXPORT bool QPen_isDetached(QPenH handle);

#endif
