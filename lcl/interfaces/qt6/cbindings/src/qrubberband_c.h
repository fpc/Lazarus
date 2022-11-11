//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QRUBBERBAND_C_H
#define QRUBBERBAND_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QRubberBandH QRubberBand_Create(QRubberBand::Shape AnonParam1, QWidgetH AnonParam2);
C_EXPORT void QRubberBand_Destroy(QRubberBandH handle);
C_EXPORT QRubberBand::Shape QRubberBand_shape(QRubberBandH handle);
C_EXPORT void QRubberBand_setGeometry(QRubberBandH handle, PRect r);
C_EXPORT void QRubberBand_setGeometry2(QRubberBandH handle, int x, int y, int w, int h);
C_EXPORT void QRubberBand_move(QRubberBandH handle, int x, int y);
C_EXPORT void QRubberBand_move2(QRubberBandH handle, const QPointH p);
C_EXPORT void QRubberBand_resize(QRubberBandH handle, int w, int h);
C_EXPORT void QRubberBand_resize2(QRubberBandH handle, const QSizeH s);

#endif
