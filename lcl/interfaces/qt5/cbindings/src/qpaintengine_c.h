//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPAINTENGINE_C_H
#define QPAINTENGINE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT bool QPaintEngine_isActive(QPaintEngineH handle);
C_EXPORT void QPaintEngine_setActive(QPaintEngineH handle, bool newState);
C_EXPORT bool QPaintEngine_begin(QPaintEngineH handle, QPaintDeviceH pdev);
C_EXPORT bool QPaintEngine_end(QPaintEngineH handle);
C_EXPORT void QPaintEngine_drawRects(QPaintEngineH handle, PRect rects, int rectCount);
C_EXPORT void QPaintEngine_drawRects2(QPaintEngineH handle, const QRectFH rects, int rectCount);
C_EXPORT void QPaintEngine_drawLines(QPaintEngineH handle, const QLineH lines, int lineCount);
C_EXPORT void QPaintEngine_drawLines2(QPaintEngineH handle, const QLineFH lines, int lineCount);
C_EXPORT void QPaintEngine_drawEllipse(QPaintEngineH handle, const QRectFH r);
C_EXPORT void QPaintEngine_drawEllipse2(QPaintEngineH handle, PRect r);
C_EXPORT void QPaintEngine_drawPath(QPaintEngineH handle, const QPainterPathH path);
C_EXPORT void QPaintEngine_drawPoints(QPaintEngineH handle, const QPointFH points, int pointCount);
C_EXPORT void QPaintEngine_drawPoints2(QPaintEngineH handle, const QPointH points, int pointCount);
C_EXPORT void QPaintEngine_drawPolygon(QPaintEngineH handle, const QPointFH points, int pointCount, QPaintEngine::PolygonDrawMode mode);
C_EXPORT void QPaintEngine_drawPolygon2(QPaintEngineH handle, const QPointH points, int pointCount, QPaintEngine::PolygonDrawMode mode);
C_EXPORT void QPaintEngine_drawPixmap(QPaintEngineH handle, const QRectFH r, const QPixmapH pm, const QRectFH sr);
C_EXPORT void QPaintEngine_drawTiledPixmap(QPaintEngineH handle, const QRectFH r, const QPixmapH pixmap, const QPointFH s);
C_EXPORT void QPaintEngine_drawImage(QPaintEngineH handle, const QRectFH r, const QImageH pm, const QRectFH sr, unsigned int flags);
C_EXPORT void QPaintEngine_setPaintDevice(QPaintEngineH handle, QPaintDeviceH device);
C_EXPORT QPaintDeviceH QPaintEngine_paintDevice(QPaintEngineH handle);
C_EXPORT void QPaintEngine_setSystemClip(QPaintEngineH handle, const QRegionH baseClip);
C_EXPORT void QPaintEngine_systemClip(QPaintEngineH handle, QRegionH retval);
C_EXPORT void QPaintEngine_setSystemRect(QPaintEngineH handle, PRect rect);
C_EXPORT void QPaintEngine_systemRect(QPaintEngineH handle, PRect retval);
C_EXPORT void QPaintEngine_coordinateOffset(QPaintEngineH handle, PQtPoint retval);
C_EXPORT QPaintEngine::Type QPaintEngine_type(QPaintEngineH handle);
C_EXPORT void QPaintEngine_fix_neg_rect(QPaintEngineH handle, int* x, int* y, int* w, int* h);
C_EXPORT bool QPaintEngine_testDirty(QPaintEngineH handle, unsigned int df);
C_EXPORT void QPaintEngine_setDirty(QPaintEngineH handle, unsigned int df);
C_EXPORT void QPaintEngine_clearDirty(QPaintEngineH handle, unsigned int df);
C_EXPORT bool QPaintEngine_hasFeature(QPaintEngineH handle, unsigned int feature);
C_EXPORT QPainterH QPaintEngine_painter(QPaintEngineH handle);
C_EXPORT void QPaintEngine_syncState(QPaintEngineH handle);
C_EXPORT bool QPaintEngine_isExtended(QPaintEngineH handle);

#endif
