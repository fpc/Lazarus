//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPAINTDEVICE_C_H
#define QPAINTDEVICE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT int QPaintDevice_colorCount(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_depth(QPaintDeviceH handle);
C_EXPORT qreal QPaintDevice_devicePixelRatio(QPaintDeviceH handle);
C_EXPORT qreal QPaintDevice_devicePixelRatioF(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_devType(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_height(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_heightMM(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_logicalDpiX(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_logicalDpiY(QPaintDeviceH handle);
C_EXPORT QPaintEngineH QPaintDevice_paintEngine(QPaintDeviceH handle);
C_EXPORT bool QPaintDevice_paintingActive(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_physicalDpiX(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_physicalDpiY(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_width(QPaintDeviceH handle);
C_EXPORT int QPaintDevice_widthMM(QPaintDeviceH handle);

#endif
