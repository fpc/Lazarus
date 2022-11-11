//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpaintdevice_c.h"

int QPaintDevice_devType(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->devType();
}

bool QPaintDevice_paintingActive(QPaintDeviceH handle)
{
	return (bool) ((QPaintDevice *)handle)->paintingActive();
}

QPaintEngineH QPaintDevice_paintEngine(QPaintDeviceH handle)
{
	return (QPaintEngineH) ((QPaintDevice *)handle)->paintEngine();
}

int QPaintDevice_width(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->width();
}

int QPaintDevice_height(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->height();
}

int QPaintDevice_widthMM(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->widthMM();
}

int QPaintDevice_heightMM(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->heightMM();
}

int QPaintDevice_logicalDpiX(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->logicalDpiX();
}

int QPaintDevice_logicalDpiY(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->logicalDpiY();
}

int QPaintDevice_physicalDpiX(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->physicalDpiX();
}

int QPaintDevice_physicalDpiY(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->physicalDpiY();
}

qreal QPaintDevice_devicePixelRatio(QPaintDeviceH handle)
{
	return (qreal) ((QPaintDevice *)handle)->devicePixelRatio();
}

qreal QPaintDevice_devicePixelRatioF(QPaintDeviceH handle)
{
	return (qreal) ((QPaintDevice *)handle)->devicePixelRatioF();
}


int QPaintDevice_colorCount(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->colorCount();
}

int QPaintDevice_depth(QPaintDeviceH handle)
{
	return (int) ((QPaintDevice *)handle)->depth();
}

