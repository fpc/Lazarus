//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpicture_c.h"

QPictureH QPicture_Create(int formatVersion)
{
	return (QPictureH) new QPicture(formatVersion);
}

void QPicture_Destroy(QPictureH handle)
{
	delete (QPicture *)handle;
}

QPictureH QPicture_Create2(const QPictureH AnonParam1)
{
	return (QPictureH) new QPicture(*(const QPicture*)AnonParam1);
}

bool QPicture_isNull(QPictureH handle)
{
	return (bool) ((QPicture *)handle)->isNull();
}

int QPicture_devType(QPictureH handle)
{
	return (int) ((QPicture *)handle)->devType();
}

uint QPicture_size(QPictureH handle)
{
	return (uint) ((QPicture *)handle)->size();
}

const char* QPicture_data(QPictureH handle)
{
	return (const char*) ((QPicture *)handle)->data();
}

void QPicture_setData(QPictureH handle, const char* data, uint size)
{
	((QPicture *)handle)->setData(data, size);
}

bool QPicture_play(QPictureH handle, QPainterH p)
{
	return (bool) ((QPicture *)handle)->play((QPainter*)p);
}

bool QPicture_load(QPictureH handle, QIODeviceH dev)
{
	return (bool) ((QPicture *)handle)->load((QIODevice*)dev);
}

bool QPicture_load2(QPictureH handle, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) ((QPicture *)handle)->load(t_fileName);
}

bool QPicture_save(QPictureH handle, QIODeviceH dev)
{
	return (bool) ((QPicture *)handle)->save((QIODevice*)dev);
}

bool QPicture_save2(QPictureH handle, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) ((QPicture *)handle)->save(t_fileName);
}

void QPicture_boundingRect(QPictureH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPicture *)handle)->boundingRect();
	copyQRectToPRect(t_retval, retval);
}

void QPicture_setBoundingRect(QPictureH handle, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QPicture *)handle)->setBoundingRect(t_r);
}

void QPicture_swap(QPictureH handle, QPictureH other)
{
	((QPicture *)handle)->swap(*(QPicture*)other);
}

void QPicture_detach(QPictureH handle)
{
	((QPicture *)handle)->detach();
}

bool QPicture_isDetached(QPictureH handle)
{
	return (bool) ((QPicture *)handle)->isDetached();
}

QPaintEngineH QPicture_paintEngine(QPictureH handle)
{
	return (QPaintEngineH) ((QPicture *)handle)->paintEngine();
}

