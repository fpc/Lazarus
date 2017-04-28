//******************************************************************************
//  Copyright (c) 2017 Zeljan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qbuffer_c.h"

QBufferH QBuffer_Create(QObjectH parent)
{
	return (QBufferH) new QBuffer((QObject*)parent);
}

QBufferH QBuffer_Create2(QByteArrayH bytearray, QObjectH parent)
{
	return (QBufferH) new QBuffer((QByteArray*)bytearray, (QObject*)parent);
}

void QBuffer_Destroy(QBufferH handle)
{
	delete (QBuffer *)handle;
}

QByteArrayH QBuffer_buffer(QBufferH handle)
{
  return (QByteArrayH) &((QBuffer *)handle)->buffer();
}

const QByteArrayH QBuffer_constBuffer(QBufferH handle)
{
  return (const QByteArrayH) &((QBuffer *)handle)->buffer();
}

void QBuffer_setBuffer(QBufferH handle,QByteArrayH buffer)
{
  ((QBuffer *)handle)->setBuffer((QByteArray*)buffer);
}

void QBuffer_setData(QBufferH handle,char* buffer,int size)
{
  ((QBuffer *)handle)->setData(buffer, size);
}

bool QBuffer_atEnd(QBufferH handle)
{
	return (bool) ((QBuffer *)handle)->atEnd();
}

bool QBuffer_canReadLine(QBufferH handle)
{
	return (bool) ((QBuffer *)handle)->canReadLine();
}

void QBuffer_close(QBufferH handle)
{
	((QBuffer *)handle)->close();
}

bool QBuffer_open(QBufferH handle, unsigned int mode)
{
	return (bool) ((QBuffer *)handle)->open((QIODevice::OpenMode)mode);
}


qint64 QBuffer_pos(QBufferH handle)
{
	return (qint64) ((QBuffer *)handle)->pos();
}

qint64 QBuffer_size(QBufferH handle)
{
	return (qint64) ((QBuffer *)handle)->size();
}

bool QBuffer_seek(QBufferH handle, qint64 pos)
{
	return (bool) ((QBuffer *)handle)->seek(pos);
}



