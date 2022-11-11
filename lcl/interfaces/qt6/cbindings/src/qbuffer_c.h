//******************************************************************************
//  Copyright (c) 2017 by Zeljan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QBUFFER_C_H
#define QBUFFER_C_H

#include <QtCore>
#include "pascalbind.h"
C_EXPORT QBufferH QBuffer_Create(QObjectH parent);
C_EXPORT QBufferH QBuffer_Create2(QByteArrayH bytearray, QObjectH parent);
C_EXPORT void QBuffer_Destroy(QBufferH handle);

C_EXPORT QByteArrayH QBuffer_buffer(QBufferH handle);
C_EXPORT const QByteArrayH QBuffer_constBuffer(QBufferH handle);
C_EXPORT void QBuffer_setBuffer(QBufferH handle,QByteArrayH buffer);
C_EXPORT void QBuffer_setData(QBufferH handle,char* buffer,int size);

C_EXPORT bool QBuffer_atEnd(QBufferH handle);
C_EXPORT bool QBuffer_canReadLine(QBufferH handle);
C_EXPORT void QBuffer_close(QBufferH handle);
C_EXPORT bool QBuffer_open(QBufferH handle, unsigned int mode);
C_EXPORT qint64 QBuffer_pos(QBufferH handle);
C_EXPORT bool QBuffer_seek(QBufferH handle, qint64 pos);
C_EXPORT qint64 QBuffer_size(QBufferH handle);

#endif
