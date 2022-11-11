//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qimagewriter_c.h"

QImageWriterH QImageWriter_Create()
{
	return (QImageWriterH) new QImageWriter();
}

void QImageWriter_Destroy(QImageWriterH handle)
{
	delete (QImageWriter *)handle;
}

QImageWriterH QImageWriter_Create2(QIODeviceH device, const QByteArrayH format)
{
	return (QImageWriterH) new QImageWriter((QIODevice*)device, *(const QByteArray*)format);
}

QImageWriterH QImageWriter_Create3(PWideString fileName, const QByteArrayH format)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (QImageWriterH) new QImageWriter(t_fileName, *(const QByteArray*)format);
}

void QImageWriter_setFormat(QImageWriterH handle, const QByteArrayH format)
{
	((QImageWriter *)handle)->setFormat(*(const QByteArray*)format);
}

void QImageWriter_format(QImageWriterH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QImageWriter *)handle)->format();
}

void QImageWriter_setDevice(QImageWriterH handle, QIODeviceH device)
{
	((QImageWriter *)handle)->setDevice((QIODevice*)device);
}

QIODeviceH QImageWriter_device(QImageWriterH handle)
{
	return (QIODeviceH) ((QImageWriter *)handle)->device();
}

void QImageWriter_setFileName(QImageWriterH handle, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	((QImageWriter *)handle)->setFileName(t_fileName);
}

void QImageWriter_fileName(QImageWriterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QImageWriter *)handle)->fileName();
	copyQStringToPWideString(t_retval, retval);
}

void QImageWriter_setQuality(QImageWriterH handle, int quality)
{
	((QImageWriter *)handle)->setQuality(quality);
}

int QImageWriter_quality(QImageWriterH handle)
{
	return (int) ((QImageWriter *)handle)->quality();
}

void QImageWriter_setCompression(QImageWriterH handle, int compression)
{
	((QImageWriter *)handle)->setCompression(compression);
}

int QImageWriter_compression(QImageWriterH handle)
{
	return (int) ((QImageWriter *)handle)->compression();
}

void QImageWriter_setText(QImageWriterH handle, PWideString key, PWideString text)
{
	QString t_key;
	QString t_text;
	copyPWideStringToQString(key, t_key);
	copyPWideStringToQString(text, t_text);
	((QImageWriter *)handle)->setText(t_key, t_text);
}

bool QImageWriter_canWrite(QImageWriterH handle)
{
	return (bool) ((QImageWriter *)handle)->canWrite();
}

bool QImageWriter_optimizedWrite(QImageWriterH handle)
{
	return (bool) ((QImageWriter *)handle)->optimizedWrite();
}

void QImageWriter_setOptimizedWrite(QImageWriterH handle, bool ok)
{
	((QImageWriter *)handle)->setOptimizedWrite(ok);
}

bool QImageWriter_progressiveScanWrite(QImageWriterH handle)
{
	return (bool) ((QImageWriter *)handle)->progressiveScanWrite();
}

void QImageWriter_setProgressiveScanWrite(QImageWriterH handle, bool ok)
{
	((QImageWriter *)handle)->setProgressiveScanWrite(ok);
}


bool QImageWriter_write(QImageWriterH handle, const QImageH image)
{
	return (bool) ((QImageWriter *)handle)->write(*(const QImage*)image);
}

QImageWriter::ImageWriterError QImageWriter_error(QImageWriterH handle)
{
	return (QImageWriter::ImageWriterError) ((QImageWriter *)handle)->error();
}

void QImageWriter_errorString(QImageWriterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QImageWriter *)handle)->errorString();
	copyQStringToPWideString(t_retval, retval);
}

bool QImageWriter_supportsOption(QImageWriterH handle, QImageIOHandler::ImageOption option)
{
	return (bool) ((QImageWriter *)handle)->supportsOption(option);
}

void QImageWriter_subType(QImageWriterH handle, QByteArrayH retval)
{
  *(QByteArray *)retval = ((QImageWriter *)handle)->subType();
}

void QImageWriter_setSubType(QImageWriterH handle, const QByteArrayH subtype)
{
  ((QImageWriter *)handle)->setSubType(*(const QByteArray *)subtype);
}

