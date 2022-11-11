//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextdocumentwriter_c.h"

QTextDocumentWriterH QTextDocumentWriter_Create()
{
	return (QTextDocumentWriterH) new QTextDocumentWriter();
}

void QTextDocumentWriter_Destroy(QTextDocumentWriterH handle)
{
	delete (QTextDocumentWriter *)handle;
}

QTextDocumentWriterH QTextDocumentWriter_Create2(QIODeviceH device, const QByteArrayH format)
{
	return (QTextDocumentWriterH) new QTextDocumentWriter((QIODevice*)device, *(const QByteArray*)format);
}

QTextDocumentWriterH QTextDocumentWriter_Create3(PWideString fileName, const QByteArrayH format)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (QTextDocumentWriterH) new QTextDocumentWriter(t_fileName, *(const QByteArray*)format);
}

void QTextDocumentWriter_setFormat(QTextDocumentWriterH handle, const QByteArrayH format)
{
	((QTextDocumentWriter *)handle)->setFormat(*(const QByteArray*)format);
}

void QTextDocumentWriter_format(QTextDocumentWriterH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QTextDocumentWriter *)handle)->format();
}

void QTextDocumentWriter_setDevice(QTextDocumentWriterH handle, QIODeviceH device)
{
	((QTextDocumentWriter *)handle)->setDevice((QIODevice*)device);
}

QIODeviceH QTextDocumentWriter_device(QTextDocumentWriterH handle)
{
	return (QIODeviceH) ((QTextDocumentWriter *)handle)->device();
}

void QTextDocumentWriter_setFileName(QTextDocumentWriterH handle, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	((QTextDocumentWriter *)handle)->setFileName(t_fileName);
}

void QTextDocumentWriter_fileName(QTextDocumentWriterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextDocumentWriter *)handle)->fileName();
	copyQStringToPWideString(t_retval, retval);
}

bool QTextDocumentWriter_write(QTextDocumentWriterH handle, const QTextDocumentH document)
{
	return (bool) ((QTextDocumentWriter *)handle)->write((const QTextDocument*)document);
}

bool QTextDocumentWriter_write2(QTextDocumentWriterH handle, const QTextDocumentFragmentH fragment)
{
	return (bool) ((QTextDocumentWriter *)handle)->write(*(const QTextDocumentFragment*)fragment);
}

