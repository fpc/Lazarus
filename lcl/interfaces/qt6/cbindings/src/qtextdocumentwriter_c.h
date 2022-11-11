//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTDOCUMENTWRITER_C_H
#define QTEXTDOCUMENTWRITER_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QTextDocumentWriterH QTextDocumentWriter_Create();
C_EXPORT void QTextDocumentWriter_Destroy(QTextDocumentWriterH handle);
C_EXPORT QTextDocumentWriterH QTextDocumentWriter_Create2(QIODeviceH device, const QByteArrayH format);
C_EXPORT QTextDocumentWriterH QTextDocumentWriter_Create3(PWideString fileName, const QByteArrayH format);
C_EXPORT void QTextDocumentWriter_setFormat(QTextDocumentWriterH handle, const QByteArrayH format);
C_EXPORT void QTextDocumentWriter_format(QTextDocumentWriterH handle, QByteArrayH retval);
C_EXPORT void QTextDocumentWriter_setDevice(QTextDocumentWriterH handle, QIODeviceH device);
C_EXPORT QIODeviceH QTextDocumentWriter_device(QTextDocumentWriterH handle);
C_EXPORT void QTextDocumentWriter_setFileName(QTextDocumentWriterH handle, PWideString fileName);
C_EXPORT void QTextDocumentWriter_fileName(QTextDocumentWriterH handle, PWideString retval);
C_EXPORT bool QTextDocumentWriter_write(QTextDocumentWriterH handle, const QTextDocumentH document);
C_EXPORT bool QTextDocumentWriter_write2(QTextDocumentWriterH handle, const QTextDocumentFragmentH fragment);

#endif
