//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMIMEDATA_C_H
#define QMIMEDATA_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QMimeDataH QMimeData_Create();
C_EXPORT void QMimeData_Destroy(QMimeDataH handle);
C_EXPORT bool QMimeData_hasUrls(QMimeDataH handle);
C_EXPORT void QMimeData_text(QMimeDataH handle, PWideString retval);
C_EXPORT void QMimeData_setText(QMimeDataH handle, PWideString text);
C_EXPORT bool QMimeData_hasText(QMimeDataH handle);
C_EXPORT void QMimeData_html(QMimeDataH handle, PWideString retval);
C_EXPORT void QMimeData_setHtml(QMimeDataH handle, PWideString html);
C_EXPORT bool QMimeData_hasHtml(QMimeDataH handle);
C_EXPORT void QMimeData_imageData(QMimeDataH handle, QVariantH retval);
C_EXPORT void QMimeData_setImageData(QMimeDataH handle, const QVariantH image);
C_EXPORT bool QMimeData_hasImage(QMimeDataH handle);
C_EXPORT void QMimeData_colorData(QMimeDataH handle, QVariantH retval);
C_EXPORT void QMimeData_setColorData(QMimeDataH handle, const QVariantH color);
C_EXPORT bool QMimeData_hasColor(QMimeDataH handle);
C_EXPORT void QMimeData_data(QMimeDataH handle, QByteArrayH retval, PWideString mimetype);
C_EXPORT void QMimeData_setData(QMimeDataH handle, PWideString mimetype, const QByteArrayH data);
C_EXPORT void QMimeData_removeFormat(QMimeDataH handle, PWideString mimetype);
C_EXPORT bool QMimeData_hasFormat(QMimeDataH handle, PWideString mimetype);
C_EXPORT void QMimeData_formats(QMimeDataH handle, QStringListH retval);
C_EXPORT void QMimeData_clear(QMimeDataH handle);

#endif
