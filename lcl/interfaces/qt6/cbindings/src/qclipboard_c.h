//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCLIPBOARD_C_H
#define QCLIPBOARD_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QClipboard_clear(QClipboardH handle, QClipboard::Mode mode);
C_EXPORT bool QClipboard_supportsSelection(QClipboardH handle);
C_EXPORT bool QClipboard_supportsFindBuffer(QClipboardH handle);
C_EXPORT bool QClipboard_ownsSelection(QClipboardH handle);
C_EXPORT bool QClipboard_ownsClipboard(QClipboardH handle);
C_EXPORT bool QClipboard_ownsFindBuffer(QClipboardH handle);
C_EXPORT void QClipboard_text(QClipboardH handle, PWideString retval, QClipboard::Mode mode);
C_EXPORT void QClipboard_text2(QClipboardH handle, PWideString retval, PWideString subtype, QClipboard::Mode mode);
C_EXPORT void QClipboard_setText(QClipboardH handle, PWideString AnonParam1, QClipboard::Mode mode);
C_EXPORT const QMimeDataH QClipboard_mimeData(QClipboardH handle, QClipboard::Mode mode);
C_EXPORT void QClipboard_setMimeData(QClipboardH handle, QMimeDataH data, QClipboard::Mode mode);
C_EXPORT void QClipboard_image(QClipboardH handle, QImageH retval, QClipboard::Mode mode);
C_EXPORT void QClipboard_pixmap(QClipboardH handle, QPixmapH retval, QClipboard::Mode mode);
C_EXPORT void QClipboard_setImage(QClipboardH handle, const QImageH AnonParam1, QClipboard::Mode mode);
C_EXPORT void QClipboard_setPixmap(QClipboardH handle, const QPixmapH AnonParam1, QClipboard::Mode mode);

#endif
