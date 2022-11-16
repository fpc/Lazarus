//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTTEXTDOCUMENTLAYOUT_C_H
#define QABSTRACTTEXTDOCUMENTLAYOUT_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT int QAbstractTextDocumentLayout_hitTest(QAbstractTextDocumentLayoutH handle, const QPointFH point, Qt::HitTestAccuracy accuracy);
C_EXPORT void QAbstractTextDocumentLayout_anchorAt(QAbstractTextDocumentLayoutH handle, PWideString retval, const QPointFH pos);
C_EXPORT int QAbstractTextDocumentLayout_pageCount(QAbstractTextDocumentLayoutH handle);
C_EXPORT void QAbstractTextDocumentLayout_documentSize(QAbstractTextDocumentLayoutH handle, QSizeFH retval);
C_EXPORT void QAbstractTextDocumentLayout_frameBoundingRect(QAbstractTextDocumentLayoutH handle, QRectFH retval, QTextFrameH frame);
C_EXPORT void QAbstractTextDocumentLayout_blockBoundingRect(QAbstractTextDocumentLayoutH handle, QRectFH retval, const QTextBlockH block);
C_EXPORT void QAbstractTextDocumentLayout_setPaintDevice(QAbstractTextDocumentLayoutH handle, QPaintDeviceH device);
C_EXPORT QPaintDeviceH QAbstractTextDocumentLayout_paintDevice(QAbstractTextDocumentLayoutH handle);
C_EXPORT QTextDocumentH QAbstractTextDocumentLayout_document(QAbstractTextDocumentLayoutH handle);
C_EXPORT void QAbstractTextDocumentLayout_registerHandler(QAbstractTextDocumentLayoutH handle, int objectType, QObjectH component);
C_EXPORT void QAbstractTextDocumentLayout_unregisterHandler(QAbstractTextDocumentLayoutH handle, int objectType, QObjectH component);
C_EXPORT void QAbstractTextDocumentLayout_blockWithMarkerAt(QAbstractTextDocumentLayoutH handle, PQtPointF pt, QTextBlockH retval);
C_EXPORT void QAbstractTextDocumentLayout_formatAt(QAbstractTextDocumentLayoutH handle, PQtPointF pt, QTextFormatH retval);
C_EXPORT void QAbstractTextDocumentLayout_imageAt(QAbstractTextDocumentLayoutH handle, PQtPointF pt, PWideString retval);
C_EXPORT QTextObjectInterfaceH QAbstractTextDocumentLayout_handlerForObject(QAbstractTextDocumentLayoutH handle, int objectType);
C_EXPORT void QTextObjectInterface_intrinsicSize(QTextObjectInterfaceH handle, QSizeFH retval, QTextDocumentH doc, int posInDocument, const QTextFormatH format);
C_EXPORT void QTextObjectInterface_drawObject(QTextObjectInterfaceH handle, QPainterH painter, const QRectFH rect, QTextDocumentH doc, int posInDocument, const QTextFormatH format);

#endif
