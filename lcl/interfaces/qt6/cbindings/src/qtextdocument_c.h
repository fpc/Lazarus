//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTDOCUMENT_C_H
#define QTEXTDOCUMENT_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QTextDocumentH QTextDocument_Create(QObjectH parent);
C_EXPORT void QTextDocument_Destroy(QTextDocumentH handle);
C_EXPORT QTextDocumentH QTextDocument_Create2(PWideString text, QObjectH parent);
C_EXPORT QTextDocumentH QTextDocument_clone(QTextDocumentH handle, QObjectH parent);
C_EXPORT bool QTextDocument_isEmpty(QTextDocumentH handle);
C_EXPORT void QTextDocument_clear(QTextDocumentH handle);
C_EXPORT void QTextDocument_setUndoRedoEnabled(QTextDocumentH handle, bool enable);
C_EXPORT bool QTextDocument_isUndoRedoEnabled(QTextDocumentH handle);
C_EXPORT bool QTextDocument_isUndoAvailable(QTextDocumentH handle);
C_EXPORT bool QTextDocument_isRedoAvailable(QTextDocumentH handle);
C_EXPORT int QTextDocument_availableUndoSteps(QTextDocumentH handle);
C_EXPORT int QTextDocument_availableRedoSteps(QTextDocumentH handle);
C_EXPORT int QTextDocument_revision(QTextDocumentH handle);
C_EXPORT void QTextDocument_setDocumentLayout(QTextDocumentH handle, QAbstractTextDocumentLayoutH layout);
C_EXPORT QAbstractTextDocumentLayoutH QTextDocument_documentLayout(QTextDocumentH handle);
C_EXPORT void QTextDocument_setMetaInformation(QTextDocumentH handle, QTextDocument::MetaInformation info, PWideString AnonParam2);
C_EXPORT void QTextDocument_metaInformation(QTextDocumentH handle, PWideString retval, QTextDocument::MetaInformation info);
C_EXPORT void QTextDocument_toHtml(QTextDocumentH handle, PWideString retval);
C_EXPORT void QTextDocument_setHtml(QTextDocumentH handle, PWideString html);
C_EXPORT void QTextDocument_toPlainText(QTextDocumentH handle, PWideString retval);
C_EXPORT void QTextDocument_setPlainText(QTextDocumentH handle, PWideString text);
C_EXPORT void QTextDocument_characterAt(QTextDocumentH handle, PWideChar retval, int pos);
C_EXPORT void QTextDocument_find(QTextDocumentH handle, QTextCursorH retval, PWideString subString, int from, unsigned int options);
C_EXPORT void QTextDocument_find2(QTextDocumentH handle, QTextCursorH retval, PWideString subString, const QTextCursorH from, unsigned int options);
C_EXPORT void QTextDocument_find3(QTextDocumentH handle, QTextCursorH retval, const QRegularExpressionH expr, int from, unsigned int options);
C_EXPORT void QTextDocument_find4(QTextDocumentH handle, QTextCursorH retval, const QRegularExpressionH expr, const QTextCursorH from, unsigned int options);
C_EXPORT QTextFrameH QTextDocument_frameAt(QTextDocumentH handle, int pos);
C_EXPORT QTextFrameH QTextDocument_rootFrame(QTextDocumentH handle);
C_EXPORT QTextObjectH QTextDocument_object(QTextDocumentH handle, int objectIndex);
C_EXPORT QTextObjectH QTextDocument_objectForFormat(QTextDocumentH handle, const QTextFormatH AnonParam1);
C_EXPORT void QTextDocument_findBlock(QTextDocumentH handle, QTextBlockH retval, int pos);
C_EXPORT void QTextDocument_findBlockByNumber(QTextDocumentH handle, QTextBlockH retval, int blockNumber);
C_EXPORT void QTextDocument_findBlockByLineNumber(QTextDocumentH handle, QTextBlockH retval, int blockNumber);
C_EXPORT void QTextDocument_begin(QTextDocumentH handle, QTextBlockH retval);
C_EXPORT void QTextDocument_end(QTextDocumentH handle, QTextBlockH retval);
C_EXPORT void QTextDocument_firstBlock(QTextDocumentH handle, QTextBlockH retval);
C_EXPORT void QTextDocument_lastBlock(QTextDocumentH handle, QTextBlockH retval);
C_EXPORT void QTextDocument_setPageSize(QTextDocumentH handle, const QSizeFH size);
C_EXPORT void QTextDocument_pageSize(QTextDocumentH handle, QSizeFH retval);
C_EXPORT void QTextDocument_setDefaultFont(QTextDocumentH handle, const QFontH font);
C_EXPORT void QTextDocument_defaultFont(QTextDocumentH handle, QFontH retval);
C_EXPORT int QTextDocument_pageCount(QTextDocumentH handle);
C_EXPORT bool QTextDocument_isModified(QTextDocumentH handle);
C_EXPORT void QTextDocument_print(QTextDocumentH handle, QPagedPaintDeviceH printer);
C_EXPORT void QTextDocument_resource(QTextDocumentH handle, QVariantH retval, int type, const QUrlH name);
C_EXPORT void QTextDocument_addResource(QTextDocumentH handle, int type, const QUrlH name, const QVariantH resource);
C_EXPORT void QTextDocument_markContentsDirty(QTextDocumentH handle, int from, int length);
C_EXPORT void QTextDocument_setUseDesignMetrics(QTextDocumentH handle, bool b);
C_EXPORT bool QTextDocument_useDesignMetrics(QTextDocumentH handle);
C_EXPORT void QTextDocument_drawContents(QTextDocumentH handle, QPainterH painter, const QRectFH rect);
C_EXPORT void QTextDocument_setTextWidth(QTextDocumentH handle, qreal width);
C_EXPORT qreal QTextDocument_textWidth(QTextDocumentH handle);
C_EXPORT qreal QTextDocument_idealWidth(QTextDocumentH handle);
C_EXPORT qreal QTextDocument_indentWidth(QTextDocumentH handle);
C_EXPORT void QTextDocument_setIndentWidth(QTextDocumentH handle, qreal width);
C_EXPORT qreal QTextDocument_documentMargin(QTextDocumentH handle);
C_EXPORT void QTextDocument_setDocumentMargin(QTextDocumentH handle, qreal margin);
C_EXPORT void QTextDocument_adjustSize(QTextDocumentH handle);
C_EXPORT void QTextDocument_size(QTextDocumentH handle, QSizeFH retval);
C_EXPORT int QTextDocument_blockCount(QTextDocumentH handle);
C_EXPORT int QTextDocument_lineCount(QTextDocumentH handle);
C_EXPORT int QTextDocument_characterCount(QTextDocumentH handle);
C_EXPORT void QTextDocument_setDefaultStyleSheet(QTextDocumentH handle, PWideString sheet);
C_EXPORT void QTextDocument_defaultStyleSheet(QTextDocumentH handle, PWideString retval);
C_EXPORT void QTextDocument_undo(QTextDocumentH handle, QTextCursorH cursor);
C_EXPORT void QTextDocument_redo(QTextDocumentH handle, QTextCursorH cursor);
C_EXPORT void QTextDocument_clearUndoRedoStacks(QTextDocumentH handle, QTextDocument::Stacks historyToClear);
C_EXPORT int QTextDocument_maximumBlockCount(QTextDocumentH handle);
C_EXPORT void QTextDocument_setMaximumBlockCount(QTextDocumentH handle, int maximum);
C_EXPORT void QTextDocument_defaultTextOption(QTextDocumentH handle, QTextOptionH retval);
C_EXPORT void QTextDocument_setDefaultTextOption(QTextDocumentH handle, const QTextOptionH option);
C_EXPORT Qt::CursorMoveStyle QTextDocument_defaultCursorMoveStyle(QTextDocumentH handle);
C_EXPORT void QTextDocument_setDefaultCursorMoveStyle(QTextDocumentH handle, Qt::CursorMoveStyle style);
C_EXPORT void QTextDocument_undo2(QTextDocumentH handle);
C_EXPORT void QTextDocument_redo2(QTextDocumentH handle);
C_EXPORT void QTextDocument_setModified(QTextDocumentH handle, bool m);
C_EXPORT void QTextDocument_baseUrl(QTextDocumentH handle, QUrlH retval);
C_EXPORT void QTextDocument_setBaseUrl(QTextDocumentH handle, const QUrlH url);
C_EXPORT void QTextDocument_toRawText(QTextDocumentH handle, PWideString retval);

#endif
