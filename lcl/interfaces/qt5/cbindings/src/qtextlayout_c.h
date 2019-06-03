//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTLAYOUT_C_H
#define QTEXTLAYOUT_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QTextInlineObjectH QTextInlineObject_Create();
C_EXPORT void QTextInlineObject_Destroy(QTextInlineObjectH handle);
C_EXPORT bool QTextInlineObject_isValid(QTextInlineObjectH handle);
C_EXPORT void QTextInlineObject_rect(QTextInlineObjectH handle, QRectFH retval);
C_EXPORT qreal QTextInlineObject_width(QTextInlineObjectH handle);
C_EXPORT qreal QTextInlineObject_ascent(QTextInlineObjectH handle);
C_EXPORT qreal QTextInlineObject_descent(QTextInlineObjectH handle);
C_EXPORT qreal QTextInlineObject_height(QTextInlineObjectH handle);
C_EXPORT Qt::LayoutDirection QTextInlineObject_textDirection(QTextInlineObjectH handle);
C_EXPORT void QTextInlineObject_setWidth(QTextInlineObjectH handle, qreal w);
C_EXPORT void QTextInlineObject_setAscent(QTextInlineObjectH handle, qreal a);
C_EXPORT void QTextInlineObject_setDescent(QTextInlineObjectH handle, qreal d);
C_EXPORT int QTextInlineObject_textPosition(QTextInlineObjectH handle);
C_EXPORT int QTextInlineObject_formatIndex(QTextInlineObjectH handle);
C_EXPORT void QTextInlineObject_format(QTextInlineObjectH handle, QTextFormatH retval);
C_EXPORT QTextLayoutH QTextLayout_Create();
C_EXPORT void QTextLayout_Destroy(QTextLayoutH handle);
C_EXPORT QTextLayoutH QTextLayout_Create2(PWideString text);
C_EXPORT QTextLayoutH QTextLayout_Create3(PWideString text, const QFontH font, QPaintDeviceH paintdevice);
C_EXPORT QTextLayoutH QTextLayout_Create4(const QTextBlockH b);
C_EXPORT void QTextLayout_setFont(QTextLayoutH handle, const QFontH f);
C_EXPORT void QTextLayout_font(QTextLayoutH handle, QFontH retval);
C_EXPORT void QTextLayout_setRawFont(QTextLayoutH handle, const QRawFontH rawFont);
C_EXPORT void QTextLayout_setText(QTextLayoutH handle, PWideString string);
C_EXPORT void QTextLayout_text(QTextLayoutH handle, PWideString retval);
C_EXPORT void QTextLayout_setTextOption(QTextLayoutH handle, const QTextOptionH option);
C_EXPORT const QTextOptionH QTextLayout_textOption(QTextLayoutH handle);
C_EXPORT void QTextLayout_setPreeditArea(QTextLayoutH handle, int position, PWideString text);
C_EXPORT int QTextLayout_preeditAreaPosition(QTextLayoutH handle);
C_EXPORT void QTextLayout_preeditAreaText(QTextLayoutH handle, PWideString retval);
C_EXPORT void QTextLayout_setCacheEnabled(QTextLayoutH handle, bool enable);
C_EXPORT bool QTextLayout_cacheEnabled(QTextLayoutH handle);
C_EXPORT void QTextLayout_setCursorMoveStyle(QTextLayoutH handle, Qt::CursorMoveStyle style);
C_EXPORT Qt::CursorMoveStyle QTextLayout_cursorMoveStyle(QTextLayoutH handle);
C_EXPORT void QTextLayout_beginLayout(QTextLayoutH handle);
C_EXPORT void QTextLayout_endLayout(QTextLayoutH handle);
C_EXPORT void QTextLayout_clearLayout(QTextLayoutH handle);
C_EXPORT void QTextLayout_createLine(QTextLayoutH handle, QTextLineH retval);
C_EXPORT int QTextLayout_lineCount(QTextLayoutH handle);
C_EXPORT void QTextLayout_lineAt(QTextLayoutH handle, QTextLineH retval, int i);
C_EXPORT void QTextLayout_lineForTextPosition(QTextLayoutH handle, QTextLineH retval, int pos);
C_EXPORT bool QTextLayout_isValidCursorPosition(QTextLayoutH handle, int pos);
C_EXPORT int QTextLayout_nextCursorPosition(QTextLayoutH handle, int oldPos, QTextLayout::CursorMode mode);
C_EXPORT int QTextLayout_previousCursorPosition(QTextLayoutH handle, int oldPos, QTextLayout::CursorMode mode);
C_EXPORT int QTextLayout_leftCursorPosition(QTextLayoutH handle, int oldPos);
C_EXPORT int QTextLayout_rightCursorPosition(QTextLayoutH handle, int oldPos);
C_EXPORT void QTextLayout_drawCursor(QTextLayoutH handle, QPainterH p, const QPointFH pos, int cursorPosition);
C_EXPORT void QTextLayout_drawCursor2(QTextLayoutH handle, QPainterH p, const QPointFH pos, int cursorPosition, int width);
C_EXPORT void QTextLayout_position(QTextLayoutH handle, PQtPointF retval);
C_EXPORT void QTextLayout_setPosition(QTextLayoutH handle, const QPointFH p);
C_EXPORT void QTextLayout_boundingRect(QTextLayoutH handle, QRectFH retval);
C_EXPORT qreal QTextLayout_minimumWidth(QTextLayoutH handle);
C_EXPORT qreal QTextLayout_maximumWidth(QTextLayoutH handle);
C_EXPORT void QTextLayout_glyphRuns(QTextLayoutH handle, PPtrIntArray retval, int from, int length);
C_EXPORT void QTextLayout_setFlags(QTextLayoutH handle, int flags);

C_EXPORT void QTextLayout_clearFormats(QTextLayoutH handle);
C_EXPORT void QTextLayout_formats(QTextLayoutH handle, PPtrIntArray retval);
C_EXPORT void QTextLayout_setFormats(QTextLayoutH handle, PPtrIntArray fmts);

C_EXPORT QTextLineH QTextLine_Create();
C_EXPORT void QTextLine_Destroy(QTextLineH handle);
C_EXPORT bool QTextLine_isValid(QTextLineH handle);
C_EXPORT void QTextLine_rect(QTextLineH handle, QRectFH retval);
C_EXPORT qreal QTextLine_x(QTextLineH handle);
C_EXPORT qreal QTextLine_y(QTextLineH handle);
C_EXPORT qreal QTextLine_width(QTextLineH handle);
C_EXPORT qreal QTextLine_ascent(QTextLineH handle);
C_EXPORT qreal QTextLine_descent(QTextLineH handle);
C_EXPORT qreal QTextLine_height(QTextLineH handle);
C_EXPORT qreal QTextLine_leading(QTextLineH handle);
C_EXPORT void QTextLine_setLeadingIncluded(QTextLineH handle, bool included);
C_EXPORT bool QTextLine_leadingIncluded(QTextLineH handle);
C_EXPORT qreal QTextLine_naturalTextWidth(QTextLineH handle);
C_EXPORT qreal QTextLine_horizontalAdvance(QTextLineH handle);
C_EXPORT void QTextLine_naturalTextRect(QTextLineH handle, QRectFH retval);
C_EXPORT qreal QTextLine_cursorToX(QTextLineH handle, int* cursorPos, QTextLine::Edge edge);
C_EXPORT qreal QTextLine_cursorToX2(QTextLineH handle, int cursorPos, QTextLine::Edge edge);
C_EXPORT int QTextLine_xToCursor(QTextLineH handle, qreal x, QTextLine::CursorPosition AnonParam2);
C_EXPORT void QTextLine_setLineWidth(QTextLineH handle, qreal width);
C_EXPORT void QTextLine_setNumColumns(QTextLineH handle, int columns);
C_EXPORT void QTextLine_setNumColumns2(QTextLineH handle, int columns, qreal alignmentWidth);
C_EXPORT void QTextLine_setPosition(QTextLineH handle, const QPointFH pos);
C_EXPORT void QTextLine_position(QTextLineH handle, PQtPointF retval);
C_EXPORT int QTextLine_textStart(QTextLineH handle);
C_EXPORT int QTextLine_textLength(QTextLineH handle);
C_EXPORT int QTextLine_lineNumber(QTextLineH handle);
C_EXPORT void QTextLine_glyphRuns(QTextLineH handle, PPtrIntArray retval, int from, int length);

#endif
