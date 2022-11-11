//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextlayout_c.h"

QTextInlineObjectH QTextInlineObject_Create()
{
	return (QTextInlineObjectH) new QTextInlineObject();
}

void QTextInlineObject_Destroy(QTextInlineObjectH handle)
{
	delete (QTextInlineObject *)handle;
}

bool QTextInlineObject_isValid(QTextInlineObjectH handle)
{
	return (bool) ((QTextInlineObject *)handle)->isValid();
}

void QTextInlineObject_rect(QTextInlineObjectH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QTextInlineObject *)handle)->rect();
}

qreal QTextInlineObject_width(QTextInlineObjectH handle)
{
	return (qreal) ((QTextInlineObject *)handle)->width();
}

qreal QTextInlineObject_ascent(QTextInlineObjectH handle)
{
	return (qreal) ((QTextInlineObject *)handle)->ascent();
}

qreal QTextInlineObject_descent(QTextInlineObjectH handle)
{
	return (qreal) ((QTextInlineObject *)handle)->descent();
}

qreal QTextInlineObject_height(QTextInlineObjectH handle)
{
	return (qreal) ((QTextInlineObject *)handle)->height();
}

Qt::LayoutDirection QTextInlineObject_textDirection(QTextInlineObjectH handle)
{
	return (Qt::LayoutDirection) ((QTextInlineObject *)handle)->textDirection();
}

void QTextInlineObject_setWidth(QTextInlineObjectH handle, qreal w)
{
	((QTextInlineObject *)handle)->setWidth(w);
}

void QTextInlineObject_setAscent(QTextInlineObjectH handle, qreal a)
{
	((QTextInlineObject *)handle)->setAscent(a);
}

void QTextInlineObject_setDescent(QTextInlineObjectH handle, qreal d)
{
	((QTextInlineObject *)handle)->setDescent(d);
}

int QTextInlineObject_textPosition(QTextInlineObjectH handle)
{
	return (int) ((QTextInlineObject *)handle)->textPosition();
}

int QTextInlineObject_formatIndex(QTextInlineObjectH handle)
{
	return (int) ((QTextInlineObject *)handle)->formatIndex();
}

void QTextInlineObject_format(QTextInlineObjectH handle, QTextFormatH retval)
{
	*(QTextFormat *)retval = ((QTextInlineObject *)handle)->format();
}

QTextLayoutH QTextLayout_Create()
{
	return (QTextLayoutH) new QTextLayout();
}

void QTextLayout_Destroy(QTextLayoutH handle)
{
	delete (QTextLayout *)handle;
}

QTextLayoutH QTextLayout_Create2(PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QTextLayoutH) new QTextLayout(t_text);
}

QTextLayoutH QTextLayout_Create3(PWideString text, const QFontH font, QPaintDeviceH paintdevice)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QTextLayoutH) new QTextLayout(t_text, *(const QFont*)font, (QPaintDevice*)paintdevice);
}

QTextLayoutH QTextLayout_Create4(const QTextBlockH b)
{
	return (QTextLayoutH) new QTextLayout(*(const QTextBlock*)b);
}

void QTextLayout_setFont(QTextLayoutH handle, const QFontH f)
{
	((QTextLayout *)handle)->setFont(*(const QFont*)f);
}

void QTextLayout_font(QTextLayoutH handle, QFontH retval)
{
	*(QFont *)retval = ((QTextLayout *)handle)->font();
}

void QTextLayout_setRawFont(QTextLayoutH handle, const QRawFontH rawFont)
{
	((QTextLayout *)handle)->setRawFont(*(const QRawFont*)rawFont);
}

void QTextLayout_setText(QTextLayoutH handle, PWideString string)
{
	QString t_string;
	copyPWideStringToQString(string, t_string);
	((QTextLayout *)handle)->setText(t_string);
}

void QTextLayout_text(QTextLayoutH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextLayout *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QTextLayout_setTextOption(QTextLayoutH handle, const QTextOptionH option)
{
	((QTextLayout *)handle)->setTextOption(*(const QTextOption*)option);
}

const QTextOptionH QTextLayout_textOption(QTextLayoutH handle)
{
	return (const QTextOptionH) &((QTextLayout *)handle)->textOption();
}

void QTextLayout_setPreeditArea(QTextLayoutH handle, int position, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextLayout *)handle)->setPreeditArea(position, t_text);
}

int QTextLayout_preeditAreaPosition(QTextLayoutH handle)
{
	return (int) ((QTextLayout *)handle)->preeditAreaPosition();
}

void QTextLayout_preeditAreaText(QTextLayoutH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextLayout *)handle)->preeditAreaText();
	copyQStringToPWideString(t_retval, retval);
}


void QTextLayout_setCacheEnabled(QTextLayoutH handle, bool enable)
{
	((QTextLayout *)handle)->setCacheEnabled(enable);
}

bool QTextLayout_cacheEnabled(QTextLayoutH handle)
{
	return (bool) ((QTextLayout *)handle)->cacheEnabled();
}

void QTextLayout_setCursorMoveStyle(QTextLayoutH handle, Qt::CursorMoveStyle style)
{
	((QTextLayout *)handle)->setCursorMoveStyle(style);
}

Qt::CursorMoveStyle QTextLayout_cursorMoveStyle(QTextLayoutH handle)
{
	return (Qt::CursorMoveStyle) ((QTextLayout *)handle)->cursorMoveStyle();
}

void QTextLayout_beginLayout(QTextLayoutH handle)
{
	((QTextLayout *)handle)->beginLayout();
}

void QTextLayout_endLayout(QTextLayoutH handle)
{
	((QTextLayout *)handle)->endLayout();
}

void QTextLayout_clearLayout(QTextLayoutH handle)
{
	((QTextLayout *)handle)->clearLayout();
}

void QTextLayout_createLine(QTextLayoutH handle, QTextLineH retval)
{
	*(QTextLine *)retval = ((QTextLayout *)handle)->createLine();
}

int QTextLayout_lineCount(QTextLayoutH handle)
{
	return (int) ((QTextLayout *)handle)->lineCount();
}

void QTextLayout_lineAt(QTextLayoutH handle, QTextLineH retval, int i)
{
	*(QTextLine *)retval = ((QTextLayout *)handle)->lineAt(i);
}

void QTextLayout_lineForTextPosition(QTextLayoutH handle, QTextLineH retval, int pos)
{
	*(QTextLine *)retval = ((QTextLayout *)handle)->lineForTextPosition(pos);
}

bool QTextLayout_isValidCursorPosition(QTextLayoutH handle, int pos)
{
	return (bool) ((QTextLayout *)handle)->isValidCursorPosition(pos);
}

int QTextLayout_nextCursorPosition(QTextLayoutH handle, int oldPos, QTextLayout::CursorMode mode)
{
	return (int) ((QTextLayout *)handle)->nextCursorPosition(oldPos, mode);
}

int QTextLayout_previousCursorPosition(QTextLayoutH handle, int oldPos, QTextLayout::CursorMode mode)
{
	return (int) ((QTextLayout *)handle)->previousCursorPosition(oldPos, mode);
}

int QTextLayout_leftCursorPosition(QTextLayoutH handle, int oldPos)
{
	return (int) ((QTextLayout *)handle)->leftCursorPosition(oldPos);
}

int QTextLayout_rightCursorPosition(QTextLayoutH handle, int oldPos)
{
	return (int) ((QTextLayout *)handle)->rightCursorPosition(oldPos);
}

void QTextLayout_drawCursor(QTextLayoutH handle, QPainterH p, const QPointFH pos, int cursorPosition)
{
	((QTextLayout *)handle)->drawCursor((QPainter*)p, *(const QPointF*)pos, cursorPosition);
}

void QTextLayout_drawCursor2(QTextLayoutH handle, QPainterH p, const QPointFH pos, int cursorPosition, int width)
{
	((QTextLayout *)handle)->drawCursor((QPainter*)p, *(const QPointF*)pos, cursorPosition, width);
}

void QTextLayout_position(QTextLayoutH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QTextLayout *)handle)->position();
}

void QTextLayout_setPosition(QTextLayoutH handle, const QPointFH p)
{
	((QTextLayout *)handle)->setPosition(*(const QPointF*)p);
}

void QTextLayout_boundingRect(QTextLayoutH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QTextLayout *)handle)->boundingRect();
}

qreal QTextLayout_minimumWidth(QTextLayoutH handle)
{
	return (qreal) ((QTextLayout *)handle)->minimumWidth();
}

qreal QTextLayout_maximumWidth(QTextLayoutH handle)
{
	return (qreal) ((QTextLayout *)handle)->maximumWidth();
}

void QTextLayout_glyphRuns(QTextLayoutH handle, PPtrIntArray retval, int from, int length)
{
	QList<QGlyphRun> t_retval;
	t_retval = ((QTextLayout *)handle)->glyphRuns(from, length);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QTextLayout_setFlags(QTextLayoutH handle, int flags)
{
	((QTextLayout *)handle)->setFlags(flags);
}

void QTextLayout_clearFormats(QTextLayoutH handle)
{
	((QTextLayout *)handle)->clearFormats();
}

int QTextLayout_formatsCount(QTextLayoutH handle)
{
  return ((QTextLayout *)handle)->formats().size();
}

int QTextLayout_textFormatsRanges(QTextLayoutH handle, PTextRange ranges, int maxCount)
{
  QVector<QTextLayout::FormatRange> t_retval;
  int res = 0;
  int cnt = maxCount;
  int sz;

  t_retval = ((QTextLayout *)handle)->formats();
  sz = t_retval.size();
  if (sz > cnt) cnt = sz;

  for (int i=0; i<sz; i++)
  {
    ranges[i].start = t_retval.at(i).start;
    ranges[i].length = t_retval.at(i).length;
  }
  return res;
}

QTextLineH QTextLine_Create()
{
	return (QTextLineH) new QTextLine();
}

void QTextLine_Destroy(QTextLineH handle)
{
	delete (QTextLine *)handle;
}

bool QTextLine_isValid(QTextLineH handle)
{
	return (bool) ((QTextLine *)handle)->isValid();
}

void QTextLine_rect(QTextLineH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QTextLine *)handle)->rect();
}

qreal QTextLine_x(QTextLineH handle)
{
	return (qreal) ((QTextLine *)handle)->x();
}

qreal QTextLine_y(QTextLineH handle)
{
	return (qreal) ((QTextLine *)handle)->y();
}

qreal QTextLine_width(QTextLineH handle)
{
	return (qreal) ((QTextLine *)handle)->width();
}

qreal QTextLine_ascent(QTextLineH handle)
{
	return (qreal) ((QTextLine *)handle)->ascent();
}

qreal QTextLine_descent(QTextLineH handle)
{
	return (qreal) ((QTextLine *)handle)->descent();
}

qreal QTextLine_height(QTextLineH handle)
{
	return (qreal) ((QTextLine *)handle)->height();
}

qreal QTextLine_leading(QTextLineH handle)
{
	return (qreal) ((QTextLine *)handle)->leading();
}

void QTextLine_setLeadingIncluded(QTextLineH handle, bool included)
{
	((QTextLine *)handle)->setLeadingIncluded(included);
}

bool QTextLine_leadingIncluded(QTextLineH handle)
{
	return (bool) ((QTextLine *)handle)->leadingIncluded();
}

qreal QTextLine_naturalTextWidth(QTextLineH handle)
{
	return (qreal) ((QTextLine *)handle)->naturalTextWidth();
}

qreal QTextLine_horizontalAdvance(QTextLineH handle)
{
	return (qreal) ((QTextLine *)handle)->horizontalAdvance();
}

void QTextLine_naturalTextRect(QTextLineH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QTextLine *)handle)->naturalTextRect();
}

qreal QTextLine_cursorToX(QTextLineH handle, int* cursorPos, QTextLine::Edge edge)
{
	return (qreal) ((QTextLine *)handle)->cursorToX(cursorPos, edge);
}

qreal QTextLine_cursorToX2(QTextLineH handle, int cursorPos, QTextLine::Edge edge)
{
	return (qreal) ((QTextLine *)handle)->cursorToX(cursorPos, edge);
}

int QTextLine_xToCursor(QTextLineH handle, qreal x, QTextLine::CursorPosition AnonParam2)
{
	return (int) ((QTextLine *)handle)->xToCursor(x, AnonParam2);
}

void QTextLine_setLineWidth(QTextLineH handle, qreal width)
{
	((QTextLine *)handle)->setLineWidth(width);
}

void QTextLine_setNumColumns(QTextLineH handle, int columns)
{
	((QTextLine *)handle)->setNumColumns(columns);
}

void QTextLine_setNumColumns2(QTextLineH handle, int columns, qreal alignmentWidth)
{
	((QTextLine *)handle)->setNumColumns(columns, alignmentWidth);
}

void QTextLine_setPosition(QTextLineH handle, const QPointFH pos)
{
	((QTextLine *)handle)->setPosition(*(const QPointF*)pos);
}

void QTextLine_position(QTextLineH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QTextLine *)handle)->position();
}

int QTextLine_textStart(QTextLineH handle)
{
	return (int) ((QTextLine *)handle)->textStart();
}

int QTextLine_textLength(QTextLineH handle)
{
	return (int) ((QTextLine *)handle)->textLength();
}

int QTextLine_lineNumber(QTextLineH handle)
{
	return (int) ((QTextLine *)handle)->lineNumber();
}

void QTextLine_glyphRuns(QTextLineH handle, PPtrIntArray retval, int from, int length)
{
	QList<QGlyphRun> t_retval;
	t_retval = ((QTextLine *)handle)->glyphRuns(from, length);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}


