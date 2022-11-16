//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextdocument_c.h"

QTextDocumentH QTextDocument_Create(QObjectH parent)
{
	return (QTextDocumentH) new QTextDocument((QObject*)parent);
}

void QTextDocument_Destroy(QTextDocumentH handle)
{
	delete (QTextDocument *)handle;
}

QTextDocumentH QTextDocument_Create2(PWideString text, QObjectH parent)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QTextDocumentH) new QTextDocument(t_text, (QObject*)parent);
}

QTextDocumentH QTextDocument_clone(QTextDocumentH handle, QObjectH parent)
{
	return (QTextDocumentH) ((QTextDocument *)handle)->clone((QObject*)parent);
}

bool QTextDocument_isEmpty(QTextDocumentH handle)
{
	return (bool) ((QTextDocument *)handle)->isEmpty();
}

void QTextDocument_clear(QTextDocumentH handle)
{
	((QTextDocument *)handle)->clear();
}

void QTextDocument_setUndoRedoEnabled(QTextDocumentH handle, bool enable)
{
	((QTextDocument *)handle)->setUndoRedoEnabled(enable);
}

bool QTextDocument_isUndoRedoEnabled(QTextDocumentH handle)
{
	return (bool) ((QTextDocument *)handle)->isUndoRedoEnabled();
}

bool QTextDocument_isUndoAvailable(QTextDocumentH handle)
{
	return (bool) ((QTextDocument *)handle)->isUndoAvailable();
}

bool QTextDocument_isRedoAvailable(QTextDocumentH handle)
{
	return (bool) ((QTextDocument *)handle)->isRedoAvailable();
}

int QTextDocument_availableUndoSteps(QTextDocumentH handle)
{
	return (int) ((QTextDocument *)handle)->availableUndoSteps();
}

int QTextDocument_availableRedoSteps(QTextDocumentH handle)
{
	return (int) ((QTextDocument *)handle)->availableRedoSteps();
}

int QTextDocument_revision(QTextDocumentH handle)
{
	return (int) ((QTextDocument *)handle)->revision();
}

void QTextDocument_setDocumentLayout(QTextDocumentH handle, QAbstractTextDocumentLayoutH layout)
{
	((QTextDocument *)handle)->setDocumentLayout((QAbstractTextDocumentLayout*)layout);
}

QAbstractTextDocumentLayoutH QTextDocument_documentLayout(QTextDocumentH handle)
{
	return (QAbstractTextDocumentLayoutH) ((QTextDocument *)handle)->documentLayout();
}

void QTextDocument_setMetaInformation(QTextDocumentH handle, QTextDocument::MetaInformation info, PWideString AnonParam2)
{
	QString t_AnonParam2;
	copyPWideStringToQString(AnonParam2, t_AnonParam2);
	((QTextDocument *)handle)->setMetaInformation(info, t_AnonParam2);
}

void QTextDocument_metaInformation(QTextDocumentH handle, PWideString retval, QTextDocument::MetaInformation info)
{
	QString t_retval;
	t_retval = ((QTextDocument *)handle)->metaInformation(info);
	copyQStringToPWideString(t_retval, retval);
}

void QTextDocument_toHtml(QTextDocumentH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextDocument *)handle)->toHtml();
	copyQStringToPWideString(t_retval, retval);
}

void QTextDocument_setHtml(QTextDocumentH handle, PWideString html)
{
	QString t_html;
	copyPWideStringToQString(html, t_html);
	((QTextDocument *)handle)->setHtml(t_html);
}

void QTextDocument_toPlainText(QTextDocumentH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextDocument *)handle)->toPlainText();
	copyQStringToPWideString(t_retval, retval);
}

void QTextDocument_setPlainText(QTextDocumentH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextDocument *)handle)->setPlainText(t_text);
}

void QTextDocument_characterAt(QTextDocumentH handle, PWideChar retval, int pos)
{
	*(QChar *)retval = ((QTextDocument *)handle)->characterAt(pos);
}

void QTextDocument_find(QTextDocumentH handle, QTextCursorH retval, PWideString subString, int from, unsigned int options)
{
	QString t_subString;
	copyPWideStringToQString(subString, t_subString);
	*(QTextCursor *)retval = ((QTextDocument *)handle)->find(t_subString, from, (QTextDocument::FindFlags)options);
}

void QTextDocument_find2(QTextDocumentH handle, QTextCursorH retval, PWideString subString, const QTextCursorH from, unsigned int options)
{
	QString t_subString;
	copyPWideStringToQString(subString, t_subString);
	*(QTextCursor *)retval = ((QTextDocument *)handle)->find(t_subString, *(const QTextCursor*)from, (QTextDocument::FindFlags)options);
}


void QTextDocument_find3(QTextDocumentH handle, QTextCursorH retval, const QRegularExpressionH expr, int from, unsigned int options)
{
	*(QTextCursor *)retval = ((QTextDocument *)handle)->find(*(const QRegularExpression*)expr, from, (QTextDocument::FindFlags)options);
}

void QTextDocument_find4(QTextDocumentH handle, QTextCursorH retval, const QRegularExpressionH expr, const QTextCursorH from, unsigned int options)
{
	*(QTextCursor *)retval = ((QTextDocument *)handle)->find(*(const QRegularExpression*)expr, *(const QTextCursor*)from, (QTextDocument::FindFlags)options);
}


QTextFrameH QTextDocument_frameAt(QTextDocumentH handle, int pos)
{
	return (QTextFrameH) ((QTextDocument *)handle)->frameAt(pos);
}

QTextFrameH QTextDocument_rootFrame(QTextDocumentH handle)
{
	return (QTextFrameH) ((QTextDocument *)handle)->rootFrame();
}

QTextObjectH QTextDocument_object(QTextDocumentH handle, int objectIndex)
{
	return (QTextObjectH) ((QTextDocument *)handle)->object(objectIndex);
}

QTextObjectH QTextDocument_objectForFormat(QTextDocumentH handle, const QTextFormatH AnonParam1)
{
	return (QTextObjectH) ((QTextDocument *)handle)->objectForFormat(*(const QTextFormat*)AnonParam1);
}

void QTextDocument_findBlock(QTextDocumentH handle, QTextBlockH retval, int pos)
{
	*(QTextBlock *)retval = ((QTextDocument *)handle)->findBlock(pos);
}

void QTextDocument_findBlockByNumber(QTextDocumentH handle, QTextBlockH retval, int blockNumber)
{
	*(QTextBlock *)retval = ((QTextDocument *)handle)->findBlockByNumber(blockNumber);
}

void QTextDocument_findBlockByLineNumber(QTextDocumentH handle, QTextBlockH retval, int blockNumber)
{
	*(QTextBlock *)retval = ((QTextDocument *)handle)->findBlockByLineNumber(blockNumber);
}

void QTextDocument_begin(QTextDocumentH handle, QTextBlockH retval)
{
	*(QTextBlock *)retval = ((QTextDocument *)handle)->begin();
}

void QTextDocument_end(QTextDocumentH handle, QTextBlockH retval)
{
	*(QTextBlock *)retval = ((QTextDocument *)handle)->end();
}

void QTextDocument_firstBlock(QTextDocumentH handle, QTextBlockH retval)
{
	*(QTextBlock *)retval = ((QTextDocument *)handle)->firstBlock();
}

void QTextDocument_lastBlock(QTextDocumentH handle, QTextBlockH retval)
{
	*(QTextBlock *)retval = ((QTextDocument *)handle)->lastBlock();
}

void QTextDocument_setPageSize(QTextDocumentH handle, const QSizeFH size)
{
	((QTextDocument *)handle)->setPageSize(*(const QSizeF*)size);
}

void QTextDocument_pageSize(QTextDocumentH handle, QSizeFH retval)
{
	*(QSizeF *)retval = ((QTextDocument *)handle)->pageSize();
}

void QTextDocument_setDefaultFont(QTextDocumentH handle, const QFontH font)
{
	((QTextDocument *)handle)->setDefaultFont(*(const QFont*)font);
}

void QTextDocument_defaultFont(QTextDocumentH handle, QFontH retval)
{
	*(QFont *)retval = ((QTextDocument *)handle)->defaultFont();
}

int QTextDocument_pageCount(QTextDocumentH handle)
{
	return (int) ((QTextDocument *)handle)->pageCount();
}

bool QTextDocument_isModified(QTextDocumentH handle)
{
	return (bool) ((QTextDocument *)handle)->isModified();
}

void QTextDocument_print(QTextDocumentH handle, QPagedPaintDeviceH printer)
{
	((QTextDocument *)handle)->print((QPagedPaintDevice*)printer);
}

void QTextDocument_resource(QTextDocumentH handle, QVariantH retval, int type, const QUrlH name)
{
	*(QVariant *)retval = ((QTextDocument *)handle)->resource(type, *(const QUrl*)name);
}

void QTextDocument_addResource(QTextDocumentH handle, int type, const QUrlH name, const QVariantH resource)
{
	((QTextDocument *)handle)->addResource(type, *(const QUrl*)name, *(const QVariant*)resource);
}

void QTextDocument_markContentsDirty(QTextDocumentH handle, int from, int length)
{
	((QTextDocument *)handle)->markContentsDirty(from, length);
}

void QTextDocument_setUseDesignMetrics(QTextDocumentH handle, bool b)
{
	((QTextDocument *)handle)->setUseDesignMetrics(b);
}

bool QTextDocument_useDesignMetrics(QTextDocumentH handle)
{
	return (bool) ((QTextDocument *)handle)->useDesignMetrics();
}

void QTextDocument_drawContents(QTextDocumentH handle, QPainterH painter, const QRectFH rect)
{
	((QTextDocument *)handle)->drawContents((QPainter*)painter, *(const QRectF*)rect);
}

void QTextDocument_setTextWidth(QTextDocumentH handle, qreal width)
{
	((QTextDocument *)handle)->setTextWidth(width);
}

qreal QTextDocument_textWidth(QTextDocumentH handle)
{
	return (qreal) ((QTextDocument *)handle)->textWidth();
}

qreal QTextDocument_idealWidth(QTextDocumentH handle)
{
	return (qreal) ((QTextDocument *)handle)->idealWidth();
}

qreal QTextDocument_indentWidth(QTextDocumentH handle)
{
	return (qreal) ((QTextDocument *)handle)->indentWidth();
}

void QTextDocument_setIndentWidth(QTextDocumentH handle, qreal width)
{
	((QTextDocument *)handle)->setIndentWidth(width);
}

qreal QTextDocument_documentMargin(QTextDocumentH handle)
{
	return (qreal) ((QTextDocument *)handle)->documentMargin();
}

void QTextDocument_setDocumentMargin(QTextDocumentH handle, qreal margin)
{
	((QTextDocument *)handle)->setDocumentMargin(margin);
}

void QTextDocument_adjustSize(QTextDocumentH handle)
{
	((QTextDocument *)handle)->adjustSize();
}

void QTextDocument_size(QTextDocumentH handle, QSizeFH retval)
{
	*(QSizeF *)retval = ((QTextDocument *)handle)->size();
}

int QTextDocument_blockCount(QTextDocumentH handle)
{
	return (int) ((QTextDocument *)handle)->blockCount();
}

int QTextDocument_lineCount(QTextDocumentH handle)
{
	return (int) ((QTextDocument *)handle)->lineCount();
}

int QTextDocument_characterCount(QTextDocumentH handle)
{
	return (int) ((QTextDocument *)handle)->characterCount();
}

void QTextDocument_setDefaultStyleSheet(QTextDocumentH handle, PWideString sheet)
{
	QString t_sheet;
	copyPWideStringToQString(sheet, t_sheet);
	((QTextDocument *)handle)->setDefaultStyleSheet(t_sheet);
}

void QTextDocument_defaultStyleSheet(QTextDocumentH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextDocument *)handle)->defaultStyleSheet();
	copyQStringToPWideString(t_retval, retval);
}

void QTextDocument_undo(QTextDocumentH handle, QTextCursorH cursor)
{
	((QTextDocument *)handle)->undo((QTextCursor*)cursor);
}

void QTextDocument_redo(QTextDocumentH handle, QTextCursorH cursor)
{
	((QTextDocument *)handle)->redo((QTextCursor*)cursor);
}

void QTextDocument_clearUndoRedoStacks(QTextDocumentH handle, QTextDocument::Stacks historyToClear)
{
	((QTextDocument *)handle)->clearUndoRedoStacks(historyToClear);
}

int QTextDocument_maximumBlockCount(QTextDocumentH handle)
{
	return (int) ((QTextDocument *)handle)->maximumBlockCount();
}

void QTextDocument_setMaximumBlockCount(QTextDocumentH handle, int maximum)
{
	((QTextDocument *)handle)->setMaximumBlockCount(maximum);
}

void QTextDocument_defaultTextOption(QTextDocumentH handle, QTextOptionH retval)
{
	*(QTextOption *)retval = ((QTextDocument *)handle)->defaultTextOption();
}

void QTextDocument_setDefaultTextOption(QTextDocumentH handle, const QTextOptionH option)
{
	((QTextDocument *)handle)->setDefaultTextOption(*(const QTextOption*)option);
}

Qt::CursorMoveStyle QTextDocument_defaultCursorMoveStyle(QTextDocumentH handle)
{
	return (Qt::CursorMoveStyle) ((QTextDocument *)handle)->defaultCursorMoveStyle();
}

void QTextDocument_setDefaultCursorMoveStyle(QTextDocumentH handle, Qt::CursorMoveStyle style)
{
	((QTextDocument *)handle)->setDefaultCursorMoveStyle(style);
}

void QTextDocument_undo2(QTextDocumentH handle)
{
	((QTextDocument *)handle)->undo();
}

void QTextDocument_redo2(QTextDocumentH handle)
{
	((QTextDocument *)handle)->redo();
}

void QTextDocument_setModified(QTextDocumentH handle, bool m)
{
	((QTextDocument *)handle)->setModified(m);
}

void QTextDocument_baseUrl(QTextDocumentH handle, QUrlH retval)
{
  *(QUrl *)retval = ((QTextDocument *)handle)->baseUrl();
}

void QTextDocument_setBaseUrl(QTextDocumentH handle, const QUrlH url)
{
  ((QTextDocument *)handle)->setBaseUrl(*(const QUrl*)url);
}

qreal QTextDocument_baselineOffset(QTextDocumentH handle)
{
  return (qreal) ((QTextDocument *)handle)->baselineOffset();
}

void QTextDocument_setBaselineOffset(QTextDocumentH handle, qreal offset)
{
  ((QTextDocument *)handle)->setBaselineOffset(offset);
}

void QTextDocument_toRawText(QTextDocumentH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextDocument *)handle)->toRawText();
	copyQStringToPWideString(t_retval, retval);
}

void QTextDocument_allFormats(QTextDocumentH handle, PPtrIntArray retval)
{
	QList<QTextFormat> t_retval;
	t_retval = ((QTextDocument *)handle)->allFormats();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

qreal QTextDocument_subScriptBaseline(QTextDocumentH handle)
{
  return (qreal) ((QTextDocument *)handle)->subScriptBaseline();
}

void QTextDocument_setSubScriptBaseline(QTextDocumentH handle, qreal baseline)
{
  ((QTextDocument *)handle)->setSubScriptBaseline(baseline);
}

qreal QTextDocument_superScriptBaseline(QTextDocumentH handle)
{
  return (qreal) ((QTextDocument *)handle)->superScriptBaseline();
}

void QTextDocument_setSuperScriptBaseline(QTextDocumentH handle, qreal baseline)
{
  ((QTextDocument *)handle)->setSuperScriptBaseline(baseline);
}

QTextDocument::ResourceProvider QTextDocument_resourceProvider(QTextDocumentH handle)
{
  return (QTextDocument::ResourceProvider) ((QTextDocument *)handle)->resourceProvider();
}

void QTextDocument_setResourceProvider(QTextDocumentH handle, QTextDocument::ResourceProvider provider)
{
  ((QTextDocument *)handle)->setResourceProvider(provider);
}

QTextDocument::ResourceProvider QTextDocument_defaultResourceProvider()
{
  return (QTextDocument::ResourceProvider) QTextDocument::defaultResourceProvider();
}

void QTextDocument_setDefaultResourceProvider(QTextDocument::ResourceProvider provider)
{
  QTextDocument::setDefaultResourceProvider(provider);
}

