//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextobject_c.h"

void QTextObject_format(QTextObjectH handle, QTextFormatH retval)
{
	*(QTextFormat *)retval = ((QTextObject *)handle)->format();
}

int QTextObject_formatIndex(QTextObjectH handle)
{
	return (int) ((QTextObject *)handle)->formatIndex();
}

QTextDocumentH QTextObject_document(QTextObjectH handle)
{
	return (QTextDocumentH) ((QTextObject *)handle)->document();
}

int QTextObject_objectIndex(QTextObjectH handle)
{
	return (int) ((QTextObject *)handle)->objectIndex();
}

QTextFrameH QTextFrame_Create(QTextDocumentH doc)
{
	return (QTextFrameH) new QTextFrame((QTextDocument*)doc);
}

void QTextFrame_Destroy(QTextFrameH handle)
{
	delete (QTextFrame *)handle;
}

void QTextFrame_setFrameFormat(QTextFrameH handle, const QTextFrameFormatH format)
{
	((QTextFrame *)handle)->setFrameFormat(*(const QTextFrameFormat*)format);
}

void QTextFrame_frameFormat(QTextFrameH handle, QTextFrameFormatH retval)
{
	*(QTextFrameFormat *)retval = ((QTextFrame *)handle)->frameFormat();
}

void QTextFrame_firstCursorPosition(QTextFrameH handle, QTextCursorH retval)
{
	*(QTextCursor *)retval = ((QTextFrame *)handle)->firstCursorPosition();
}

void QTextFrame_lastCursorPosition(QTextFrameH handle, QTextCursorH retval)
{
	*(QTextCursor *)retval = ((QTextFrame *)handle)->lastCursorPosition();
}

int QTextFrame_firstPosition(QTextFrameH handle)
{
	return (int) ((QTextFrame *)handle)->firstPosition();
}

int QTextFrame_lastPosition(QTextFrameH handle)
{
	return (int) ((QTextFrame *)handle)->lastPosition();
}

void QTextFrame_childFrames(QTextFrameH handle, PPtrIntArray retval)
{
	QList<QTextFrame*> t_retval;
	t_retval = ((QTextFrame *)handle)->childFrames();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QTextFrameH QTextFrame_parentFrame(QTextFrameH handle)
{
	return (QTextFrameH) ((QTextFrame *)handle)->parentFrame();
}

QTextBlockH QTextBlock_Create()
{
	return (QTextBlockH) new QTextBlock();
}

void QTextBlock_Destroy(QTextBlockH handle)
{
	delete (QTextBlock *)handle;
}

QTextBlockH QTextBlock_Create2(const QTextBlockH o)
{
	return (QTextBlockH) new QTextBlock(*(const QTextBlock*)o);
}

bool QTextBlock_isValid(QTextBlockH handle)
{
	return (bool) ((QTextBlock *)handle)->isValid();
}

int QTextBlock_position(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->position();
}

int QTextBlock_length(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->length();
}

bool QTextBlock_contains(QTextBlockH handle, int position)
{
	return (bool) ((QTextBlock *)handle)->contains(position);
}

QTextLayoutH QTextBlock_layout(QTextBlockH handle)
{
	return (QTextLayoutH) ((QTextBlock *)handle)->layout();
}

void QTextBlock_clearLayout(QTextBlockH handle)
{
	((QTextBlock *)handle)->clearLayout();
}

void QTextBlock_blockFormat(QTextBlockH handle, QTextBlockFormatH retval)
{
	*(QTextBlockFormat *)retval = ((QTextBlock *)handle)->blockFormat();
}

int QTextBlock_blockFormatIndex(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->blockFormatIndex();
}

void QTextBlock_charFormat(QTextBlockH handle, QTextCharFormatH retval)
{
	*(QTextCharFormat *)retval = ((QTextBlock *)handle)->charFormat();
}

int QTextBlock_charFormatIndex(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->charFormatIndex();
}

Qt::LayoutDirection QTextBlock_textDirection(QTextBlockH handle)
{
	return (Qt::LayoutDirection) ((QTextBlock *)handle)->textDirection();
}

void QTextBlock_text(QTextBlockH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextBlock *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

const QTextDocumentH QTextBlock_document(QTextBlockH handle)
{
	return (const QTextDocumentH) ((QTextBlock *)handle)->document();
}

QTextListH QTextBlock_textList(QTextBlockH handle)
{
	return (QTextListH) ((QTextBlock *)handle)->textList();
}

QTextBlockUserDataH QTextBlock_userData(QTextBlockH handle)
{
	return (QTextBlockUserDataH) ((QTextBlock *)handle)->userData();
}

void QTextBlock_setUserData(QTextBlockH handle, QTextBlockUserDataH data)
{
	((QTextBlock *)handle)->setUserData((QTextBlockUserData*)data);
}

int QTextBlock_userState(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->userState();
}

void QTextBlock_setUserState(QTextBlockH handle, int state)
{
	((QTextBlock *)handle)->setUserState(state);
}

int QTextBlock_revision(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->revision();
}

void QTextBlock_setRevision(QTextBlockH handle, int rev)
{
	((QTextBlock *)handle)->setRevision(rev);
}

bool QTextBlock_isVisible(QTextBlockH handle)
{
	return (bool) ((QTextBlock *)handle)->isVisible();
}

void QTextBlock_setVisible(QTextBlockH handle, bool visible)
{
	((QTextBlock *)handle)->setVisible(visible);
}

int QTextBlock_blockNumber(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->blockNumber();
}

int QTextBlock_firstLineNumber(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->firstLineNumber();
}

void QTextBlock_setLineCount(QTextBlockH handle, int count)
{
	((QTextBlock *)handle)->setLineCount(count);
}

int QTextBlock_lineCount(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->lineCount();
}

void QTextBlock_next(QTextBlockH handle, QTextBlockH retval)
{
	*(QTextBlock *)retval = ((QTextBlock *)handle)->next();
}

void QTextBlock_previous(QTextBlockH handle, QTextBlockH retval)
{
	*(QTextBlock *)retval = ((QTextBlock *)handle)->previous();
}

int QTextBlock_fragmentIndex(QTextBlockH handle)
{
	return (int) ((QTextBlock *)handle)->fragmentIndex();
}

int QTextBlock_textFormatsCount(QTextBlockH handle)
{
  return ((QTextBlock *)handle)->textFormats().size();
}

int QTextBlock_textFormatsRanges(QTextBlockH handle, PTextRange ranges, int maxCount)
{
  QVector<QTextLayout::FormatRange> t_retval;
  int res = 0;
  int cnt = maxCount;
  int sz;

  t_retval = ((QTextBlock *)handle)->textFormats();
  sz = t_retval.size();
  if (sz > cnt) cnt = sz;

  for (int i=0; i<sz; i++)
  {
    ranges[i].start = t_retval.at(i).start;
    ranges[i].length = t_retval.at(i).length;
  }
  return res;
}


QTextFragmentH QTextFragment_Create()
{
	return (QTextFragmentH) new QTextFragment();
}

void QTextFragment_Destroy(QTextFragmentH handle)
{
	delete (QTextFragment *)handle;
}

QTextFragmentH QTextFragment_Create2(const QTextFragmentH o)
{
	return (QTextFragmentH) new QTextFragment(*(const QTextFragment*)o);
}

bool QTextFragment_isValid(QTextFragmentH handle)
{
	return (bool) ((QTextFragment *)handle)->isValid();
}

int QTextFragment_position(QTextFragmentH handle)
{
	return (int) ((QTextFragment *)handle)->position();
}

int QTextFragment_length(QTextFragmentH handle)
{
	return (int) ((QTextFragment *)handle)->length();
}

bool QTextFragment_contains(QTextFragmentH handle, int position)
{
	return (bool) ((QTextFragment *)handle)->contains(position);
}

void QTextFragment_charFormat(QTextFragmentH handle, QTextCharFormatH retval)
{
	*(QTextCharFormat *)retval = ((QTextFragment *)handle)->charFormat();
}

int QTextFragment_charFormatIndex(QTextFragmentH handle)
{
	return (int) ((QTextFragment *)handle)->charFormatIndex();
}

void QTextFragment_text(QTextFragmentH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextFragment *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QTextFragment_glyphRuns(QTextFragmentH handle, PPtrIntArray retval, int from, int length)
{
	QList<QGlyphRun> t_retval;
	t_retval = ((QTextFragment *)handle)->glyphRuns(from, length);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

