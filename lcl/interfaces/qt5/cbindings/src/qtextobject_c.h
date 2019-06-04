//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTOBJECT_C_H
#define QTEXTOBJECT_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QTextObject_format(QTextObjectH handle, QTextFormatH retval);
C_EXPORT int QTextObject_formatIndex(QTextObjectH handle);
C_EXPORT QTextDocumentH QTextObject_document(QTextObjectH handle);
C_EXPORT int QTextObject_objectIndex(QTextObjectH handle);
C_EXPORT QTextFrameH QTextFrame_Create(QTextDocumentH doc);
C_EXPORT void QTextFrame_Destroy(QTextFrameH handle);
C_EXPORT void QTextFrame_setFrameFormat(QTextFrameH handle, const QTextFrameFormatH format);
C_EXPORT void QTextFrame_frameFormat(QTextFrameH handle, QTextFrameFormatH retval);
C_EXPORT void QTextFrame_firstCursorPosition(QTextFrameH handle, QTextCursorH retval);
C_EXPORT void QTextFrame_lastCursorPosition(QTextFrameH handle, QTextCursorH retval);
C_EXPORT int QTextFrame_firstPosition(QTextFrameH handle);
C_EXPORT int QTextFrame_lastPosition(QTextFrameH handle);
C_EXPORT void QTextFrame_childFrames(QTextFrameH handle, PPtrIntArray retval);
C_EXPORT QTextFrameH QTextFrame_parentFrame(QTextFrameH handle);
C_EXPORT QTextBlockH QTextBlock_Create();
C_EXPORT void QTextBlock_Destroy(QTextBlockH handle);
C_EXPORT QTextBlockH QTextBlock_Create2(const QTextBlockH o);
C_EXPORT bool QTextBlock_isValid(QTextBlockH handle);
C_EXPORT int QTextBlock_position(QTextBlockH handle);
C_EXPORT int QTextBlock_length(QTextBlockH handle);
C_EXPORT bool QTextBlock_contains(QTextBlockH handle, int position);
C_EXPORT QTextLayoutH QTextBlock_layout(QTextBlockH handle);
C_EXPORT void QTextBlock_clearLayout(QTextBlockH handle);
C_EXPORT void QTextBlock_blockFormat(QTextBlockH handle, QTextBlockFormatH retval);
C_EXPORT int QTextBlock_blockFormatIndex(QTextBlockH handle);
C_EXPORT void QTextBlock_charFormat(QTextBlockH handle, QTextCharFormatH retval);
C_EXPORT int QTextBlock_charFormatIndex(QTextBlockH handle);
C_EXPORT Qt::LayoutDirection QTextBlock_textDirection(QTextBlockH handle);
C_EXPORT void QTextBlock_text(QTextBlockH handle, PWideString retval);
C_EXPORT const QTextDocumentH QTextBlock_document(QTextBlockH handle);
C_EXPORT QTextListH QTextBlock_textList(QTextBlockH handle);
C_EXPORT QTextBlockUserDataH QTextBlock_userData(QTextBlockH handle);
C_EXPORT void QTextBlock_setUserData(QTextBlockH handle, QTextBlockUserDataH data);
C_EXPORT int QTextBlock_userState(QTextBlockH handle);
C_EXPORT void QTextBlock_setUserState(QTextBlockH handle, int state);
C_EXPORT int QTextBlock_revision(QTextBlockH handle);
C_EXPORT void QTextBlock_setRevision(QTextBlockH handle, int rev);
C_EXPORT bool QTextBlock_isVisible(QTextBlockH handle);
C_EXPORT void QTextBlock_setVisible(QTextBlockH handle, bool visible);
C_EXPORT int QTextBlock_blockNumber(QTextBlockH handle);
C_EXPORT int QTextBlock_firstLineNumber(QTextBlockH handle);
C_EXPORT void QTextBlock_setLineCount(QTextBlockH handle, int count);
C_EXPORT int QTextBlock_lineCount(QTextBlockH handle);
C_EXPORT void QTextBlock_next(QTextBlockH handle, QTextBlockH retval);
C_EXPORT void QTextBlock_previous(QTextBlockH handle, QTextBlockH retval);
C_EXPORT int QTextBlock_fragmentIndex(QTextBlockH handle);
C_EXPORT void QTextBlock_textFormats(QTextBlockH handle, PPtrIntArray retval);
C_EXPORT QTextFragmentH QTextFragment_Create();
C_EXPORT void QTextFragment_Destroy(QTextFragmentH handle);
C_EXPORT QTextFragmentH QTextFragment_Create2(const QTextFragmentH o);
C_EXPORT bool QTextFragment_isValid(QTextFragmentH handle);
C_EXPORT int QTextFragment_position(QTextFragmentH handle);
C_EXPORT int QTextFragment_length(QTextFragmentH handle);
C_EXPORT bool QTextFragment_contains(QTextFragmentH handle, int position);
C_EXPORT void QTextFragment_charFormat(QTextFragmentH handle, QTextCharFormatH retval);
C_EXPORT int QTextFragment_charFormatIndex(QTextFragmentH handle);
C_EXPORT void QTextFragment_text(QTextFragmentH handle, PWideString retval);
C_EXPORT void QTextFragment_glyphRuns(QTextFragmentH handle, PPtrIntArray retval, int from, int length);

#endif
