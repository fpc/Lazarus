//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLABEL_C_H
#define QLABEL_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QLabelH QLabel_Create(QWidgetH parent, unsigned int f);
C_EXPORT void QLabel_Destroy(QLabelH handle);
C_EXPORT QLabelH QLabel_Create2(PWideString text, QWidgetH parent, unsigned int f);
C_EXPORT void QLabel_text(QLabelH handle, PWideString retval);
C_EXPORT void QLabel_pixmap(QLabelH handle, QPixmapH retval);
C_EXPORT void QLabel_picture(QLabelH handle, QPictureH retval);
C_EXPORT QMovieH QLabel_movie(QLabelH handle);
C_EXPORT Qt::TextFormat QLabel_textFormat(QLabelH handle);
C_EXPORT void QLabel_setTextFormat(QLabelH handle, Qt::TextFormat AnonParam1);
C_EXPORT unsigned int QLabel_alignment(QLabelH handle);
C_EXPORT void QLabel_setAlignment(QLabelH handle, unsigned int AnonParam1);
C_EXPORT void QLabel_setWordWrap(QLabelH handle, bool on);
C_EXPORT bool QLabel_wordWrap(QLabelH handle);
C_EXPORT int QLabel_indent(QLabelH handle);
C_EXPORT void QLabel_setIndent(QLabelH handle, int AnonParam1);
C_EXPORT int QLabel_margin(QLabelH handle);
C_EXPORT void QLabel_setMargin(QLabelH handle, int AnonParam1);
C_EXPORT bool QLabel_hasScaledContents(QLabelH handle);
C_EXPORT void QLabel_setScaledContents(QLabelH handle, bool AnonParam1);
C_EXPORT void QLabel_sizeHint(QLabelH handle, PSize retval);
C_EXPORT void QLabel_minimumSizeHint(QLabelH handle, PSize retval);
C_EXPORT void QLabel_setBuddy(QLabelH handle, QWidgetH AnonParam1);
C_EXPORT QWidgetH QLabel_buddy(QLabelH handle);
C_EXPORT int QLabel_heightForWidth(QLabelH handle, int AnonParam1);
C_EXPORT bool QLabel_openExternalLinks(QLabelH handle);
C_EXPORT void QLabel_setOpenExternalLinks(QLabelH handle, bool open);
C_EXPORT void QLabel_setTextInteractionFlags(QLabelH handle, unsigned int flags);
C_EXPORT unsigned int QLabel_textInteractionFlags(QLabelH handle);
C_EXPORT void QLabel_setSelection(QLabelH handle, int AnonParam1, int AnonParam2);
C_EXPORT bool QLabel_hasSelectedText(QLabelH handle);
C_EXPORT void QLabel_selectedText(QLabelH handle, PWideString retval);
C_EXPORT int QLabel_selectionStart(QLabelH handle);
C_EXPORT void QLabel_setText(QLabelH handle, PWideString AnonParam1);
C_EXPORT void QLabel_setPixmap(QLabelH handle, const QPixmapH AnonParam1);
C_EXPORT void QLabel_setPicture(QLabelH handle, const QPictureH AnonParam1);
C_EXPORT void QLabel_setMovie(QLabelH handle, QMovieH movie);
C_EXPORT void QLabel_setNum(QLabelH handle, int AnonParam1);
C_EXPORT void QLabel_setNum2(QLabelH handle, double AnonParam1);
C_EXPORT void QLabel_clear(QLabelH handle);

#endif
