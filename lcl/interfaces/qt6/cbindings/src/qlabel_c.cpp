//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlabel_c.h"

QLabelH QLabel_Create(QWidgetH parent, unsigned int f)
{
	return (QLabelH) new QLabel((QWidget*)parent, (Qt::WindowFlags)f);
}

void QLabel_Destroy(QLabelH handle)
{
	delete (QLabel *)handle;
}

QLabelH QLabel_Create2(PWideString text, QWidgetH parent, unsigned int f)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QLabelH) new QLabel(t_text, (QWidget*)parent, (Qt::WindowFlags)f);
}

void QLabel_text(QLabelH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLabel *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QLabel_pixmap(QLabelH handle, QPixmapH retval)
{
	*(QPixmap *)retval = ((QLabel *)handle)->pixmap();
}

void QLabel_picture(QLabelH handle, QPictureH retval)
{
	*(QPicture *)retval = ((QLabel *)handle)->picture();
}

QMovieH QLabel_movie(QLabelH handle)
{
	return (QMovieH) ((QLabel *)handle)->movie();
}

Qt::TextFormat QLabel_textFormat(QLabelH handle)
{
	return (Qt::TextFormat) ((QLabel *)handle)->textFormat();
}

void QLabel_setTextFormat(QLabelH handle, Qt::TextFormat AnonParam1)
{
	((QLabel *)handle)->setTextFormat(AnonParam1);
}

unsigned int QLabel_alignment(QLabelH handle)
{
	return (unsigned int) ((QLabel *)handle)->alignment();
}

void QLabel_setAlignment(QLabelH handle, unsigned int AnonParam1)
{
	((QLabel *)handle)->setAlignment((Qt::Alignment)AnonParam1);
}

void QLabel_setWordWrap(QLabelH handle, bool on)
{
	((QLabel *)handle)->setWordWrap(on);
}

bool QLabel_wordWrap(QLabelH handle)
{
	return (bool) ((QLabel *)handle)->wordWrap();
}

int QLabel_indent(QLabelH handle)
{
	return (int) ((QLabel *)handle)->indent();
}

void QLabel_setIndent(QLabelH handle, int AnonParam1)
{
	((QLabel *)handle)->setIndent(AnonParam1);
}

int QLabel_margin(QLabelH handle)
{
	return (int) ((QLabel *)handle)->margin();
}

void QLabel_setMargin(QLabelH handle, int AnonParam1)
{
	((QLabel *)handle)->setMargin(AnonParam1);
}

bool QLabel_hasScaledContents(QLabelH handle)
{
	return (bool) ((QLabel *)handle)->hasScaledContents();
}

void QLabel_setScaledContents(QLabelH handle, bool AnonParam1)
{
	((QLabel *)handle)->setScaledContents(AnonParam1);
}

void QLabel_sizeHint(QLabelH handle, PSize retval)
{
	*(QSize *)retval = ((QLabel *)handle)->sizeHint();
}

void QLabel_minimumSizeHint(QLabelH handle, PSize retval)
{
	*(QSize *)retval = ((QLabel *)handle)->minimumSizeHint();
}

void QLabel_setBuddy(QLabelH handle, QWidgetH AnonParam1)
{
	((QLabel *)handle)->setBuddy((QWidget*)AnonParam1);
}

QWidgetH QLabel_buddy(QLabelH handle)
{
	return (QWidgetH) ((QLabel *)handle)->buddy();
}

int QLabel_heightForWidth(QLabelH handle, int AnonParam1)
{
	return (int) ((QLabel *)handle)->heightForWidth(AnonParam1);
}

bool QLabel_openExternalLinks(QLabelH handle)
{
	return (bool) ((QLabel *)handle)->openExternalLinks();
}

void QLabel_setOpenExternalLinks(QLabelH handle, bool open)
{
	((QLabel *)handle)->setOpenExternalLinks(open);
}

void QLabel_setTextInteractionFlags(QLabelH handle, unsigned int flags)
{
	((QLabel *)handle)->setTextInteractionFlags((Qt::TextInteractionFlags)flags);
}

unsigned int QLabel_textInteractionFlags(QLabelH handle)
{
	return (unsigned int) ((QLabel *)handle)->textInteractionFlags();
}

void QLabel_setSelection(QLabelH handle, int AnonParam1, int AnonParam2)
{
	((QLabel *)handle)->setSelection(AnonParam1, AnonParam2);
}

bool QLabel_hasSelectedText(QLabelH handle)
{
	return (bool) ((QLabel *)handle)->hasSelectedText();
}

void QLabel_selectedText(QLabelH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLabel *)handle)->selectedText();
	copyQStringToPWideString(t_retval, retval);
}

int QLabel_selectionStart(QLabelH handle)
{
	return (int) ((QLabel *)handle)->selectionStart();
}

void QLabel_setText(QLabelH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QLabel *)handle)->setText(t_AnonParam1);
}

void QLabel_setPixmap(QLabelH handle, const QPixmapH AnonParam1)
{
	((QLabel *)handle)->setPixmap(*(const QPixmap*)AnonParam1);
}

void QLabel_setPicture(QLabelH handle, const QPictureH AnonParam1)
{
	((QLabel *)handle)->setPicture(*(const QPicture*)AnonParam1);
}

void QLabel_setMovie(QLabelH handle, QMovieH movie)
{
	((QLabel *)handle)->setMovie((QMovie*)movie);
}

void QLabel_setNum(QLabelH handle, int AnonParam1)
{
	((QLabel *)handle)->setNum(AnonParam1);
}

void QLabel_setNum2(QLabelH handle, double AnonParam1)
{
	((QLabel *)handle)->setNum(AnonParam1);
}

void QLabel_clear(QLabelH handle)
{
	((QLabel *)handle)->clear();
}

