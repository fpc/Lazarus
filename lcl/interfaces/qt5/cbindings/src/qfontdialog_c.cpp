//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfontdialog_c.h"

QFontDialogH QFontDialog_Create(QWidgetH parent)
{
	return (QFontDialogH) new QFontDialog((QWidget*)parent);
}

void QFontDialog_Destroy(QFontDialogH handle)
{
	delete (QFontDialog *)handle;
}

QFontDialogH QFontDialog_Create2(const QFontH initial, QWidgetH parent)
{
	return (QFontDialogH) new QFontDialog(*(const QFont*)initial, (QWidget*)parent);
}

void QFontDialog_setCurrentFont(QFontDialogH handle, const QFontH font)
{
	((QFontDialog *)handle)->setCurrentFont(*(const QFont*)font);
}

void QFontDialog_currentFont(QFontDialogH handle, QFontH retval)
{
	*(QFont *)retval = ((QFontDialog *)handle)->currentFont();
}

void QFontDialog_selectedFont(QFontDialogH handle, QFontH retval)
{
	*(QFont *)retval = ((QFontDialog *)handle)->selectedFont();
}

void QFontDialog_setOption(QFontDialogH handle, QFontDialog::FontDialogOption option, bool on)
{
	((QFontDialog *)handle)->setOption(option, on);
}

bool QFontDialog_testOption(QFontDialogH handle, QFontDialog::FontDialogOption option)
{
	return (bool) ((QFontDialog *)handle)->testOption(option);
}

void QFontDialog_setOptions(QFontDialogH handle, unsigned int options)
{
	((QFontDialog *)handle)->setOptions((QFontDialog::FontDialogOptions)options);
}

unsigned int QFontDialog_options(QFontDialogH handle)
{
	return (unsigned int) ((QFontDialog *)handle)->options();
}

void QFontDialog_open(QFontDialogH handle, QObjectH receiver, const char* member)
{
	((QFontDialog *)handle)->open((QObject*)receiver, member);
}

void QFontDialog_setVisible(QFontDialogH handle, bool visible)
{
	((QFontDialog *)handle)->setVisible(visible);
}

void QFontDialog_getFont(QFontH retval, bool* ok, QWidgetH parent)
{
	*(QFont *)retval = QFontDialog::getFont(ok, (QWidget*)parent);
}

void QFontDialog_getFont2(QFontH retval, bool* ok, const QFontH initial, QWidgetH parent, PWideString title, unsigned int options)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	*(QFont *)retval = QFontDialog::getFont(ok, *(const QFont*)initial, (QWidget*)parent, t_title, (QFontDialog::FontDialogOptions)options);
}

