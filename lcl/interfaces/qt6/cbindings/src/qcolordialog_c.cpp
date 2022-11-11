//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcolordialog_c.h"

QColorDialogH QColorDialog_Create(QWidgetH parent)
{
	return (QColorDialogH) new QColorDialog((QWidget*)parent);
}

void QColorDialog_Destroy(QColorDialogH handle)
{
	delete (QColorDialog *)handle;
}

QColorDialogH QColorDialog_Create2(const QColorH initial, QWidgetH parent)
{
	return (QColorDialogH) new QColorDialog(*(const QColor*)initial, (QWidget*)parent);
}

void QColorDialog_setCurrentColor(QColorDialogH handle, const QColorH color)
{
	((QColorDialog *)handle)->setCurrentColor(*(const QColor*)color);
}

void QColorDialog_currentColor(QColorDialogH handle, PQColor retval)
{
	*(QColor *)retval = ((QColorDialog *)handle)->currentColor();
}

void QColorDialog_selectedColor(QColorDialogH handle, PQColor retval)
{
	*(QColor *)retval = ((QColorDialog *)handle)->selectedColor();
}

void QColorDialog_setOption(QColorDialogH handle, QColorDialog::ColorDialogOption option, bool on)
{
	((QColorDialog *)handle)->setOption(option, on);
}

bool QColorDialog_testOption(QColorDialogH handle, QColorDialog::ColorDialogOption option)
{
	return (bool) ((QColorDialog *)handle)->testOption(option);
}

void QColorDialog_setOptions(QColorDialogH handle, unsigned int options)
{
	((QColorDialog *)handle)->setOptions((QColorDialog::ColorDialogOptions)options);
}

unsigned int QColorDialog_options(QColorDialogH handle)
{
	return (unsigned int) ((QColorDialog *)handle)->options();
}

void QColorDialog_open(QColorDialogH handle, QObjectH receiver, const char* member)
{
	((QColorDialog *)handle)->open((QObject*)receiver, member);
}

void QColorDialog_setVisible(QColorDialogH handle, bool visible)
{
	((QColorDialog *)handle)->setVisible(visible);
}

bool QColorDialog_getColor(PQColor retval, const QColorH initial, QWidgetH parent, PWideString title, unsigned int options)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
  QColor tempColor;
  tempColor = QColorDialog::getColor(*(const QColor*)initial, (QWidget*)parent, t_title, (QColorDialog::ColorDialogOptions)options);
	// *(QColor *)retval = QColorDialog::getColor(*(const QColor*)initial, (QWidget*)parent, t_title, (QColorDialog::ColorDialogOptions)options);
  *(QColor *)retval = tempColor;
  return (bool) tempColor.isValid();
}

int QColorDialog_customCount()
{
	return (int) QColorDialog::customCount();
}

void QColorDialog_customColor(PQColor retval, int index)
{
	*(QColor *)retval = QColorDialog::customColor(index);
}

void QColorDialog_setCustomColor(int index, PQColor color)
{
	QColorDialog::setCustomColor(index, *(QColor *)color);
}

void QColorDialog_standardColor(PQColor retval, int index)
{
	*(QColor *)retval = QColorDialog::standardColor(index);
}

void QColorDialog_setStandardColor(int index, PQColor color)
{
	QColorDialog::setStandardColor(index, *(QColor *)color);
}

