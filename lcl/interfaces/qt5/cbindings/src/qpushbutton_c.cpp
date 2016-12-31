//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpushbutton_c.h"

QPushButtonH QPushButton_Create(QWidgetH parent)
{
	return (QPushButtonH) new QPushButton((QWidget*)parent);
}

void QPushButton_Destroy(QPushButtonH handle)
{
	delete (QPushButton *)handle;
}

QPushButtonH QPushButton_Create2(PWideString text, QWidgetH parent)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QPushButtonH) new QPushButton(t_text, (QWidget*)parent);
}

QPushButtonH QPushButton_Create3(const QIconH icon, PWideString text, QWidgetH parent)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QPushButtonH) new QPushButton(*(const QIcon*)icon, t_text, (QWidget*)parent);
}

void QPushButton_sizeHint(QPushButtonH handle, PSize retval)
{
	*(QSize *)retval = ((QPushButton *)handle)->sizeHint();
}

void QPushButton_minimumSizeHint(QPushButtonH handle, PSize retval)
{
	*(QSize *)retval = ((QPushButton *)handle)->minimumSizeHint();
}

bool QPushButton_autoDefault(QPushButtonH handle)
{
	return (bool) ((QPushButton *)handle)->autoDefault();
}

void QPushButton_setAutoDefault(QPushButtonH handle, bool AnonParam1)
{
	((QPushButton *)handle)->setAutoDefault(AnonParam1);
}

bool QPushButton_isDefault(QPushButtonH handle)
{
	return (bool) ((QPushButton *)handle)->isDefault();
}

void QPushButton_setDefault(QPushButtonH handle, bool AnonParam1)
{
	((QPushButton *)handle)->setDefault(AnonParam1);
}

void QPushButton_setMenu(QPushButtonH handle, QMenuH menu)
{
	((QPushButton *)handle)->setMenu((QMenu*)menu);
}

QMenuH QPushButton_menu(QPushButtonH handle)
{
	return (QMenuH) ((QPushButton *)handle)->menu();
}

void QPushButton_setFlat(QPushButtonH handle, bool AnonParam1)
{
	((QPushButton *)handle)->setFlat(AnonParam1);
}

bool QPushButton_isFlat(QPushButtonH handle)
{
	return (bool) ((QPushButton *)handle)->isFlat();
}

void QPushButton_showMenu(QPushButtonH handle)
{
	((QPushButton *)handle)->showMenu();
}

