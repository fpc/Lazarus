//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractbutton_c.h"

void QAbstractButton_setText(QAbstractButtonH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QAbstractButton *)handle)->setText(t_text);
}

void QAbstractButton_text(QAbstractButtonH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAbstractButton *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QAbstractButton_setIcon(QAbstractButtonH handle, const QIconH icon)
{
	((QAbstractButton *)handle)->setIcon(*(const QIcon*)icon);
}

void QAbstractButton_icon(QAbstractButtonH handle, QIconH retval)
{
	*(QIcon *)retval = ((QAbstractButton *)handle)->icon();
}

void QAbstractButton_iconSize(QAbstractButtonH handle, PSize retval)
{
	*(QSize *)retval = ((QAbstractButton *)handle)->iconSize();
}

void QAbstractButton_setShortcut(QAbstractButtonH handle, const QKeySequenceH key)
{
	((QAbstractButton *)handle)->setShortcut(*(const QKeySequence*)key);
}

void QAbstractButton_shortcut(QAbstractButtonH handle, QKeySequenceH retval)
{
	*(QKeySequence *)retval = ((QAbstractButton *)handle)->shortcut();
}

void QAbstractButton_setCheckable(QAbstractButtonH handle, bool AnonParam1)
{
	((QAbstractButton *)handle)->setCheckable(AnonParam1);
}

bool QAbstractButton_isCheckable(QAbstractButtonH handle)
{
	return (bool) ((QAbstractButton *)handle)->isCheckable();
}

bool QAbstractButton_isChecked(QAbstractButtonH handle)
{
	return (bool) ((QAbstractButton *)handle)->isChecked();
}

void QAbstractButton_setDown(QAbstractButtonH handle, bool AnonParam1)
{
	((QAbstractButton *)handle)->setDown(AnonParam1);
}

bool QAbstractButton_isDown(QAbstractButtonH handle)
{
	return (bool) ((QAbstractButton *)handle)->isDown();
}

void QAbstractButton_setAutoRepeat(QAbstractButtonH handle, bool AnonParam1)
{
	((QAbstractButton *)handle)->setAutoRepeat(AnonParam1);
}

bool QAbstractButton_autoRepeat(QAbstractButtonH handle)
{
	return (bool) ((QAbstractButton *)handle)->autoRepeat();
}

void QAbstractButton_setAutoRepeatDelay(QAbstractButtonH handle, int AnonParam1)
{
	((QAbstractButton *)handle)->setAutoRepeatDelay(AnonParam1);
}

int QAbstractButton_autoRepeatDelay(QAbstractButtonH handle)
{
	return (int) ((QAbstractButton *)handle)->autoRepeatDelay();
}

void QAbstractButton_setAutoRepeatInterval(QAbstractButtonH handle, int AnonParam1)
{
	((QAbstractButton *)handle)->setAutoRepeatInterval(AnonParam1);
}

int QAbstractButton_autoRepeatInterval(QAbstractButtonH handle)
{
	return (int) ((QAbstractButton *)handle)->autoRepeatInterval();
}

void QAbstractButton_setAutoExclusive(QAbstractButtonH handle, bool AnonParam1)
{
	((QAbstractButton *)handle)->setAutoExclusive(AnonParam1);
}

bool QAbstractButton_autoExclusive(QAbstractButtonH handle)
{
	return (bool) ((QAbstractButton *)handle)->autoExclusive();
}

QButtonGroupH QAbstractButton_group(QAbstractButtonH handle)
{
	return (QButtonGroupH) ((QAbstractButton *)handle)->group();
}

void QAbstractButton_setIconSize(QAbstractButtonH handle, const QSizeH size)
{
	((QAbstractButton *)handle)->setIconSize(*(const QSize*)size);
}

void QAbstractButton_animateClick(QAbstractButtonH handle)
{
	((QAbstractButton *)handle)->animateClick();
}

void QAbstractButton_click(QAbstractButtonH handle)
{
	((QAbstractButton *)handle)->click();
}

void QAbstractButton_toggle(QAbstractButtonH handle)
{
	((QAbstractButton *)handle)->toggle();
}

void QAbstractButton_setChecked(QAbstractButtonH handle, bool AnonParam1)
{
	((QAbstractButton *)handle)->setChecked(AnonParam1);
}

