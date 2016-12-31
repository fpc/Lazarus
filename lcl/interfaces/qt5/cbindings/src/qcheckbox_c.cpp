//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcheckbox_c.h"

QCheckBoxH QCheckBox_Create(QWidgetH parent)
{
	return (QCheckBoxH) new QCheckBox((QWidget*)parent);
}

void QCheckBox_Destroy(QCheckBoxH handle)
{
	delete (QCheckBox *)handle;
}

QCheckBoxH QCheckBox_Create2(PWideString text, QWidgetH parent)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QCheckBoxH) new QCheckBox(t_text, (QWidget*)parent);
}

void QCheckBox_sizeHint(QCheckBoxH handle, PSize retval)
{
	*(QSize *)retval = ((QCheckBox *)handle)->sizeHint();
}

void QCheckBox_minimumSizeHint(QCheckBoxH handle, PSize retval)
{
	*(QSize *)retval = ((QCheckBox *)handle)->minimumSizeHint();
}

void QCheckBox_setTristate(QCheckBoxH handle, bool y)
{
	((QCheckBox *)handle)->setTristate(y);
}

bool QCheckBox_isTristate(QCheckBoxH handle)
{
	return (bool) ((QCheckBox *)handle)->isTristate();
}

Qt::CheckState QCheckBox_checkState(QCheckBoxH handle)
{
	return (Qt::CheckState) ((QCheckBox *)handle)->checkState();
}

void QCheckBox_setCheckState(QCheckBoxH handle, Qt::CheckState state)
{
	((QCheckBox *)handle)->setCheckState(state);
}

