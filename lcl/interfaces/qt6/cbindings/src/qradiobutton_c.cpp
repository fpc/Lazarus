//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qradiobutton_c.h"

QRadioButtonH QRadioButton_Create(QWidgetH parent)
{
	return (QRadioButtonH) new QRadioButton((QWidget*)parent);
}

void QRadioButton_Destroy(QRadioButtonH handle)
{
	delete (QRadioButton *)handle;
}

QRadioButtonH QRadioButton_Create2(PWideString text, QWidgetH parent)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QRadioButtonH) new QRadioButton(t_text, (QWidget*)parent);
}

void QRadioButton_sizeHint(QRadioButtonH handle, PSize retval)
{
	*(QSize *)retval = ((QRadioButton *)handle)->sizeHint();
}

void QRadioButton_minimumSizeHint(QRadioButtonH handle, PSize retval)
{
	*(QSize *)retval = ((QRadioButton *)handle)->minimumSizeHint();
}

