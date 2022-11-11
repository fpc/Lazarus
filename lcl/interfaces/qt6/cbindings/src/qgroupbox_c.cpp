//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qgroupbox_c.h"

QGroupBoxH QGroupBox_Create(QWidgetH parent)
{
	return (QGroupBoxH) new QGroupBox((QWidget*)parent);
}

void QGroupBox_Destroy(QGroupBoxH handle)
{
	delete (QGroupBox *)handle;
}

QGroupBoxH QGroupBox_Create2(PWideString title, QWidgetH parent)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	return (QGroupBoxH) new QGroupBox(t_title, (QWidget*)parent);
}

void QGroupBox_title(QGroupBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QGroupBox *)handle)->title();
	copyQStringToPWideString(t_retval, retval);
}

void QGroupBox_setTitle(QGroupBoxH handle, PWideString title)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	((QGroupBox *)handle)->setTitle(t_title);
}

unsigned int QGroupBox_alignment(QGroupBoxH handle)
{
	return (unsigned int) ((QGroupBox *)handle)->alignment();
}

void QGroupBox_setAlignment(QGroupBoxH handle, int alignment)
{
	((QGroupBox *)handle)->setAlignment(alignment);
}

void QGroupBox_minimumSizeHint(QGroupBoxH handle, PSize retval)
{
	*(QSize *)retval = ((QGroupBox *)handle)->minimumSizeHint();
}

bool QGroupBox_isFlat(QGroupBoxH handle)
{
	return (bool) ((QGroupBox *)handle)->isFlat();
}

void QGroupBox_setFlat(QGroupBoxH handle, bool flat)
{
	((QGroupBox *)handle)->setFlat(flat);
}

bool QGroupBox_isCheckable(QGroupBoxH handle)
{
	return (bool) ((QGroupBox *)handle)->isCheckable();
}

void QGroupBox_setCheckable(QGroupBoxH handle, bool checkable)
{
	((QGroupBox *)handle)->setCheckable(checkable);
}

bool QGroupBox_isChecked(QGroupBoxH handle)
{
	return (bool) ((QGroupBox *)handle)->isChecked();
}

void QGroupBox_setChecked(QGroupBoxH handle, bool checked)
{
	((QGroupBox *)handle)->setChecked(checked);
}

