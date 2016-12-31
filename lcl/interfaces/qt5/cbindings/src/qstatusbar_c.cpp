//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstatusbar_c.h"

QStatusBarH QStatusBar_Create(QWidgetH parent)
{
	return (QStatusBarH) new QStatusBar((QWidget*)parent);
}

void QStatusBar_Destroy(QStatusBarH handle)
{
	delete (QStatusBar *)handle;
}

void QStatusBar_addWidget(QStatusBarH handle, QWidgetH widget, int stretch)
{
	((QStatusBar *)handle)->addWidget((QWidget*)widget, stretch);
}

int QStatusBar_insertWidget(QStatusBarH handle, int index, QWidgetH widget, int stretch)
{
	return (int) ((QStatusBar *)handle)->insertWidget(index, (QWidget*)widget, stretch);
}

void QStatusBar_addPermanentWidget(QStatusBarH handle, QWidgetH widget, int stretch)
{
	((QStatusBar *)handle)->addPermanentWidget((QWidget*)widget, stretch);
}

int QStatusBar_insertPermanentWidget(QStatusBarH handle, int index, QWidgetH widget, int stretch)
{
	return (int) ((QStatusBar *)handle)->insertPermanentWidget(index, (QWidget*)widget, stretch);
}

void QStatusBar_removeWidget(QStatusBarH handle, QWidgetH widget)
{
	((QStatusBar *)handle)->removeWidget((QWidget*)widget);
}

void QStatusBar_setSizeGripEnabled(QStatusBarH handle, bool AnonParam1)
{
	((QStatusBar *)handle)->setSizeGripEnabled(AnonParam1);
}

bool QStatusBar_isSizeGripEnabled(QStatusBarH handle)
{
	return (bool) ((QStatusBar *)handle)->isSizeGripEnabled();
}

void QStatusBar_currentMessage(QStatusBarH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStatusBar *)handle)->currentMessage();
	copyQStringToPWideString(t_retval, retval);
}

void QStatusBar_showMessage(QStatusBarH handle, PWideString text, int timeout)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QStatusBar *)handle)->showMessage(t_text, timeout);
}

void QStatusBar_clearMessage(QStatusBarH handle)
{
	((QStatusBar *)handle)->clearMessage();
}

