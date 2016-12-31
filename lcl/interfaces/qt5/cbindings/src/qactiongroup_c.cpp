//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qactiongroup_c.h"

QActionGroupH QActionGroup_Create(QObjectH parent)
{
	return (QActionGroupH) new QActionGroup((QObject*)parent);
}

void QActionGroup_Destroy(QActionGroupH handle)
{
	delete (QActionGroup *)handle;
}

QActionH QActionGroup_addAction(QActionGroupH handle, QActionH a)
{
	return (QActionH) ((QActionGroup *)handle)->addAction((QAction*)a);
}

QActionH QActionGroup_addAction2(QActionGroupH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QActionGroup *)handle)->addAction(t_text);
}

QActionH QActionGroup_addAction3(QActionGroupH handle, const QIconH icon, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QActionGroup *)handle)->addAction(*(const QIcon*)icon, t_text);
}

void QActionGroup_removeAction(QActionGroupH handle, QActionH a)
{
	((QActionGroup *)handle)->removeAction((QAction*)a);
}

void QActionGroup_actions(QActionGroupH handle, PPtrIntArray retval)
{
	QList<QAction*> t_retval;
	t_retval = ((QActionGroup *)handle)->actions();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QActionH QActionGroup_checkedAction(QActionGroupH handle)
{
	return (QActionH) ((QActionGroup *)handle)->checkedAction();
}

bool QActionGroup_isExclusive(QActionGroupH handle)
{
	return (bool) ((QActionGroup *)handle)->isExclusive();
}

bool QActionGroup_isEnabled(QActionGroupH handle)
{
	return (bool) ((QActionGroup *)handle)->isEnabled();
}

bool QActionGroup_isVisible(QActionGroupH handle)
{
	return (bool) ((QActionGroup *)handle)->isVisible();
}

void QActionGroup_setEnabled(QActionGroupH handle, bool AnonParam1)
{
	((QActionGroup *)handle)->setEnabled(AnonParam1);
}

void QActionGroup_setDisabled(QActionGroupH handle, bool b)
{
	((QActionGroup *)handle)->setDisabled(b);
}

void QActionGroup_setVisible(QActionGroupH handle, bool AnonParam1)
{
	((QActionGroup *)handle)->setVisible(AnonParam1);
}

void QActionGroup_setExclusive(QActionGroupH handle, bool AnonParam1)
{
	((QActionGroup *)handle)->setExclusive(AnonParam1);
}

