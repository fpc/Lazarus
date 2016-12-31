//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qbuttongroup_c.h"

QButtonGroupH QButtonGroup_Create(QObjectH parent)
{
	return (QButtonGroupH) new QButtonGroup((QObject*)parent);
}

void QButtonGroup_Destroy(QButtonGroupH handle)
{
	delete (QButtonGroup *)handle;
}

void QButtonGroup_setExclusive(QButtonGroupH handle, bool AnonParam1)
{
	((QButtonGroup *)handle)->setExclusive(AnonParam1);
}

bool QButtonGroup_exclusive(QButtonGroupH handle)
{
	return (bool) ((QButtonGroup *)handle)->exclusive();
}

void QButtonGroup_addButton(QButtonGroupH handle, QAbstractButtonH AnonParam1, int id)
{
	((QButtonGroup *)handle)->addButton((QAbstractButton*)AnonParam1, id);
}

void QButtonGroup_removeButton(QButtonGroupH handle, QAbstractButtonH AnonParam1)
{
	((QButtonGroup *)handle)->removeButton((QAbstractButton*)AnonParam1);
}

void QButtonGroup_buttons(QButtonGroupH handle, PPtrIntArray retval)
{
	QList<QAbstractButton*> t_retval;
	t_retval = ((QButtonGroup *)handle)->buttons();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QAbstractButtonH QButtonGroup_checkedButton(QButtonGroupH handle)
{
	return (QAbstractButtonH) ((QButtonGroup *)handle)->checkedButton();
}

QAbstractButtonH QButtonGroup_button(QButtonGroupH handle, int id)
{
	return (QAbstractButtonH) ((QButtonGroup *)handle)->button(id);
}

void QButtonGroup_setId(QButtonGroupH handle, QAbstractButtonH button, int id)
{
	((QButtonGroup *)handle)->setId((QAbstractButton*)button, id);
}

int QButtonGroup_id(QButtonGroupH handle, QAbstractButtonH button)
{
	return (int) ((QButtonGroup *)handle)->id((QAbstractButton*)button);
}

int QButtonGroup_checkedId(QButtonGroupH handle)
{
	return (int) ((QButtonGroup *)handle)->checkedId();
}

