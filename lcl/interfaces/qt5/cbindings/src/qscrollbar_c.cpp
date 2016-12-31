//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qscrollbar_c.h"

QScrollBarH QScrollBar_Create(QWidgetH parent)
{
	return (QScrollBarH) new QScrollBar((QWidget*)parent);
}

void QScrollBar_Destroy(QScrollBarH handle)
{
	delete (QScrollBar *)handle;
}

QScrollBarH QScrollBar_Create2(Qt::Orientation AnonParam1, QWidgetH parent)
{
	return (QScrollBarH) new QScrollBar(AnonParam1, (QWidget*)parent);
}

void QScrollBar_sizeHint(QScrollBarH handle, PSize retval)
{
	*(QSize *)retval = ((QScrollBar *)handle)->sizeHint();
}

bool QScrollBar_event(QScrollBarH handle, QEventH event)
{
	return (bool) ((QScrollBar *)handle)->event((QEvent*)event);
}

