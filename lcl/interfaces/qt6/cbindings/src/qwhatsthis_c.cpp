//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwhatsthis_c.h"

void QWhatsThis_enterWhatsThisMode()
{
	QWhatsThis::enterWhatsThisMode();
}

bool QWhatsThis_inWhatsThisMode()
{
	return (bool) QWhatsThis::inWhatsThisMode();
}

void QWhatsThis_leaveWhatsThisMode()
{
	QWhatsThis::leaveWhatsThisMode();
}

void QWhatsThis_showText(const QPointH pos, PWideString text, QWidgetH w)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	QWhatsThis::showText(*(const QPoint*)pos, t_text, (QWidget*)w);
}

void QWhatsThis_hideText()
{
	QWhatsThis::hideText();
}

QActionH QWhatsThis_createAction(QObjectH parent)
{
	return (QActionH) QWhatsThis::createAction((QObject*)parent);
}

