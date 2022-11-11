//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstylefactory_c.h"

void QStyleFactory_keys(QStringListH retval)
{
	*(QStringList *)retval = QStyleFactory::keys();
}

QStyleH QStyleFactory_create(PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	return (QStyleH) QStyleFactory::create(t_AnonParam1);
}

