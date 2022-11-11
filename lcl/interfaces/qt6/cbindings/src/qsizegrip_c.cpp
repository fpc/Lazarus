//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsizegrip_c.h"

QSizeGripH QSizeGrip_Create(QWidgetH parent)
{
	return (QSizeGripH) new QSizeGrip((QWidget*)parent);
}

void QSizeGrip_Destroy(QSizeGripH handle)
{
	delete (QSizeGrip *)handle;
}

void QSizeGrip_sizeHint(QSizeGripH handle, PSize retval)
{
	*(QSize *)retval = ((QSizeGrip *)handle)->sizeHint();
}

void QSizeGrip_setVisible(QSizeGripH handle, bool AnonParam1)
{
	((QSizeGrip *)handle)->setVisible(AnonParam1);
}

