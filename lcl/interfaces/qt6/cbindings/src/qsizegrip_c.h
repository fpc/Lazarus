//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSIZEGRIP_C_H
#define QSIZEGRIP_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QSizeGripH QSizeGrip_Create(QWidgetH parent);
C_EXPORT void QSizeGrip_Destroy(QSizeGripH handle);
C_EXPORT void QSizeGrip_sizeHint(QSizeGripH handle, PSize retval);
C_EXPORT void QSizeGrip_setVisible(QSizeGripH handle, bool AnonParam1);

#endif
