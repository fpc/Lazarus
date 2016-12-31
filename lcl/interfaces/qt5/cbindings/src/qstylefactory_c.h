//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTYLEFACTORY_C_H
#define QSTYLEFACTORY_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT void QStyleFactory_keys(QStringListH retval);
C_EXPORT QStyleH QStyleFactory_create(PWideString AnonParam1);

#endif
