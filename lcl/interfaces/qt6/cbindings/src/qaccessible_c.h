//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QACCESSIBLE_C_H
#define QACCESSIBLE_C_H

#include <QtWidgets>
#include "pascalbind.h"

typedef unsigned QAccessibleId;
typedef QAccessibleInterface*(*InterfaceFactory)(const QString &key, QObject*);

C_EXPORT QAccessibleInterfaceH QAccessible_accessibleInterface(QAccessibleId uniqueId);
C_EXPORT void QAccessible_deleteAccessibleInterface(QAccessibleId uniqueId);
C_EXPORT void QAccessible_installFactory(InterfaceFactory factory);
C_EXPORT bool QAccessible_isActive();
C_EXPORT QAccessibleInterfaceH QAccessible_queryAccessibleInterface(QObjectH object_);
C_EXPORT QAccessibleId QAccessible_registerAccessibleInterface(QAccessibleInterfaceH iface);
C_EXPORT void QAccessible_removeFactory(InterfaceFactory  factory);
C_EXPORT void QAccessible_setRootObject(QObjectH object_);
C_EXPORT QAccessibleId QAccessible_uniqueId(QAccessibleInterfaceH iface);
C_EXPORT void QAccessible_updateAccessibility(QAccessibleEvent event);
#endif
