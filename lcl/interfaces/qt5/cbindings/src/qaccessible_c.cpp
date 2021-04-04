//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qaccessible_c.h"

QAccessibleInterfaceH QAccessible_accessibleInterface(QAccessibleId uniqueId)
{
 	return (QAccessibleInterfaceH) QAccessible::accessibleInterface(uniqueId);
}

void QAccessible_deleteAccessibleInterface(QAccessibleId uniqueId)
{
	QAccessible::deleteAccessibleInterface(uniqueId);
}

void QAccessible_installFactory(InterfaceFactory factory)
{
	QAccessible::installFactory(factory);
}

bool QAccessible_isActive()
{
	return QAccessible::isActive();
}

QAccessibleInterfaceH QAccessible_queryAccessibleInterface(QObjectH object_)
{
	return (QAccessibleInterfaceH) QAccessible::queryAccessibleInterface((QObject *)object_);
}

QAccessibleId QAccessible_registerAccessibleInterface(QAccessibleInterfaceH iface)
{
	return (QAccessibleId) QAccessible::registerAccessibleInterface((QAccessibleInterface *)iface);
}

void QAccessible_removeFactory(InterfaceFactory factory)
{
	QAccessible::removeFactory(factory);
}

void QAccessible_setRootObject(QObjectH object_)
{
	QAccessible::setRootObject((QObject *)object_);
}

QAccessibleId QAccessible_uniqueId(QAccessibleInterfaceH iface)
{
	return (QAccessibleId) QAccessible::uniqueId((QAccessibleInterface *)iface);
}

void QAccessible_updateAccessibility(QAccessibleEventH event)
{
	QAccessible::updateAccessibility((QAccessibleEvent *)event);
}

