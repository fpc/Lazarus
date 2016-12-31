//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qitemeditorfactory_c.h"

QWidgetH QItemEditorCreatorBase_createWidget(QItemEditorCreatorBaseH handle, QWidgetH parent)
{
	return (QWidgetH) ((QItemEditorCreatorBase *)handle)->createWidget((QWidget*)parent);
}

void QItemEditorCreatorBase_valuePropertyName(QItemEditorCreatorBaseH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QItemEditorCreatorBase *)handle)->valuePropertyName();
}

QItemEditorFactoryH QItemEditorFactory_Create()
{
	return (QItemEditorFactoryH) new QItemEditorFactory();
}

void QItemEditorFactory_Destroy(QItemEditorFactoryH handle)
{
	delete (QItemEditorFactory *)handle;
}

QWidgetH QItemEditorFactory_createEditor(QItemEditorFactoryH handle, int userType, QWidgetH parent)
{
	return (QWidgetH) ((QItemEditorFactory *)handle)->createEditor(userType, (QWidget*)parent);
}

void QItemEditorFactory_valuePropertyName(QItemEditorFactoryH handle, QByteArrayH retval, int userType)
{
	*(QByteArray *)retval = ((QItemEditorFactory *)handle)->valuePropertyName(userType);
}

void QItemEditorFactory_registerEditor(QItemEditorFactoryH handle, int userType, QItemEditorCreatorBaseH creator)
{
	((QItemEditorFactory *)handle)->registerEditor(userType, (QItemEditorCreatorBase*)creator);
}

const QItemEditorFactoryH QItemEditorFactory_defaultFactory()
{
	return (const QItemEditorFactoryH) QItemEditorFactory::defaultFactory();
}

void QItemEditorFactory_setDefaultFactory(QItemEditorFactoryH factory)
{
	QItemEditorFactory::setDefaultFactory((QItemEditorFactory*)factory);
}

