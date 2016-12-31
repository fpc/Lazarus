//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QITEMEDITORFACTORY_C_H
#define QITEMEDITORFACTORY_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QWidgetH QItemEditorCreatorBase_createWidget(QItemEditorCreatorBaseH handle, QWidgetH parent);
C_EXPORT void QItemEditorCreatorBase_valuePropertyName(QItemEditorCreatorBaseH handle, QByteArrayH retval);
C_EXPORT QItemEditorFactoryH QItemEditorFactory_Create();
C_EXPORT void QItemEditorFactory_Destroy(QItemEditorFactoryH handle);
C_EXPORT QWidgetH QItemEditorFactory_createEditor(QItemEditorFactoryH handle, int userType, QWidgetH parent);
C_EXPORT void QItemEditorFactory_valuePropertyName(QItemEditorFactoryH handle, QByteArrayH retval, int userType);
C_EXPORT void QItemEditorFactory_registerEditor(QItemEditorFactoryH handle, int userType, QItemEditorCreatorBaseH creator);
C_EXPORT const QItemEditorFactoryH QItemEditorFactory_defaultFactory();
C_EXPORT void QItemEditorFactory_setDefaultFactory(QItemEditorFactoryH factory);

#endif
