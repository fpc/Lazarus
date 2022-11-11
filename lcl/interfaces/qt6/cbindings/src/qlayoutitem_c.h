//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLAYOUTITEM_C_H
#define QLAYOUTITEM_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT void QLayoutItem_sizeHint(QLayoutItemH handle, PSize retval);
C_EXPORT void QLayoutItem_minimumSize(QLayoutItemH handle, PSize retval);
C_EXPORT void QLayoutItem_maximumSize(QLayoutItemH handle, PSize retval);
C_EXPORT unsigned int QLayoutItem_expandingDirections(QLayoutItemH handle);
C_EXPORT void QLayoutItem_setGeometry(QLayoutItemH handle, PRect AnonParam1);
C_EXPORT void QLayoutItem_geometry(QLayoutItemH handle, PRect retval);
C_EXPORT bool QLayoutItem_isEmpty(QLayoutItemH handle);
C_EXPORT bool QLayoutItem_hasHeightForWidth(QLayoutItemH handle);
C_EXPORT int QLayoutItem_heightForWidth(QLayoutItemH handle, int AnonParam1);
C_EXPORT int QLayoutItem_minimumHeightForWidth(QLayoutItemH handle, int AnonParam1);
C_EXPORT void QLayoutItem_invalidate(QLayoutItemH handle);
C_EXPORT QWidgetH QLayoutItem_widget(QLayoutItemH handle);
C_EXPORT QLayoutH QLayoutItem_layout(QLayoutItemH handle);
C_EXPORT QSpacerItemH QLayoutItem_spacerItem(QLayoutItemH handle);
C_EXPORT unsigned int QLayoutItem_alignment(QLayoutItemH handle);
C_EXPORT void QLayoutItem_setAlignment(QLayoutItemH handle, unsigned int a);
C_EXPORT unsigned int QLayoutItem_controlTypes(QLayoutItemH handle);
C_EXPORT QSpacerItemH QSpacerItem_Create(int w, int h, QSizePolicy::Policy hData, QSizePolicy::Policy vData);
C_EXPORT void QSpacerItem_Destroy(QSpacerItemH handle);
C_EXPORT void QSpacerItem_changeSize(QSpacerItemH handle, int w, int h, QSizePolicy::Policy hData, QSizePolicy::Policy vData);
C_EXPORT void QSpacerItem_sizeHint(QSpacerItemH handle, PSize retval);
C_EXPORT void QSpacerItem_minimumSize(QSpacerItemH handle, PSize retval);
C_EXPORT void QSpacerItem_maximumSize(QSpacerItemH handle, PSize retval);
C_EXPORT unsigned int QSpacerItem_expandingDirections(QSpacerItemH handle);
C_EXPORT bool QSpacerItem_isEmpty(QSpacerItemH handle);
C_EXPORT void QSpacerItem_setGeometry(QSpacerItemH handle, PRect AnonParam1);
C_EXPORT void QSpacerItem_geometry(QSpacerItemH handle, PRect retval);
C_EXPORT QSpacerItemH QSpacerItem_spacerItem(QSpacerItemH handle);
C_EXPORT QWidgetItemH QWidgetItem_Create(QWidgetH w);
C_EXPORT void QWidgetItem_Destroy(QWidgetItemH handle);
C_EXPORT void QWidgetItem_sizeHint(QWidgetItemH handle, PSize retval);
C_EXPORT void QWidgetItem_minimumSize(QWidgetItemH handle, PSize retval);
C_EXPORT void QWidgetItem_maximumSize(QWidgetItemH handle, PSize retval);
C_EXPORT unsigned int QWidgetItem_expandingDirections(QWidgetItemH handle);
C_EXPORT bool QWidgetItem_isEmpty(QWidgetItemH handle);
C_EXPORT void QWidgetItem_setGeometry(QWidgetItemH handle, PRect AnonParam1);
C_EXPORT void QWidgetItem_geometry(QWidgetItemH handle, PRect retval);
C_EXPORT QWidgetH QWidgetItem_widget(QWidgetItemH handle);
C_EXPORT bool QWidgetItem_hasHeightForWidth(QWidgetItemH handle);
C_EXPORT int QWidgetItem_heightForWidth(QWidgetItemH handle, int AnonParam1);
C_EXPORT unsigned int QWidgetItem_controlTypes(QWidgetItemH handle);

#endif
