//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlcltabwidget_c.h"

QTabBarH QLCLTabWidget_tabBarHandle(QTabWidgetH protectedhandle)
{
	return (QTabBarH) QLCLTabWidget::tabBarHandle((QTabWidget*)protectedhandle);
}

