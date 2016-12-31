//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlclabstractspinbox_c.h"

QLineEditH QLCLAbstractSpinBox_lineEditHandle(QAbstractSpinBoxH protectedhandle)
{
	return (QLineEditH) QLCLAbstractSpinBox::lineEditHandle((QAbstractSpinBox*)protectedhandle);
}

