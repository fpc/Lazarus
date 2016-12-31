//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsizepolicy_c.h"

QSizePolicyH QSizePolicy_Create()
{
	return (QSizePolicyH) new QSizePolicy();
}

void QSizePolicy_Destroy(QSizePolicyH handle)
{
	delete (QSizePolicy *)handle;
}

QSizePolicyH QSizePolicy_Create2(QSizePolicy::Policy horizontal, QSizePolicy::Policy vertical, QSizePolicy::ControlType type)
{
	return (QSizePolicyH) new QSizePolicy(horizontal, vertical, type);
}

QSizePolicy::Policy QSizePolicy_horizontalPolicy(QSizePolicyH handle)
{
	return (QSizePolicy::Policy) ((QSizePolicy *)handle)->horizontalPolicy();
}

QSizePolicy::Policy QSizePolicy_verticalPolicy(QSizePolicyH handle)
{
	return (QSizePolicy::Policy) ((QSizePolicy *)handle)->verticalPolicy();
}

QSizePolicy::ControlType QSizePolicy_controlType(QSizePolicyH handle)
{
	return (QSizePolicy::ControlType) ((QSizePolicy *)handle)->controlType();
}

void QSizePolicy_setHorizontalPolicy(QSizePolicyH handle, QSizePolicy::Policy d)
{
	((QSizePolicy *)handle)->setHorizontalPolicy(d);
}

void QSizePolicy_setVerticalPolicy(QSizePolicyH handle, QSizePolicy::Policy d)
{
	((QSizePolicy *)handle)->setVerticalPolicy(d);
}

void QSizePolicy_setControlType(QSizePolicyH handle, QSizePolicy::ControlType type)
{
	((QSizePolicy *)handle)->setControlType(type);
}

unsigned int QSizePolicy_expandingDirections(QSizePolicyH handle)
{
	return (unsigned int) ((QSizePolicy *)handle)->expandingDirections();
}

void QSizePolicy_setHeightForWidth(QSizePolicyH handle, bool b)
{
	((QSizePolicy *)handle)->setHeightForWidth(b);
}

bool QSizePolicy_hasHeightForWidth(QSizePolicyH handle)
{
	return (bool) ((QSizePolicy *)handle)->hasHeightForWidth();
}

void QSizePolicy_setWidthForHeight(QSizePolicyH handle, bool b)
{
	((QSizePolicy *)handle)->setWidthForHeight(b);
}

bool QSizePolicy_hasWidthForHeight(QSizePolicyH handle)
{
	return (bool) ((QSizePolicy *)handle)->hasWidthForHeight();
}

int QSizePolicy_horizontalStretch(QSizePolicyH handle)
{
	return (int) ((QSizePolicy *)handle)->horizontalStretch();
}

int QSizePolicy_verticalStretch(QSizePolicyH handle)
{
	return (int) ((QSizePolicy *)handle)->verticalStretch();
}

void QSizePolicy_setHorizontalStretch(QSizePolicyH handle, int stretchFactor)
{
	((QSizePolicy *)handle)->setHorizontalStretch(stretchFactor);
}

void QSizePolicy_setVerticalStretch(QSizePolicyH handle, int stretchFactor)
{
	((QSizePolicy *)handle)->setVerticalStretch(stretchFactor);
}

void QSizePolicy_transpose(QSizePolicyH handle)
{
	((QSizePolicy *)handle)->transpose();
}

