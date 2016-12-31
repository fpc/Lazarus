//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSIZEPOLICY_C_H
#define QSIZEPOLICY_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QSizePolicyH QSizePolicy_Create();
C_EXPORT void QSizePolicy_Destroy(QSizePolicyH handle);
C_EXPORT QSizePolicyH QSizePolicy_Create2(QSizePolicy::Policy horizontal, QSizePolicy::Policy vertical, QSizePolicy::ControlType type);
C_EXPORT QSizePolicy::Policy QSizePolicy_horizontalPolicy(QSizePolicyH handle);
C_EXPORT QSizePolicy::Policy QSizePolicy_verticalPolicy(QSizePolicyH handle);
C_EXPORT QSizePolicy::ControlType QSizePolicy_controlType(QSizePolicyH handle);
C_EXPORT void QSizePolicy_setHorizontalPolicy(QSizePolicyH handle, QSizePolicy::Policy d);
C_EXPORT void QSizePolicy_setVerticalPolicy(QSizePolicyH handle, QSizePolicy::Policy d);
C_EXPORT void QSizePolicy_setControlType(QSizePolicyH handle, QSizePolicy::ControlType type);
C_EXPORT unsigned int QSizePolicy_expandingDirections(QSizePolicyH handle);
C_EXPORT void QSizePolicy_setHeightForWidth(QSizePolicyH handle, bool b);
C_EXPORT bool QSizePolicy_hasHeightForWidth(QSizePolicyH handle);
C_EXPORT void QSizePolicy_setWidthForHeight(QSizePolicyH handle, bool b);
C_EXPORT bool QSizePolicy_hasWidthForHeight(QSizePolicyH handle);
C_EXPORT int QSizePolicy_horizontalStretch(QSizePolicyH handle);
C_EXPORT int QSizePolicy_verticalStretch(QSizePolicyH handle);
C_EXPORT void QSizePolicy_setHorizontalStretch(QSizePolicyH handle, int stretchFactor);
C_EXPORT void QSizePolicy_setVerticalStretch(QSizePolicyH handle, int stretchFactor);
C_EXPORT void QSizePolicy_transpose(QSizePolicyH handle);

#endif
