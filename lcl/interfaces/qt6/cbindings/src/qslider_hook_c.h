//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSLIDER_HOOK_C_H
#define QSLIDER_HOOK_C_H

#include "qslider_hook.h"

C_EXPORT QSlider_hookH QSlider_hook_Create(QObjectH handle);
C_EXPORT void QSlider_hook_Destroy(QSlider_hookH handle);

#endif
