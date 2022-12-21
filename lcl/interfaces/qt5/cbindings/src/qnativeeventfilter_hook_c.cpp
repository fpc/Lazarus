//******************************************************************************
//  Copyright (c) 2005-2022 by Matteo Salvi
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************

#include "qnativeeventfilter_hook_c.h"

Q_NativeEventFilter_hookH Q_NativeEventFilter_hook_Create(QCoreApplicationH handle)
{
    return (Q_NativeEventFilter_hookH) new Q_NativeEventFilter_hook((QCoreApplication*)handle);
}

void Q_NativeEventFilter_hook_Destroy(Q_NativeEventFilter_hookH handle)
{
    delete (Q_NativeEventFilter_hook *)handle;
}

void Q_NativeEventFilter_hook_installfilter(Q_NativeEventFilter_hookH handle, QHookH hook)
{
    ((Q_NativeEventFilter_hook *)handle)->hook_installfilter(hook);
}

void Q_NativeEventFilter_hook_destroyed(Q_NativeEventFilter_hookH handle, QHookH hook)
{
    ((Q_NativeEventFilter_hook *)handle)->hook_destroyed(hook);
}

void Q_NativeEventFilter_hook_removefilter(Q_NativeEventFilter_hookH handle)
{
    ((Q_NativeEventFilter_hook *)handle)->hook_removefilter();
}
