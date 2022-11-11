//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCLMESSAGEEVENT_C_H
#define QLCLMESSAGEEVENT_C_H

#include "qlclmessageevent.h"
#include "pascalbind.h"

C_EXPORT QLCLMessageEventH QLCLMessageEvent_Create(QEvent::Type aType);
C_EXPORT void QLCLMessageEvent_Destroy(QLCLMessageEventH handle);
C_EXPORT QLCLMessageEventH QLCLMessageEvent_Create2(QEvent::Type aType, PTRUINT aMsg, PTRUINT aWParam, PTRUINT aLParam, PTRUINT aMsgResult);
C_EXPORT PTRUINT QLCLMessageEvent_getMsg(QLCLMessageEventH handle);
C_EXPORT PTRUINT QLCLMessageEvent_getWParam(QLCLMessageEventH handle);
C_EXPORT PTRUINT QLCLMessageEvent_getLParam(QLCLMessageEventH handle);
C_EXPORT PTRUINT QLCLMessageEvent_getMsgResult(QLCLMessageEventH handle);
C_EXPORT void QLCLMessageEvent_setMsg(QLCLMessageEventH handle, PTRUINT Value);
C_EXPORT void QLCLMessageEvent_setWParam(QLCLMessageEventH handle, PTRUINT Value);
C_EXPORT void QLCLMessageEvent_setLParam(QLCLMessageEventH handle, PTRUINT Value);
C_EXPORT void QLCLMessageEvent_setMsgResult(QLCLMessageEventH handle, PTRUINT Value);

#endif
