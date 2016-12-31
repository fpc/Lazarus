//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlclmessageevent_c.h"

QLCLMessageEventH QLCLMessageEvent_Create(QEvent::Type aType)
{
	return (QLCLMessageEventH) new QLCLMessageEvent(aType);
}

void QLCLMessageEvent_Destroy(QLCLMessageEventH handle)
{
	delete (QLCLMessageEvent *)handle;
}

QLCLMessageEventH QLCLMessageEvent_Create2(QEvent::Type aType, PTRUINT aMsg, PTRUINT aWParam, PTRUINT aLParam, PTRUINT aMsgResult)
{
	return (QLCLMessageEventH) new QLCLMessageEvent(aType, aMsg, aWParam, aLParam, aMsgResult);
}

PTRUINT QLCLMessageEvent_getMsg(QLCLMessageEventH handle)
{
	return (PTRUINT) ((QLCLMessageEvent *)handle)->getMsg();
}

PTRUINT QLCLMessageEvent_getWParam(QLCLMessageEventH handle)
{
	return (PTRUINT) ((QLCLMessageEvent *)handle)->getWParam();
}

PTRUINT QLCLMessageEvent_getLParam(QLCLMessageEventH handle)
{
	return (PTRUINT) ((QLCLMessageEvent *)handle)->getLParam();
}

PTRUINT QLCLMessageEvent_getMsgResult(QLCLMessageEventH handle)
{
	return (PTRUINT) ((QLCLMessageEvent *)handle)->getMsgResult();
}

void QLCLMessageEvent_setMsg(QLCLMessageEventH handle, PTRUINT Value)
{
	((QLCLMessageEvent *)handle)->setMsg(Value);
}

void QLCLMessageEvent_setWParam(QLCLMessageEventH handle, PTRUINT Value)
{
	((QLCLMessageEvent *)handle)->setWParam(Value);
}

void QLCLMessageEvent_setLParam(QLCLMessageEventH handle, PTRUINT Value)
{
	((QLCLMessageEvent *)handle)->setLParam(Value);
}

void QLCLMessageEvent_setMsgResult(QLCLMessageEventH handle, PTRUINT Value)
{
	((QLCLMessageEvent *)handle)->setMsgResult(Value);
}

