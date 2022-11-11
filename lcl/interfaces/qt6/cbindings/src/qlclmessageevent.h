#ifndef QLCLMESSAGEEVENT_H
#define QLCLMESSAGEEVENT_H

#include <QEvent>
#include "pascalbind.h"

class QLCLMessageEvent : public QEvent {

public:

  PTRUINT Msg;
  PTRUINT WParam;
  PTRUINT LParam;
  PTRUINT MsgResult;

  QLCLMessageEvent(Type aType) : QEvent(aType),Msg(0),WParam(0),LParam(0),MsgResult(0) {};
  QLCLMessageEvent(Type aType, PTRUINT aMsg, PTRUINT aWParam, PTRUINT aLParam, PTRUINT aMsgResult) : QEvent(aType),Msg(aMsg),WParam(aWParam),LParam(aLParam),MsgResult(aMsgResult) {};

  // Destroy Test: uncomment following line and recompile binding library
  //~QLCLMessageEvent() { printf("LCLMessage got destroyed\n"); };
  
  PTRUINT getMsg() { return Msg;}
  PTRUINT getWParam() { return WParam;}
  PTRUINT getLParam() { return LParam;}
  PTRUINT getMsgResult() { return MsgResult;}

  void setMsg(PTRUINT Value) {Msg = Value;}
  void setWParam(PTRUINT Value) {WParam = Value;}
  void setLParam(PTRUINT Value) {LParam = Value;}
  void setMsgResult(PTRUINT Value) {MsgResult = Value;}

};

#endif
