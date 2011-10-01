program testmethodcompare;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils; 

var
  M1, M2: TNotifyEvent;

  procedure WriteReport;
  begin
    writeln('WriteReport',
      ' M1.Data=',PtrUInt(TMethod(M1).Data),
      ' M1.Code=',PtrUInt(TMethod(M1).Code),
      ' M2.Data=',PtrUInt(TMethod(M2).Data),
      ' M2.Code=',PtrUInt(TMethod(M2).Code),
      ' Assigned(M1)=',Assigned(M1),
      ' Assigned(M2)=',Assigned(M2),
      ' M1=nil=',(M1=nil),
      ' M2=nil=',(M2=nil),
      ' M1<>nil=',(M1<>nil),
      ' M2<>nil=',(M2<>nil),
      ' M1=M2=',(M1=M2),
      ' M1<>M2=',(M1<>M2),
      ''
      );
  end;

  function Meth(Code, Data: Pointer): TNotifyEvent;
  var
    m: TMethod;
  begin
    m.Code:=Code;
    m.Data:=Data;
    Result:=TNotifyEvent(m);
  end;

begin
  M1:=nil;
  M2:=nil;
  WriteReport;
  M1:=Meth(Pointer(1),Pointer(0));
  WriteReport;
  M2:=M1;
  WriteReport;
  M2:=nil;
  M1:=Meth(Pointer(0),Pointer(1));
  WriteReport;
  M2:=M1;
  WriteReport;
  M2:=nil;
  M1:=Meth(Pointer(1),Pointer(1));
  WriteReport;
  M2:=M1;
  WriteReport;
end.

