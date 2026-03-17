unit CocoaDebug;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Types, LCLType, LCLProc, Controls,
  CocoaAll,
  CocoaPrivate, CocoaCallback, CocoaUtils, Cocoa_Extra;

type

  { TCocoaDebugUtil }

  TCocoaDebugUtil = class
  public
    class function toString(const obj: NSObject): String; overload;
    class function toString(const cb: ICommonCallback): String; overload;
    class procedure dumpParents(fromView: NSView);
  end;

implementation

{ TCocoaDebugUtil }

class function TCocoaDebugUtil.toString(const obj: NSObject): String;
begin
  Result := IntToStr(PtrUInt(obj));
  if Assigned(obj) then
    Result := Result +' '+obj.lclClassName+' lcl: '+toString(obj.lclGetCallback);
end;

class function TCocoaDebugUtil.toString(const cb: ICommonCallback): String;
var
  trg : TObject;
begin
  Result := IntToStr(PtrUInt(cb));
  if Assigned(cb) then
  begin
    trg := cb.GetTarget;
    Result := Result + ' trg: '+IntToStr(PtrUInt(trg));
    if Assigned(trg) then
    begin
      Result := Result + ' '+trg.ClassName;
      if trg is TWinControl then
        Result := Result +' '+TWinControl(trg).Name;
    end;
  end;
end;

class procedure TCocoaDebugUtil.dumpParents(fromView: NSView);
begin
  while Assigned(fromView) do begin
    writeln(fromView.lclClassName);
    fromView := fromView.superView;
  end;
end;

end.

