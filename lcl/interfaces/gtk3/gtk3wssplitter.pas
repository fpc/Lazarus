unit gtk3wssplitter;

{$mode objfpc}{$H+}
{$I gtk3defines.inc}

interface

uses
  // LCL
  LCLProc, ExtCtrls, Classes, Controls, SysUtils, types, Graphics, LCLType,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSPairSplitter,
  Gtk3WSControls,
  PairSplitter;

type

  { TGtk3WSPairSplitterSide }

  TGtk3WSPairSplitterSide = class(TWSPairSplitterSide)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtk3WSCustomPairSplitter }

  TGtk3WSCustomPairSplitter = class(TWSCustomPairSplitter)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function AddSide(ASplitter: TCustomPairSplitter; ASide: TPairSplitterSide; Side: integer): Boolean; override;
    class function RemoveSide(ASplitter: TCustomPairSplitter; ASide: TPairSplitterSide; Side: integer): Boolean; override;
    class function SetPosition(ASplitter: TCustomPairSplitter; var NewPosition: integer): Boolean; override;
    // special cursor handling
    class function GetSplitterCursor(ASplitter: TCustomPairSplitter; var ACursor: TCursor): Boolean; override;
    class function SetSplitterCursor(ASplitter: TCustomPairSplitter; ACursor: TCursor): Boolean; override;
  end;

implementation
uses
  wsproc,gtk3widgets,lazgtk3;

{ TGtk3WSPairSplitterSide }

class function TGtk3WSPairSplitterSide.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result:=TLclIntfHandle(TGtk3Window.Create(AWinControl, AParams));
end;

{ TGtk3WSSplitter }


{ TGtk3WSCustomPairSplitter }

class function TGtk3WSCustomPairSplitter.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result:=TLclIntfHandle(TGtk3Paned.Create(AWinControl, AParams));
end;

class function TGtk3WSCustomPairSplitter.AddSide(ASplitter: TCustomPairSplitter;
  ASide: TPairSplitterSide; Side: integer): Boolean;
var
  paned: TGtk3Paned;
  wside:TGtk3Widget;
begin
  Result := False;
  if not (WSCheckHandleAllocated(ASplitter, 'AddSide - splitter') and
          WSCheckHandleAllocated(ASide, 'AddSide - side'))
  then Exit;

  if (Side < 0) or (Side > 1) then exit;

  paned:=TGtk3Paned(ASplitter.Handle);
  wside:=TGtk3Widget(ASide.Handle);

  if Side=0 then
  begin
    PGtkWIdget(wside.Widget)^.set_parent(nil);
    PGtkPaned(paned.Widget)^.add1(wside.Widget);
  end
  else
  begin
    PGtkWidget(wside.Widget)^.set_parent(nil);
  	PGtkPaned(paned.Widget)^.add2(wside.Widget);
  end;
  Result := True;
end;

class function TGtk3WSCustomPairSplitter.RemoveSide(ASplitter: TCustomPairSplitter;
  ASide: TPairSplitterSide; Side: integer): Boolean;
begin
  Result := False;
end;

class function TGtk3WSCustomPairSplitter.SetPosition(
  ASplitter: TCustomPairSplitter; var NewPosition: integer): Boolean;
var
  paned:PGtkPaned;
begin
  Result:=false;
  if not WSCheckHandleAllocated(ASplitter, ClassName+'.SetPosition') then
  	Exit;

  paned:=PGtkPaned(TGtk3Paned(ASplitter.Handle).Widget);
  paned^.set_position(NewPosition);
  Result:=true;
  ///Result:=inherited SetPosition(ASplitter, NewPosition);
end;

class function TGtk3WSCustomPairSplitter.GetSplitterCursor(
  ASplitter: TCustomPairSplitter; var ACursor: TCursor): Boolean;
begin
  ACursor:=crHsplit;
  Result:=true;
end;

class function TGtk3WSCustomPairSplitter.SetSplitterCursor(
  ASplitter: TCustomPairSplitter; ACursor: TCursor): Boolean;
begin
  //ASplitter.Cursor:=ACursor;
  Result:=false;
end;


end.

