{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author:  Michael W. Vogel

 List of ModulePageControls linked to SourceEditorInterfaces. This List is used
 in just one SourceEditor window.

}

unit DockedSourceEditorPageControls;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  // RTL
  Classes, SysUtils, fgl,
  // LCL
  Forms,
  // IDEIntf
  SrcEditorIntf,
  // DockedFormEditor
  DockedModulePageControl;

type

  { TSourceEditorPageControl }

  TSourceEditorPageControl = record
    PageControl: TModulePageControl;
    SourceEditor: TSourceEditorInterface;
    class operator = (Item1, Item2: TSourceEditorPageControl): Boolean;
  end;

  { TSourceEditorPageControls }

  TSourceEditorPageControls = class(specialize TFPGList<TSourceEditorPageControl>)
  private
    function GetPageControl(ASrcEditor: TSourceEditorInterface): TModulePageControl;
    function GetSourceEditor(APageControl: TModulePageControl): TSourceEditorInterface;
  public
    procedure Add(ASrcEditor: TSourceEditorInterface; APageControl: TModulePageControl); overload;
    function Contains(APageControl: TModulePageControl): Boolean;
    function Contains(ASrcEditor: TSourceEditorInterface): Boolean;
    function IndexOf(APageControl: TModulePageControl): Integer; overload;
    function IndexOf(ASrcEditor: TSourceEditorInterface): Integer; overload;
    procedure Remove(ASrcEditor: TSourceEditorInterface); overload;
  public
    property PageControl[ASrcEditor: TSourceEditorInterface]: TModulePageControl read GetPageControl;
    property SourceEditor[APageControl: TModulePageControl]: TSourceEditorInterface read GetSourceEditor;
  end;

implementation

{ TSourceEditorPageControl }

class operator TSourceEditorPageControl. = (Item1, Item2: TSourceEditorPageControl): Boolean;
begin
  Result := (Item1.PageControl = Item2.PageControl) and
            (Item1.SourceEditor = Item2.SourceEditor);
end;

{ TSourceEditorPageControls }

function TSourceEditorPageControls.GetPageControl(ASrcEditor: TSourceEditorInterface): TModulePageControl;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ASrcEditor);
  if LIndex >= 0 then
    Result := Items[LIndex].PageControl
  else
    Result := nil;
end;

function TSourceEditorPageControls.GetSourceEditor(APageControl: TModulePageControl): TSourceEditorInterface;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(APageControl);
  if LIndex >= 0 then
    Result := Items[LIndex].SourceEditor
  else
    Result := nil;
end;

procedure TSourceEditorPageControls.Add(ASrcEditor: TSourceEditorInterface; APageControl: TModulePageControl);
var
  LSourceEditorPageControl: TSourceEditorPageControl;
begin
  LSourceEditorPageControl.SourceEditor := ASrcEditor;
  LSourceEditorPageControl.PageControl := APageControl;
  Add(LSourceEditorPageControl);
end;

function TSourceEditorPageControls.Contains(APageControl: TModulePageControl): Boolean;
begin
  Result := IndexOf(APageControl) >= 0;
end;

function TSourceEditorPageControls.Contains(ASrcEditor: TSourceEditorInterface): Boolean;
begin
  Result := IndexOf(ASrcEditor) >= 0;
end;

function TSourceEditorPageControls.IndexOf(APageControl: TModulePageControl): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].PageControl = APageControl then
      Exit(i);
end;

function TSourceEditorPageControls.IndexOf(ASrcEditor: TSourceEditorInterface): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].SourceEditor = ASrcEditor then
      Exit(i);
end;

procedure TSourceEditorPageControls.Remove(ASrcEditor: TSourceEditorInterface);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ASrcEditor);
  if LIndex < 0 then Exit;
  Delete(LIndex);
end;

end.

