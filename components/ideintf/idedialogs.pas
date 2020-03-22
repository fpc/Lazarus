{ Copyright (C) 2004

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Common IDE dialogs.
}
unit IDEDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Dialogs,
  // LazUtils
  UITypes, LazFileCache,
  // BuildIntf
  LazMsgDialogs;

type

  { TIDEOpenDialog }

  TIDEOpenDialog = class(TOpenDialog)
  protected
    function DoExecute: boolean; override;
  end;
  TIDEOpenDialogClass = class of TIDEOpenDialog;

  { TIDESaveDialog }

  TIDESaveDialog = class(TSaveDialog)
  protected
    function DoExecute: boolean; override;
  end;
  TIDESaveDialogClass = class of TIDESaveDialog;

  TIDESelectDirectory = function(const Title, InitialDir: string): string of object;
  TInitIDEFileDialog = procedure(AFileDialog: TFileDialog) of object;
  TStoreIDEFileDialog = procedure(AFileDialog: TFileDialog) of object;

var  // set by the IDE
  LazIDESelectDirectory: TIDESelectDirectory = nil;
  InitIDEFileDialog: TInitIDEFileDialog = nil;
  StoreIDEFileDialog: TStoreIDEFileDialog = nil;
  IDEOpenDialogClass: TIDEOpenDialogClass = TIDEOpenDialog;
  IDESaveDialogClass: TIDESaveDialogClass = TIDESaveDialog;

// Wrapper function for LazIDESelectDirectory with a default parameter.
function LazSelectDirectory(const Title: string; const InitialDir: string = ''): string;

// Wrapper function for LazMessageDialog in LazMsgDialogs.
function IDEMessageDialog(const aCaption, aMsg: string;
                          DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                          const HelpKeyword: string = ''): Integer;
// Wrapper function for LazQuestionDialog in LazMsgDialogs.
function IDEQuestionDialog(const aCaption, aMsg: string;
                           DlgType: TMsgDlgType; Buttons: array of const;
                           const HelpKeyword: string = ''): Integer;

function IDEMessageDialogAb(const aCaption, aMsg: string;
                   DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                   ShowAbort: boolean; const HelpKeyword: string = ''): Integer;
function IDEQuestionDialogAb(const aCaption, aMsg: string;
                   DlgType: TMsgDlgType; Buttons: array of const;
                   HideAbort: boolean; const HelpKeyword: string = ''): Integer;

type
  { TIgnoreIDEQuestionItem }

  TIgnoreQuestionDuration = (
    iiidIDERestart,
    iiid24H,
    iiidForever
    );
  TIgnoreQuestionDurations = set of TIgnoreQuestionDuration;

  TIgnoreIDEQuestionItem = class
  private
    FIdentifier: string;
  public
    Date: TDateTime;
    Flag: string;
    Duration: TIgnoreQuestionDuration;
    constructor Create(const TheIdentifier: string);
    property Identifier: string read FIdentifier;
  end;

  { TIgnoreIDEQuestionList }

  TIgnoreIDEQuestionList = class
  public
    function Add(const Identifier: string;
                 const Duration: TIgnoreQuestionDuration;
                 const Flag: string = ''): TIgnoreIDEQuestionItem; virtual; abstract;
    procedure Delete(const Identifier: string); virtual; abstract;
    function Find(const Identifier: string): TIgnoreIDEQuestionItem; virtual; abstract;
  end;

var
  IgnoreQuestions: TIgnoreIDEQuestionList = nil;

implementation

function LazSelectDirectory(const Title: string; const InitialDir: string): string;
begin
  Result:=LazIDESelectDirectory(Title,InitialDir);
end;

function IDEMessageDialog(const aCaption, aMsg: string;
                          DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                          const HelpKeyword: string = ''): Integer;
begin
  Result := LazMessageDialog(aCaption, aMsg, DlgType, Buttons, HelpKeyword);
end;

function IDEQuestionDialog(const aCaption, aMsg: string;
                           DlgType: TMsgDlgType; Buttons: array of const;
                           const HelpKeyword: string = ''): Integer;
begin
  Result := LazQuestionDialog(aCaption, aMsg, DlgType, Buttons, HelpKeyword);
end;

function IDEMessageDialogAb(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; ShowAbort: boolean; const HelpKeyword: string): Integer;
begin
  if ShowAbort then begin
    // add an abort button for 'Cancel all' and replace a Cancel with Ignore
    Buttons:=Buttons+[mbAbort];
    if mbCancel in Buttons then
      Buttons:=Buttons-[mbCancel]+[mbIgnore];
  end;
  Result:=IDEMessageDialog(aCaption,aMsg,DlgType,Buttons,HelpKeyword);
end;

function IDEQuestionDialogAb(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: array of const;
  HideAbort: boolean; const HelpKeyword: string): Integer;
var
  NewButtons: array of TVarRec;
  i: Integer;
  j: Integer;
begin
  SetLength(NewButtons,High(Buttons)-Low(Buttons)+1);
  i:=low(Buttons);
  j:=0;
  while i<=High(Buttons) do begin
    if HideAbort
    and (Buttons[i].VType=vtInteger)
    and (Buttons[i].VInteger=mrAbort) then begin
      // skip abort button
      inc(i);
      // and skip abort caption
      if Buttons[i].VType<>vtInteger then
        inc(i);
    end else begin
      NewButtons[j]:=Buttons[i];
      inc(i);
      inc(j);
    end;
  end;
  SetLength(NewButtons,j);
  Result:=IDEQuestionDialog(aCaption,aMsg,DlgType,NewButtons,HelpKeyword);
end;

{ TIDESaveDialog }

function TIDESaveDialog.DoExecute: boolean;
begin
  Result:=inherited DoExecute;
  LazFileCache.InvalidateFileStateCache;
end;

{ TIDEOpenDialog }

function TIDEOpenDialog.DoExecute: boolean;
begin
  Result:=inherited DoExecute;
  LazFileCache.InvalidateFileStateCache;
end;

{ TIgnoreIDEQuestionItem }

constructor TIgnoreIDEQuestionItem.Create(const TheIdentifier: string);
begin
  fIdentifier:=TheIdentifier;
end;

end.

