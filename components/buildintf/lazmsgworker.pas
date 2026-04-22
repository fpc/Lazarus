unit LazMsgWorker;

{$mode objfpc}{$H+}

interface

uses
  System.UITypes,
  // BuildIntf
  IDEExternToolIntf;

type
  TLazMessageWorker = function(const aCaption, aMsg: string;
                               DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                               const HelpKeyword: string = ''): Integer of object;
  TLazQuestionWorker = function(const aCaption, aMsg: string;
                                DlgType: TMsgDlgType; Buttons: array of const;
                                const HelpKeyword: string = ''): Integer of object;
  TShowMessageEvent = function(aUrgency: TMessageLineUrgency; aMsg: string;
              aSrcFilename: string=''; aLineNumber: integer=0; aColumn: integer=0;
              aViewCaption: string=''): TMessageLine of object;
  TMainTitleChangeEvent = procedure(const ATitle: string) of object;
  TSaveAllEditorChangesEvent = function(): boolean;

var  // set by the IDE
  LazMessageWorker: TLazMessageWorker = nil;
  LazQuestionWorker: TLazQuestionWorker = nil;
  // In the IDE this is the same as IDEMessagesWindow.AddCustomMessage.
  // A cmd line program can also set a handler for printing messages.
  OnShowMessage: TShowMessageEvent;
  // Typically the IDE's main title.
  OnMainTitleChange: TMainTitleChangeEvent;
  // Source editor changes
  OnSaveAllEditorChanges: TSaveAllEditorChangesEvent;

function LazMessageDialogAb(const aCaption, aMsg: string;
                   DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                   ShowAbort: boolean; const HelpKeyword: string = ''): Integer;
function LazQuestionDialogAb(const aCaption, aMsg: string;
                   DlgType: TMsgDlgType; Buttons: array of const;
                   HideAbort: boolean; const HelpKeyword: string = ''): Integer;

implementation

function LazMessageDialogAb(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; ShowAbort: boolean; const HelpKeyword: string): Integer;
begin
  if ShowAbort then begin
    // add an abort button for 'Cancel all' and replace a Cancel with Ignore
    Buttons:=Buttons+[mbAbort];
    if mbCancel in Buttons then
      Buttons:=Buttons-[mbCancel]+[mbIgnore];
  end;
  Result:=LazMessageWorker(aCaption,aMsg,DlgType,Buttons,HelpKeyword);
end;

function LazQuestionDialogAb(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: array of const;
  HideAbort: boolean; const HelpKeyword: string): Integer;
var
  NewButtons: array of TVarRec;
  i: Integer;
  j: Integer;
begin
  SetLength(NewButtons{%H-},High(Buttons)-Low(Buttons)+1);
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
  Result:=LazQuestionWorker(aCaption,aMsg,DlgType,NewButtons,HelpKeyword);
end;

end.

