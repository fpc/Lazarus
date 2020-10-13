unit LazMsgWorker;

{$mode objfpc}{$H+}

interface

uses
  // LazUtils
  UITypes;

type
  TLazMessageWorker = function(const aCaption, aMsg: string;
                               DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                               const HelpKeyword: string = ''): Integer of object;
  TLazQuestionWorker = function(const aCaption, aMsg: string;
                                DlgType: TMsgDlgType; Buttons: array of const;
                                const HelpKeyword: string = ''): Integer of object;

var  // set by the IDE
  LazMessageWorker: TLazMessageWorker = nil;
  LazQuestionWorker: TLazQuestionWorker = nil;

implementation

end.

