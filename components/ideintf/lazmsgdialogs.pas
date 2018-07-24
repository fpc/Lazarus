unit LazMsgDialogs;

{$mode objfpc}{$H+}

interface

uses
  // LazUtils
  UITypes;

type
  TLazMessageDialog = function(const aCaption, aMsg: string;
                               DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                               const HelpKeyword: string = ''): Integer of object;
  TLazQuestionDialog = function(const aCaption, aMsg: string;
                                DlgType: TMsgDlgType; Buttons: array of const;
                                const HelpKeyword: string = ''): Integer of object;

var  // set by the IDE
  LazMessageDialog: TLazMessageDialog = nil;
  LazQuestionDialog: TLazQuestionDialog = nil;

implementation

end.

