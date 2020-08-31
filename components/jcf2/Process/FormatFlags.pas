unit FormatFlags;

{ AFS 20 July 2001

  Code formatter exclusions flags
  These flags are used to switch off formatting based on special comments }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is FormatFlags, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  SysUtils;

type

  TFormatFlag = (eAllFormat,
    eObfuscate,
    eAddSpace, eRemoveSpace,
    eAddReturn, eRemoveReturn,
    eAlignVars, eAlignConst, eAlignTypeDef, eAlignAssign, eAlignComment, eAlignField,
    eCapsReservedWord, eCapsSpecificWord,
    eIndent, eLineBreaking, eBlockStyle,
    eWarning,
    eFindReplace, eFindReplaceUses, eRemoveComments);


  { these flags control:

    AllFormat: all clarify processes - turn the formatter as a whole on or off
    space: all processes that insert or remove spaces
    indent: inenting of code blocks etc
    return: all processes that insert or remove returns - note tat there is some overlap with
    eAlign: alignment of vars, assigns etc
    eLineBreaking: spliting long lines into 2 or more
    eBlockStyle - where to put begins & ends, else, etc
    eWarning: supress warnings
  }

  TFormatFlags = set of TFormatFlag;

{ read a comment for comment enabled flag data }
function ReadCommentJcfFlags(psComment: string; out psError: string;
  out peFlags: TFormatFlags; out pbOn: boolean): boolean;

const
  FORMAT_COMMENT_PREFIX     = '//jcf:';
  FORMAT_COMMENT_PREFIX_LEN = 6;

  ALL_FLAGS: TFormatFlags = [Low(TFormatFlag)..High(TFormatFlag)];

implementation

uses
  { local }
  JcfStringUtils;

type
  TRFlagNameData = record
    sName: string;
    eFlags: TFormatFlags;
  end;

const
  FORMAT_FLAG_NAMES: array[1..29] of TRFlagNameData =
    (
    (sName: 'format'; eFlags: [eAllFormat]),
    (sName: 'obfuscate'; eFlags: [eObfuscate]),


    (sName: 'space'; eFlags: [eAddSpace, eRemoveSpace]),
    (sName: 'addspace'; eFlags: [eAddSpace]),
    (sName: 'removespace'; eFlags: [eRemoveSpace]),


    (sName: 'return'; eFlags: [eAddReturn, eRemoveReturn]),
    (sName: 'addreturn'; eFlags: [eAddReturn]),
    (sName: 'removereturn'; eFlags: [eRemoveReturn]),

    (sName: 'add'; eFlags: [eAddReturn, eAddSpace]),
    (sName: 'remove'; eFlags: [eRemoveReturn, eRemoveSpace]),


    (sName: 'align'; eFlags: [eAlignVars, eAlignConst, eAlignTypeDef,
    eAlignAssign, eAlignComment]),
    (sName: 'aligndef'; eFlags: [eAlignVars, eAlignConst, eAlignTypeDef]),
    (sName: 'alignfn'; eFlags: [eAlignVars, eAlignAssign]),

    (sName: 'alignvars'; eFlags: [eAlignVars]),
    (sName: 'alignconst'; eFlags: [eAlignConst]),
    (sName: 'aligntypedef'; eFlags: [eAlignTypeDef]),
    (sName: 'alignassign'; eFlags: [eAlignAssign]),
    (sName: 'aligncomment'; eFlags: [eAlignComment]),
    (sName: 'alignfield'; eFlags: [eAlignField]),


    (sName: 'indent'; eFlags: [eIndent]),

    (sName: 'caps'; eFlags: [eCapsReservedWord, eCapsSpecificWord]),
    (sName: 'capsreservedwords'; eFlags: [eCapsReservedWord]),
    (sName: 'capsspecificword'; eFlags: [eCapsSpecificWord]),


    (sName: 'linebreaking'; eFlags: [eLineBreaking]),
    (sName: 'blockstyle'; eFlags: [eBlockStyle]),

    (sName: 'warnings'; eFlags: [eWarning]),
    (sName: 'findreplace'; eFlags: [eFindReplace]),
    (sName: 'findreplaceuses'; eFlags: [eFindReplaceUses]),

    (sName: 'reovecomments'; eFlags: [eRemoveComments])
    );


{ can stop and restart formating using these comments
 from DelForExp - Egbbert Van Nes's program }
const
  OLD_NOFORMAT_ON  = '{(*}';
  OLD_NOFORMAT_OFF = '{*)}';

  NOFORMAT_ON  = FORMAT_COMMENT_PREFIX + 'format=off';
  NOFORMAT_OFF = FORMAT_COMMENT_PREFIX + 'format=on';

{ like StrToBoolean, but recognises 'on' and 'off' too }
function LStrToBoolean(const ps: string): boolean;
begin
  if AnsiSameText(ps, 'on') then
    Result := True
  else if AnsiSameText(ps, 'off') then
    Result := False
  else
    Result := StrToBoolean(ps);
end;

{ this function works as follows
  Give it a comment text (psComment)
  and it returns
  - True if the comment is a special JCF flags comment
  psError is empty if the flags could be parsed, else contains an error message
  psFlags returns the set of flags referenced
  pbOn tells if they were turned on or off
}
function ReadCommentJcfFlags(psComment: string; out psError: string;
  out peFlags: TFormatFlags; out pbOn: boolean): boolean;
var
  lsPrefix, lsRest: string;
  lsSetting, lsState: string;
  lbFlagFound: boolean;
  liLoop:      integer;
begin
  Result  := False;
  psError := '';

  // translate {(*} comments to jcf:format=on comments
  if psComment = OLD_NOFORMAT_ON then
    psComment := NOFORMAT_ON
  else if psComment = OLD_NOFORMAT_OFF then
    psComment := NOFORMAT_OFF;

  { all comments without the required prefix are of no import to this code
    if it's not one, then exit without error }
  lsPrefix := StrLeft(psComment, 6);
  if not (AnsiSameText(lsPrefix, FORMAT_COMMENT_PREFIX)) then
    exit;

  // should be a valid jcf flag directive after here
  Result := True;
  lsRest := Trim(StrRestOf(psComment, 7));

  { rest should read <setting>=<state>
    where the setting is one of the format flags, and the state is 'on' or 'off'
  }
  lsSetting := Trim(StrBefore('=', lsRest));
  lsState   := Trim(StrAfter('=', lsRest));

  { is the comment well formed? }
  if (lsSetting = '') or (lsState = '') then
  begin
    psError := 'Comment ' + StrDoubleQuote(psComment) +
      ' has prefix but cannot be parsed';
    exit;
  end;

  { try and get a state flag from the string, abort if it fails }
  try
    pbOn := LStrToBoolean(lsState);
  except
    On EJcfConversionError do
    begin
      psError := 'In comment ' + StrDoubleQuote(psComment) + ' , ' +
        ' state ' + StrDoubleQuote(lsState) + ' cannot be parsed to either on or off';
      exit;
    end
    else
      raise;
  end;

  lbFlagFound := False;

  // accept jcf:all=on to reset state to normal by removing all flags
  if AnsiSameText(lsSetting, 'all') then
  begin
    peFlags     := ALL_FLAGS;
    lbFlagFound := True;
  end
  else
  begin
    { match the setting from the table }
    for liLoop := low(FORMAT_FLAG_NAMES) to high(FORMAT_FLAG_NAMES) do
    begin
      if AnsiSameText(lsSetting, FORMAT_FLAG_NAMES[liLoop].sName) then
      begin
        peFlags     := FORMAT_FLAG_NAMES[liLoop].eFlags;
        lbFlagFound := True;
        break;
      end;
    end;
  end;

  if not lbFlagFound then
  begin
    // unknown setting - nothing to do except log a message
    psError := 'In comment ' + StrDoubleQuote(psComment) + ' , ' +
      ' setting ' + StrDoubleQuote(lsSetting) + ' is not known';
    exit;
  end;
end;

end.
