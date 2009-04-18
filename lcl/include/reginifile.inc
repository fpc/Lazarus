{******************************************************************************
                                TRegIniFile
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{------------------------------------------------------------------------------
  Method: TRegIniFile.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TRegIniFile.Create(const FN: String);
begin
  inherited Create;
  fFileName := FN;
end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.DeleteKey(const Section, Ident: String);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.EraseSection(const Section: string);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.ReadSection(const Section: string; Strings: TStrings);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.ReadSections(Strings: TStrings);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.WriteInteger(const Section, Ident: string; Value: LongInt);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.WriteString(const Section, Ident, Value: String);
begin

end;



{------------------------------------------------------------------------------
  Function: TRegIniFile.MyFunction
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
  Result := Default;
end;

{------------------------------------------------------------------------------
  Function: TRegIniFile.MyFunction
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegIniFile.ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
begin
  Result := Default;
end;

{------------------------------------------------------------------------------
  Function: TRegIniFile.MyFunction
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegIniFile.ReadString(const Section, Ident, Default: String): String;
begin
  Result := Default;
end;


