unit pdfvrlexico;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  Token = record
    tipo: Int64;
    token_string: String;
  end;

    TPDFCommandCode = (cc_NONE, cc_m_START_PATH, cc_l_ADD_LINE_TO_PATH,
     cc_H_CLOSE_PATH, cc_S_END_PATH, cc_hS_CLOSE_AND_END_PATH,
     cc_c_BEZIER_TO_X_Y_USING_X2_Y2_AND_X3_Y3,
     cc_v_BEZIER_TO_X_Y_USING_CURRENT_POS_AND_X2_Y2,
     cc_y_BEZIER_TO_X_Y_USING_X_Y_AND_X2_Y2,
     cc_CONCATENATE_MATRIX,cc_RESTORE_MATRIX);

    Command = record
            cord_x3: String;
            cord_y3: String;
            cord_x2: String;
            cord_y2: String;
            cord_x: String;
            cord_y: String;
            my_operator: String;
            code: TPDFCommandCode;
    end;

    PageHeader = record
            page_length: Int64;
            flate_decode: Boolean;
    end;

  AnLexico = class
  public
    Doc: TStream;
    bytesRemaining: Int64;
    constructor Create();
    function getToken(): Token;
    function getPageToken(): Token;
  end;

implementation

function AnLexico.getToken(): Token;
var
  t: Byte;
  mytoken: Token;
begin
    mytoken.tipo := 0;
    while( bytesRemaining > 0 ) do
    begin
         t := Doc.ReadByte();
         bytesRemaining := bytesRemaining - 1;
         // numbers or points or minus
         if((((t >= 48) and (t <= 57)) or (t = 46 ) or (t = 45)) and
           ((mytoken.tipo = 1) or (mytoken.tipo = 0))) then
             begin
                  mytoken.token_string := mytoken.token_string + char(t);
                  mytoken.tipo:=1;
             end
         else if(((t >= 65) and (t <= 90)) or ((t >= 97) and (t <= 122)) // letters
                   or (t = 42) // *
                   and ((mytoken.tipo = 2) or (mytoken.tipo = 0))) then
              begin
                   mytoken.token_string := mytoken.token_string + char(t);
                   mytoken.tipo:=2;
              end
         else // everything else
              begin
                   if (mytoken.tipo <> 0) then
                   begin
                        // solve CorelDraw problem after "stream"
                        if ((t=13) and (bytesRemaining>0)) then
                        begin
                             t := Doc.ReadByte();
                             bytesRemaining:=bytesRemaining-1;
                        end;
                        Result := mytoken;
                        Exit;
                   end;
              end;
  end;
  Result := mytoken;
end;

function AnLexico.getPageToken(): Token;
var
  t: Byte;
  mytoken: Token;
begin
     mytoken.tipo := 0;
     if (bytesRemaining > 0) then
     begin
         t := Doc.ReadByte;
         mytoken.token_string:=char(t);
         bytesRemaining := bytesRemaining - 1;
     end;
     Result := mytoken;
end;

constructor AnLexico.Create();
begin
  inherited Create;
end;

end.

