unit ipUnicode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8;

function IsWordBreak(u1, u2: DWord): boolean;

implementation

function IsWordBreak(u1, u2: DWord): boolean;
begin
  // check no break before
  case u2 of

  $0021, // !
  $0029, // )
  $002C, // ,
  $002E, // .
  $003A, // :
  $003B, // ;
  $003E, // >
  $003F, // ?
  $005D, // ]
  $007D, // }
  $2019, // ’
  $201D, // ”

  $3001, // ideographic comma, no break before
  $3002, // 。ideographic full stop, no break before
  $3009, //	〉	Right Angle Bracket
  $300B, //	》	Right Double Angle Bracket
  $300D, //	」	Right Corner Bracket
  $300F, //	』	Right White Corner Bracket
  $3011, //	】	Right Black Lenticular Bracket
  $3015, //	〕	Right Tortoise Shell Bracket
  $3017, //	〗	Right White Lenticular Bracket
  $3019, //	〙	Right White Tortoise Shell Bracket
  $301B: //	〛	Right White Square Bracket
    exit(false); // no break before
  end;

  // check break after
  case u1 of
  $0009, // tab
  $000A, // LF
  $000C: Result:=u2<>10; // CR
  $0020: Result:=true; // space
  $0028, // (
  $003C, // <
  $005B, // [
  $007B: Result:=false; // {
  $00A0: Result:=false; // nbsp
  $00AD: Result:=true; // soft hyphen
  $2009: Result:=true; // thin space
  $200B: Result:=true; // zero width space
  $2018: Result:=false; // ‘
  $201C: Result:=false; // “

  $3002, // 。chinese full stop
  $300B, // 》
  $300D, // 」
  $300F, // 』
  $3011, // 】
  $FF01, // ！chinese exclamation mark
  $FF09, // ）chinese closing bracket
  $FF0C, // ，chinese comma
  $FF1F: // ？chinese question mark
    Result:=true;
  else
    Result:=false;
  end;

  // todos:
  // break after hyphen-minus, en dash, em dash
end;

end.

