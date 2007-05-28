unit ModeMacPas; 

{$mode macpas}{$H+}

interface

uses
  Classes, SysUtils; 

{$ifc defined FPC_BIG_ENDIAN}
aaa1
  {$setc TARGET_RT_BIG_ENDIAN := TRUE}
  {$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
aaa2
  {$setc TARGET_RT_BIG_ENDIAN := FALSE}
  {$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
aaa3
  {$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
aaa

implementation

end.

