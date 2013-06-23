{$DEFINE GTK3}
{. $DEFINE USEGTK3ALPHA}
{. $DEFINE GTK3DEBUGCORE}
{. $DEFINE GTK3DEBUGSIZE}
{. $DEFINE VerboseGtk3DeviceContext}
{. $DEFINE VerboseGtk3WinApi}
{. $DEFINE GTK3DEBUGREPARENTING}
{. $DEFINE GTK3DEBUGMOUSE}
{. $DEFINE GTK3DEBUGFOCUS}
{. $DEFINE GTK3DEBUGCELLRENDERER}
{. $DEFINE GTK3DEBUGNOTIMPLEMENTED}
{. $DEFINE GTK3DEBUGKEYPRESS}
{. $DEFINE GTK3DEBUGMENUS}
{. $DEFINE GTK3DEBUGPREFERREDSIZE}
{$IFNDEF USEGTK3ALPHA}
(*
{$error '             . . .                '}
{$error '              \|/                 '}
{$error '            `--+--'               '}
{$error '              /|\                 '}
{$error '             ' | '                '}
{$error '               |                  '}
{$error '               |                  '}
{$error '           ,--'#`--.              '}
{$error '           |#######|              '}
{$error '        _.-'#######`-._           '}
{$error '     ,-'###############`-.        '}
{$error '   ,'#####################`,      '}
{$error '  /#########################\     '}
{$error ' |###########################|    '}
{$error '|#############################|   '}
{$error '|#############################|   '}
{$error '|#############################|   '}
{$error '|#############################|   '}
{$error ' |###########################|    '}
{$error '  \#########################/     '}
{$error '   `.#####################,'      '}
{$error '     `._###############_,'        '}
{$error '        `--..#####..--'           '}
*)
{$error 'THIS IS CURRENTLY ALPHA CODE AND IT IS USED ONLY BY GTK3LCL DEVELOPERS.'}
{$error 'IF YOU WANT TO TEST gtk3 THEN UNCOMMENT DEFINE USEGTK3ALPHA ABOVE.'}
{$error 'PLEASE DO NOT FILL BUG REPORTS ON MANTIS WHILE GTK3LCL IS IN ALPHA STAGE'}
{$ENDIF}


