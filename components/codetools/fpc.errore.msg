# This files overrides all localized compiler messages that are parsed by the
# codetools

parser_m_macro_defined=03101_M_Macro defined: $1
% When \var{-vm} is used, the compiler tells you when it defines macros.
parser_m_macro_undefined=03102_M_Macro undefined: $1
% When \var{-vm} is used, the compiler tells you when it undefines macros.
parser_m_macro_set_to=03103_M_Macro $1 set to $2
% When \var{-vm} is used, the compiler tells you what values macros get.

general_d_defining_symbol=11037_D_Defining symbol: $1
general_d_undefining_symbol=11038_D_Undefining symbol: $1

general_t_exepath=01003_T_Using executable path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's binaries.
general_t_unitpath=01004_T_Using unit path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=01005_T_Using include path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for it's include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=01006_T_Using library path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=01007_T_Using object path: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.


# end.

