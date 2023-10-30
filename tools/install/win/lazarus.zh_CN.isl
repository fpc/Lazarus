#include "ChineseSimplified_from_inno_6_0.isl"
; Last-Translator: ACTom <i@actom.me>\n"
[CustomMessages]


DelUserConf=删除以前安装的所有用户配置文件
CleanUp=清理：

InstallQtLocal=安装 Qt4 界面 DLL
InstallQt=全局安装 Qt4 界面 DLL
InstallQt5Local=安装 Qt5 界面 DLL
InstallQt5Global=全局安装 Qt5 界面 DLL
InstallChm=安装 CHM 帮助文件
InstallOpenSSL=安装 openssl 库（在线软件包管理器需使用）
InstallOpenSSLGlobal=全局安装 openssl 库
AssociateGroup=关联文件扩展名

CheckSecondClick=创建新的辅助安装
CheckSecondInfo=辅助安装允许安装两个或多个版本的 Lazarus。每个版本都有自己的配置。使用此选项前，请阅读有关多重安装的常见问题。

FolderHasSpaces=所选文件夹包含空格，请选择不含空格的文件夹。
FolderNotEmpty=目标文件夹不是空的，是否继续安装？
FolderNotEmpty2=目标文件夹不是空的。

FolderForSecondNoFile=目标文件夹不是空的，也不包含可升级的辅助 Lazarus 安装。%0:s请选择空文件夹，或包含现有辅助 Lazarus 安装的文件夹进行更新。
FolderForSecondBadFile=目标文件夹不是空的。安装程序无法检测其中是否包含可升级的辅助 Lazarus 安装。%0:s请选择一个空文件夹，或一个包含现有辅助 Lazarus 安装的文件夹进行更新。
FolderForSecondUpgrading=目标文件夹不是空的。%0:s 它包含一个使用以下文件夹进行配置的辅助 Lazarus 安装：%0:s%1:s%0:s%0:s%0:s是否继续安装？
FolderForSecondUpgradingPrimary=目标文件夹不是空的。%0:s它包含一个默认的（非辅助）Lazarus 安装。%0:s如果您继续，它将变为辅助安装。%0:s%0:s%0:s是否继续安装？

FolderForSecondBadUninstall=目标文件夹不是空的。安装程序无法验证使用该文件夹是否安全。%0:s请选择一个空文件夹，或一个已有辅助 Lazarus 安装的文件夹进行更新。

SecondConfCapt=选择配置文件夹
SecondConfCapt2=您希望在哪里存储配置？
SecondConfBody=选择一个新的空文件夹来存储配置，然后点击"下一步"继续。

FolderForConfig=配置文件夹

FolderForConfNotEmpty=选择的文件夹不是空的。

AskUninstallTitle1=以前的安装
AskUninstallTitle2=您想要运行卸载程序吗？
BtnUninstall=卸载
ChkContinue=继续而不卸载

OldInDestFolder1=目标文件夹中存在 %1:s 的另一个安装。如果您想先卸载，请使用下面的按钮。
OldInDestFolder2=
OldInDestFolder3=
OldInDestFolder4=

OldInOtherFolder1=在 %2:s 发现另一个 %1:s 的安装，请使用下面的按钮立即卸载它。如果希望保留，请勾选复选框继续。
OldInOtherFolder2=警告：不同安装之间可能会有冲突，也可能无法正常运行。
OldInOtherFolder3=注意：您没有为本次安装选择专用的配置文件夹。
OldInOtherFolder4=如果您希望进行多次安装，请返回并勾选 "创建新的辅助安装"。

OldInBadFolder1=警告：在 %2:s 发现了 %1:s 的另一个安装，但在 %3:s 发现了卸载程序，请确保卸载程序正确无误。
OldInBadFolder2=警告：不同安装之间可能会有冲突，也可能无法正常运行。
OldInBadFolder3=注意：如果您希望进行多次安装，请返回并勾选 "创建新的辅助安装"。
OldInBadFolder4=请使用下面的按钮立即卸载。如果希望保留，请勾选复选框继续。

OldSecondInDestFolder1=目标文件夹中存在 %1:s 的另一个安装。如果您想先卸载，请使用下面的按钮。
OldSecondInDestFolder2=
OldSecondInDestFolder3=这是一个辅助安装，配置文件夹是（将来也会保留）：
OldSecondInDestFolder4=%4:s

OldSecondInOtherFolder1=
OldSecondInOtherFolder2=
OldSecondInOtherFolder3=
OldSecondInOtherFolder4=

OldSecondInBadFolder1=
OldSecondInBadFolder2=
OldSecondInBadFolder3=
OldSecondInBadFolder4=

SecondTaskUpdate=更新辅助安装，其配置文件位于文件夹：%0:s%1:s%2:s
SecondTaskCreate=创建辅助安装，其配置文件位于文件夹：%0:s%1:s%2:s

DuringInstall=Lazarus 中文交流群（QQ 群：192903718）	常见问题: http://wiki.lazarus.freepascal.org/Lazarus_Faq%0:s%0:s    Lazarus 是什么？%0:sLazarus 是一个跨平台的 Pascal IDE。它的目标是一次编写，到处编译。%0:s%0:s    如何减小 exe 文件的大小？%0:s默认的二进制文件非常大，因为它们包含了调试信息。对于发布版本，您可以在项目设置中将其关闭。%0:s%0:s    协议：%0:s- LCL 采用除链接外的 LGPL 协议，这允许您使用任何协议创建应用程序，包括专有协议。%0:s- IDE 采用 GPL 协议，如果您发布修改过的 IDE，则必须遵循 GPL 协议。%0:s- 其他的软件包和组件有各自的协议，请参阅每个软件包的自述文件。

UninstVerbose=即将从 %0:s 文件夹卸载 %1:s，是否继续？
