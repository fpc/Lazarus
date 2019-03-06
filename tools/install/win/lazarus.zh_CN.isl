#include "compiler:Languages\Chinese.isl"
; Last-Translator: 郑建平@夏宗萍 aka  robsean <robsean@126.com>\n"
[CustomMessages]


DelUserConf=删除来自先前安装的所有用户配置文件
CleanUp=清理:

InstallQt=全局安装Qt接口DLL
InstallChm=安装CHM帮助文件
AssociateGroup=关联文件拓展名

CheckSecondClick=创建一个新的第二的安装
CheckSecondInfo=一个第二的安装允许Lazarus的两个或更多版本来安装.每个版本将有它自己的配置.请在使用这个选项前阅读关于多个安装的FAQ.

FolderHasSpaces=选择的文件夹包含空格,请选择一个不带有空格的文件夹.
FolderNotEmpty=目标文件夹不是空的.继续安装?
FolderNotEmpty2=目标文件夹不是空的.

FolderForSecondNoFile=目标文件夹不是空的,并不包含一个可升级的第二的Lazarus安装.%0:s请选择一个空文件夹,或者用于升级的带有一个存在的第二的Lazarus安装的一个文件夹.
FolderForSecondBadFile=目标文件夹不是空的.安装器不能查出它是否包含一个可升级的第二的Lazarus安装.%0:s请选择一个空文件夹, 或者用于升级的带有一个存在的第二的Lazarus安装的一个文件夹.
FolderForSecondUpgrading=目标文件夹不是空的.%0:s它包含一个第二的Lazarus安装，使用下面的文件夹用于配置:%0:s%1:s%0:s%0:s继续安装?
FolderForSecondUpgradingPrimary=目标文件夹不是空的.%0:s它包含一个默认的(非第二的)Lazarus安装.%0:s如果你继续，它将更改到一个第二的安装.%0:s%0:s%0:s继续安装?

FolderForSecondBadUninstall=目标文件夹不是空的.安装器不能核实它是否是安全的来使用.%0:s请选择一个空文件夹,或一个带有存在的第二的Lazarus安装的，用于更新的一个文件夹.

SecondConfCapt=选择配置文件夹
SecondConfCapt2=你希望这个Lazarus安装在哪里，存储它的配置?
SecondConfBody=选择一个新的用于这个Lazarus安装的空文件夹来存储它的配置，然后使用'Next'继续.

FolderForConfig=配置用文件夹

FolderForConfNotEmpty=选择的文件夹不是空的.

AskUninstallTitle1=先前的安装
AskUninstallTitle2=你想要运行卸载器?
BtnUninstall=卸载
ChkContinue=继续而不卸载

OldInDestFolder1=另一个%1:s的安装存在在目标文件夹.如果你想先卸载,请使用下面的按钮.
OldInDestFolder2=
OldInDestFolder3=
OldInDestFolder4=

OldInOtherFolder1=另一个%1:s的安装被找到，在%2:s.现在请使用下面的按钮来卸载它.如果你想保持它，请打勾复选框来继续.
OldInOtherFolder2=警告:这里可能在不同的安装之间冲突，并且它们可能没有令人满意的功能.
OldInOtherFolder3=注意:你没有为这个安装选择一个专用的配置文件夹.
OldInOtherFolder4=如果你想使用多个安装，请返回和打勾: "创建一个新的第二的安装".

OldInBadFolder1=警告:另一个%1:s的安装被找到，在%2:s,但是卸载器被找到，在%3:s. 请确保卸载器是正确的.
OldInBadFolder2=警告:这里可能在不同的安装之间冲突，并且它们可能没有令人满意的功能.
OldInBadFolder3=注意:如果你想使用多个安装，请返回和打勾: "创建一个新的第二的安装".
OldInBadFolder4=现在请使用下面的按钮来卸载它.如果你想保持它,请打勾复选框来继续.

OldSecondInDestFolder1=另一个%1:s安装存在在目标文件夹.如果你想先卸载,请使用下面的按钮.
OldSecondInDestFolder2=
OldSecondInDestFolder3=这是一个第二的安装，用于配置的文件夹是(并将被保持):
OldSecondInDestFolder4=%4:s

OldSecondInOtherFolder1=
OldSecondInOtherFolder2=
OldSecondInOtherFolder3=
OldSecondInOtherFolder4=

OldSecondInBadFolder1=
OldSecondInBadFolder2=
OldSecondInBadFolder3=
OldSecondInBadFolder4=

SecondTaskUpdate=更新用于第二的安装的配置，在文件夹中:%0:s%1:s%2:s
SecondTaskCreate=创建用于第二的安装的配置，在文件夹中:%0:s%1:s%2:s

DuringInstall=Lazarus中文翻译（QQ：192903718），我们常见问题解答的一些信息: http://wiki.lazarus.freepascal.org/Lazarus_Faq%0:s%0:s    Lazarus是什么?%0:sLazarus是Pascal的一个跨平台IDE.它的目标是一次编写,到处编译.%0:s%0:s    如何减少exe文件的大小?%0:s默认二级制文件是非常大的,因为它们包括调试信息.对于发布版本,你可以在工程设置中切换取消这个.%0:s%0:s    许可:%0:s- LCL被许可为链接除外的LGPL.这允许你来创建带有任何你所想的许可的应用程序,包括专有的.%0:s- IDE被许可为GPL.如果你发布一个修改的IDE,你必需遵循GPL.%0:s- 其它的软件包和组件有各种各样的许可.看每个软件包的readme文件.

UninstVerbose=即将从文件夹%0:s卸载%1:s.继续?
