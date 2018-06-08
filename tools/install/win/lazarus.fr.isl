#include "compiler:Languages\French.isl"

[CustomMessages]


DelUserConf=Supprimer tous les fichiers de configuration de l'utilisateur provenant d'installations précédentes
CleanUp=Nettoyer: 

InstallQt=Installer la DLL de l'interface QT globalement
InstallChm=Installer les fichiers d'aide CHM
AssociateGroup=Associer les extensions de fichiers

CheckSecondClick=Créer une nouvelle installation secondaire
CheckSecondInfo=Une installation secondaire permet d'installer plusieurs versions de Lazarus. Chaque version aura sa propre configuration. Veuillez lire la FAQ sur les installations multiples avant d'utiliser cette option.

FolderHasSpaces=Le nom du dossier sélectionné contient des espaces : veuillez sélectionner un dossier sans espaces dans son chemin ou son nom.
FolderNotEmpty=Le dossier de destination n'est pas vide. Voulez-vous continuer l'installation ?
FolderNotEmpty2=Le dossier de destination n'est pas vide.

FolderForSecondNoFile=Le dossier de destination n'est pas vide et ne contient pas d'installation secondaire de Lazarus pouvant être mise à jour.%0:sVeuillez choisir un dossier vide ou un dossier avec une installation secondaire existante de Lazarus à mettre à jour.
FolderForSecondBadFile=Le dossier de destination n'est pas vide. Le programme d'installation ne pourrait pas détecter s'il contient une installation secondaire de Lazarus pouvant être mise à jour.%0:sVeuillez choisir un dossier vide ou un dossier avec une installation secondaire existante de Lazarus à mettre à jour.
FolderForSecondUpgrading=Le dossier de destination n'est pas vide.%0:sIl contient une installation secondaire de Lazarus utilisant le dossier suivant pour la configuration : %0:s%1:s%0:s%0:sVoulez-vous continuer l'installation ?
FolderForSecondUpgradingPrimary=Le dossier de destination n'est pas vide.%0:sIl contient une installation (non secondaire) par défaut de Lazarus.%0:sSi vous continuez, il sera modifié en une installation secondaire.%0:s%0:s%0:sVoulez-vous continuer l'installation ?

FolderForSecondBadUninstall=Le dossier de destination n'est pas vide. Le programme d'installation ne pourrait pas vérifier s'il est sûr de l'utiliser.%0:sVeuillez choisir un dossier vide ou un dossier avec une installation secondaire existante de Lazarus à mettre à jour.

SecondConfCapt=Sélectionnez le dossier de configuration
SecondConfCapt2=Où voulez-vous que cette installation de Lazarus enregistre sa configuration ?
SecondConfBody=Sélectionnez un nouveau dossier vide pour que cette installation de Lazarus enregistre sa configuration, puis continuez avec 'Suivant'.

FolderForConfig=Dossier de configuration

FolderForConfNotEmpty=Le dossier sélectionné n'est pas vide.

AskUninstallTitle1=Installation précédente
AskUninstallTitle2=Voulez-vous exécuter le programme de désinstallation ?
BtnUninstall=Désinstaller
ChkContinue=Continuer sans désinstaller

OldInDestFolder1=Une autre installation de %1:s existe dans le dossier de destination. Si vous souhaitez la désinstaller préalablement, veuillez utiliser le bouton ci-dessous.
OldInDestFolder2=
OldInDestFolder3=
OldInDestFolder4=

OldInOtherFolder1=Une autre installation de %1:s a été trouvée en %2:s. Veuillez utiliser le bouton ci-dessous pour la désinstaller maintenant. Si vous souhaitez la conserver, veuillez cocher la case pour continuer.
OldInOtherFolder2=Avertissement: Il se peut qu'il y ait des conflits entre les différentes installations et qu'elles ne fonctionnent pas correctement.
OldInOtherFolder3=Note: Vous n'avez pas choisi de dossier de configuration dédié à cette installation.
OldInOtherFolder4=Si vous souhaitez utiliser plus d'une installation, veuillez revenir en arrière pour cocher : "Créer une nouvelle installation secondaire".

OldInBadFolder1=Avertissement: Une autre installation de %1:s a été trouvée en %2:s, mais le programme de désinstallation a été trouvé en %3:s. Veuillez vérifier que le programme de désinstallation est correct.
OldInBadFolder2=Avertissement: Il se peut qu'il y ait des conflits entre les différentes installations et qu'elles ne fonctionnent pas correctement.
OldInBadFolder3=Note: Si vous souhaitez utiliser plus d'une installation, veuillez revenir en arrière et cocher : "Créer une nouvelle installation secondaire".
OldInBadFolder4=Veuillez utiliser le bouton ci-dessous pour la désinstaller maintenant. Si vous souhaitez la conserver, veuillez cocher la case pour continuer.

OldSecondInDestFolder1=Une autre installation de %1:s existe dans le dossier de destination. Si vous souhaitez la désinstaller préalablement, veuillez utiliser le bouton ci-dessous.
OldSecondInDestFolder2=
OldSecondInDestFolder3=Il s'agit d'une installation secondaire et le dossier pour la configuration sera conservé : 
OldSecondInDestFolder4=%4:s

OldSecondInOtherFolder1=
OldSecondInOtherFolder2=
OldSecondInOtherFolder3=
OldSecondInOtherFolder4=

OldSecondInBadFolder1=
OldSecondInBadFolder2=
OldSecondInBadFolder3=
OldSecondInBadFolder4=

SecondTaskUpdate=Mise à jour de l'installation secondaire avec configuration dans le dossier : %0:s%1:s%2:s
SecondTaskCreate=Création de l'installation secondaire avec configuration dans le dossier : %0:s%1:s%2:s

DuringInstall=Quelques informations issues de notre FAQ : http://wiki.lazarus.freepascal.org/Lazarus_Faq/fr%0:s%0:s    Qu'est-ce que Lazarus ?%0:sLazarus est un EDI multiplateformes pour Pascal. Son slogan est "écrire une fois, compiler partout".%0:s%0:s    Comment réduire la taille des fichiers exécutables ?%0:sLes fichiers binaires produits par défaut sont volumineux parce qu'ils incluent des informations pour le débogage. Pour les versions de production, vous pouvez désactiver la production de ces informations au niveau des options du projet.%0:s%0:s    Licence : %0:s- La LCL est distribuée sous licence LGPL avec une exception pour l'édition de liens. Cette licence vous autorise à créer des applications avec la licence que vous voulez, y compris une licence commerciale. L'EDI est sous licence GPL. Si vous distribuez un EDI modifié, vous devez suivre la GPL.%0:s- Les autres paquets et composants ont des licences variées. Consultez le fichier "readme" de chacun d'eux.

UninstVerbose=Vous êtes sur le point de désinstaller %1:s depuis le dossier %0:s. Voulez-vous continuer ?
