#include "compiler:Languages\French.isl"

[CustomMessages]


DelUserConf=Supprimer tous les fichiers de configuration de l'utilisateur provenant d'installations précédentes
CleanUp=Nettoyer: 

InstallQt=Installer la DLL de l'interface QT
InstallChm=Installer les fichiers d'aide CHM
AssociateGroup=Associer les extensions de fichiers

CheckSecondClick=Créer une nouvelle installation secondaire
CheckSecondInfo=Une installation secondaire permet d'avoir deux ou plus versions de Lazarus d'installées. Chaque version aura sa propre configuration. Lisez la FAQ sur les installations multiples avant d'utiliser cette option, s'il vous plaît.

FolderHasSpaces=Le nom du dossier sélectionné contient des espaces, sélectionnez un dossier sans espaces dans son chemin ou son nom, s'il vous plaît.
FolderNotEmpty=Le dossier de destination n'est pas vide. Continuer l'installation ?
FolderNotEmpty2=Le dossier de destination n'est pas vide.

FolderForSecondNoFile=Le dossier de destination n'est pas vide et ne contient pas une installation secondaire de Lazarus pouvant être mise à jour.%0:sChoisissez un dossier vide, ou un dossier avec une installation secondaire existante de Lazarus à mettre à jour, s'il vous plaît.
FolderForSecondBadFile=Le dossier de destination n'est pas vide. Le programme d'installation ne pourrait pas détecter s'il contient une installation secondaire de Lazarus pouvant être mise à jour.%0:sChoisissez un dossier vide, ou un dossier avec une installation secondaire existante de Lazarus à mettre à jour, s'il vous plaît.
FolderForSecondUpgrading=Le dossier de destination n'est pas vide.%0:sIl contient une installation secondaire de Lazarus utilisant le dossier suivant pour la configuration: %0:s%1:s%0:s%0:sContinuer l'installation ?
FolderForSecondUpgradingPrimary=Le dossier de destination n'est pas vide.%0:sIl contient une installation (non secondaire) par défaut de Lazarus.%0:sSi vous continuez, il sera modifié en une installation secondaire.%0:s%0:s%0:sContinuer l'installation ?

FolderForSecondBadUninstall=Le dossier de destination n'est pas vide. Le programme d'installation ne pourrait pas vérifier s'il est sûr de l'utiliser.%0:sChoisissez un dossier vide, ou un dossier avec une installation secondaire existante de Lazarus à mettre à jour, s'il vous plaît.

SecondConfCapt=Sélectionner le dossier de configuration
SecondConfCapt2=Ou voulez-vous que cette installation de Lazarus enregistre sa configuration ?
SecondConfBody=Sélectionnez un nouveau dossier vide pour que cette installation de Lazarus enregistre sa configuration, puis continuez avec 'Suivant'.

FolderForConfig=Dossier pour la config

FolderForConfNotEmpty=Le dossier sélectionné n'est pas vide.

AskUninstallTitle1=Installation Précédente
AskUninstallTitle2=Voulez-vous exécuter le programme de désinstallation ?
BtnUninstall=Désinstaller
ChkContinue=Continuer sans désinstaller

OldInDestFolder1=Une autre installation de %1:s existe dans le dossier de destination. Si vous souhaitez la désinstaller préalablement, utilisez le bouton ci-dessous, s'il vous plaît.
OldInDestFolder2=
OldInDestFolder3=
OldInDestFolder4=

OldInOtherFolder1=Une autre installation de %1:s a été trouvée en %2:s. Utilisez le bouton ci-dessous pour la désinstaller maintenant, s'il vous plaît. Si vous souhaitez la conserver, cochez la case pour continuer, s'il vous plaît.
OldInOtherFolder2=Avertissement: Il se peut qu'il y ait des conflits entre les différentes installations et qu'elles ne fonctionnent pas correctement.
OldInOtherFolder3=Note: Vous n'avez pas choisi un dossier de configuration dédiée à cette installation.
OldInOtherFolder4=Si vous souhaitez utiliser plus d'une installation, revenez en arrière et cochez, s'il vous plaît: "Créer une nouvelle installation secondaire".

OldInBadFolder1=Avertissement: Une autre installation de %1:s a été trouvée en %2:s, mais le programme de désinstallation a été trouvé en %3:s. Vérifiez que le programme de désinstallation est correct, s'il vous plaît.
OldInBadFolder2=Avertissement: Il se peut qu'il y ait des conflits entre les différentes installations et qu'elles ne fonctionnent pas correctement.
OldInBadFolder3=Note: Si vous souhaitez utiliser plus d'une installation, revenez en arrière et cochez, s'il vous plaît: "Créer une nouvelle installation secondaire".
OldInBadFolder4=Utilisez le bouton ci-dessous pour la désinstaller maintenant, s'il vous plaît. Si vous souhaitez la conserver, cochez la case pour continuer, s'il vous plaît.

OldSecondInDestFolder1=Une autre installation de %1:s existe dans le dossier de destination. Si vous souhaitez la désinstaller préalablement, utilisez le bouton ci-dessous, s'il vous plaît.
OldSecondInDestFolder2=
OldSecondInDestFolder3=C'est une installation secondaire et le dossier pour la configuration est (et sera conservé): 
OldSecondInDestFolder4=%4:s

OldSecondInOtherFolder1=
OldSecondInOtherFolder2=
OldSecondInOtherFolder3=
OldSecondInOtherFolder4=

OldSecondInBadFolder1=
OldSecondInBadFolder2=
OldSecondInBadFolder3=
OldSecondInBadFolder4=

SecondTaskUpdate=Mise à jour de l'installation secondaire avec configuration dans le dossier: %0:s%1:s%2:s
SecondTaskCreate=Création de l'installation secondaire avec configuration dans le dossier: %0:s%1:s%2:s

DuringInstall=Quelques informations issues de notre FAQ: http://wiki.lazarus.freepascal.org/Lazarus_Faq/fr%0:s%0:s    Qu'est ce que Lazarus ?%0:sLazarus est un EDI multiplateformes pour Pascal. Son slogan est écrire une fois, compiler partout.%0:s%0:s    Comment réduire la taille des fichiers exe ?%0:sLes fichiers binaires produits par défaut sont très grands parce qu'ils incluent des informations pour le débogage. Pour les versions de production, vous pouvez désactiver ceci au niveau des options du projet.%0:s%0:s    Licence: %0:s- La LCL est distribuée sous licence LGPL avec une exception pour l'édition de liens. Ceci vous autorise à créer des applications avec la licence que vous voulez, y compris une licence commerciale. L'EDI est sous licence GPL. Si vous distribuez un EDI modifié, vous devez suivre la GPL.%0:s- Les autres paquets et composants ont des licences variées. Consultez le fichier readme de chacun d'entre eux.

UninstVerbose=Sur le point de désinstaller %1:s depuis le dossier %0:s. Continuer ?
