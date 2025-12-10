   PROGRAM


jFiles:TemplateVersion equate('3.14')
xFiles:TemplateVersion equate('4.36')
StringTheory:TemplateVersion equate('3.81')
Reflection:TemplateVersion equate('1.31')
MyTable:TemplateVersion equate('1.27')
GPFRep:Version equate ('2.41')           !Deprecated - but exists for backward compatibility
GPFReporter:TemplateVersion equate ('2.41')
HyperActive:TemplateVersion equate('2.35')
CapesoftMessageBox:TemplateVersion equate('2.51')
ResizeAndSplit:TemplateVersion equate('5.12')
Cryptonite:TemplateVersion   equate('2.05')
Draw:TemplateVersion equate('4.38')
WinEvent:TemplateVersion      equate('5.45')

   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABFILE.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('STAMRSZ.INC'),ONCE
   INCLUDE('EZJSONEX.INC'),ONCE
   INCLUDE('MSXML6.INC'),ONCE
   include('jFiles.inc'),ONCE
   include('xfiles.inc'),ONCE
  include('dwrDctParser.inc'),ONCE
  include('StringTheory.Inc'),ONCE
  INCLUDE('UltimateNotify.inc'),ONCE
  INCLUDE('UltimateNotifyManager.inc'),ONCE
  INCLUDE('UltimateString.inc'),ONCE
   INCLUDE('dwrDctParser.inc'),ONCE  
   INCLUDE('debuger.inc'),ONCE  
   INCLUDE('mhList.inc'),ONCE  
   INCLUDE('JS_ListMark.inc'),ONCE  
   INCLUDE('dwrBase.inc'),ONCE  
   INCLUDE('dwrMyQueue.inc'),ONCE  
   INCLUDE('dwrSQL.inc'),ONCE  
   INCLUDE('NYSDockingPane.INC'),ONCE
   INCLUDE('NYSEquates.EQU'),ONCE
   INCLUDE('NYSCommon.INC'),ONCE
   INCLUDE('NYSTemplateHelper.INC'),ONCE
  include('Reflection.Inc'),ONCE
  include('MyTable.Inc'),ONCE
  Include('csGPF.Inc'),ONCE
   include('Hyper.Inc'),ONCE
  INCLUDE('NinjaDonutHoleBaseClass.inc'),ONCE
  INCLUDE('NinjaBaseWindowComponentClass.inc'),ONCE
  INCLUDE('NinjaDonutHoleRegistrationClass.inc'),ONCE
  INCLUDE('NinjaDonutHoleHostClass.inc'),ONCE
  INCLUDE('NinjaDonutHoleWindowClass.inc'),ONCE
  INCLUDE('NinjaDonutHoleMultiHostClass.inc'),ONCE
   INCLUDE('NYSSyntaxEdit.INC'),ONCE
   INCLUDE('NYSSuiteControls.INC'),ONCE
  include('ResizeAndSplit.Inc'),ONCE
  include('Cryptonite.inc'),Once
  include('CsBlowfish.Inc'),Once
   INCLUDE('StAbTag.TRN', 'Strings'),ONCE
Include('HeaderSort.inc'), Once
  include('draw.inc'),once
  include('drawheader.inc'),once
  include('drawqr.inc'),once
  include('drawGauge.inc'),once
   INCLUDE('NYSSkinFramework.INC'),ONCE
   INCLUDE('NYSPropertyGrid.INC'),ONCE
not:closeWindow               EQUATE(3292)
not:RecordUpdated             EQUATE(3293)
INCLUDE('BackgroundManagerClass.inc'),ONCE
INCLUDE('CFCQLocator.inc')
    Include('WinEvent.Inc'),Once

   MAP
     MODULE('DICTIONARYEDITOR_BC.CLW')
DctInit     PROCEDURE                                      ! Initializes the dictionary definition module
DctKill     PROCEDURE                                      ! Kills the dictionary definition module
     END
!--- Application Global and Exported Procedure Definitions --------------------------------------------
     MODULE('DICTIONARYEDITOR002.CLW')
Main3                  PROCEDURE   !
     END
     MODULE('DICTIONARYEDITOR019.CLW')
ds_Stop                PROCEDURE(<string StopText>)   !
     END
     MODULE('DICTIONARYEDITOR020.CLW')
ds_Halt                PROCEDURE(UNSIGNED Level=0,<STRING HaltText>)   !
     END
     MODULE('DICTIONARYEDITOR021.CLW')
ds_Message             FUNCTION(STRING MessageTxt,<STRING HeadingTxt>,<STRING IconSent>,<STRING ButtonsPar>,UNSIGNED Defaults=0,BOOL StylePar=FALSE),UNSIGNED,PROC   !
     END
     MODULE('DICTIONARYEDITOR032.CLW')
FormatMessage          FUNCTION(long pError),String   !
     END
     MODULE('DICTIONARYEDITOR033.CLW')
ParseFontFromLong      PROCEDURE(string pFontHandle,LOGFONT pFontGrp)   !
     END
     INCLUDE('OCX.CLW'),ONCE
     MODULE('DICT$TAG.CLW')
       INCLUDE('STABTAG2.CLW','ProgramMap:User')
       INCLUDE('STABTAG2.CLW','ProgramMap:PtrM')
       INCLUDE('STABTAG2.CLW','ProgramMap:PosM')
       INCLUDE('STABTAG2.CLW','ProgramMap:PtrF')
       INCLUDE('STABTAG2.CLW','ProgramMap:PosF')
       INCLUDE('STABTAG2.CLW','ProgramMap:TagSet')
     END
       INCLUDE('cwutil.inc'),ONCE
       Module('Win32')
         Dct_GetDC               (LONG hWnd),LONG,RAW,PASCAL,Name('GetDC')
         Dct_ReleaseDC           (LONG hWnd, LONG hDC),LONG,RAW,PASCAL,Name('ReleaseDC')
     
         Dct_CreateFontA         (LONG nHeight,LONG nWidth,LONG nEscapement,LONG nOrientation, |
                                 LONG fnWeight, BYTE fdwItalic, BYTE fdwUnderline, BYTE fdwStrikeOut, |
                                 BYTE fdwCharSet, BYTE fdwOutputPrecision, BYTE fdwClipPrecision,      |
                                 BYTE fdwQuality, BYTE fdwPitchAndFamily, *CSTRING lpszFace),LONG,RAW,PASCAL,Name('CreateFontA')
     
         Dct_SelectObject        (LONG hdc, LONG hObject),LONG,RAW,PASCAL,Name('SelectObject')
         Dct_DeleteObject        (LONG hObject),BOOL,RAW,PASCAL,Name('DeleteObject')
         !Dct_GetGlyphOutlineA    (LONG hdc, ULONG ch, ULONG format, *GlyphMetrics lpMetrics, LONG cbBuffer, *BYTE lpBuffer, *LOGFONT lpMat2),LONG,PASCAL,RAW,Name('GetGlyphOutlineA')
         Dct_GetGlyphOutlineA    (LONG hdc, UNSIGNED uChar, UNSIGNED fuFormat, *GLYPHMETRICS lpgm,LONG cjBuffer, LONG pvBuffer, *MAT2 lpmat2),LONG,RAW,PASCAL,Name('GetGlyphOutlineA')
         !Dct_GetGlyphOutlineA    (LONG hdc, UNSIGNED uChar, UNSIGNED fuFormat, LONG lpgm,LONG cjBuffer, LONG pvBuffer, LONG lpmat2),LONG,RAW,PASCAL,Name('GetGlyphOutlineA')
         Dct_FormatMessage       (ulong dwFlags, ulong lpSource, ulong dwMessageId, ulong dwLanguageId, *cstring lpBuffer,ulong nSize, ulong Arguments), ulong, raw, pascal, name('FormatMessageA')
         Dct_GetLastError        (),long,PASCAL,name('GetLastError')
     
         Dct_CreateFileA         (*CSTRING lpFileName, LONG dwDesiredAccess, LONG dwShareMode, LONG lpSec,LONG dwCreationDisposition, LONG dwFlagsAndAttributes, LONG hTemplateFile),LONG,PASCAL,RAW,Name('CreateFileA')
         Dct_WriteFile           (LONG hFile, LONG lpBuffer, LONG nBytesToWrite, *LONG lpBytesWritten, LONG lpOverlapped),BOOL,PASCAL,RAW,Name('WriteFile')
         Dct_CloseHandle         (LONG hObject),BOOL,PASCAL,RAW,Name('CloseHandle')
         Dct_GetObjectA          (LONG hGDIObj, LONG cbBuffer, LONG pBuffer),LONG,PASCAL,RAW,Name('GetObjectA')
       End
       MyOKToEndSessionHandler(long pLogoff),long,pascal
       MyEndSessionHandler(long pLogoff),pascal
   END

  include('StringTheory.Inc'),ONCE
  include('MyTable.Inc'),ONCE
Glo:st               StringTheory,THREAD
Glo:SaveBtnFeq       LONG
SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
TagSet_              FILE,DRIVER('TOPSPEED'),RECLAIM,PRE(TS_),BINDABLE,CREATE,THREAD !                     
TblKey                   KEY(TS_:Tbl),NOCASE,PRIMARY       !                     
UsrTblKey                KEY(TS_:Usr,TS_:Tbl),DUP,NOCASE,OPT !                     
UsrSourceDateKey         KEY(TS_:Usr,TS_:Source,TS_:Date),DUP,NOCASE,OPT !                     
Record                   RECORD,PRE()
Tbl                         SHORT                          !                     
Usr                         LONG                           !                     
Source                      STRING(50)                     !                     
Date                        LONG                           !                     
Count                       LONG                           !                     
Description                 STRING(250)                    !                     
                         END
                     END                       

TagFile_             FILE,DRIVER('TOPSPEED'),RECLAIM,PRE(TF_),BINDABLE,CREATE,THREAD !                     
PtrKey                   KEY(TF_:Usr,TF_:Tbl,TF_:Ptr),NOCASE,OPT,PRIMARY !                     
ValKey                   KEY(TF_:Usr,TF_:Tbl,TF_:Val),DUP,NOCASE,OPT !                     
Record                   RECORD,PRE()
Usr                         LONG                           !                     
Tbl                         SHORT                          !                     
Ptr                         LONG                           !                     
Val                         STRING(10)                     !                     
                         END
                     END                       

TagFilePos_          FILE,DRIVER('TOPSPEED'),RECLAIM,PRE(TP_),BINDABLE,CREATE,THREAD !                     
PosKey                   KEY(TP_:Usr,TP_:Tbl,TP_:Pos),NOCASE,OPT,PRIMARY !                     
ValKey                   KEY(TP_:Usr,TP_:Tbl,TP_:Val),DUP,NOCASE,OPT !                     
Record                   RECORD,PRE()
Usr                         LONG                           !                     
Tbl                         SHORT                          !                     
Pos                         STRING(256)                    !                     
Val                         STRING(10)                     !                     
                         END
                     END                       

Dictionaries         FILE,DRIVER('TOPSPEED'),NAME('Dictionaries.tps'),PRE(Dct),CREATE,BINDABLE,THREAD !                     
PKDctGUIDKey             KEY(Dct:GUID),NOCASE,PRIMARY      !                     
FKDctNameKey             KEY(Dct:DctName),DUP,NOCASE       !                     
CommentsBlob                BLOB,NAME('CommentsBlob | Private') !                     
OptionsBlob                 BLOB,NAME('OptionsBlob | Private') !                     
Record                   RECORD,PRE()
GUID                        STRING(16),NAME('PKGuid | Private') !                     
DctName                     STRING(100),NAME('Name | Attribute') !                     
Version                     STRING(5),NAME('Version | Attribute') !                     
StructureChecked            STRING(5),NAME('StructureChecked | Attribute') !                     
DctxFormat                  STRING(5),NAME('DctxFormat | Attribute') !                     
                         END
                     END                       

Tables               FILE,DRIVER('TOPSPEED'),NAME('Tables.tps'),PRE(Tab),CREATE,BINDABLE,THREAD !                     
PKTabGuidKey             KEY(Tab:PKGuid),NOCASE,PRIMARY    !                     
TabTableTypeGuidKey      KEY(Tab:TableTypeGuid),DUP,NOCASE !                     
FKTabParentGuidKey       KEY(Tab:ParentGUID),DUP,NOCASE    !                     
FKTabDictionaryGuidKey   KEY(Tab:DictionaryGuid),DUP,NOCASE !                     
FKTabTableGuidKey        KEY(Tab:Guid),DUP,NOCASE          !                     
FKTabGuidKey             KEY(Tab:Guid),DUP,NOCASE          !                     
FKTabDctxOrderKey        KEY(Tab:DctxOrder),DUP,NOCASE     !                     
FKTabTableNameKey        KEY(Tab:TableName),DUP,NOCASE     !                     
FKTabDctGuidAndOrderKey  KEY(Tab:DictionaryGuid,Tab:DctxOrder),DUP,NOCASE ! Sort table by DCT Guid and DctxOrder value
FKTabParentGuidAndOrderKey KEY(Tab:ParentGUID,Tab:DctxOrder),DUP,NOCASE !                     
AuditBlob                   BLOB,NAME('AuditBlob | Private') !                     
CommentsBlob                BLOB,NAME('CommentsBlob | Private') !                     
OptionsBlob                 BLOB,NAME('OptionsBlob | Private') !                     
SqlCreateBlob               BLOB,NAME('SqlCreateBlob | Private') !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
TableTypeGuid               STRING(16),NAME('TableTypeGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
DictionaryGuid              STRING(16),NAME('DictionaryGuid | Private') !                     
DctxOrder                   REAL,NAME('DctxOrder | Private') !                     
Guid                        STRING(38),NAME('Guid | attribute') !                     
Ident                       STRING(5),NAME('Ident | attribute') !                     
Usage                       STRING(100),NAME('Usage | Attribute') !                     
TableName                   STRING(100),NAME('Name | attribute') !                     
Description                 STRING(255),NAME('Description | attribute') !                     
TablePrefix                 STRING(20),NAME('Prefix | attribute') !                     
TableDriver                 STRING(30),NAME('Driver | attribute') !                     
DriverOption                STRING(100),NAME('DriverOption | attribute') !                     
Owner                       STRING(100),NAME('Owner | attribute') !                     
TablePath                   STRING(100),NAME('Path | attribute') !                     
Create                      STRING(5),NAME('Create | attribute') !                     
Reclaim                     STRING(5),NAME('Reclaim | Attribute') !                     
Encrypt                     STRING(5),NAME('Encrypt | Attribute') !                     
OEM                         STRING(5),NAME('OEM | Attribute') !                     
Thread                      STRING(5),NAME('Thread | attribute') !                     
Bindable                    STRING(5),NAME('Bindable | attribute') !                     
                         END
                     END                       

DctVersions          FILE,DRIVER('TOPSPEED'),NAME('DctVersions.Tps'),PRE(Ver),CREATE,BINDABLE,THREAD !                     
PKVerGuidKey             KEY(Ver:GUID),NOCASE,PRIMARY      !                     
FKVerParentGuidKey       KEY(Ver:ParentGUID),DUP,NOCASE    !                     
FKVerDictionaryGuidKey   KEY(Ver:DictionaryGuid),DUP,NOCASE !                     
Record                   RECORD,PRE()
GUID                        STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
DictionaryGuid              STRING(16),NAME('DictionaryGuid | Private') !                     
DctVersion                  STRING(5),NAME('Version | Attribute') !                     
Description                 STRING(255),NAME('Description | Attribute') !                     
                         END
                     END                       

Options              FILE,DRIVER('TOPSPEED'),NAME('Options.tps'),PRE(Opt),CREATE,BINDABLE,THREAD !                     
PKOptGuidKey             KEY(Opt:PKGuid),NOCASE,PRIMARY    !                     
FKOptParentGuidKey       KEY(Opt:ParentGUID),DUP,NOCASE    !                     
FKOptDictionaryGuidKey   KEY(Opt:DictionaryGuid),DUP,NOCASE !                     
FKOptTableGuidKey        KEY(Opt:TableGuid),DUP,NOCASE     !                     
FKOptFieldGuidKey        KEY(Opt:FieldGuid),DUP,NOCASE     !                     
FKOptKeyGuidKey          KEY(Opt:KeyGuid),DUP,NOCASE       !                     
FKOptRelationGuidKey     KEY(Opt:RelationGuid),DUP,NOCASE  !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
DictionaryGuid              STRING(16),NAME('DictionaryGuid | Private') !                     
TableGuid                   STRING(16),NAME('TableGuid | Private') !                     
FieldGuid                   STRING(16),NAME('FieldGuid | Private') !                     
KeyGuid                     STRING(16),NAME('KeyGuid | Private') !                     
RelationGuid                STRING(16),NAME('RelationGuid | Private') !                     
ParentNode                  STRING(20),NAME('ParentNode | Private') !                     
Property                    STRING(100),NAME('Property | Attribute') !                     
PropertyType                STRING(5),NAME('PropertyType | Attribute') !                     
PropertyValue               STRING(100),NAME('PropertyValue | Attribute') !                     
                         END
                     END                       

Fields               FILE,DRIVER('TOPSPEED'),NAME('Fields.tps'),PRE(Fld),CREATE,BINDABLE,THREAD !                     
PKFldGuidKey             KEY(Fld:PKGuid),NOCASE,PRIMARY    !                     
FKFldParentGuidKey       KEY(Fld:ParentGUID),DUP,NOCASE    !                     
FKFldTableGuidKey        KEY(Fld:TableGuid),DUP,NOCASE     !                     
FKFldFieldOrderKey       KEY(Fld:FieldOrder),DUP,NOCASE    !                     
FKFldFieldNameKey        KEY(Fld:FieldName),DUP,NOCASE     !                     
FKFldDctGuidAndOrderKey  KEY(Fld:ParentGUID,Fld:FieldOrder),DUP,NOCASE !                     
FKFldParentGuidAndOrderKey KEY(Fld:ParentGUID,Fld:FieldOrder),DUP,NOCASE !                     
FKFldFieldGuidKey        KEY(Fld:Guid),DUP,NOCASE          !                     
WindowControlBlob           BLOB,NAME('WindowControlGrp | Private') !                     
ReportControlBlob           BLOB,NAME('ReportControlGrp | Private') !                     
AuditBlob                   BLOB,NAME('AuditBlob | Private') !                     
ValidityBlob                BLOB,NAME('ValidityBlob | Private') !                     
CommentsBlob                BLOB,NAME('CommentsBlob | Private') !                     
OptionsBlob                 BLOB,NAME('OptionsBlob | Private') !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
TableGuid                   STRING(16),NAME('TableGuid | Private') !                     
FieldOrder                  REAL,NAME('FieldOrder | Private') !                     
LastInGroup                 BYTE,NAME('LastInGroup | Private') !                     
Guid                        STRING(38),NAME('Guid | Attribute') !                     
Ident                       STRING(5),NAME('Ident | Attribute') !                     
FieldName                   STRING(100),NAME('Name | Attribute') !                     
FieldPrefix                 STRING(20),NAME('Prefix | attribute') !                     
DerivedFrom                 STRING(50),NAME('DerivedFrom | Attribute') !                     
Description                 STRING(100),NAME('Description | Attribute') !                     
BaseType                    STRING(20),NAME('BaseType | Attribute') !                     
DataType                    STRING(20),NAME('DataType | Attribute') !                     
RowPicture                  STRING(20),NAME('RowPicture | Attribute') !                     
Reference                   STRING(5),NAME('Reference | Attribute') !                     
Thread                      STRING(5),NAME('Thread | Attribute') !                     
Binary                      STRING(5),NAME('Binary | Attribute') !                     
FieldSize                   STRING(20),NAME('Size | Attribute') !                     
Dim1                        STRING(3),NAME('Dim1 | Attribute') !                     
Dim2                        STRING(3),NAME('Dim2 | Attribute') !                     
Dim3                        STRING(3),NAME('Dim3 | Attribute') !                     
Dim4                        STRING(3),NAME('Dim4 | Attribute') !                     
Places                      STRING(3),NAME('Places | Attribute') !                     
ScreenPicture               STRING(20),NAME('ScreenPicture | Attribute') !                     
ScreenPrompt                STRING(100),NAME('ScreenPrompt | Attribute') !                     
ReportHeading               STRING(100),NAME('ReportHeading | Attribute') !                     
Freeze                      STRING(5),NAME('Freeze | attribute') !                     
InitialValue                STRING(100),NAME('InitialValue | Attribute') !                     
FormTab                     STRING(100),NAME('FormTab | Attribute') !                     
Case_                       STRING(10),NAME('Case | Attribute') !                     
TypingMode                  STRING(20),NAME('TypingMode | Attribute') !                     
Over                        STRING(100),NAME('Over | Attribute') !                     
Justification               STRING(20),NAME('Justification | Attribute') !                     
Immediate                   STRING(5),NAME('Immediate | Attribute') !                     
ReadOnly                    STRING(5),NAME('ReadOnly | Attribute') !                     
Password_                   STRING(5),NAME('Password | Attribute') !                     
Offset                      STRING(20),NAME('Offset | Attribute') !                     
ExternalName                STRING(100),NAME('ExternalName | Attribute') !                     
NoPopulate                  STRING(5),NAME('NoPopulate | Attribute') !                     
VerticalSpace               STRING(5),NAME('VerticalSpace | Attribute') !                     
HelpId                      STRING(100),NAME('HelpId | Attribute') !                     
Message                     STRING(100),NAME('Message | Attribute') !                     
Tooltip                     STRING(150),NAME('Tooltip | Attribute') !                     
WindowControl               STRING(255),NAME('WindowControl | Attribute') !                     
ReportControl               STRING(255),NAME('ReportControl | Attribute') !                     
IndentBy                    LONG,NAME('IndentBy | Private') !                     
NumEndTags                  LONG,NAME('NumEndTags | Private') !                     
CtrlType                    STRING(6),NAME('CtrlType | Private') !                     
                         END
                     END                       

WindowControls       FILE,DRIVER('TOPSPEED'),NAME('WindowControls.tps'),PRE(Win),CREATE,BINDABLE,THREAD !                     
PKWinGuidKey             KEY(Win:GUID),NOCASE,PRIMARY      !                     
FKWinParentGUIDKey       KEY(Win:ParentGUID),DUP,NOCASE    !                     
FKWinTableGuidKey        KEY(Win:TableGuid),DUP,NOCASE     !                     
FKWinFieldGuidKey        KEY(Win:FieldGuid),DUP,NOCASE     !                     
Lines                       BLOB,NAME('Lines | Private')   !                     
Record                   RECORD,PRE()
GUID                        STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
TableGuid                   STRING(16),NAME('ParentGUID | Private') !                     
FieldGuid                   STRING(16),NAME('FieldGuid | Private') !                     
                         END
                     END                       

Lines                FILE,DRIVER('TOPSPEED'),NAME('Lines.tps'),PRE(Lin),CREATE,BINDABLE,THREAD !                     
PKLinGuidKey             KEY(Lin:GUID),NOCASE,PRIMARY      !                     
FKLinParentGuidKey       KEY(Lin:ParentGUID),DUP,NOCASE    !                     
FKLinDictionaryGuidKey   KEY(Lin:DictionaryGuid),DUP,NOCASE !                     
FKLinWinControlGuidKey   KEY(Lin:WinControlGuid),DUP,NOCASE !                     
Record                   RECORD,PRE()
GUID                        STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
DictionaryGuid              STRING(16),NAME('DictionaryGuid | Private') !                     
WinControlGuid              STRING(16),NAME('WinControlGuid | Private') !                     
Text                        STRING(255),NAME('Text | Attribute') !                     
                         END
                     END                       

Keys                 FILE,DRIVER('TOPSPEED'),NAME('Keys.tps'),PRE(Key),CREATE,BINDABLE,THREAD !                     
PKKeyGuidKey             KEY(Key:PKGuid),NOCASE,PRIMARY    !                     
FKKeyDictionaryGuidKey   KEY(Key:DictionaryGuid),DUP,NOCASE !                     
FKKeyParentGuidKey       KEY(Key:ParentGUID),DUP,NOCASE    !                     
FKKeyTableGuidKey        KEY(Key:TableGuid),DUP,NOCASE     !                     
FKKeyNameKey             KEY(Key:KeyName),DUP,NOCASE       !                     
FKKeyOrderKey            KEY(Key:Order),DUP,NOCASE         !                     
FKKeyParentGuidAndOrderKey KEY(Key:ParentGUID,Key:KeyOrder),DUP,NOCASE !                     
FKKeyKeyGuidKey          KEY(Key:KeyGuid),DUP,NOCASE       !                     
AuditBlob                   BLOB,NAME('AuditBlob | Private') !                     
CommentsBlob                BLOB,NAME('CommentsBlob | Private') !                     
OptionsBlob                 BLOB,NAME('OptionsBlob | Private') !                     
ComponentsBlob              BLOB,NAME('ComponentsBlob | Private') !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
DictionaryGuid              STRING(16),NAME('DictionaryGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
TableGuid                   STRING(16),NAME('TableGuid | Private') !                     
KeyOrder                    REAL,NAME('KeyOrder | Private') !                     
KeyGuid                     STRING(38),NAME('Guid | Attribute') !                     
Ident                       STRING(5),NAME('Ident | Attribute') !                     
Order                       STRING(5),NAME('Order | Attribute') !                     
KeyName                     STRING(100),NAME('Name | Attribute') !                     
ExternalName                STRING(100),NAME('ExternalName | Attribute') !                     
Description                 STRING(100),NAME('Description | Attribute') !                     
KeyType                     STRING(20),NAME('KeyType | Attribute') !                     
NoPopulate                  STRING(5),NAME('NoPopulate | Attribute') !                     
Unique                      STRING(5),NAME('Unique | Attribute') !                     
Primary                     STRING(5),NAME('Primary | Attribute') !                     
AutoNumber                  STRING(5),NAME('AutoNumber | Attribute') !                     
Exclude                     STRING(5),NAME('Exclude | Attribute') !                     
CaseSensitive               STRING(5),NAME('CaseSensitive | Attribute') !                     
                         END
                     END                       

Validities           FILE,DRIVER('TOPSPEED'),NAME('Validities.tps'),PRE(Val),CREATE,BINDABLE,THREAD !                     
PKValGuidKey             KEY(Val:GUID),NOCASE,PRIMARY      !                     
FKValParentGuidKey       KEY(Val:ParentGUID),DUP,NOCASE    !                     
FKValFieldGuidKey        KEY(Val:FieldGuid),DUP,NOCASE     !                     
Record                   RECORD,PRE()
GUID                        STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
FieldGuid                   STRING(16),NAME('FieldGuid | Private') !                     
Check                       STRING(20),NAME('Check | Attribute') !                     
Lookup                      STRING(40),NAME('Lookup | Attribute') !                     
Choices                     STRING(100),NAME('Choices | Attribute') !                     
Values                      STRING(100),NAME('Values | Attribute') !                     
RangeHigh                   STRING(20),NAME('RangeHigh | Attribute') !                     
TrueValue                   STRING(100),NAME('TrueValue | Attribute') !                     
FalseValue                  STRING(100),NAME('FalseValue | Attribute') !                     
                         END
                     END                       

Relations            FILE,DRIVER('TOPSPEED'),NAME('Relations.tps'),PRE(Rel),CREATE,BINDABLE,THREAD !                     
PKRelGuidKey             KEY(Rel:PKGuid),NOCASE,PRIMARY    !                     
FKRelParentGuidKey       KEY(Rel:ParentGUID),DUP,NOCASE    !                     
FKRelPrimaryTableKey     KEY(Rel:PrimaryTable),DUP,NOCASE  !                     
FKRelForeignTableKey     KEY(Rel:ForeignTable),DUP,NOCASE  !                     
FKRelDctxOrderKey        KEY(Rel:DctxOrder),DUP,NOCASE     !                     
FKRelPrimaryKeyKey       KEY(Rel:PrimaryKey),DUP,NOCASE    !                     
FKRelForeignKeyKey       KEY(Rel:ForeignKey),DUP,NOCASE    !                     
AuditBlob                   BLOB,NAME('AuditBlob | Private') !                     
MappingsBlob                BLOB,NAME('MappingsBlob | Private') !                     
CommentsBlob                BLOB,NAME('CommentsBlob | Private') !                     
OptionsBlob                 BLOB,NAME('OptionsBlob | Private') !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
DctxOrder                   LONG,NAME('DctxOrder | Private') !                     
Guid                        STRING(38),NAME('Guid | Attribute') !                     
PrimaryTable                STRING(38),NAME('PrimaryTable | Attribute') !                     
ForeignTable                STRING(38),NAME('ForeignTable | Attribute') !                     
PrimaryKey                  STRING(38),NAME('PrimaryKey | Attribute') !                     
ForeignKey                  STRING(38),NAME('ForeignKey | Attribute') !                     
Delete                      STRING(20),NAME('Delete | Attribute') !                     
Update                      STRING(20),NAME('Update | Attribute') !                     
                         END
                     END                       

ForeignMappings      FILE,DRIVER('TOPSPEED'),NAME('ForeignMappings.tps'),PRE(For),CREATE,BINDABLE,THREAD !                     
PKForGuidKey             KEY(For:GUID),NOCASE,PRIMARY      !                     
FKForParentGuidKey       KEY(For:ParentGUID),DUP,NOCASE    !                     
FKRelRelationGuidKey     KEY(For:RelationGuid),DUP,NOCASE  !                     
Record                   RECORD,PRE()
GUID                        STRING(16),NAME('PKguid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
RelationGuid                STRING(16),NAME('RelationGuid | Private') !                     
MapGuid                     STRING(40),NAME('Guid | Attribute') !                     
FieldGuid                   STRING(40),NAME('Field | Attribute') !                     
                         END
                     END                       

PrimaryMappings      FILE,DRIVER('TOPSPEED'),NAME('PrimaryMappings.tps'),PRE(Prim),CREATE,BINDABLE,THREAD !                     
PKPrimGuidKey            KEY(Prim:GUID),NOCASE,PRIMARY     !                     
FKPrimParentGuidKey      KEY(Prim:ParentGUID),DUP,NOCASE   !                     
FKPrimRelationGuidKey    KEY(Prim:RelationGuid),DUP,NOCASE !                     
Record                   RECORD,PRE()
GUID                        STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
RelationGuid                STRING(16),NAME('RelationGuid | Private') !                     
MapGuid                     STRING(40),NAME('Guid | Attribute') !                     
FieldGuid                   STRING(40),NAME('Field | Attribute') !                     
                         END
                     END                       

KitchenSink          FILE,DRIVER('TOPSPEED'),NAME('KitchenSink.tps'),PRE(Kit),BINDABLE,CREATE,THREAD ! KitchenSink Description
PKKitGuidKey             KEY(Kit:GUID),NOCASE,PRIMARY      !                     
Record                   RECORD,PRE()
GUID                        STRING(17),NAME('GUID')        !                     
ValidityChecks              STRING(20),NAME('ValidityChecks') !                     
NotZeroOrBlank              LONG                           !                     
NumericRange                LONG,NAME('NumericRange')      !                     
TrueOrFalse                 BYTE                           !                     
MustBeInList                STRING(16)                     !                     
DateTimeStr                 STRING(8)                      !                     
DateTimeGrp                 GROUP,OVER(DateTimeStr)        !                     
DateFld                       DATE                         !                     
TimeFld                       TIME                         !                     
                            END                            !                     
                         END
                     END                       

Triggers             FILE,DRIVER('TOPSPEED'),NAME('Triggers.tps'),PRE(Tri),CREATE,BINDABLE,THREAD !                     
PKTriGuidKey             KEY(Tri:PKGuid),NOCASE,PRIMARY    !                     
FKTriParentGuidKey       KEY(Tri:ParentGUID),DUP,NOCASE    !                     
FKTriTableGuidKey        KEY(Tri:TableGuid),DUP,NOCASE     !                     
CommentsBlob                BLOB,NAME('CommentsBlob | Private') !                     
Code_                       BLOB,NAME('Code | Private')    !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
TableGuid                   STRING(16),NAME('TableGuid | Private') !                     
TriggerOrder                REAL,NAME('TriggerOrder | Private') !                     
Guid                        STRING(40),NAME('Guid | Attribute') !                     
TriggerType                 STRING(20),NAME('TriggerType | Attribute') !                     
Audit                       GROUP,NAME('Audit')            !                     
createuser                    STRING(50),NAME('CreateUser | attribute') !                     
createdate                    STRING(11),NAME('CreateDate | attribute') !                     
createtime                    STRING(10),NAME('CreateTime | attribute') !                     
createversionnumber           STRING(3),NAME('CreateVersionNumber | attribute') !                     
modifieduser                  STRING(50),NAME('ModifiedUser | attribute') !                     
modifieddate                  STRING(11),NAME('ModifiedDate | attribute') !                     
modifiedtime                  STRING(10),NAME('ModifiedTime | attribute') !                     
modifiedversionnumber         STRING(3),NAME('ModifiedVersionNumber | attribute') !                     
                            END                            !                     
                         END
                     END                       

DctCode              FILE,DRIVER('TOPSPEED'),NAME('Code.tps'),PRE(Cod),CREATE,BINDABLE,THREAD !                     
PKCodGuidKey             KEY(Cod:GUID),NOCASE,PRIMARY      !                     
FKCodParentGuidKey       KEY(Cod:ParentGUID),DUP,NOCASE    !                     
FKCodTriggerGuidKey      KEY(Cod:TriggerGuid),DUP,NOCASE   !                     
Record                   RECORD,PRE()
GUID                        STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
TriggerGuid                 STRING(16),NAME('TriggerGuid | Attribute') !                     
Line                        STRING(255),NAME('Line | Attribute') !                     
                         END
                     END                       

DataTypesLkUp        FILE,DRIVER('TOPSPEED'),NAME('DataTypesLkUp.tps'),PRE(Dat),CREATE,BINDABLE,THREAD !                     
PKDatGuidKey             KEY(Dat:GUID),NOCASE,PRIMARY      !                     
FKDatTypeKey             KEY(Dat:Type),DUP,NOCASE          !                     
Record                   RECORD,PRE()
GUID                        STRING(16)                     !                     
Type                        STRING(10)                     !                     
DisplayValue                STRING(10),NAME('DisplayValue') !                     
Usages                      STRING(20),NAME('Usages')      !                     
                         END
                     END                       

DriversLkUp          FILE,DRIVER('TOPSPEED'),NAME('DriversLkUp.tps'),PRE(Dri),CREATE,BINDABLE,THREAD !                     
PKDriGuidKey             KEY(Dri:GUID),NOCASE,PRIMARY      !                     
FKDriDriverKey           KEY(Dri:Driver),DUP,NOCASE        !                     
Record                   RECORD,PRE()
GUID                        STRING(16)                     !                     
Driver                      STRING(20)                     !                     
                         END
                     END                       

Aliases              FILE,DRIVER('TOPSPEED'),NAME('Aliases.tps'),PRE(Ali),CREATE,BINDABLE,THREAD !                     
PKAliGuidKey             KEY(Ali:PKGuid),NOCASE,PRIMARY    !                     
FKAliTableGuid           KEY(Ali:TableGuid),DUP,NOCASE     !                     
FKAliParentGuidKey       KEY(Ali:ParentGUID),DUP,NOCASE    !                     
AuditBlob                   BLOB,NAME('AuditBlob | Private') !                     
CommentsBlob                BLOB,NAME('CommentsBlob | Private') !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
TableGuid                   STRING(16),NAME('TableGuid | Private') !                     
AliasGuid                   STRING(40),NAME('Guid | attribute') !                     
Ident                       STRING(5),NAME('Ident | attribute') !                     
AliasName                   STRING(100),NAME('Name | attribute') !                     
Prefix                      STRING(20),NAME('Prefix | attribute') !                     
                         END
                     END                       

DuplicateKeys        FILE,DRIVER('TOPSPEED'),NAME('DuplicateKeys.tps'),PRE(DupK),CREATE,BINDABLE,THREAD !                     
PKDupKGuidKey            KEY(DupK:PKGuid),NOCASE,PRIMARY   !                     
FKDupKDictionaryGuid     KEY(DupK:DictionaryGuid),DUP,NOCASE !                     
FKDupKTableGuid          KEY(DupK:TableGuid),DUP,NOCASE    !                     
FKDupKPKGuidKey          KEY(DupK:KeyPKGuid),DUP,NOCASE    !                     
FKDupKKeyGuidKey         KEY(DupK:KeyGuid),DUP,NOCASE      !                     
FLDupKKeyNameKey         KEY(DupK:KeyName),DUP,NOCASE      !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
DictionaryGuid              STRING(16)                     !                     
TableGuid                   STRING(16)                     !                     
KeyPKGuid                   STRING(16)                     !                     
KeyGuid                     STRING(38)                     !                     
KeyName                     STRING(100)                    !                     
Tables                      STRING(1000)                   !                     
                         END
                     END                       

TreeMemTable         FILE,DRIVER('MEMORY'),PRE(Tree),CREATE,BINDABLE,THREAD !                     
PKTreeGuidKey            KEY(Tree:PKGuid),NOCASE,PRIMARY   !                     
FKTreeDictionaryGuidKey  KEY(Tree:DictionaryGuid),DUP,NOCASE !                     
FKTreeTableGuidKey       KEY(Tree:TableGuid),DUP,NOCASE    !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
DictionaryGuid              STRING(16)                     !                     
TableGuid                   STRING(16)                     !                     
                         END
                     END                       

DctComments          FILE,DRIVER('TOPSPEED'),NAME('DctComments.tps'),PRE(Dcom),CREATE,BINDABLE,THREAD !                     
PKDcomGuidKey            KEY(Dcom:PKGuid),NOCASE,PRIMARY   !                     
FKDcomDictionaryGuidKey  KEY(Dcom:DictionaryGuid),DUP,NOCASE !                     
Comments                    BLOB,NAME('Comments | Private') !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
DictionaryGuid              STRING(16),NAME('DictionaryGuid | Private') !                     
                         END
                     END                       

TableTypes           FILE,DRIVER('TOPSPEED'),NAME('TableTypes.tps'),PRE(TblTypes),CREATE,BINDABLE,THREAD !                     
TbleTypesPKGuidKey       KEY(TblTypes:PKGuid),NOCASE,PRIMARY !                     
TblTypesDescriptionKey   KEY(TblTypes:Description),DUP,NOCASE !                     
TblTypesTypeOrderKey     KEY(TblTypes:TypeOrder),DUP,NOCASE !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
Description                 STRING(20),NAME('Description') !                     
TypeOrder                   LONG,NAME('TypeOrder')         !                     
                         END
                     END                       

FieldDefaultsLkUp    FILE,DRIVER('TOPSPEED'),NAME('FieldDefaultsLkUp.tps'),PRE(Fie),CREATE,BINDABLE,THREAD !                     
PKGuidKey                KEY(Fie:PKGuid),NAME('PKFie_GuidKey'),NOCASE,PRIMARY !                     
TypeNameKey              KEY(Fie:TypeName),DUP,NAME('Fie_TypeNameKey'),NOCASE !                     
BaseTypeKey              KEY(Fie:BaseType),DUP,NAME('Fie_BaseTypeKey'),NOCASE !                     
DataTypeKey              KEY(Fie:DataType),DUP,NAME('Fie_DataTypeKey'),NOCASE !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
TypeName                    STRING(12),NAME('TypeName')    !                     
BaseType                    STRING(100),NAME('BaseType')   !                     
DataType                    STRING(12),NAME('DataType')    !                     
RowPicture                  STRING(10),NAME('RowPicture')  !                     
Reference                   STRING(5),NAME('Reference')    !                     
Size_                       LONG,NAME('Size')              !                     
Places                      LONG,NAME('Places')            !                     
ScreenPicture               STRING(10),NAME('ScreenPicture') !                     
ScreenPrompt                STRING(100),NAME('ScreenPrompt') !                     
ReportHeading               STRING(100),NAME('ReportHeading') !                     
Justification               STRING(10),NAME('Justification') !                     
Offset                      LONG,NAME('Offset')            !                     
ReportControl               STRING(50),NAME('ReportControl') !                     
WindowControl               STRING(150),NAME('WindowControl') !                     
Validity                    STRING(30),NAME('Validity')    !                     
                         END
                     END                       

TablePools           FILE,DRIVER('TOPSPEED'),NAME('Tables.tps'),PRE(TabPool),CREATE,BINDABLE,THREAD !                     
PKTabGuidKey             KEY(TabPool:PKGuid),NOCASE,PRIMARY !                     
TabTableTypeGuidKey      KEY(TabPool:TableTypeGuid),DUP,NOCASE !                     
FKTabParentGuidKey       KEY(TabPool:ParentGUID),DUP,NOCASE !                     
FKTabDictionaryGuidKey   KEY(TabPool:DictionaryGuid),DUP,NOCASE !                     
FKTabTableGuidKey        KEY(TabPool:Guid),DUP,NOCASE      !                     
FKTabGuidKey             KEY(TabPool:Guid),DUP,NOCASE      !                     
FKTabDctxOrderKey        KEY(TabPool:DctxOrder),DUP,NOCASE !                     
FKTabTableNameKey        KEY(TabPool:TableName),DUP,NOCASE !                     
FKTabDctGuidAndOrderKey  KEY(TabPool:DictionaryGuid,TabPool:DctxOrder),DUP,NOCASE ! Sort table by DCT Guid and DctxOrder value
FKTabParentGuidAndOrderKey KEY(TabPool:ParentGUID,TabPool:DctxOrder),DUP,NOCASE !                     
AuditBlob                   BLOB,NAME('AuditBlob | Private') !                     
CommentsBlob                BLOB,NAME('CommentsBlob | Private') !                     
OptionsBlob                 BLOB,NAME('OptionsBlob | Private') !                     
SqlCreateBlob               BLOB,NAME('SqlCreateBlob | Private') !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
TableTypeGuid               STRING(16),NAME('TableTypeGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
DictionaryGuid              STRING(16),NAME('DictionaryGuid | Private') !                     
DctxOrder                   REAL,NAME('DctxOrder | Private') !                     
Guid                        STRING(38),NAME('Guid | attribute') !                     
Ident                       STRING(5),NAME('Ident | attribute') !                     
Usage                       STRING(100),NAME('Usage | Attribute') !                     
TableName                   STRING(100),NAME('Name | attribute') !                     
Description                 STRING(255),NAME('Description | attribute') !                     
TablePrefix                 STRING(20),NAME('Prefix | attribute') !                     
TableDriver                 STRING(30),NAME('Driver | attribute') !                     
DriverOption                STRING(100),NAME('DriverOption | attribute') !                     
Owner                       STRING(100),NAME('Owner | attribute') !                     
TablePath                   STRING(100),NAME('Path | attribute') !                     
Create                      STRING(5),NAME('Create | attribute') !                     
Reclaim                     STRING(5),NAME('Reclaim | Attribute') !                     
Encrypt                     STRING(5),NAME('Encrypt | Attribute') !                     
OEM                         STRING(5),NAME('OEM | Attribute') !                     
Thread                      STRING(5),NAME('Thread | attribute') !                     
Bindable                    STRING(5),NAME('Bindable | attribute') !                     
                         END
                     END                       

TableGlobals         FILE,DRIVER('TOPSPEED'),NAME('Tables.tps'),PRE(TabGlo),CREATE,BINDABLE,THREAD !                     
PKTabGuidKey             KEY(TabGlo:PKGuid),NOCASE,PRIMARY !                     
TabTableTypeGuidKey      KEY(TabGlo:TableTypeGuid),DUP,NOCASE !                     
FKTabParentGuidKey       KEY(TabGlo:ParentGUID),DUP,NOCASE !                     
FKTabDictionaryGuidKey   KEY(TabGlo:DictionaryGuid),DUP,NOCASE !                     
FKTabTableGuidKey        KEY(TabGlo:Guid),DUP,NOCASE       !                     
FKTabGuidKey             KEY(TabGlo:Guid),DUP,NOCASE       !                     
FKTabDctxOrderKey        KEY(TabGlo:DctxOrder),DUP,NOCASE  !                     
FKTabTableNameKey        KEY(TabGlo:TableName),DUP,NOCASE  !                     
FKTabDctGuidAndOrderKey  KEY(TabGlo:DictionaryGuid,TabGlo:DctxOrder),DUP,NOCASE ! Sort table by DCT Guid and DctxOrder value
FKTabParentGuidAndOrderKey KEY(TabGlo:ParentGUID,TabGlo:DctxOrder),DUP,NOCASE !                     
AuditBlob                   BLOB,NAME('AuditBlob | Private') !                     
CommentsBlob                BLOB,NAME('CommentsBlob | Private') !                     
OptionsBlob                 BLOB,NAME('OptionsBlob | Private') !                     
SqlCreateBlob               BLOB,NAME('SqlCreateBlob | Private') !                     
Record                   RECORD,PRE()
PKGuid                      STRING(16),NAME('PKGuid | Private') !                     
TableTypeGuid               STRING(16),NAME('TableTypeGuid | Private') !                     
ParentGUID                  STRING(16),NAME('ParentGUID | Private') !                     
DictionaryGuid              STRING(16),NAME('DictionaryGuid | Private') !                     
DctxOrder                   REAL,NAME('DctxOrder | Private') !                     
Guid                        STRING(38),NAME('Guid | attribute') !                     
Ident                       STRING(5),NAME('Ident | attribute') !                     
Usage                       STRING(100),NAME('Usage | Attribute') !                     
TableName                   STRING(100),NAME('Name | attribute') !                     
Description                 STRING(255),NAME('Description | attribute') !                     
TablePrefix                 STRING(20),NAME('Prefix | attribute') !                     
TableDriver                 STRING(30),NAME('Driver | attribute') !                     
DriverOption                STRING(100),NAME('DriverOption | attribute') !                     
Owner                       STRING(100),NAME('Owner | attribute') !                     
TablePath                   STRING(100),NAME('Path | attribute') !                     
Create                      STRING(5),NAME('Create | attribute') !                     
Reclaim                     STRING(5),NAME('Reclaim | Attribute') !                     
Encrypt                     STRING(5),NAME('Encrypt | Attribute') !                     
OEM                         STRING(5),NAME('OEM | Attribute') !                     
Thread                      STRING(5),NAME('Thread | attribute') !                     
Bindable                    STRING(5),NAME('Bindable | attribute') !                     
                         END
                     END                       

!endregion

! ----- ThisGPF --------------------------------------------------------------------------
ThisGPF              Class(GPFReporterClass)
    ! derived method declarations
Construct              PROCEDURE ()
Destruct               PROCEDURE ()
_GetSymbol             PROCEDURE (ulong pAddress,byte pStackTrace=1),string ,VIRTUAL
_LookupExceptionCode   PROCEDURE (ulong p_ExceptionCode),string ,VIRTUAL
_VectoredExceptionHandler_ PROCEDURE (ulong p_e),long ,VIRTUAL
_StackDetails          PROCEDURE (ulong p_e,byte p_details,ulong p_hProcess),string ,VIRTUAL
_LocateDebugSymbols    PROCEDURE (long phModule),byte ,VIRTUAL
_ReadBlockFromFile     PROCEDURE (ulong pOffset,long pReadBytes,*string pFileBlock,*string pFileName) ,VIRTUAL
_GetModuleName         PROCEDURE (long phModule),string ,VIRTUAL
_GetModuleHandle       PROCEDURE (ulong pAddress),long ,VIRTUAL
LookupAddress          PROCEDURE () ,VIRTUAL
Initialize             PROCEDURE () ,VIRTUAL
FilterExceptions       PROCEDURE (ulong pException) ,VIRTUAL
_EncodeEmail           PROCEDURE (string pEmailText),string ,VIRTUAL
_FindFirstBreak        PROCEDURE (string pText,long pMaxLen),long ,VIRTUAL
ExtraReportText        PROCEDURE () ,VIRTUAL
_GetDLLVersion         PROCEDURE (string pDLLName),string ,VIRTUAL
_SetFileNames          PROCEDURE () ,VIRTUAL
DeleteDumpFile         PROCEDURE () ,VIRTUAL
_InitReportText        PROCEDURE () ,VIRTUAL
_ExecuteCommands       PROCEDURE () ,VIRTUAL
_StackDump             PROCEDURE (long pStart,long pEnd,long pStackLevel),string ,VIRTUAL
_FindLinePosition      PROCEDURE (string pText,long pLineNumber),long ,VIRTUAL
_DebugLog              PROCEDURE (string pDebugData,byte pFirstLine=0) ,VIRTUAL
_FormatLineInfo        PROCEDURE (long pLineNumber,string pProcName,string pSourceName,string pModuleName,byte pStackTrace,byte pNoProcFound,byte pExactAddress,byte pNoLineNumber),string ,VIRTUAL
_GetAssert             PROCEDURE (long pSP,long pBP),long ,VIRTUAL
_GetOtherMessage       PROCEDURE (long pSP,long pBP),long ,VIRTUAL
_RestartProgram        PROCEDURE () ,VIRTUAL
                     End  ! ThisGPF
! ----- end ThisGPF -----------------------------------------------------------------------
         include('MessageBox.inc'),once
ThisMessageBoxGlobal class(csThreadSafeMessageClass)
                     end

ThisMessageBox       class(csEnhancedMessageClass) ,thread
AssignGlobalClass        procedure    ,virtual
Init                     procedure  (long UseABCClasses=0,long UseDefaultFile=0)  ,virtual
PrimeLog                 procedure  (<string ExtraDetails>)  ,virtual
                      end
StartSearchEXEName          long(1)
EndSearchEXEName            long(0)
! ----- GetGlyphOutline constants -----
GGO_BITMAP              EQUATE(1)          ! 1-bpp bitmap
GGO_GRAY8_BITMAP        EQUATE(16)         ! 8-bit anti-aliased bitmap
GDI_ERROR               EQUATE(-1)         ! returned on error

ANSI_CHARSET            EQUATE(0)
OUT_DEFAULT_PRECIS      EQUATE(0)
CLIP_DEFAULT_PRECIS     EQUATE(0)
DEFAULT_QUALITY         EQUATE(0)
DEFAULT_PITCH           EQUATE(0)
FF_DONTCARE             EQUATE(0)
FW_NORMAL               EQUATE(400)

GlyphMetrics            GROUP,TYPE
gmBlackBoxX             LONG
gmBlackBoxY             LONG
gmptGlyphOrigin          GROUP
x                          LONG
y                          LONG
                         END
gmCellIncX              LONG
gmCellIncY              LONG
                        END

MAT2                    GROUP,TYPE
eM11                    LONG
eM12                    LONG
eM21                    LONG
eM22                    LONG
                        END

LOGFONT                 GROUP,TYPE
lfHeight                LONG
lfWidth                 LONG
lfEscapement            LONG
lfOrientation           LONG
lfWeight                LONG
lfItalic                BYTE
lfUnderline             BYTE
lfStrikeOut             BYTE
lfCharSet               BYTE
lfOutPrecision          BYTE
lfClipPrecision         BYTE
lfQuality               BYTE
lfPitchAndFamily        BYTE
lfFaceName              CSTRING(64)
                        END

! Win32 constants
GENERIC_READ            EQUATE(80000000h)
GENERIC_WRITE           EQUATE(40000000h)   
FILE_SHARE_NONE         EQUATE(0)
CREATE_ALWAYS           EQUATE(2)
FILE_ATTRIBUTE_NORMAL   EQUATE(80h)
INVALID_HANDLE_VALUE    EQUATE(-1)

BITMAPFILEHEADER        GROUP,TYPE
bfType                  USHORT
bfSize                  LONG
bfReserved1             USHORT
bfReserved2             USHORT
bfOffBits               LONG
                        END

BITMAPINFOHEADER        GROUP,TYPE
biSize                  LONG
biWidth                 LONG
biHeight                LONG
biPlanes                USHORT
biBitCount              USHORT
biCompression           LONG
biSizeImage             LONG
biXPelsPerMeter         LONG
biYPelsPerMeter         LONG
biClrUsed               LONG
biClrImportant          LONG
                        END
GloDct         CLASS(dwrDctParser) 
               END
UltimateNotify_TplVersion    CSTRING('vv1.0')
NotifyManager         CLASS(UltimateNotifyManager)
                     END
WE::MustClose       long
WE::CantCloseNow    long
NYS006_TplVersion       CSTRING('v25.05.07')
DockingPaneWndMgr       CLASS(DockingPaneWndMgrClass)
                        END
NYS950_TplVersion       CSTRING('v25.05.07')
TemplateHelper          CLASS(TemplateHelperClass)
Init                      PROCEDURE(),DERIVED
InitComplete              PROCEDURE(),DERIVED
InitTemplateSettings      PROCEDURE(),DERIVED
Kill                      PROCEDURE(),DERIVED
KillComplete              PROCEDURE(),DERIVED
                        END
DonutHoleRegistrator        NinjaDonutHoleRegistrationClass
mhResizeFrame        MH::ResizeFrameClass                  ! SuperStuff mhResize Global Frame Object
MH::Glo:SaveMaximize BYTE(0),THREAD
NYS011_TplVersion       CSTRING('v25.05.07')
EVENT:NYS009_Event      EQUATE(641h)
EVENT:NYS009_SelChgd    EQUATE(647h)
NYS009_TplVersion       CSTRING('v25.09.10')
NYS009_SelFeq           LONG,THREAD
NYS009_RetRef           CSTRING(20),THREAD
NYS009_EventName        CSTRING(101),THREAD
NYS008_TplVersion       CSTRING('v25.09.10')
SkinFramework           CLASS(SkinFrameworkClass)
                        END
NYS004_TplVersion       CSTRING('v25.05.07')
Access:TagSet_       &FileManager,THREAD                   ! FileManager for TagSet_
Relate:TagSet_       &RelationManager,THREAD               ! RelationManager for TagSet_
Access:TagFile_      &FileManager,THREAD                   ! FileManager for TagFile_
Relate:TagFile_      &RelationManager,THREAD               ! RelationManager for TagFile_
Access:TagFilePos_   &FileManager,THREAD                   ! FileManager for TagFilePos_
Relate:TagFilePos_   &RelationManager,THREAD               ! RelationManager for TagFilePos_
Access:Dictionaries  &FileManager,THREAD                   ! FileManager for Dictionaries
Relate:Dictionaries  &RelationManager,THREAD               ! RelationManager for Dictionaries
Access:Tables        &FileManager,THREAD                   ! FileManager for Tables
Relate:Tables        &RelationManager,THREAD               ! RelationManager for Tables
Access:DctVersions   &FileManager,THREAD                   ! FileManager for DctVersions
Relate:DctVersions   &RelationManager,THREAD               ! RelationManager for DctVersions
Access:Options       &FileManager,THREAD                   ! FileManager for Options
Relate:Options       &RelationManager,THREAD               ! RelationManager for Options
Access:Fields        &FileManager,THREAD                   ! FileManager for Fields
Relate:Fields        &RelationManager,THREAD               ! RelationManager for Fields
Access:WindowControls &FileManager,THREAD                  ! FileManager for WindowControls
Relate:WindowControls &RelationManager,THREAD              ! RelationManager for WindowControls
Access:Lines         &FileManager,THREAD                   ! FileManager for Lines
Relate:Lines         &RelationManager,THREAD               ! RelationManager for Lines
Access:Keys          &FileManager,THREAD                   ! FileManager for Keys
Relate:Keys          &RelationManager,THREAD               ! RelationManager for Keys
Access:Validities    &FileManager,THREAD                   ! FileManager for Validities
Relate:Validities    &RelationManager,THREAD               ! RelationManager for Validities
Access:Relations     &FileManager,THREAD                   ! FileManager for Relations
Relate:Relations     &RelationManager,THREAD               ! RelationManager for Relations
Access:ForeignMappings &FileManager,THREAD                 ! FileManager for ForeignMappings
Relate:ForeignMappings &RelationManager,THREAD             ! RelationManager for ForeignMappings
Access:PrimaryMappings &FileManager,THREAD                 ! FileManager for PrimaryMappings
Relate:PrimaryMappings &RelationManager,THREAD             ! RelationManager for PrimaryMappings
Access:KitchenSink   &FileManager,THREAD                   ! FileManager for KitchenSink
Relate:KitchenSink   &RelationManager,THREAD               ! RelationManager for KitchenSink
Access:Triggers      &FileManager,THREAD                   ! FileManager for Triggers
Relate:Triggers      &RelationManager,THREAD               ! RelationManager for Triggers
Access:DctCode       &FileManager,THREAD                   ! FileManager for DctCode
Relate:DctCode       &RelationManager,THREAD               ! RelationManager for DctCode
Access:DataTypesLkUp &FileManager,THREAD                   ! FileManager for DataTypesLkUp
Relate:DataTypesLkUp &RelationManager,THREAD               ! RelationManager for DataTypesLkUp
Access:DriversLkUp   &FileManager,THREAD                   ! FileManager for DriversLkUp
Relate:DriversLkUp   &RelationManager,THREAD               ! RelationManager for DriversLkUp
Access:Aliases       &FileManager,THREAD                   ! FileManager for Aliases
Relate:Aliases       &RelationManager,THREAD               ! RelationManager for Aliases
Access:DuplicateKeys &FileManager,THREAD                   ! FileManager for DuplicateKeys
Relate:DuplicateKeys &RelationManager,THREAD               ! RelationManager for DuplicateKeys
Access:TreeMemTable  &FileManager,THREAD                   ! FileManager for TreeMemTable
Relate:TreeMemTable  &RelationManager,THREAD               ! RelationManager for TreeMemTable
Access:DctComments   &FileManager,THREAD                   ! FileManager for DctComments
Relate:DctComments   &RelationManager,THREAD               ! RelationManager for DctComments
Access:TableTypes    &FileManager,THREAD                   ! FileManager for TableTypes
Relate:TableTypes    &RelationManager,THREAD               ! RelationManager for TableTypes
Access:FieldDefaultsLkUp &FileManager,THREAD               ! FileManager for FieldDefaultsLkUp
Relate:FieldDefaultsLkUp &RelationManager,THREAD           ! RelationManager for FieldDefaultsLkUp
Access:TablePools    &FileManager,THREAD                   ! FileManager for TablePools
Relate:TablePools    &RelationManager,THREAD               ! RelationManager for TablePools
Access:TableGlobals  &FileManager,THREAD                   ! FileManager for TableGlobals
Relate:TableGlobals  &RelationManager,THREAD               ! RelationManager for TableGlobals

GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               INIClass                              ! Global non-volatile storage manager
GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons

Dictionary           CLASS,THREAD
Construct              PROCEDURE
Destruct               PROCEDURE
                     END


  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  INIMgr.Init('.\DictionaryEditor.INI', NVD_INI)           ! Configure INIManager to use INI file
  DctInit()
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.Init()
  TemplateHelper.InitTemplateSettings()
  TemplateHelper.SuppressInitGlobalObjects = FALSE
  TemplateHelper.InitGlobalObjects()
  TemplateHelper.InitComplete()
  !---- Noyantis : Template Helper - End ----
  SYSTEM{PROP:Icon} = 'Dictionary.ico'
                 !CapeSoft MessageBox init code
  ThisMessageBox.init(1,1)
                 !End of CapeSoft MessageBox init code
  mhResizeFrame.Median            = 4
    ds_SetOKToEndSessionHandler(address(MyOKToEndSessionHandler))
    ds_SetEndSessionHandler(address(MyEndSessionHandler))
  Main3
  INIMgr.Update
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.KillGlobalObjects()
  TemplateHelper.Kill()
  TemplateHelper.KillComplete()
    ThisGPF.RestartProgram = 0
      ThisMessageBox.Kill()                     !CapeSoft MessageBox template generated code
  INIMgr.Kill                                              ! Destroy INI manager
    
!----------------------------------------------------
ThisGPF.Construct     PROCEDURE ()
  CODE
!----------------------------------------------------
ThisGPF.Destruct     PROCEDURE ()
  CODE
!----------------------------------------------------
ThisGPF._GetSymbol     PROCEDURE (ulong pAddress,byte pStackTrace=1)
ReturnValue   any
  CODE
  ReturnValue = PARENT._GetSymbol (pAddress,pStackTrace)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._LookupExceptionCode     PROCEDURE (ulong p_ExceptionCode)
ReturnValue   any
  CODE
  ReturnValue = PARENT._LookupExceptionCode (p_ExceptionCode)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._VectoredExceptionHandler_     PROCEDURE (ulong p_e)
ReturnValue   long
  CODE
  ReturnValue = PARENT._VectoredExceptionHandler_ (p_e)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._StackDetails     PROCEDURE (ulong p_e,byte p_details,ulong p_hProcess)
ReturnValue   any
  CODE
  ReturnValue = PARENT._StackDetails (p_e,p_details,p_hProcess)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._LocateDebugSymbols     PROCEDURE (long phModule)
ReturnValue   byte
  CODE
  ReturnValue = PARENT._LocateDebugSymbols (phModule)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._ReadBlockFromFile     PROCEDURE (ulong pOffset,long pReadBytes,*string pFileBlock,*string pFileName)
  CODE
  PARENT._ReadBlockFromFile (pOffset,pReadBytes,pFileBlock,pFileName)
!----------------------------------------------------
ThisGPF._GetModuleName     PROCEDURE (long phModule)
ReturnValue   any
  CODE
  ReturnValue = PARENT._GetModuleName (phModule)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._GetModuleHandle     PROCEDURE (ulong pAddress)
ReturnValue   long
  CODE
  ReturnValue = PARENT._GetModuleHandle (pAddress)
    Return ReturnValue
!----------------------------------------------------
ThisGPF.LookupAddress     PROCEDURE ()
  CODE
  PARENT.LookupAddress ()
!----------------------------------------------------
ThisGPF.Initialize     PROCEDURE ()
  CODE
  ThisGPF.EmailAddress = 'The developer <support@example.com>'
  ThisGPF.WindowTitle = ''
  ThisGPF.DumpFileName = 'GPFReport.log'
  ThisGPF.AllowEmail = 1
  ThisGPF.DumpFileAppend = 1
  ThisGPF.RestartProgram = 0
  ThisGPF.ShowDetails = 0
  ThisGPF.DebugEmail = 0
  ThisGPF.DebugLogEnabled = 0
  ThisGPF.WaitWinEnabled = 0
  ThisGPF.Workstation = ds_GetWorkstationName()     ! requires winevent ver 3.61 or later
  ThisGPF.UserName = ds_GetUserName()               ! requires winevent ver 3.61 or later
  PARENT.Initialize ()
!----------------------------------------------------
ThisGPF.FilterExceptions     PROCEDURE (ulong pException)
  CODE
  PARENT.FilterExceptions (pException)
!----------------------------------------------------
ThisGPF._EncodeEmail     PROCEDURE (string pEmailText)
ReturnValue   any
  CODE
  ReturnValue = PARENT._EncodeEmail (pEmailText)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._FindFirstBreak     PROCEDURE (string pText,long pMaxLen)
ReturnValue   long
  CODE
  ReturnValue = PARENT._FindFirstBreak (pText,pMaxLen)
    Return ReturnValue
!----------------------------------------------------
ThisGPF.ExtraReportText     PROCEDURE ()
  CODE
  ! ThisGPF.ReportText = 'Add your own report text here.'<13,10>This is on the next line.'
  PARENT.ExtraReportText ()
!----------------------------------------------------
ThisGPF._GetDLLVersion     PROCEDURE (string pDLLName)
ReturnValue   any
  CODE
  ReturnValue = PARENT._GetDLLVersion (pDLLName)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._SetFileNames     PROCEDURE ()
  CODE
  PARENT._SetFileNames ()
!----------------------------------------------------
ThisGPF.DeleteDumpFile     PROCEDURE ()
  CODE
  PARENT.DeleteDumpFile ()
!----------------------------------------------------
ThisGPF._InitReportText     PROCEDURE ()
  CODE
  PARENT._InitReportText ()
!----------------------------------------------------
ThisGPF._ExecuteCommands     PROCEDURE ()
  CODE
  PARENT._ExecuteCommands ()
!----------------------------------------------------
ThisGPF._StackDump     PROCEDURE (long pStart,long pEnd,long pStackLevel)
ReturnValue   any
  CODE
  ReturnValue = PARENT._StackDump (pStart,pEnd,pStackLevel)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._FindLinePosition     PROCEDURE (string pText,long pLineNumber)
ReturnValue   long
  CODE
  ReturnValue = PARENT._FindLinePosition (pText,pLineNumber)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._DebugLog     PROCEDURE (string pDebugData,byte pFirstLine=0)
  CODE
  PARENT._DebugLog (pDebugData,pFirstLine)
!----------------------------------------------------
ThisGPF._FormatLineInfo     PROCEDURE (long pLineNumber,string pProcName,string pSourceName,string pModuleName,byte pStackTrace,byte pNoProcFound,byte pExactAddress,byte pNoLineNumber)
ReturnValue   any
  CODE
  ReturnValue = PARENT._FormatLineInfo (pLineNumber,pProcName,pSourceName,pModuleName,pStackTrace,pNoProcFound,pExactAddress,pNoLineNumber)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._GetAssert     PROCEDURE (long pSP,long pBP)
ReturnValue   long
  CODE
  ReturnValue = PARENT._GetAssert (pSP,pBP)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._GetOtherMessage     PROCEDURE (long pSP,long pBP)
ReturnValue   long
  CODE
  ReturnValue = PARENT._GetOtherMessage (pSP,pBP)
    Return ReturnValue
!----------------------------------------------------
ThisGPF._RestartProgram     PROCEDURE ()
  CODE
  PARENT._RestartProgram ()
ThisMessageBox.AssignGlobalClass        procedure     !CapeSoft MessageBox Object Procedure

  Code
    parent.AssignGlobalClass 
    self.GlobalClass &= ThisMessageBoxGlobal

ThisMessageBox.Init                     procedure  (long UseABCClasses=0,long UseDefaultFile=0)   !CapeSoft MessageBox Object Procedure
TempVar         long,dim(2)
TMPLogFileName  string(252)

  Code
    parent.Init (UseABCClasses,UseDefaultFile)
    system{prop:MessageHook} = address(ds_Message)
    system{prop:StopHook} = address(ds_Stop)
    system{prop:HaltHook} = address(ds_Halt)
  self.SetGlobalSetting('LogMessages',1)
  self.SetGlobalSetting('ShowTimeOut',0)
  self.SetGlobalSetting('GPFHotKey', CtrlAltG)
  self.SetGlobalSetting('CopyKey', CtrlC)
    self.SetGlobalSetting('INIFile','CSMesBox.INI')
    self.SetGlobalSetting('INISection', 'CS_Messages')
    TMPLogFileName = command('0')
    loop
      EndSearchEXEName = instring('\',TMPLogFileName,1,StartSearchEXEName)
      if ~EndSearchEXEName then break .
      StartSearchEXEName = EndSearchEXEName + 1
    end
    if StartSearchEXEName > 1
      TMPLogFileName = TMPLogFileName[1:(StartSearchEXEName - 1)]
    else
      TMPLogFileName = path() & '\'
    end
    self.SetGlobalSetting('LogFileName',clip(TMPLogFileName) & '\MessageBox.Log')
    self.SetGlobalSetting('DateFormat', '@d17')

ThisMessageBox.PrimeLog                 procedure  (<string ExtraDetails>)   !CapeSoft MessageBox Object Procedure

  Code
              !CapeSoft MessageBox Template Generated code to prime the logging records
                 !Because there is no File selected, record priming is handled in the object and
                 !the default ASCII file is used to log the records.
    parent.PrimeLog (ExtraDetails)

! ------ winevent -------------------------------------------------------------------
MyOKToEndSessionHandler procedure(long pLogoff)
OKToEndSession    long(TRUE)
! Setting the return value OKToEndSession = FALSE
! will tell windows not to shutdown / logoff now.
! If parameter pLogoff = TRUE if the user is logging off.

  code
  return(OKToEndSession)

! ------ winevent -------------------------------------------------------------------
MyEndSessionHandler procedure(long pLogoff)
! If parameter pLogoff = TRUE if the user is logging off.

  code
TemplateHelper.Init  PROCEDURE()
  CODE

  PARENT.Init()

  RETURN

TemplateHelper.InitComplete  PROCEDURE()
  CODE

  PARENT.InitComplete()

  RETURN

TemplateHelper.InitTemplateSettings  PROCEDURE()
  CODE

  PARENT.InitTemplateSettings()
  TemplateHelper.DataBind.Driver                     = nysDatabind_Driver_Default
  
  TemplateHelper.DataFiles.DriverOptions             = ''
  TemplateHelper.DataFiles.FileOwner                 = ''
  TemplateHelper.DataFiles.FilePath                  = LONGPATH()
  
  TemplateHelper.Debug.Application                   = 'DictionaryEditor.EXE'
  TemplateHelper.Debug.DisplayRuntimeErrors          = 0
  TemplateHelper.Debug.DebugViewActive               = FALSE
  TemplateHelper.Debug.IncludeEventFunc              = 0
  TemplateHelper.Debug.IncludeInitLockApply          = 0
  TemplateHelper.Debug.IncludeInitLockClear          = 0
  TemplateHelper.Debug.IncludePrepareLockApply       = 0
  TemplateHelper.Debug.IncludePrepareLockClear       = 0
  TemplateHelper.Debug.IncludeTakeEvent              = 0
  TemplateHelper.Debug.IncludeTakeNotify             = 0
  TemplateHelper.Debug.IncludeTakeTimer              = 0
  TemplateHelper.Debug.IncludeTakeWindowEvent        = 0
  TemplateHelper.Debug.IncludeThreadLockApply        = 0
  TemplateHelper.Debug.IncludeThreadLockClear        = 0
  TemplateHelper.Debug.Prefix                        = 'NYS'
  TemplateHelper.Debug.UltimateDebugActive           = 1
  
  TemplateHelper.Performance.MenuBarExists           = FALSE
  TemplateHelper.Performance.SwitchLocaleForDecPoint = 0
  TemplateHelper.Performance.TrapMouseMovements      = 0
  TemplateHelper.Performance.UseCriticalSectionLocks = 0
  TemplateHelper.Performance.UseYieldLocks           = 1
  
  TemplateHelper.SaveRestore.Mode                    = nysSaveRestore_Off
  TemplateHelper.SaveRestore.UserID                  = ''
  TemplateHelper.SaveRestore.NewContent              = 1
  TemplateHelper.SaveRestore.RemovedContent          = 1
  TemplateHelper.SaveRestore.Codejock_RC_Grouping    = 1
  TemplateHelper.SaveRestore.Codejock_RC_Sorting     = 1
  
  TemplateHelper.ViewBuffer.PageSize                 = 1
  TemplateHelper.ViewBuffer.Behind                   = 0
  TemplateHelper.ViewBuffer.Ahead                    = 0
  TemplateHelper.ViewBuffer.Timeout                  = 0
  
  TemplateHelper.RegCtrl                             = nysRegAction_DoNotRegister
  
  TemplateHelper.SetBaseVersion_Chilkat(nysChilkat_na, 0)
  TemplateHelper.SetBaseVersion_Codejock(nysCodejock_v24_0_0)
  TemplateHelper.SetBaseVersion_Zoople(nysAppType_HtmlEditor, nysZoople_na)
  TemplateHelper.SetBaseVersion_Zoople(nysAppType_HtmlViewer, nysZoople_na)
  TemplateHelper.SetClarionFamily('ABC')
  TemplateHelper.SetClarionVersion('11000')
  TemplateHelper.SetImageResource('')
  TemplateHelper.SetLicenceCode(nysAppType_ShortcutBar, 'REV-QAQ-QMJ-ETA')
  TemplateHelper.SetLicenceCode(nysAppType_CommandBars, 'QQS-PNF-OJV-VBX')
  TemplateHelper.SetLicenceCode(nysAppType_CalendarPro, 'WAD-FOY-VBC-AED')
  TemplateHelper.SetLicenceCode(nysAppType_PropertyGrid, 'HVN-LFW-DIX-XRR')
  TemplateHelper.SetLicenceCode(nysAppType_TaskPanel, 'DJN-TXA-SGX-EFY')
  TemplateHelper.SetLicenceCode(nysAppType_DockingPane, 'UCY-KMS-CII-OCF')
  TemplateHelper.SetLicenceCode(nysAppType_ReportControl, 'HIF-MPA-DRR-OPF')
  TemplateHelper.SetLicenceCode(nysAppType_SkinFramework, 'GGE-OLD-QQR-EJS')
  TemplateHelper.SetLicenceCode(nysAppType_SuiteCtrls, 'NSR-VTA-EXQ-TPT')
  TemplateHelper.SetLicenceCode(nysAppType_SyntaxEdit, 'DPV-TGO-RWX-NGL')
  TemplateHelper.SetLicenceCode(nysAppType_ChartPro, 'CHA-RTY-EKD-EME')
  TemplateHelper.SetLicenceCode(nysAppType_FlowGraph, 'JKL-NMB-QPO-DGZ')
  TemplateHelper.SetLicenceCode(nysAppType_Markup, 'YU4-GH3-78G-BNP')
  
  TemplateHelper.UsedInSolution.AppType_CalendarPro   = FALSE
  TemplateHelper.UsedInSolution.AppType_ChartPro      = FALSE
  TemplateHelper.UsedInSolution.AppType_Chilkat       = FALSE
  TemplateHelper.UsedInSolution.AppType_CommandBars   = FALSE
  TemplateHelper.UsedInSolution.AppType_DevExtreme    = FALSE
  TemplateHelper.UsedInSolution.AppType_DockingPane   = TRUE
  TemplateHelper.UsedInSolution.AppType_FlowGraph     = FALSE
  TemplateHelper.UsedInSolution.AppType_Gauge         = FALSE
  TemplateHelper.UsedInSolution.AppType_GCalc         = FALSE
  TemplateHelper.UsedInSolution.AppType_GFileFind     = FALSE
  TemplateHelper.UsedInSolution.AppType_GMedia        = FALSE
  TemplateHelper.UsedInSolution.AppType_GSecurity     = FALSE
  TemplateHelper.UsedInSolution.AppType_GVisuals      = FALSE
  TemplateHelper.UsedInSolution.AppType_HtmlEditor    = FALSE
  TemplateHelper.UsedInSolution.AppType_HtmlViewer    = FALSE
  TemplateHelper.UsedInSolution.AppType_Markup        = FALSE
  TemplateHelper.UsedInSolution.AppType_Pdf           = FALSE
  TemplateHelper.UsedInSolution.AppType_PropertyGrid  = TRUE
  TemplateHelper.UsedInSolution.AppType_ReportControl = FALSE
  TemplateHelper.UsedInSolution.AppType_ShortcutBar   = FALSE
  TemplateHelper.UsedInSolution.AppType_SkinFramework = TRUE
  TemplateHelper.UsedInSolution.AppType_Spreadsheet   = FALSE
  TemplateHelper.UsedInSolution.AppType_SuiteCtrls    = TRUE
  TemplateHelper.UsedInSolution.AppType_SyntaxEdit    = TRUE
  TemplateHelper.UsedInSolution.AppType_TaskPanel     = FALSE
  TemplateHelper.UsedInSolution.AppType_Utils         = FALSE
  TemplateHelper.UsedInSolution.AppType_WordProcessor = FALSE

  RETURN

TemplateHelper.Kill  PROCEDURE()
  CODE

  PARENT.Kill()

  RETURN

TemplateHelper.KillComplete  PROCEDURE()
  CODE

  PARENT.KillComplete()

  RETURN



Dictionary.Construct PROCEDURE

  CODE
  IF THREAD()<>1
     DctInit()
  END


Dictionary.Destruct PROCEDURE

  CODE
  DctKill()

