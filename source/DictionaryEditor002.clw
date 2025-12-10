

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR002.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('DICTIONARYEDITOR001.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR009.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR010.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR011.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR012.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR013.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR014.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR015.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR023.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR024.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR025.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR026.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR027.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR028.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR031.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
Main3 PROCEDURE 

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
!MyDct               dwrDctParser

json                Class(JSONClass)
ValidateRecord      Procedure(),Long,Proc,Derived
                    End

st                  StringTheory
LneSt               StringTheory
SqlCreateSt         StringTheory
x					Long
y                   Long
MySQL               dwrSQL

TempQPointer            Long
TablesListContracted    Long
MoveMouseDownRow        Long
NextIsInGroup           Long



ListMarker          JS_ListMark
!MarkCount           LONG


PopupString         STRING(50)     
TablesPopupMgr      PopupClass  
FieldsPopupMgr      PopupClass          
PopupMgr            PopupClass                

!xml                 xFilesTree

DctEvent:PopulateTree       Equate(EVENT:User+900)
DctEvent:PopulateTablesTree Equate(EVENT:User+901)

!QLFld                       CFCQueueLocatorClass
!QL                          CFCQueueLocatorClass

!SettingsGrp                 Group(SettingsGroupType).

local						CLASS
PreviousFieldGuid           String(16)
SelectedFieldGuid           String(16)
NextFieldGuid               String(16)
ActiveDonutThread			Long
ClearMarked                 Procedure()
CloseUnusedDonutHoles       Procedure(string PkGuid)
GetActiveDonutThread		Procedure(),Long
!GetPreviousFieldRecord      Procedure()
SetActiveDonutThread		Procedure(long pThread)
CloseActiveDonutHole		Procedure()
PopulateTree                Procedure()
PopulateTableTypesTree      Procedure()
ParseMarked                 Procedure()
SetFieldGuids               Procedure(long pMouseDownRow)
SetTaggedCount              Procedure()
							End

!OldThread                   Long

HostMgrQ                    Queue
ThreadNo                    LONG
PkGuid                      String(16)
Created                     BYTE
NeedsSave                   BYTE
                            End



GroupEndsQ                  Queue,Pre(GrpEnds)
ItemGuid                    String(16)
                            End
DwrTreeQ               QUEUE,PRE(Tree2)
DisplayStr      STRING(200)
NormalFG        LONG
NormalBG        LONG
SelectedFG      LONG
SelectedBG      LONG
Icon            SHORT
Level           LONG !Closed=-1,Child=2
Loaded          SHORT
Position        STRING(1024)
WinMark         BYTE,Name('WinMark')
RealMark        BYTE,Name('RealMark')
ItemType        String(20),Name('ItemType')
ItemGuid        String(16),Name('ItemGuid')
WinGuid         String(38),Name('WinGuid')
Ndx             Long,Name('Ndx')  
TablePrefix     String(20),Name('TablePrefix') 
                END
DwrTreeQ:1             QUEUE,PRE(Tree9)
DisplayStr      STRING(200)
NormalFG        LONG
NormalBG        LONG
SelectedFG      LONG
SelectedBG      LONG
Icon            SHORT
Level           LONG !Closed=-1,Child=2
Loaded          SHORT
Position        STRING(1024)
WinMark         BYTE,Name('WinMark')
RealMark        BYTE,Name('RealMark')
ItemType        String(20),Name('ItemType')
ItemGuid        String(16),Name('ItemGuid')
WinGuid         String(38),Name('WinGuid')
Ndx             Long,Name('Ndx')  
TablePrefix     String(20),Name('TablePrefix') 
                END
TablePoolsView9    View(TablePools)
                    Project(TabPool:PKGuid)
                    Project(TabPool:TableTypeGuid)
                    Project(TabPool:ParentGUID)
                    Project(TabPool:DictionaryGuid)
                    Project(TabPool:DctxOrder)
                    Project(TabPool:Guid)
                    Project(TabPool:Ident)
                    Project(TabPool:Usage)
                    Project(TabPool:TableName)
                    Project(TabPool:Description)
                    Project(TabPool:TablePrefix)
                    Project(TabPool:TableDriver)
                    Project(TabPool:DriverOption)
                    Project(TabPool:Owner)
                    Project(TabPool:TablePath)
                    Project(TabPool:Create)
                    Project(TabPool:Reclaim)
                    Project(TabPool:Encrypt)
                    Project(TabPool:OEM)
                    Project(TabPool:Thread)
                    Project(TabPool:Bindable)
                    Project(TabPool:AuditBlob)
                    Project(TabPool:CommentsBlob)
                    Project(TabPool:OptionsBlob)
                    Project(TabPool:SqlCreateBlob)
                End ! TablePoolsView9
TableGlobalsView9    View(TableGlobals)
                    Project(TabGlo:PKGuid)
                    Project(TabGlo:TableTypeGuid)
                    Project(TabGlo:ParentGUID)
                    Project(TabGlo:DictionaryGuid)
                    Project(TabGlo:DctxOrder)
                    Project(TabGlo:Guid)
                    Project(TabGlo:Ident)
                    Project(TabGlo:Usage)
                    Project(TabGlo:TableName)
                    Project(TabGlo:Description)
                    Project(TabGlo:TablePrefix)
                    Project(TabGlo:TableDriver)
                    Project(TabGlo:DriverOption)
                    Project(TabGlo:Owner)
                    Project(TabGlo:TablePath)
                    Project(TabGlo:Create)
                    Project(TabGlo:Reclaim)
                    Project(TabGlo:Encrypt)
                    Project(TabGlo:OEM)
                    Project(TabGlo:Thread)
                    Project(TabGlo:Bindable)
                    Project(TabGlo:AuditBlob)
                    Project(TabGlo:CommentsBlob)
                    Project(TabGlo:OptionsBlob)
                    Project(TabGlo:SqlCreateBlob)
                End ! TableGlobalsView9
TablesView9    View(Tables)
                    Project(Tab:PKGuid)
                    Project(Tab:TableTypeGuid)
                    Project(Tab:ParentGUID)
                    Project(Tab:DictionaryGuid)
                    Project(Tab:DctxOrder)
                    Project(Tab:Guid)
                    Project(Tab:Ident)
                    Project(Tab:Usage)
                    Project(Tab:TableName)
                    Project(Tab:Description)
                    Project(Tab:TablePrefix)
                    Project(Tab:TableDriver)
                    Project(Tab:DriverOption)
                    Project(Tab:Owner)
                    Project(Tab:TablePath)
                    Project(Tab:Create)
                    Project(Tab:Reclaim)
                    Project(Tab:Encrypt)
                    Project(Tab:OEM)
                    Project(Tab:Thread)
                    Project(Tab:Bindable)
                    Project(Tab:AuditBlob)
                    Project(Tab:CommentsBlob)
                    Project(Tab:OptionsBlob)
                    Project(Tab:SqlCreateBlob)
                End ! TablesView9
AliasesView9    View(Aliases)
                    Project(Ali:PKGuid)
                    Project(Ali:ParentGUID)
                    Project(Ali:TableGuid)
                    Project(Ali:AliasGuid)
                    Project(Ali:Ident)
                    Project(Ali:AliasName)
                    Project(Ali:Prefix)
                    Project(Ali:AuditBlob)
                    Project(Ali:CommentsBlob)
                End ! AliasesView9
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
NotifyParam          STRING(1500)                          ! 
SettingsGrp          GROUP(SettingsGroupType),PRE()        ! 
                     END                                   ! 
DctSearch            STRING(30)                            ! 
TableSearch          STRING(30)                            ! 
FieldSearch          STRING(30)                            ! 
TaggedCount          LONG                                  ! 
DctFile              STRING(255)                           ! 
FieldsQ              QUEUE,PRE(FieldsQ)                    ! 
PKGuid               STRING(16)                            ! 
ParentGUID           STRING(16)                            ! 
TableGuid            STRING(16)                            ! 
FieldOrder           REAL                                  ! 
LastInGroup          BYTE                                  ! 
Guid                 STRING(38)                            ! 
Ident                STRING(5)                             ! 
FieldName            STRING(100)                           ! 
FieldPrefix          STRING(20)                            ! 
DerivedFrom          STRING(50)                            ! 
Description          STRING(100)                           ! 
BaseType             STRING(20)                            ! 
DataType             STRING(20)                            ! 
RowPicture           STRING(20)                            ! 
Reference            STRING(5)                             ! 
Thread               STRING(5)                             ! 
Binary               STRING(5)                             ! 
FieldSize            STRING(20)                            ! 
Dim1                 STRING(3)                             ! 
Dim2                 STRING(3)                             ! 
Dim3                 STRING(3)                             ! 
Dim4                 STRING(3)                             ! 
Places               STRING(3)                             ! 
ScreenPicture        STRING(20)                            ! 
ScreenPrompt         STRING(100)                           ! 
ReportHeading        STRING(100)                           ! 
Freeze               STRING(5)                             ! 
InitialValue         STRING(100)                           ! 
FormTab              STRING(100)                           ! 
Case_                STRING(10)                            ! 
TypingMode           STRING(20)                            ! 
Over                 STRING(100)                           ! 
Justification        STRING(20)                            ! 
Immediate            STRING(5)                             ! 
ReadOnly             STRING(5)                             ! 
Password_            STRING(5)                             ! 
Offset               STRING(20)                            ! 
ExternalName         STRING(100)                           ! 
NoPopulate           STRING(5)                             ! 
VerticalSpace        STRING(5)                             ! 
HelpId               STRING(100)                           ! 
Message              STRING(100)                           ! 
Tooltip              STRING(150)                           ! 
WindowControl        STRING(255)                           ! 
ReportControl        STRING(255)                           ! 
                     END                                   ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
ql17          CFCQueueLocatorClass
QueueSearchStr17    String(30)
ql11          CFCQueueLocatorClass
QueueSearchStr11    String(30)
BRW12::View:Browse   VIEW(Dictionaries)
                       PROJECT(Dct:DctName)
                       PROJECT(Dct:GUID)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?DictionariesList
Dct:DctName            LIKE(Dct:DctName)              !List box control field - type derived from field
Dct:GUID               LIKE(Dct:GUID)                 !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
Window               WINDOW('Dictionary Editor'),AT(0,0,577,376),FONT('Segoe UI',10),RESIZE,ICON('Dictionary.ico'), |
  GRAY,MAX,SYSTEM,IMM
                       TOOLBAR,AT(0,0,577,37),USE(?TOOLBAR1),WALLPAPER('Background1.png')
                         IMAGE('Logo.png'),AT(0,0,182,34),USE(?IMAGE1)
                         BUTTON,AT(194,5,31,25),USE(?ImportBtn),ICON('Import.ico'),FLAT
                         BUTTON,AT(228,5,31,25),USE(?ExportBtn),ICON('Export.ico'),FLAT
                         BUTTON,AT(262,5,31,25),USE(?SaveBtn),ICON('Save1.ico'),DISABLE,FLAT
                         BUTTON,AT(295,5,31,25),USE(?BulkBtn),ICON('Bulk1.ico'),FLAT
                         PROMPT('Tagged Count:'),AT(489,24),USE(?TaggedCount:Prompt),HIDE,TRN
                         STRING(@n_4),AT(542,24),USE(TaggedCount),RIGHT(1),HIDE,TRN
                         BUTTON,AT(363,5,31,25),USE(?SettingsBtn),ICON('Settings9.ico'),FLAT
                         BUTTON('Button1'),AT(503,8),USE(?BUTTON1),HIDE
                         BUTTON,AT(329,5,31,25),USE(?DuplicateKeysBtn),ICON('DuplicateKeys.ico'),FLAT
                       END
                       LIST,AT(134,31,131,305),USE(?DwrTreeList),HVSCROLL,FORMAT('100L*JT@s200@#1#'),FROM(DwrTreeQ)
                       LIST,AT(3,31,126,71),USE(?DictionariesList),LEFT(2),HVSCROLL,FORMAT('400L(2)~Dictionaries~@s100@'), |
  FROM(Queue:Browse),IMM
                       REGION,AT(270,2,305,335),USE(?DonutHoleHostRegion)
                       LIST,AT(2,136,126,200),USE(?DwrTreeList:2),HVSCROLL,FORMAT('100L*JT~Tables~@s200@#1#'),FROM(DwrTreeQ:1)
                       BUTTON,AT(2,107,15,12),USE(?AddNewTableBtn),ICON('AddNew.ico'),FLAT
                       BUTTON,AT(131,2,15,12),USE(?AddNewFieldBtn),ICON('AddNew.ico'),FLAT
                       BUTTON,AT(2,2,15,12),USE(?AddNewDctBtn),ICON('AddNew.ico'),FLAT
                       ENTRY(@s30),AT(3,17,125,10),USE(DctSearch),LEFT(2)
                       REGION,AT(265,31,4,306),USE(?EasyHSplit),IMM
                       REGION,AT(129,31,4,306),USE(?EasyHSplit:2),IMM
                       REGION,AT(2,104,123,2),USE(?EasyVSplit),IMM
                       ENTRY(@s30),AT(3,123,107,10),USE(?SearchEntry)
                       BUTTON,AT(114,122,13,11),USE(?SearchBtn),ALRT(MouseRight),ICON('SearchForward1.ico'),FLAT
                       ENTRY(@s30),AT(132,17,115,10),USE(?SearchEntry:2)
                       BUTTON,AT(253,16,13,11),USE(?SearchBtn:2),ALRT(MouseRight),ICON('SearchForward1.ico'),FLAT
                       BUTTON,AT(149,2,15,12),USE(?MoveFieldUpBtn),ICON('Upload1.ico'),FLAT
                       BUTTON,AT(167,2,15,12),USE(?MoveFieldDownBtn),ICON('Download.ico'),FLAT
                     END

FieldsView           View(Fields)
                    Project(Fld:PKGuid)
                    Project(Fld:ParentGUID)
                    Project(Fld:TableGuid)
                    Project(Fld:FieldOrder)
                    Project(Fld:LastInGroup)
                    Project(Fld:Guid)
                    Project(Fld:Ident)
                    Project(Fld:FieldName)
                    Project(Fld:DerivedFrom)
                    Project(Fld:Description)
                    Project(Fld:BaseType)
                    Project(Fld:DataType)
                    Project(Fld:Reference)
                    Project(Fld:Thread)
                    Project(Fld:Binary)
                    Project(Fld:FieldSize)
                    Project(Fld:Places)
                    Project(Fld:ScreenPicture)
                    Project(Fld:ScreenPrompt)
                    Project(Fld:ReportHeading)
                    Project(Fld:InitialValue)
                    Project(Fld:Case_)
                    Project(Fld:Over)
                    Project(Fld:Justification)
                    Project(Fld:ReadOnly)
                    Project(Fld:Offset)
                    Project(Fld:ExternalName)
                    Project(Fld:NoPopulate)
                    Project(Fld:VerticalSpace)
                    Project(Fld:HelpId)
                    Project(Fld:Message)
                    Project(Fld:Tooltip)
                    Project(Fld:WindowControl)
                    Project(Fld:ReportControl)
                    Project(Fld:WindowControlBlob)
                    Project(Fld:AuditBlob)
                    Project(Fld:ValidityBlob)
                    Project(Fld:CommentsBlob)
                    Project(Fld:OptionsBlob)
                    End
KeysView           View(Keys)
                    Project(Key:PKGuid)
                    Project(Key:DictionaryGuid)
                    Project(Key:ParentGUID)
                    Project(Key:TableGuid)
                    Project(Key:KeyOrder)
                    Project(Key:KeyGuid)
                    Project(Key:Ident)
                    Project(Key:Order)
                    Project(Key:KeyName)
                    Project(Key:ExternalName)
                    Project(Key:Description)
                    Project(Key:KeyType)
                    Project(Key:Unique)
                    Project(Key:AutoNumber)
                    Project(Key:Primary)
                    Project(Key:Exclude)
                    Project(Key:CaseSensitive)
                    Project(Key:AuditBlob)
                    Project(Key:CommentsBlob)
                    Project(Key:OptionsBlob)
                    Project(Key:ComponentsBlob)
                    End
TriggersView           View(Triggers)
                    Project(Tri:PKGuid)
                    Project(Tri:ParentGUID)
                    Project(Tri:TableGuid)
                    Project(Tri:Guid)
                    Project(Tri:TriggerType)
                    Project(Tri:Audit)
                    Project(Tri:createuser)
                    Project(Tri:createdate)
                    Project(Tri:createtime)
                    Project(Tri:createversionnumber)
                    Project(Tri:modifieduser)
                    Project(Tri:modifieddate)
                    Project(Tri:modifiedtime)
                    Project(Tri:modifiedversionnumber)
                    Project(Tri:CommentsBlob)
                    Project(Tri:Code_)
                    End
RelationsView           View(Relations)
                    Project(Rel:PKGuid)
                    Project(Rel:ParentGUID)
                    Project(Rel:DctxOrder)
                    Project(Rel:Guid)
                    Project(Rel:PrimaryTable)
                    Project(Rel:ForeignTable)
                    Project(Rel:PrimaryKey)
                    Project(Rel:ForeignKey)
                    Project(Rel:Delete)
                    Project(Rel:Update)
					Project(Rel:AuditBlob)
					Project(Rel:MappingsBlob)
                    !Project(Rel:ForeignMapping)
                    !Project(Rel:FMGuid)
                    !Project(Rel:FMField)
                    !Project(Rel:PrimaryMapping)
                    !Project(Rel:PMGuid)
                    !Project(Rel:PMField)
                    End
    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeCloseEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
! ----- csResize --------------------------------------------------------------------------
csResize             Class(csResizeClass)
    ! derived method declarations
Sized                  PROCEDURE (Long pLevel=0),VIRTUAL
Reset                  PROCEDURE (),VIRTUAL
Fetch                  PROCEDURE (STRING Sect,STRING Ent,*? Val),VIRTUAL
Update                 PROCEDURE (STRING Sect,STRING Ent,STRING Val),VIRTUAL
Init                   PROCEDURE (),VIRTUAL
                     End  ! csResize
! ----- end csResize -----------------------------------------------------------------------
! ----- csVSplit16 --------------------------------------------------------------------------
csVSplit16           Class(csVSplitClass)
    ! derived method declarations
Init                   PROCEDURE (),VIRTUAL
                     End  ! csVSplit16
! ----- end csVSplit16 -----------------------------------------------------------------------
! ----- csHSplit6 --------------------------------------------------------------------------
csHSplit6            Class(csHSplitClass)
    ! derived method declarations
Init                   PROCEDURE (),VIRTUAL
Resize                 PROCEDURE (),VIRTUAL
Update                 PROCEDURE (),VIRTUAL
MoveSplitter           PROCEDURE (Long pDeltaX, Long pDeltaY),Virtual
MovedControl           PROCEDURE (SIGNED Control),VIRTUAL
ResizedControl         PROCEDURE (SIGNED Control),VIRTUAL
Moved                  PROCEDURE (UNSIGNED phWnd,LONG plParam),VIRTUAL
ChangeControl          PROCEDURE (SIGNED Control,SIGNED NewControl,UNSIGNED hWnd),VIRTUAL
AlignmentResize        PROCEDURE (LONG Pos,SIGNED Control),VIRTUAL
Alignment              PROCEDURE (LONG Pos,SIGNED Control,QAlignmentERS pQueue),VIRTUAL
GetUpPos               PROCEDURE (),SIGNED,VIRTUAL
Reset                  PROCEDURE (),VIRTUAL
GetPosition            PROCEDURE (Long pControl,*ERS_POS Pos)
                     End  ! csHSplit6
! ----- end csHSplit6 -----------------------------------------------------------------------
! ----- csHSplit14 --------------------------------------------------------------------------
csHSplit14           Class(csHSplitClass)
    ! derived method declarations
Init                   PROCEDURE (),VIRTUAL
                     End  ! csHSplit14
! ----- end csHSplit14 -----------------------------------------------------------------------
DonutHoleHost:5 	NinjaDonutHoleHostClass
BrwDct               CLASS(BrowseClass)                    ! Browse using ?DictionariesList
Q                      &Queue:Browse                  !Reference to browse queue
ResetFromFile          PROCEDURE(),DERIVED
TakeNewSelection       PROCEDURE(),DERIVED
                     END

BRW12::Sort0:Locator EntryLocatorClass                     ! Default Locator
! ----- xml --------------------------------------------------------------------------
xml                  Class(xFilesTree)
    ! derived method declarations
ValidateRecord         PROCEDURE (),Long,Proc,Virtual
AfterReflectionParse   PROCEDURE (String pGroupName,*View pView),Virtual
_AddViewNode           PROCEDURE (View pView, *Cstring pGroupName, Long pTableId),Virtual
_AddViewRecordNested   PROCEDURE (View pView, *Cstring pGroupName),Virtual
AddChild               PROCEDURE (Long pComponentType, String pStr, Long pIndex=0),Long,Proc,Virtual
AddNode                PROCEDURE (String pTag,String pValue, Long pIndex, Long pForce=false),*XmlTree,Virtual
AddNode                PROCEDURE (String pTag,Long pIndex=0),*XmlTree,Virtual
                     End  ! xml
! ----- end xml -----------------------------------------------------------------------
! ----- MyDct --------------------------------------------------------------------------
MyDct                Class(dwrDctParser)
                     End  ! MyDct
! ----- end MyDct -----------------------------------------------------------------------
FileLookup7          SelectFileClass

  CODE
? DEBUGHOOK(Aliases:Record)
? DEBUGHOOK(DataTypesLkUp:Record)
? DEBUGHOOK(DctComments:Record)
? DEBUGHOOK(DctVersions:Record)
? DEBUGHOOK(Dictionaries:Record)
? DEBUGHOOK(DuplicateKeys:Record)
? DEBUGHOOK(FieldDefaultsLkUp:Record)
? DEBUGHOOK(Fields:Record)
? DEBUGHOOK(Keys:Record)
? DEBUGHOOK(KitchenSink:Record)
? DEBUGHOOK(Options:Record)
? DEBUGHOOK(Relations:Record)
? DEBUGHOOK(TableGlobals:Record)
? DEBUGHOOK(TablePools:Record)
? DEBUGHOOK(TableTypes:Record)
? DEBUGHOOK(Tables:Record)
? DEBUGHOOK(Triggers:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !ParamsGrp.ParentThread = Thread()
  GlobalErrors.SetProcedureName('Main3')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?DwrTreeList
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('Dct:DctName',Dct:DctName)                          ! Added by: BrowseBox(ABC)
  BIND('Dct:GUID',Dct:GUID)                                ! Added by: BrowseBox(ABC)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('Main3')
    NotifyHandler.AddNotifyCode(1)
  MyDct.UpdateFilesQueue('Dictionaries',Dictionaries)
  MyDct.UpdateFilesQueue('Tables',Tables)
  MyDct.UpdateFilesQueue('DctVersions',DctVersions)
  MyDct.UpdateFilesQueue('Fields',Fields)
  MyDct.UpdateFilesQueue('Keys',Keys)
  MyDct.UpdateFilesQueue('Relations',Relations)
  MyDct.UpdateFilesQueue('Triggers',Triggers)
  MyDct.UpdateFilesQueue('Aliases',Aliases)
  MyDct.UpdateFilesQueue('DuplicateKeys',DuplicateKeys)
  MyDct.UpdateFilesQueue('DctComments',DctComments)
  MyDct.UpdateFilesQueue('Options',Options)
  MyDct.UpdateFilesQueue('TableTypes',TableTypes)
  Relate:Aliases.SetOpenRelated()
  Relate:Aliases.Open()                                    ! File Aliases used by this procedure, so make sure it's RelationManager is open
  Relate:DataTypesLkUp.Open()                              ! File DataTypesLkUp used by this procedure, so make sure it's RelationManager is open
  Relate:FieldDefaultsLkUp.Open()                          ! File FieldDefaultsLkUp used by this procedure, so make sure it's RelationManager is open
  Relate:KitchenSink.Open()                                ! File KitchenSink used by this procedure, so make sure it's RelationManager is open
  Relate:TableGlobals.Open()                               ! File TableGlobals used by this procedure, so make sure it's RelationManager is open
  Relate:TablePools.Open()                                 ! File TablePools used by this procedure, so make sure it's RelationManager is open
  Relate:TableTypes.Open()                                 ! File TableTypes used by this procedure, so make sure it's RelationManager is open
  Access:DuplicateKeys.UseFile()                           ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Options.UseFile()                                 ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:DctComments.UseFile()                             ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:DctVersions.UseFile()                             ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Triggers.UseFile()                                ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Relations.UseFile()                               ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Keys.UseFile()                                    ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Fields.UseFile()                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Tables.UseFile()                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  BrwDct.Init(?DictionariesList,Queue:Browse.ViewPosition,BRW12::View:Browse,Queue:Browse,Relate:Dictionaries,SELF) ! Initialize the browse manager
      !IF Queue:Browse.Dct:GUID
         BrwDct.StartAtCurrent=True
         !POH:Status = ''
         !POH:PO_NBR = pPONumber
      !END   
  
  
  !  		!MyDct.Trace('')  
  !  		!MyDct.Trace('')
  !  		!MyDct.Trace('Main3<39>s Thread = ' & Thread() & ' <<<<<<<<---------------------------------------------------------------')
  !  		!MyDct.Trace('')  
  !  		!MyDct.Trace('   Size(ParamsGrp) = ' & Size(ParamsGrp))  
  !  		!MyDct.Trace('')  
    MyDct.LoadSettings(SettingsGrp)  
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('Main3', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
    Window{PROP:Buffer} = 1  
    !Glo:SaveBtnFeq = ?SaveBtn{PROP:FEQ}
  
  
  
  
    !PopupMgr.Init()
    !PopupMgr.AddItem('Copy Details','CopyDetails')
    TablesPopupMgr.Init()
    TablesPopupMgr.AddMenu('SearchType')
    TablesPopupMgr.SetText('SearchType','Search Type')
    TablesPopupMgr.SetIcon('SearchType','Search1.ico')
    TablesPopupMgr.AddItem('Contains (Default)','Contains')    
    TablesPopupMgr.SetLevel('Contains',2)
    TablesPopupMgr.SetIcon('Contains','Contains.ico')
    TablesPopupMgr.AddItem('StartsWith','StartsWith')
    TablesPopupMgr.SetLevel('StartsWith',2)
    TablesPopupMgr.SetIcon('StartsWith','StartsWith.ico')
  
    FieldsPopupMgr.Init()
    FieldsPopupMgr.AddMenu('SearchType')
    FieldsPopupMgr.SetText('SearchType','Search Type')
    FieldsPopupMgr.SetIcon('SearchType','Search1.ico')
    FieldsPopupMgr.AddItem('Contains (Default)','Contains')    
    FieldsPopupMgr.SetLevel('Contains',2)
    FieldsPopupMgr.SetIcon('Contains','Contains.ico')
    FieldsPopupMgr.AddItem('StartsWith','StartsWith')
    FieldsPopupMgr.SetLevel('StartsWith',2)
    FieldsPopupMgr.SetIcon('StartsWith','StartsWith.ico')
    !MyDct.Trace('')
    !MyDct.Trace('Main3 - TablesPopupMgr - PopupString')
    !MyDct.Trace(TablesPopupMgr.Ask())    
  
    !ListMarker.Init(?DwrTreeList:2,DwrTreeQ:1,DwrTreeQ:1.RealMark)
    !ListMarker.Behavior = 0  
    !MarkCount =  ListMarker.GetMarkCount()  
    
  ?DwrTreeList{prop:lineheight} = 12
  ?DwrTreeList{PROP:iconlist,1} = 'Folder5Large.ico'  
  ?DwrTreeList{PROP:iconlist,2} = 'FolderOpenLarge.ico'  
  ?DwrTreeList{PROP:iconlist,3} = 'DialogLarge.ico'  
  ?DwrTreeList{PROP:iconlist,4} = 'Login2Large.ico'  
  ?DwrTreeList{PROP:iconlist,5} = 'ManyToOne2.ico'  
  ?DwrTreeList{PROP:iconlist,6} = 'OneToMany2.ico'  
  ?DwrTreeList{PROP:iconlist,7} = 'PrimaryKey.ico'  
  ?DwrTreeList{PROP:iconlist,8} = 'Trigger.ico'  
  ?DwrTreeList:2{prop:lineheight} = 11
  ?DwrTreeList:2{PROP:iconlist,1} = 'Folder5Large.ico'  
  ?DwrTreeList:2{PROP:iconlist,2} = 'FolderOpenLarge.ico'  
  ?DwrTreeList:2{PROP:iconlist,3} = 'Pools.ico'  
  ?DwrTreeList:2{PROP:iconlist,4} = 'Globals.ico'  
  ?DwrTreeList:2{PROP:iconlist,5} = 'Tables.ico'  
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?DictionariesList{Prop:LineHeight} = 10
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
  WinAlertMouseZoom()
  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
    NotifyHandler.InitWindow()
  BrwDct.Q &= Queue:Browse
  BrwDct.AddSortOrder(,Dct:FKDctNameKey)                   ! Add the sort order for Dct:FKDctNameKey for sort order 1
  BrwDct.AddLocator(BRW12::Sort0:Locator)                  ! Browse has a locator for sort order 1
  BRW12::Sort0:Locator.Init(?DctSearch,Dct:DctName,1,BrwDct) ! Initialize the browse locator using ?DctSearch using key: Dct:FKDctNameKey , Dct:DctName
  BrwDct.AppendOrder('Dct:DctName')                        ! Append an additional sort order
  BrwDct.AddField(Dct:DctName,BrwDct.Q.Dct:DctName)        ! Field Dct:DctName is a hot field or requires assignment from browse
  BrwDct.AddField(Dct:GUID,BrwDct.Q.Dct:GUID)              ! Field Dct:GUID is a hot field or requires assignment from browse
  csResize.Init('Main3',Window,1)
  INIMgr.Fetch('Main3',Window)                             ! Restore window settings from non-volatile store
  FileLookup7.Init
  FileLookup7.ClearOnCancel = True
  FileLookup7.Flags=BOR(FileLookup7.Flags,FILE:LongName)   ! Allow long filenames
  FileLookup7.SetMask('DCTX Files','*.dctx')               ! Set the file mask
  FileLookup7.WindowTitle='Select DCTX File'
  BrwDct.AddToolbarTarget(Toolbar)                         ! Browse accepts toolbar control
    DonutHoleHost:5.SetLogPreamble('DonutHoleHost:5')
    DonutHoleHost:5.init(DonutHoleRegistrator.IDonutHoleHostRegistration,Window,?DonutHoleHostRegion)
    SELF.AddItem(DonutHoleHost:5.WindowComponent)
  !DonutHoleHost:5.TryOpen(START(TestDonutWindow,5000))
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  csResize.Open()
  SELF.SetAlerts()
  
  
    !QLFld.Init(DwrTreeQ,?DwrTreeList,FieldSearch,?FieldSearch)
    !QLFld.SetActiveLocator(DwrTreeQ.DisplayStr)  
    !QLFld.TreeLevelFieldName = 'Tree2:Level'
    !QLFld.SetDontBreakOnFound(True)
  
    !QL.Init(DwrTreeQ:1,?DwrTreeList:2,TableSearch,?TableSearch)
    !QL.SetActiveLocator(DwrTreeQ:1.DisplayStr)  
    !QL.TreeLevelFieldName = 'Tree9:Level'
    !QL.SetDontBreakOnFound(True)  
    ParamsGrp.FieldsQRef &= DwrTreeQ
    ParamsGrp.TablesQRef &= DwrTreeQ:1
  
    POST(EVENT:Accepted,?DictionariesList)
    SELECT(?DictionariesList)
    
  ql17.Init(DwrTreeQ:1,?DwrTreeList:2,QueueSearchStr17,QueueSearchStr17,?SearchBtn)
  ql17.SetActiveLocator(DwrTreeQ:1.DisplayStr)
  ql17.SetDontBreakOnFound(True)
  ?SearchEntry{PROP:Use} = QueueSearchStr17
  ql11.Init(DwrTreeQ,?DwrTreeList,QueueSearchStr11,QueueSearchStr11,?SearchBtn:2)
  ql11.SetActiveLocator(DwrTreeQ.DisplayStr)
  ql11.SetDontBreakOnFound(True)
  ?SearchEntry:2{PROP:Use} = QueueSearchStr11
  ql17.SetSearchType(SettingsGrp.DefaultSearchType)
  ql11.SetSearchType(SettingsGrp.DefaultSearchType) 
  
  
  
  !    If Records(TableTypes) = 0
  !        TblTypes:PKGuid = Glo:st.MakeGuid()
  !        TblTypes:Description = 'Pool'
  !        TblTypes:TypeOrder = 1
  !        Access:TableTypes.Insert()
  !        TblTypes:PKGuid = Glo:st.MakeGuid()
  !        TblTypes:Description = 'Global'
  !        TblTypes:TypeOrder = 2
  !        Access:TableTypes.Insert()
  !        TblTypes:PKGuid = Glo:st.MakeGuid()
  !        TblTypes:Description = 'Table'
  !        TblTypes:TypeOrder = 3
  !        Access:TableTypes.Insert()
  !    End
  
  !    If Records(DriversLkUp) = 0
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'ADO'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'ASCII'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'BASIC'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'Btrieve'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'Clarion'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'Clipper'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'dBase3'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'dBase4'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'DOS'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'FoxPro'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'MEMORY'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'MSSQL'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'ODBC'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'Oracle'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'Scalable'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'SQLAnywhere'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'SQLite'
  !        Access:DriversLkUp.Insert()
  !        Dri:GUID = gLO:ST.MakeGuid()
  !        Dri:Driver = 'TOPSPEED'
  !        Access:DriversLkUp.Insert()
  !    End
  
    If Records(DataTypesLkUp) = 0
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'BLOB'
        Dat:DisplayValue  =  'BLOB'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'BYTE'
        Dat:DisplayValue  =  'BYTE'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'CSTRING'
        Dat:DisplayValue  =  'CSTRING'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'DATE'
        Dat:DisplayValue  =  'DATE'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'DECIMAL'
        Dat:DisplayValue  =  'DECIMAL'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'GROUP'
        Dat:DisplayValue  =  'GROUP'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'LIKE'
        Dat:DisplayValue  =  'LIKE'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'LONG'
        Dat:DisplayValue  =  'LONG'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'MEMO'
        Dat:DisplayValue  =  'MEMO'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'PICTURE'
        Dat:DisplayValue  =  'PICTURE'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'PSTRING'
        Dat:DisplayValue  =  'PSTRING'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'QUEUE'
        Dat:DisplayValue  =  'QUEUE'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'REAL'
        Dat:DisplayValue  =  'REAL'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'SHORT'
        Dat:DisplayValue  =  'SHORT'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'SREAL'
        Dat:DisplayValue  =  'SREAL'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'STRING'
        Dat:DisplayValue  =  'STRING'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'TIME'
        Dat:DisplayValue  =  'TIME'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'ULONG'
        Dat:DisplayValue  =  'ULONG'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'USHORT'
        Dat:DisplayValue  =  'USHORT'
        Dat:Usages        =  'TGP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'TYPE'
        Dat:DisplayValue  =  'TYPE'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'ANY'
        Dat:DisplayValue  =  'ANY'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'ASTRING'
        Dat:DisplayValue  =  'ASTRING'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'BFLOAT4'
        Dat:DisplayValue  =  'BFLOAT4'
        Dat:Usages        =  ' GP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'BFLOAT8'
        Dat:DisplayValue  =  'BFLOAT8'
        Dat:Usages        =  ' GP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'BSTRING'
        Dat:DisplayValue  =  'BSTRING'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'CLASS'
        Dat:DisplayValue  =  'CLASS'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'FILE'
        Dat:DisplayValue  =  'FILE'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'KEY'
        Dat:DisplayValue  =  'KEY'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'PDECIMAL'
        Dat:DisplayValue  =  'PDECIMAL'
        Dat:Usages        =  ' GP'
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  '&REPORT'
        Dat:DisplayValue  =  'REPORT'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'SIGNED'
        Dat:DisplayValue  =  'SIGNED'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'UNSIGNED'
        Dat:DisplayValue  =  'UNSIGNED'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  '&VIEW'
        Dat:DisplayValue  =  'VIEW'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
  
        Dat:GUID          =  Glo:st.MakeGuid()
        Dat:Type          =  'WINDOW'
        Dat:DisplayValue  =  'WINDOW'
        Dat:Usages        =  ' G '
        Access:DataTypesLkUp.Insert()
    End    
  
  !    st.LoadFile('DefaultFieldSettings.csv')
  !    st.Split('<13,10>','"')
  !    loop x = 1 to st.Records()
  !        LneSt.SetValue(st.GetLine(x))
  !        LneSt.Split(',','"','"',true)
  !        Fie:PKGuid         =  Glo:st.MakeGuid()
  !        Fie:TypeName       =  LneSt.GetLine(1)
  !        Fie:BaseType       =  LneSt.GetLine(2)
  !        Fie:DataType       =  LneSt.GetLine(3)
  !        Fie:RowPicture     =  LneSt.GetLine(4)
  !        Fie:Reference      =  LneSt.GetLine(5)
  !        Fie:Size_          =  LneSt.GetLine(6)
  !        Fie:Places         =  LneSt.GetLine(7)
  !        Fie:ScreenPicture  =  LneSt.GetLine(8)
  !        Fie:ScreenPrompt   =  LneSt.GetLine(9)
  !        Fie:ReportHeading  =  LneSt.GetLine(10)
  !        Fie:Justification  =  LneSt.GetLine(11)
  !        Fie:Offset         =  LneSt.GetLine(12)
  !        Fie:ReportControl  =  LneSt.GetLine(13)
  !        Fie:WindowControl  =  LneSt.GetLine(14)
  !        Fie:Validity       =  LneSt.GetLine(15)
  !        Access:FieldDefaultsLkUp.Insert()
  !        !Loop y = 1 to LneSt.Records()
  !        !End
  !    End    
  !    MyDct.Trace('')
  !    MyDct.Trace('===========================================================================================================================')
  !    MyDct.Trace('')
  !    MyDct.Trace(MyDct.XmlGetWindowControl('ENTRY','Def','StringDefault','@s20'))
  !    MyDct.Trace('')
  !    MyDct.Trace(MyDct.XmlGetWindowControl('SPIN','Def','SpinControl','@s20'))
  !    MyDct.Trace('')
  !    MyDct.Trace(MyDct.XmlGetWindowControl('STRING','Def','StringControl','@s20'))
  !    MyDct.Trace('')
  !    MyDct.Trace(MyDct.XmlGetWindowControl('TEXT','Def','TextControl','@s20'))
  !    MyDct.Trace('')
  !    MyDct.Trace('===========================================================================================================================')
  !    MyDct.Trace('')
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
    Loop x = 1 to Records(HostMgrQ)
        Get(HostMgrQ,x)
        !DonutHoleHost:5.Close(HostMgrQ.ThreadNo)
    End
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('Main3', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Aliases.Close()
    Relate:DataTypesLkUp.Close()
    Relate:FieldDefaultsLkUp.Close()
    Relate:KitchenSink.Close()
    Relate:TableGlobals.Close()
    Relate:TablePools.Close()
    Relate:TableTypes.Close()
  END
    NotifyManager.DeleteProcedure('Main3')
  csResize.Update()
  ql17.Kill()
  ql11.Kill()
  MyDct.SaveSettings(SettingsGrp)
  IF SELF.Opened
    INIMgr.Update('Main3',Window)                          ! Save window data to non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.Kill()
  NYS:DockingPane_EventMgr.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

!lNewFormThread    LONG
  CODE
  ReturnValue = PARENT.Run(Number,Request)
    !MyDct.Trace('')
    !MyDct.Trace('Main3 - ThisWindow.Run - Number = ' & Number & ' Request = ' & Request)
  
  !execute Number
  !  lNewFormThread = START(TestDonutWindow,50000)
  !end!if  
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    If FIELD() <> ?BulkBtn
        If FIELD() <> ?DwrTreeList And FIELD() <> ?DwrTreeList:2
            ?DwrTreeList:2{PROP:Mark} = ''
        End
    End  
    CASE ACCEPTED()
    OF ?SaveBtn
      !ParamsGrp.Action = 'SaveRecord'
      !json.Start()
      !json.SetTagCase(jF:CaseAsIs)
      !json.Save(ParamsGrp,st)
      !NotifyManager.NotifyProcedure(ParamsGrp.ProcedureToNotify[1:Size(ParamsGrp.ProcedureToNotify)],st.GetValue())    
      ?SaveBtn{PROP:Disable} = True  
      MyDct.SendNotifyJson(NotifyManager,ParamsGrp.ProcedureToNotify[1:Size(ParamsGrp.ProcedureToNotify)],'Main3','RecordSaved',OverStr)
      
    OF ?BulkBtn
      ?DwrTreeList:2{PROP:Mark} = DwrTreeQ:1.WinMark
      DonutHoleHost:5.TryOpen(START(BulkActionsWindow,50000,OverStr,Thread()))      
    OF ?SettingsBtn
      DonutHoleHost:5.TryOpen(START(SettingsWindow,50000,OverStr,Thread()))       
    OF ?BUTTON1
      !MyDct.ResetOrder(Tables,'FKTabParentGuidAndOrderKey','ParentGUID',ParamsGrp.DictionaryGuid)
      !Message(MyDct.SplitPromptText('DonaldWayneRidley'))      
    OF ?DuplicateKeysBtn
      DonutHoleHost:5.TryOpen(START(DuplicateKeysWindow,50000,OverStr,Thread()))      
    OF ?DwrTreeList
      !MyDct.Trace('')
      !MyDct.Trace('Main3 - ?DwrTreeList EVENT:Accepted')
      !MyDct.Trace('')
        local.SetFieldGuids(?DwrTreeList{PROPLIST:MouseDownRow})
        Clear(MoveMouseDownRow)
      	get(DwrTreeQ,choice(?DwrTreeList))
        MoveMouseDownRow = ?DwrTreeList{PROPLIST:MouseDownRow}
        ParamsGrp.FieldGuid = DwrTreeQ.ItemGuid
        ParamsGrp.ActiveGuid = DwrTreeQ.ItemGuid
        ParamsGrp.Order = DwrTreeQ.Ndx
      	  	Case Clip(DwrTreeQ.ItemType)
      		Of 'Field'	!UpdateFieldThread
      			!MyDct.Trace('Field Selected')
      		    ParamsGrp.FieldGuid     =  DwrTreeQ.ItemGuid
                ParamsGrp.RecordAction  =  ChangeRecord
                DonutHoleHost:5.TryOpen(START(UpdateField,50000,OverStr,Thread()))
                !Hide(?DonutHoleHostRegion)
                !HostMgrQ.PkGuid = DwrTreeQ.ItemGuid
                !Get(HostMgrQ,HostMgrQ.PkGuid)
                !If Errorcode()
                !  HostMgrQ.PkGuid = DwrTreeQ.ItemGuid
                !  HostMgrQ.ThreadNo = START(UpdateField,5000,OverStr,Thread())
                !  Add(HostMgrQ)
                !  DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
                !ELSE
                !  OldThread = HostMgrQ.ThreadNo
                !  HostMgrQ.ThreadNo = START(UpdateField,5000,OverStr,Thread())
                !  DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
                !  Put(HostMgrQ)
                !  !DonutHoleHost:5.Close(OldThread)
                !End    
                !UNHIDE(?DonutHoleHostRegion)
                !local.CloseUnusedDonutHoles(DwrTreeQ.ItemGuid)
      
      
      !      			If Not UpdateFieldThread
      !      				GlobalRequest = ChangeRecord
      !      				UpdateFieldThread = START(UpdateField,5000,OverStr,Thread())
      !      				DonutHoleHost:5.TryOpen(UpdateFieldThread)
      !      				!ActiveDonutThread = UpdateFieldThread
      !      			ELSE
      !      				DonutHoleHost:5.SwitchTo(UpdateFieldThread)
      !      				json.Start()
      !      				json.SetTagCase(jF:CaseAsIs)
      !      				json.Save(ParamsGrp,st)
      !      				NotifyManager.NotifyProcedure('UpdateField',st.GetValue())
      !      			End
      !      				local.SetActiveDonutThread(UpdateFieldThread)
      		Of 'Key'	!UpdateKeyThread
      			!MyDct.Trace('Key Selected')
      			ParamsGrp.KeyGuid       =  DwrTreeQ.ItemGuid
                ParamsGrp.RecordAction  =  ChangeRecord
                DonutHoleHost:5.TryOpen(START(UpdateKey,50000,OverStr,Thread()))
      
                
                !Hide(?DonutHoleHostRegion)
                !HostMgrQ.PkGuid = DwrTreeQ.ItemGuid
                !Get(HostMgrQ,HostMgrQ.PkGuid)
                !If Errorcode()
                !  HostMgrQ.PkGuid = DwrTreeQ.ItemGuid
                !  HostMgrQ.ThreadNo = START(UpdateKey,5000,OverStr,Thread())
                !  Add(HostMgrQ)
                !  DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
                !ELSE
                !  OldThread = HostMgrQ.ThreadNo
                !  HostMgrQ.ThreadNo = START(UpdateKey,5000,OverStr,Thread())
                !  DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
                !  Put(HostMgrQ)
                !  !DonutHoleHost:5.Close(OldThread)
                !End    
                !UNHIDE(?DonutHoleHostRegion)
                !local.CloseUnusedDonutHoles(DwrTreeQ.ItemGuid)
      
      
      !      			If Not UpdateKeyThread
      !      				GlobalRequest = ChangeRecord
      !      				UpdateKeyThread = START(UpdateKey,5000,OverStr,Thread())
      !      				DonutHoleHost:5.TryOpen(UpdateKeyThread)
      !      				!ActiveDonutThread = UpdateKeyThread
      !      			ELSE
      !      				DonutHoleHost:5.SwitchTo(UpdateKeyThread)
      !      				json.Start()
      !      				json.SetTagCase(jF:CaseAsIs)
      !      				json.Save(ParamsGrp,st)
      !      				NotifyManager.NotifyProcedure('UpdateKey',st.GetValue()) 
      !      			End    	
      !      				local.SetActiveDonutThread(UpdateKeyThread)
      		Of 'Trigger'	!UpdateTriggerThread
      			!MyDct.Trace('Trigger Selected')
      			ParamsGrp.TriggerGuid   =  DwrTreeQ.ItemGuid
                ParamsGrp.RecordAction  =  ChangeRecord
                DonutHoleHost:5.TryOpen(START(UpdateTrigger,50000,OverStr,Thread()))
                !Hide(?DonutHoleHostRegion)
                !HostMgrQ.PkGuid = DwrTreeQ.ItemGuid
                !Get(HostMgrQ,HostMgrQ.PkGuid)
                !If Errorcode()
                !  HostMgrQ.PkGuid = DwrTreeQ.ItemGuid
                !  HostMgrQ.ThreadNo = START(UpdateTrigger,5000,OverStr,Thread())
                !  Add(HostMgrQ)
                !  DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
                !ELSE
                !  OldThread = HostMgrQ.ThreadNo
                !  HostMgrQ.ThreadNo = START(UpdateTrigger,5000,OverStr,Thread())
                !  DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
                !  Put(HostMgrQ)
                !  !DonutHoleHost:5.Close(OldThread)
                !End    
                !UNHIDE(?DonutHoleHostRegion)
                !local.CloseUnusedDonutHoles(DwrTreeQ.ItemGuid)
      
      !      			If Not UpdateTriggerThread
      !      				GlobalRequest = ChangeRecord
      !      				UpdateTriggerThread = START(UpdateTrigger,5000,OverStr,Thread())
      !      				DonutHoleHost:5.TryOpen(UpdateTriggerThread)
      !      				!ActiveDonutThread = UpdateTriggerThread
      !      			ELSE
      !      				DonutHoleHost:5.SwitchTo(UpdateTriggerThread)
      !      				json.Start()
      !      				json.SetTagCase(jF:CaseAsIs)
      !      				json.Save(ParamsGrp,st)
      !      				NotifyManager.NotifyProcedure('UpdateTrigger',st.GetValue()) 
      !      			End    	  
      !      				local.SetActiveDonutThread(UpdateTriggerThread)    				
      		Of 'Relation'	!UpdateRelationThread
      			!MyDct.Trace('Relation Selected')
      			ParamsGrp.RelationGuid  =  DwrTreeQ.ItemGuid
                ParamsGrp.RecordAction  =  ChangeRecord
                DonutHoleHost:5.TryOpen(START(UpdateRelation,50000,OverStr,Thread()))
      
                !Hide(?DonutHoleHostRegion)
                !HostMgrQ.PkGuid = DwrTreeQ.ItemGuid
                !Get(HostMgrQ,HostMgrQ.PkGuid)
                !If Errorcode()
                !  HostMgrQ.PkGuid = DwrTreeQ.ItemGuid
                !  HostMgrQ.ThreadNo = START(UpdateRelation,5000,OverStr,Thread())
                !  Add(HostMgrQ)
                !  DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
                !ELSE
                !    !If HostMgrQ.NeedsSave = False
                !        OldThread = HostMgrQ.ThreadNo
                !        HostMgrQ.ThreadNo = START(UpdateRelation,5000,OverStr,Thread())
                !        DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
                !        Put(HostMgrQ)
                !        !DonutHoleHost:5.Close(OldThread)
                !    !End
                !End    
                !UNHIDE(?DonutHoleHostRegion)
                !local.CloseUnusedDonutHoles(DwrTreeQ.ItemGuid)
      
      
      !      			If Not UpdateRelationThread
      !      				GlobalRequest = ChangeRecord
      !      				UpdateRelationThread = START(UpdateRelation,5000,OverStr,Thread())
      !      				DonutHoleHost:5.TryOpen(UpdateRelationThread)
      !      				!ActiveDonutThread = UpdateRelationThread
      !      			ELSE
      !      				DonutHoleHost:5.SwitchTo(UpdateRelationThread)
      !      				json.Start()
      !      				json.SetTagCase(jF:CaseAsIs)
      !      				json.Save(ParamsGrp,st)
      !      				NotifyManager.NotifyProcedure('UpdateRelation',st.GetValue()) 
      !      			End    	
      !      		    local.SetActiveDonutThread(UpdateRelationThread)    
      		End
      		!MyDct.Trace('')
      		!MyDct.Trace('local.GetActiveDonutThread() = ' & local.GetActiveDonutThread() & ' <<<<<<<<---------------------------------------------------------------')
      		!MyDct.Trace('')
      		!DonutHoleHost:5.RepaintDonutholeWindow() 
    OF ?DictionariesList
      !      !MyDct.Trace('')
      !      !MyDct.Trace('Main3 - ?DictionariesList EVENT:Accepted')
      !      !MyDct.Trace('')
      !      get(Queue:Browse,choice(?DictionariesList))
      !      ParamsGrp.DictionaryGuid = Queue:Browse.Dct:GUID
      !      ParamsGrp.ActiveGuid = Queue:Browse.Dct:GUID
      !      
      !      !local.PopulateTableTypesTree()  
      !      
      !      Hide(?DonutHoleHostRegion)
      !      DonutHoleHost:5.TryOpen(START(UpdateDictionary,5000,OverStr,Thread()))
      !      Unhide(?DonutHoleHostRegion)
      !      
      !      !HostMgrQ.PkGuid = Queue:Browse.Dct:GUID
      !      !Get(HostMgrQ,HostMgrQ.PkGuid)
      !      !If Errorcode()
      !      !  HostMgrQ.PkGuid = Queue:Browse.Dct:GUID
      !      !  HostMgrQ.ThreadNo = START(UpdateDictionary,5000,OverStr,Thread())
      !      !  Add(HostMgrQ)
      !      !  DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
      !      !ELSE
      !      !  OldThread = HostMgrQ.ThreadNo
      !      !  HostMgrQ.ThreadNo = START(UpdateDictionary,5000,OverStr,Thread())
      !      !  DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
      !      !  Put(HostMgrQ)
      !      !  !DonutHoleHost:5.Close(OldThread)
      !      !End   
      !      
      !      
      !      
      !      SELECT(?DictionariesList)
      
      
      
      
      
      
      !UNHIDE(?DonutHoleHostRegion)
      !local.CloseUnusedDonutHoles(Queue:Browse.Dct:GUID)
      
      !If Not UpdateDictionaryThread
      !  GlobalRequest = ChangeRecord
      !  UpdateDictionaryThread = START(UpdateDictionary,5000,OverStr,Thread())
      !  DonutHoleHost:5.TryOpen(UpdateDictionaryThread)
      !ELSE
      !  DonutHoleHost:5.SwitchTo(UpdateDictionaryThread)
      !  json.Start()
      !  json.SetTagCase(jF:CaseAsIs)
      !  json.Save(ParamsGrp,st)
      !  NotifyManager.NotifyProcedure('UpdateDictionary',st.GetValue())
      !End
      !local.SetActiveDonutThread(UpdateDictionaryThread)
      !MyDct.Trace('')
      !MyDct.Trace('local.GetActiveDonutThread() = ' & local.GetActiveDonutThread() & ' <<<<<<<<---------------------------------------------------------------')
      !MyDct.Trace('')
    OF ?AddNewTableBtn
      ParamsGrp.GlobalRequest  =  InsertRecord
      !ParamsGrp.TableGuid = ''
      ParamsGrp.Action  =  InsertRecord
      !DonutHoleHost:5.TryOpen(START(UpdateTable,5000,OverStr,Thread()))   
      DonutHoleHost:5.TryOpen(START(UpdateTable,50000,ParamsGrp,Thread()))    
    OF ?AddNewFieldBtn
      ParamsGrp.GlobalRequest  =  InsertRecord
      !ParamsGrp.TableGuid = ''
      ParamsGrp.Action        =  InsertRecord
      ParamsGrp.RecordAction  =  ChangeRecord
      DonutHoleHost:5.TryOpen(START(UpdateField,50000,ParamsGrp,Thread()))       
    OF ?DctSearch
      !      TempQPointer = MyDct.LocateOnQueue(Queue:Browse,Clip(DctSearch),'Dct:DctName')
      !      Select(?DictionariesList,TempQPointer)
      !      !MyDct.Trace('TempQPointer = ' & TempQPointer)      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?ImportBtn
      ThisWindow.Update()
      DctFile = FileLookup7.Ask(1)
      DISPLAY
      !MyDCT.LoadFile(DctFile)
      Start(LoadDctx,,DctFile,OverStr,Thread())
      !      Dct:GUID = MyDct.GetDictionaryGuid()
      !      Access:Dictionaries.Fetch(Dct:PKDctGUIDKey)
      !      !MyDct.Trace('')
      !      !MyDct.Trace('============================================================================================================================================')
      !      !MyDct.Trace('')
      !      !MyDct.Trace('After Loading DCTX')
      !      !MyDct.Trace('<9>Dct:GUID = ' & Dct:GUID)
      !      !MyDct.Trace('<9>Records(Dictionaries) = ' & Records(Dictionaries))
      !      If Errorcode()
      !        !MyDct.Trace('<9>Error Fetching Dictionaries Record - Error = ' & ERROR())
      !      ELSE
      !        !MyDct.Trace('<9>Dictionaries Record Fetched!')
      !        BrwDct.ResetFromFile()
      !        ThisWindow.Reset(True) 
      !        POST(EVENT:Accepted,?DictionariesList)
      !        SELECT(?DictionariesList)
      !      End
      !MyDct.Trace('')
      !MyDct.Trace('============================================================================================================================================')
      !MyDct.Trace('')
      
      !BrwDct.ResetSort(True)
      !If Records(Dictionaries) = 1
      !  BrwDct.ResetSort(True)
      !  POST(EVENT:Accepted,?DictionariesList)
      !Else
        
      !End
      !POST(EVENT:NewSelection,?DictionariesList)
      !BrwDct.ResetQueue(Reset:Queue)
      !Queue:Browse.Dct:GUID = MyDct.GetDictionaryGuid()
      !get(Queue:Browse,Queue:Browse.Dct:GUID)
      !IF Queue:Browse.Dct:GUID
      !   BrwDct.StartAtCurrent=True
      !   !POH:Status = ''
      !   !POH:PO_NBR = pPONumber
      !END 
      
      !BrwTables.ResetSort(True)
      !Do REL9::RefreshTree
      !SORT(Queue:RelTree,Queue:RelTree.REL9::Display)
      !POST(EVENT:Accepted,?DictionariesList)
      !ParamsGrp.Action = 'Refresh'
      !json.Start()
      !json.SetTagCase(jF:CaseAsIs)
      !json.Save(ParamsGrp,st) 
      !NotifyManager.NotifyProcedure('BrowseDictionaries',st.GetValue())
      !NotifyManager.NotifyProcedure('TreeWindow',st.GetValue())
    OF ?ExportBtn
      ThisWindow.Update()
      START(BuildDctx2, 25000, OverStr,Thread())
      ThisWindow.Reset
    OF ?BUTTON1
      ThisWindow.Update()
      START(CreateSqlFields, 25000, OverStr,Thread())
      ThisWindow.Reset
    OF ?DictionariesList
      !MyDct.Trace('')
      !MyDct.Trace('Main3 - ?DictionariesList EVENT:Accepted')
      !MyDct.Trace(' choice(?DictionariesList) = ' & choice(?DictionariesList))
      !MyDct.Trace('')
      get(Queue:Browse,choice(?DictionariesList))
      ParamsGrp.DictionaryGuid = Queue:Browse.Dct:GUID
      ParamsGrp.ActiveGuid = Queue:Browse.Dct:GUID
      ParamsGrp.GlobalRequest = ChangeRecord
      Post(DctEvent:PopulateTablesTree) 
      !If Keycode() = MouseLeft
        DonutHoleHost:5.TryOpen(START(UpdateDictionary,50000,OverStr,Thread()))
      !End
      POST(EVENT:Accepted,?DwrTreeList:2)
    OF ?DwrTreeList:2
      !MyDct.Trace('?DwrTreeList:2 - EVENT:Accepted')
      
      
      
      get(DwrTreeQ:1,choice(?DwrTreeList:2)) 
      ParamsGrp.TableGuid            =  DwrTreeQ:1.ItemGuid !Tab:PKGuid !Queue:Browse:1.Tab:PKGuid
      ParamsGrp.RelationPrimaryGuid  =  DwrTreeQ:1.WinGuid !DwrTreeQ:1.ItemGuid  !Tab:Guid
      ParamsGrp.ActiveGuid           =  DwrTreeQ:1.ItemGuid !Tab:PKGuid !Queue:Browse:1.Tab:PKGuid
      ParamsGrp.Usage                =  DwrTreeQ:1.ItemType
      ParamsGrp.Order                =  DwrTreeQ:1.Ndx
      ParamsGrp.TablePrefix          =  DwrTreeQ:1.TablePrefix
      !MyDct.Trace(' ParamsGrp.TableGuid = ' & ParamsGrp.TableGuid)
      !MyDct.Trace(' ParamsGrp.RelationPrimaryGuid = ' & ParamsGrp.RelationPrimaryGuid)
      !MyDct.Trace(' MouseDownRow = ' & ?DwrTreeList:2{PROPLIST:MouseDownRow})
      !MyDct.Trace(' DwrTreeQ:1.WinMark = ' & DwrTreeQ:1.WinMark)
      !MyDct.Trace('')       
      If ?DwrTreeList:2{PROPLIST:MouseDownRow} > 0 and ?DwrTreeList:2{PROP:Mark} = '' !KEYCODE() = MouseLeft
        !GlobalRequest = ChangeRecord
        !ParamsGrp.Action = ChangeRecord
        ParamsGrp.GlobalRequest                =  ChangeRecord
        ?DwrTreeList:2{PROPLIST:MouseDownRow}  =  0
        DonutHoleHost:5.TryOpen(START(UpdateTable,50000,OverStr,Thread()))  
        !POST(DctEvent:PopulateTree)
      End
      
      If ?DwrTreeList:2{PROPLIST:MouseDownRow} > 0 !And ?DwrTreeList:2{Prop:Mark} = DwrTreeQ:1.RealMark
        local.SetTaggedCount()
      End
      
      If ?DwrTreeList:2{PROP:Mark} = ''
        POST(DctEvent:PopulateTree)
      End
      
      st.Start()
      json.Start()
      json.SetTagCase(jF:CaseAsIs)
      json.Save(DwrTreeQ:1,st,'',False)
      !MySQL.Trace('')
      !MySQL.Trace(st.GetValue())
      !MySQL.Trace('')
      
      !      IF KEYCODE() = MouseRight
      !        !ListMarker.TakeHotKey(CtrlU)
      !        !?DwrTreeList:2{Prop:Mark} = ''
      !        PopupString=PopupMgr.Ask()
      !        CASE PopupString               
      !        Of 'CopyDetails'
      !            !Message('CopyDetails ' & ListMarker.GetMarkCount())      !?DwrTreeList:2{PROPLIST:MouseDownRow}
      !            !SETKEYCODE(0)
      !            !ListMarker.TakeHotKey(CtrlU)  
      !        End     
      !        !?DwrTreeList:2{Prop:Mark} = DwrTreeQ:1.RealMark
      !      End      
      
      
      !REL9::CurrentChoice = ?TablesList{PROPLIST:MouseDownRow}
      !MyDct.Trace('REL9::CurrentChoice = ' & REL9::CurrentChoice)
      !GET(DwrTreeQ:1,REL9::CurrentChoice)
      !If ?DwrTreeList:2{PROPLIST:MouseDownRow} = 0
      !  ?DwrTreeList:2{PROPLIST:MouseDownRow} = 2      
      !End
      
      !get(DwrTreeQ:1,?DwrTreeList:2{PROPLIST:MouseDownRow}) 
      !MyDct.Trace('REL9::Level = ' & REL9::Level)      
        
      
        !MyDct.Trace('<9>Tab:PKGuid = ' & Tab:PKGuid)
      
        !Hide(?DonutHoleHostRegion)
      
      !        HostMgrQ.PkGuid = DwrTreeQ:1.ItemGuid !Tab:PKGuid !Queue:Browse:1.Tab:PKGuid
      !        Get(HostMgrQ,HostMgrQ.PkGuid)
      !        If Errorcode()
      !            HostMgrQ.PkGuid = Tab:PKGuid !Queue:Browse:1.Tab:PKGuid
      !            HostMgrQ.ThreadNo = START(UpdateTable,5000,OverStr,Thread())
      !            Add(HostMgrQ)
      !            DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
      !        ELSE
      !            OldThread = HostMgrQ.ThreadNo
      !            HostMgrQ.ThreadNo = START(UpdateTable,5000,OverStr,Thread())
      !            DonutHoleHost:5.TryOpen(HostMgrQ.ThreadNo)
      !            Put(HostMgrQ)
      !            !DonutHoleHost:5.Close(OldThread)
      !            !DonutHoleHost:5.SwitchTo(HostMgrQ.ThreadNo)
      !        End   
        !MyDct.Trace(POSITION(Tables)) 
      
      !ParamsGrp.TableGuid = Tab:PKGuid
        
    OF ?AddNewDctBtn
      ThisWindow.Update()
      ParamsGrp.GlobalRequest  =  InsertRecord
      !ParamsGrp.TableGuid = ''
      ParamsGrp.Action  =  InsertRecord
      !DonutHoleHost:5.TryOpen(START(UpdateTable,5000,OverStr,Thread()))   
      DonutHoleHost:5.TryOpen(START(UpdateDictionary,50000,ParamsGrp,Thread()))       
    OF ?MoveFieldUpBtn
      ThisWindow.Update()
        !MyDct.Trace('')
        !MyDct.Trace('?MoveFieldUpBtn')
        !MyDct.Trace('<9>MoveMouseDownRow = ' & MoveMouseDownRow)
        !MyDct.Trace('<9>PROPLIST:MouseDownRow = ' & ?DwrTreeList{PROPLIST:MouseDownRow})
        !MyDct.Trace('<9>DwrTreeQ.ItemGuid = ' & DwrTreeQ.ItemGuid)
        !MyDct.Trace('<9>DwrTreeQ.ItemType = ' & DwrTreeQ.ItemType)
        !MyDct.Trace('<9>ParamsGrp.Order = ' & ParamsGrp.Order)
        !MyDct.Trace('<9>DwrTreeQ.Ndx = ' & DwrTreeQ.Ndx)
        !MyDct.Trace('')
        
        !MoveMouseDownRow-=1
        Clear(NextIsInGroup)
        Case Clip(DwrTreeQ.ItemType)
        Of 'Field'
      
      
      
      
      
      
            !NextIsInGroup = 0
            Clear(Fld:Record)
                !MyDct.Trace('<9><9>Fld:FieldName = ' & Clip(Fld:FieldName) & '<9>DwrTreeQ.ItemGuid = ' & DwrTreeQ.ItemGuid)
                Fld:PKGuid = local.PreviousFieldGuid !DwrTreeQ.ItemGuid
                If Access:Fields.Fetch(Fld:PKFldGuidKey) = Level:Benign
                    !MyDct.Trace('')
                    !MyDct.Trace('First Fetch Good')
                    !MyDct.Trace('<9><9>Fld:FieldName = ' & Clip(Fld:FieldName) & '<9>DwrTreeQ.ItemGuid = ' & DwrTreeQ.ItemGuid)
                    !MyDct.Trace('')
                    !MyDct.Trace('Fld:LastInGroup = ' & Fld:LastInGroup & ' <<<<<<<<<<<<<<<<<<<<<<<<<<<<-----------------------------------------------------------------')
                    !MyDct.Trace('')
                    
                    If Fld:LastInGroup = 1
                        NextIsInGroup = 1
                        Fld:LastInGroup = 0
                        Fld:FieldOrder = (ParamsGrp.Order-2)+.5
                        Access:Fields.Update()
                    End
                ELSE
                    !MyDct.Trace('Main3 - ?MoveFieldUpBtn - First Fetch - Error Fetching Fields Record!')
                End
      
            !MoveMouseDownRow = ?DwrTreeList{PROPLIST:MouseDownRow}
            Clear(Fld:Record)
                Fld:PKGuid = local.SelectedFieldGuid !DwrTreeQ.ItemGuid
                If Access:Fields.Fetch(Fld:PKFldGuidKey) = Level:Benign
                    !MyDct.Trace('')
                    !MyDct.Trace('Second Fetch Good')
                    !MyDct.Trace('<9><9>Fld:FieldName = ' & Clip(Fld:FieldName) & '<9>DwrTreeQ.ItemGuid = ' & DwrTreeQ.ItemGuid)
                    !MyDct.Trace('')
                    !MyDct.Trace('Fld:LastInGroup = ' & Fld:LastInGroup & ' <<<<<<<<<<<<<<<<<<<<<<<<<<<<-----------------------------------------------------------------')
                    !MyDct.Trace('')
                    
                    If NextIsInGroup
                        !MoveMouseDownRow+=1
                        NextIsInGroup = 0
                        Fld:LastInGroup = 1
                        Fld:FieldOrder = (ParamsGrp.Order-1)    !+.5
                    Else
                        MoveMouseDownRow-=1
                        !?DwrTreeList{PROPLIST:MouseDownRow} = ?DwrTreeList{PROPLIST:MouseDownRow}-1
                        Fld:FieldOrder = (ParamsGrp.Order-2)+.5
                        !MoveMouseDownRow-=2
                        If Fld:LastInGroup
                        !    !MoveMouseDownRow+=1
                        !    !local.SetFieldGuids(MoveMouseDownRow)
                            NextIsInGroup = 1
                            Fld:LastInGroup = 0
                        End
                    End
                    Access:Fields.Update()
                    !ResetFieldsOrder(OverStr,Thread())
                ELSE
                    !MyDct.Trace('Main3 - ?MoveFieldUpBtn - Second Fetch - Error Fetching Fields Record!')
                End
      
            If NextIsInGroup
                NextIsInGroup = 0
                !MoveMouseDownRow-=1 !?DwrTreeList{PROPLIST:MouseDownRow}
                Fld:PKGuid = local.PreviousFieldGuid !local.NextFieldGuid !DwrTreeQ.ItemGuid
                If Access:Fields.Fetch(Fld:PKFldGuidKey) = Level:Benign
                    !MyDct.Trace('')
                    !MyDct.Trace('Second Fetch Good')
                    !MyDct.Trace('<9><9>Fld:FieldName = ' & Clip(Fld:FieldName) & '<9>DwrTreeQ.ItemGuid = ' & DwrTreeQ.ItemGuid)
                    !MyDct.Trace('')
                    !MyDct.Trace('Fld:LastInGroup = ' & Fld:LastInGroup & ' <<<<<<<<<<<<<<<<<<<<<<<<<<<<-----------------------------------------------------------------')
                    !MyDct.Trace('')
                    !MoveMouseDownRow+=1
                    Fld:LastInGroup = 1
                    Fld:FieldOrder = ParamsGrp.Order+1
                    Access:Fields.Update()
                    !ResetFieldsOrder(OverStr,Thread())
                ELSE
                    !MyDct.Trace('Main3 - ?MoveFieldUpBtn - Second Fetch - Error Fetching Fields Record!')
                End
            End      
            
            ResetFieldsOrder(OverStr,Thread())
        Of 'Key'
            Clear(Key:Record)
            MoveMouseDownRow-=1
            Key:PKGuid = DwrTreeQ.ItemGuid
            If Access:Keys.Fetch(Key:PKKeyGuidKey) = Level:Benign
                Key:KeyOrder = (ParamsGrp.Order-2)+.5
                !MoveMouseDownRow-=2
                !?DwrTreeList{PROPLIST:MouseDownRow} = ?DwrTreeList{PROPLIST:MouseDownRow}-1
                Access:Keys.Update()
                ResetKeysOrder(OverStr,Thread())
            ELSE
                !MyDct.Trace('Main3 - ?MoveFieldUpBtn - Error Fetching Keys Record!')
            End
        End
        
        local.PopulateTree()      
        Get(DwrTreeQ,DwrTreeQ.ItemGuid)
        If Not Errorcode()
            ?DwrTreeList{PROPLIST:MouseDownRow} = MoveMouseDownRow !?DwrTreeList{PROPLIST:MouseDownRow}-1
            SELECT(?DwrTreeList,MoveMouseDownRow)   ! ?DwrTreeList{PROPLIST:MouseDownRow}
            POST(Event:Accepted,?DwrTreeList)
        End
    OF ?MoveFieldDownBtn
      ThisWindow.Update()
        !MyDct.Trace('')
        !MyDct.Trace('?MoveFieldDownBtn')
        !MyDct.Trace('<9>DwrTreeQ.ItemGuid = ' & DwrTreeQ.ItemGuid)
        !MyDct.Trace('<9>DwrTreeQ.ItemType = ' & DwrTreeQ.ItemType)
        !MyDct.Trace('<9>ParamsGrp.Order = ' & ParamsGrp.Order)
        !MyDct.Trace('')
        Case Clip(DwrTreeQ.ItemType)
        Of 'Field'
            Clear(Fld:Record)
            Fld:PKGuid = local.NextFieldGuid !local.SelectedFieldGuid !DwrTreeQ.ItemGuid
            If Access:Fields.Fetch(Fld:PKFldGuidKey) = Level:Benign
                !MoveMouseDownRow+=1
                If Fld:LastInGroup
                    NextIsInGroup = 1
                    Fld:LastInGroup = 0
                !Else
                    !Fld:FieldOrder = ParamsGrp.Order + 1.5
                End
                Access:Fields.Update()
            ELSE
                !MyDct.Trace('Main3 - ?MoveFieldUpBtn - Error Fetching Fields Record!')
            End
      
            Clear(Fld:Record)
            Fld:PKGuid = local.SelectedFieldGuid !DwrTreeQ.ItemGuid
            If Access:Fields.Fetch(Fld:PKFldGuidKey) = Level:Benign
                MoveMouseDownRow+=1
                If Fld:LastInGroup
                    NextIsInGroup = 1
                    Fld:LastInGroup = 0
                Else
                    If NextIsInGroup
                        NextIsInGroup = 0
                        Fld:LastInGroup = 1
                    End
                    Fld:FieldOrder = ParamsGrp.Order + 1.5
                End
                Access:Fields.Update()
            ELSE
                !MyDct.Trace('Main3 - ?MoveFieldUpBtn - Error Fetching Fields Record!')
            End
      
            If NextIsInGroup
                NextIsInGroup = 0
                Clear(Fld:Record)
                Fld:PKGuid = local.PreviousFieldGuid !local.SelectedFieldGuid !DwrTreeQ.ItemGuid
                If Access:Fields.Fetch(Fld:PKFldGuidKey) = Level:Benign
                    MoveMouseDownRow-=1
                    Fld:LastInGroup = 1
                    Fld:FieldOrder = ParamsGrp.Order - 1
                    Access:Fields.Update()
                ELSE
                    !MyDct.Trace('Main3 - ?MoveFieldUpBtn - Error Fetching Fields Record!')
                End
            End
      
      
            ResetFieldsOrder(OverStr,Thread())
        Of 'Key'
            Clear(Key:Record)
            MoveMouseDownRow+=1
            Key:PKGuid = DwrTreeQ.ItemGuid
            If Access:Keys.Fetch(Key:PKKeyGuidKey) = Level:Benign
                Key:KeyOrder = ParamsGrp.Order + 1.5
                Access:Keys.Update()
            ELSE
                !MyDct.Trace('Main3 - ?MoveFieldUpBtn - Error Fetching Keys Record!')
            End
            ResetKeysOrder(OverStr,Thread())
        End 
        local.PopulateTree()   
        Get(DwrTreeQ,DwrTreeQ.ItemGuid)
        If Not Errorcode()
            ?DwrTreeList{PROPLIST:MouseDownRow} = MoveMouseDownRow !?DwrTreeList{PROPLIST:MouseDownRow}+1
            SELECT(?DwrTreeList,?DwrTreeList{PROPLIST:MouseDownRow})
            POST(Event:Accepted,?DwrTreeList)  
        End
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeCloseEvent PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.TakeCloseEvent()
    if ReturnValue = Level:Benign
      DonutHoleHost:5.Kill()
   end!if
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  csResize.TakeEvent()
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    Case EVENT()
    Of DctEvent:PopulateTree
        local.PopulateTree()
    Of DctEvent:PopulateTablesTree
        local.PopulateTableTypesTree()
        !QL.Init(DwrTreeQ:1,?DwrTreeList:2,TableSearch,?TableSearch)
        !QL.SetActiveLocator(DwrTreeQ:1.DisplayStr)
    End  
    
  !    CASE FIELD()
  !    OF ?DwrTreeList:2                        !Save button mimiced by Save item
  !        CASE EVENT()
  !        OF EVENT:Accepted   ! EVENT:AlertKey
  !            IF KEYCODE() = MouseRight
  !                PopupString=PopupMgr.Ask()
  !                CASE PopupString               
  !                Of 'CopyDetails'
  !                    ?DwrTreeList:2{Prop:Mark} = ''
  !                    Message('CopyDetails ' & ?DwrTreeList:2{PROPLIST:MouseDownRow})
  !                    !SETKEYCODE(0)
  !                    ?DwrTreeList:2{Prop:Mark} = DwrTreeQ:1.RealMark
  !                End     
  !            End
  !  
  !            !PopupMgr.Save('MyPopup')      !save menu definition to INI
  !            !RUN('NotePad '&INIFile)       !display/edit menu definition
  !            !ENABLE(?Restore)              !enable the Restore button
  !            !PopupMgr.SetItemEnable('Restore',True)    !enable the Restore item
  !        END
  !    End
  ReturnValue = PARENT.TakeEvent()
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeEvent(EVENT())
    IF NYS:DockingPane_EventMgr.TakeEventCycleRequired = TRUE THEN CYCLE.
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  csVSplit16.TakeEvent()
  csHSplit6.TakeEvent()
  csHSplit14.TakeEvent()
  If event() = event:VisibleOnDesktop !or event() = event:moved
    ds_VisibleOnDesktop()
  end
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeFieldEvent()
  CASE FIELD()
  OF ?DwrTreeList:2
    CASE EVENT()
    OF EVENT:Expanding
      !MyDct.Trace('Main3 - ?DwrTreeList:2 - EVENT:Expanding')      
    OF EVENT:Contracting
      !MyDct.Trace('Main3 - ?DwrTreeList:2 - EVENT:Contracting')       
    OF EVENT:Expanded
      !MyDct.Trace('Main3 - ?DwrTreeList:2 - EVENT:Expanded')
      TablesListContracted = Choose(TablesListContracted=1,0,1)        
      local.PopulateTableTypesTree()
    OF EVENT:Contracted
      !MyDct.Trace('Main3 - ?DwrTreeList:2 - EVENT:Contracted')     
      TablesListContracted = Choose(TablesListContracted=0,1,0) 
      local.PopulateTableTypesTree()
    END
  OF ?SearchBtn
    CASE EVENT()
    OF EVENT:AlertKey
      PopupString = TablesPopupMgr.Ask()
      !MyDct.Trace('')
      !MyDct.Trace('Main3 - ?SearchBtn - EVENT:AlertKey - PopupString')
      !MyDct.Trace(PopupString)
      Case PopupString
      Of 'Contains'
        ql17.SetSearchType(Queue:Contains)
      Of 'StartsWith'
        ql17.SetSearchType(Queue:StartsWith)
      End   
    END
  OF ?SearchBtn:2
    CASE EVENT()
    OF EVENT:AlertKey
      PopupString = FieldsPopupMgr.Ask()
      !MyDct.Trace('')
      !MyDct.Trace('Main3 - ?SearchBtn:2 - EVENT:AlertKey - PopupString')
      !MyDct.Trace(PopupString)
      Case PopupString
      Of 'Contains'
        ql11.SetSearchType(Queue:Contains)
      Of 'StartsWith'
        ql11.SetSearchType(Queue:StartsWith)
      End               
    END
  END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeNewSelection PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all NewSelection events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeNewSelection()
    CASE FIELD()
    OF ?DwrTreeList:2
      !MyDct.Trace('Main3 - ?DwrTreeList:2 - EVENT:NewSelection')   
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?DwrTreeList:2
      !MyDct.Trace('Main3 - ?DwrTreeList:2 - EVENT:Selected')       
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE EVENT()
    OF EVENT:CloseDown
      if WE::CantCloseNow
        WE::MustClose = 1
        cycle
      else
        self.CancelAction = cancel:cancel
        self.response = requestcancelled
      end
    END
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeWindowEvent(Window)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:GainFocus
      !  ParamsGrp.TableGuid = Tab:PKGuid
      !  POST(DctEvent:PopulateTree)   
    OF EVENT:OpenWindow
        post(event:visibleondesktop)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

json.ValidateRecord      Procedure()
RetVal      Long
    CODE
    !If DwrTreeQ:1.Level =< 0 Or DwrTreeQ:1.WinMark = False !Or DwrTreeQ:1.ItemType <> 'Table'
    !    RetVal = JF:Filtered
    !    Return RetVal
    !End
    If DwrTreeQ:1.WinMark = TRUE    
        !MySQL.Trace('json.ValidateRecord - ' & Clip(DwrTreeQ:1.DisplayStr) & ' ' & DwrTreeQ:1.Level & ' ' & DwrTreeQ:1.WinMark)
        Return jf:OK
    ELSE
        Return JF:Filtered
    End
    


    RetVal = PARENT.ValidateRecord()

    

    Return RetVal

local.ClearMarked                 Procedure()
    code
    !MyDct.Trace('')
    !MyDct.Trace('local.ClearMarked')
    !MyDct.Trace('')
    Loop x = 1 to Records(DwrTreeQ:1)
        Get(DwrTreeQ:1,x)
        DwrTreeQ:1.WinMark = 0
        Put(DwrTreeQ:1)
    End      
    ParamsGrp.TaggedCnt = 0
    ?DwrTreeList:2{PROP:Mark} = ''
    !MyDct.SendNotifyJson(NotifyManager,'BulkActionsWindow','Main3','',OverStr)
    Display(?DwrTreeList:2)

local.CloseUnusedDonutHoles       Procedure(string PkGuid)
x           Long
    code
    Loop x = 1 to Records(HostMgrQ)
        Get(HostMgrQ,x)
        If Not Errorcode()
            If HostMgrQ.PkGuid <> PkGuid[1:Size(PkGuid)]
                !DonutHoleHost:5.Close(HostMgrQ.ThreadNo)
            End
        End
    End

local.GetActiveDonutThread			Procedure()		!,Long
	CODE
	Return self.ActiveDonutThread

local.SetActiveDonutThread			Procedure(long pThread)
	CODE
	self.ActiveDonutThread = pThread
	!?DonutHoleHostRegion{PROP:Hide} = TRUE	
	!Display(?DonutHoleHostRegion)
	!DonutHoleHost:5.RepaintDonutholeWindow()
	!?DonutHoleHostRegion{PROP:Hide} = False	
	!DonutHoleHost:5.RepaintDonutholeWindow()
	Display(?DonutHoleHostRegion)
	!DonutHoleHost:5.RepaintDonutholeWindow()

local.CloseActiveDonutHole			Procedure()
	code
	If self.GetActiveDonutThread()
		!DonutHoleHost:5.Close(self.GetActiveDonutThread())
	END

local.PopulateTree                  Procedure()
LastTableName       String(100)
InGroup             Long
IndentBy            Long
GrpCnt              Long
PrevWasLast         LONG
    CODE
    !MyDct.Trace('')
    !MyDct.Trace('Main3 - local.PopulateTree')

    IndentBy = 2
    Free(GroupEndsQ)
    Free(DwrTreeQ)

    DwrTreeQ.DisplayStr = 'Columns'
    DwrTreeQ.NormalFG = COLOR:Black
    DwrTreeQ.NormalBG = COLOR:White
    DwrTreeQ.SelectedFG = COLOR:Black
    DwrTreeQ.SelectedBG = COLOR:White
    DwrTreeQ.Icon = 2
    DwrTreeQ.Level = 0           !Closed=-1,Child=2
    Add(DwrTreeQ)  

    !DwrTreeQ.DisplayStr = 'Don'
    !DwrTreeQ.NormalFG = COLOR:Black
    !DwrTreeQ.NormalBG = COLOR:White
    !DwrTreeQ.SelectedFG = COLOR:White
    !DwrTreeQ.SelectedBG = COLOR:Blue
    !DwrTreeQ.Icon = 3
    !DwrTreeQ.Level = 2           !Closed=-1,Child=2
    !Add(DwrTreeQ)  

    !DwrTreeQ.DisplayStr = 'Don'
    !DwrTreeQ.NormalFG = COLOR:Black
    !DwrTreeQ.NormalBG = COLOR:White
    !DwrTreeQ.SelectedFG = COLOR:White
    !DwrTreeQ.SelectedBG = COLOR:Blue
    !DwrTreeQ.Icon = 3
    !DwrTreeQ.Level = 2           !Closed=-1,Child=2
    !Add(DwrTreeQ)  

    InGroup = 0
    SqlCreateSt.Start()
    !SqlCreateSt.SetValue('CREATE TABLE dbo.' & Clip(Tab:TableName) & ' (<13,10>')

    Open(FieldsView)
    FieldsView{PROP:Order} = 'Fld:FieldOrder'
    FieldsView{PROP:Filter} = '(Fld:ParentGUID = ' & '''' & ParamsGrp.TableGuid & '''' & ')'
    !MyDct.Trace('<9>FieldsView{{PROP:Filter} = ' & FieldsView{PROP:Filter})
    Set(FieldsView)
    Loop
        Next(FieldsView)
        If Errorcode(); Break End


        
        SqlCreateSt.AppendA('<9>' & Clip(Fld:FieldName) & '<32>' & MySQL.GetSqlDataType(Fld:DataType,Fld:FieldSize) & ' NULL',True,',<13,10>')

        Tab:PKGuid = Fld:ParentGUID
        If Access:Tables.Fetch(Tab:PKTabGuidKey) <> Level:Benign
            !MyDct.Trace('Main3 - local.PopulateTree - Error Fetching Tables Record!')
        End

        If Fld:LastInGroup !And InGroup = 1 !And PrevWasLast = 0
            GrpCnt   -= 1
            !IndentBy -=1
        End

        If Fld:DataType = 'GROUP'
            DwrTreeQ.DisplayStr = Clip(Tab:TablePrefix) & ':' & Clip(Fld:FieldName) & ' - Group'
            GrpCnt   += 1
            IndentBy +=1
            InGroup = 1
        ElsIf Fld:DataType = 'QUEUE'
            DwrTreeQ.DisplayStr = Clip(Tab:TablePrefix) & ':' & Clip(Fld:FieldName) & ' - Queue'
            GrpCnt   += 1
            IndentBy +=1
            InGroup = 1
        Else
            DwrTreeQ.DisplayStr = Clip(Tab:TablePrefix) & ':' & Fld:FieldName
            !If Fld:LastInGroup And InGroup = 1 And PrevWasLast = 0
            !    IndentBy -=1
            !End
        End

        !MySQL.Trace('['& Clip(Fld:FieldName) &'] Fld:LastInGroup['& Fld:LastInGroup &'] GrpCnt['& GrpCnt &'] InGroup['& InGroup &'] IndentBy['& IndentBy &'] PrevWasLast['& PrevWasLast &']')

        DwrTreeQ.NormalFG    =  COLOR:Black
        DwrTreeQ.NormalBG    =  COLOR:White
        DwrTreeQ.SelectedFG  =  COLOR:Black
        DwrTreeQ.SelectedBG  =  COLOR:LightGray
        DwrTreeQ.Icon        =  3
        DwrTreeQ.Level       =  Fld:IndentBy !+ 2  !IndentBy
        DwrTreeQ.ItemType    =  'Field'
        DwrTreeQ.ItemGuid    =  Fld:PKGuid
        DwrTreeQ.Ndx         =  Fld:FieldOrder
        Add(DwrTreeQ)  
        !If Fld:LastInGroup
        !    IndentBy -=1
        !    GroupEndsQ.ItemGuid = Fld:PKGuid
        !    Add(GroupEndsQ)
        !    !InGroup = 0
        !End
        PrevWasLast = Fld:LastInGroup
    End
    Close(FieldsView)

    !MySQL.Trace('')
    !MySQL.Trace('===============================================================================================================')
    !MySQL.Trace('')
    SqlCreateSt.Prepend('CREATE TABLE dbo.' & Clip(Tab:TableName) & ' (<13,10>')
    SqlCreateSt.Append('<13,10>);')
    !MySQL.Trace('')
    !MySQL.Trace(SqlCreateSt.GetValue())
    !MySQL.Trace('')
    !MySQL.Trace('===============================================================================================================')
    !MySQL.Trace('')
    

    DwrTreeQ.DisplayStr = 'Keys'
    DwrTreeQ.NormalFG = COLOR:Black
    DwrTreeQ.NormalBG = COLOR:White
    DwrTreeQ.SelectedFG = COLOR:Black
    DwrTreeQ.SelectedBG = COLOR:White
    DwrTreeQ.Icon = 2
    DwrTreeQ.Level = 0           !Closed=-1,Child=2
    Add(DwrTreeQ)  

    Open(KeysView)
    KeysView{PROP:Order} = 'Key:Order'
    KeysView{PROP:Filter} = '(Key:ParentGUID = ' & '''' & ParamsGrp.TableGuid & '''' & ')'
    !MyDct.Trace('<9>KeysView{{PROP:Filter} = ' &  KeysView{PROP:Filter})
    Set(KeysView)
    Loop
        Next(KeysView)
        If Errorcode(); Break End
        DwrTreeQ.DisplayStr = Key:KeyName
        DwrTreeQ.NormalFG = COLOR:Black
        DwrTreeQ.NormalBG = COLOR:White
        DwrTreeQ.SelectedFG = COLOR:Black
        DwrTreeQ.SelectedBG = COLOR:LightGray
        If Upper(Key:Primary) = 'TRUE'
            DwrTreeQ.Icon = 7
        Else
            DwrTreeQ.Icon = 4
        End
        DwrTreeQ.Level = 2           !Closed=-1,Child=2
        DwrTreeQ.ItemType = 'Key'
        DwrTreeQ.ItemGuid = Key:PKGuid
        DwrTreeQ.Ndx = Key:KeyOrder
        Add(DwrTreeQ) 
    End
    Close(KeysView)

    DwrTreeQ.DisplayStr = 'Triggers'
    DwrTreeQ.NormalFG = COLOR:Black
    DwrTreeQ.NormalBG = COLOR:White
    DwrTreeQ.SelectedFG = COLOR:Black
    DwrTreeQ.SelectedBG = COLOR:White
    DwrTreeQ.Icon = 2
    DwrTreeQ.Level = 0           !Closed=-1,Child=2
    Add(DwrTreeQ)  

    Open(TriggersView)
    TriggersView{PROP:Order} = ''
    TriggersView{PROP:Filter} = '(Tri:ParentGUID = ' & '''' & ParamsGrp.TableGuid & '''' & ')'
    !MyDct.Trace('<9>TriggersView{{PROP:Filter} = ' &  TriggersView{PROP:Filter})
    Set(TriggersView)
    Loop
        Next(TriggersView)
        If Errorcode(); Break End
        DwrTreeQ.DisplayStr = Tri:PKGuid
        DwrTreeQ.NormalFG = COLOR:Black
        DwrTreeQ.NormalBG = COLOR:White
        DwrTreeQ.SelectedFG = COLOR:Black
        DwrTreeQ.SelectedBG = COLOR:LightGray
        DwrTreeQ.Icon = 8
        DwrTreeQ.Level = 2           !Closed=-1,Child=2
        DwrTreeQ.ItemType = 'Trigger'
        DwrTreeQ.ItemGuid = Tri:PKGuid
        DwrTreeQ.Ndx = Tri:TriggerOrder
        Add(DwrTreeQ) 
    End
    Close(TriggersView)

    DwrTreeQ.DisplayStr = 'Relations'
    DwrTreeQ.NormalFG = COLOR:Black
    DwrTreeQ.NormalBG = COLOR:White
    DwrTreeQ.SelectedFG = COLOR:Black
    DwrTreeQ.SelectedBG = COLOR:White
    DwrTreeQ.Icon = 2
    DwrTreeQ.Level = 0           !Closed=-1,Child=2
    Add(DwrTreeQ)  

    Open(RelationsView)
    RelationsView{PROP:Order} = ''
    RelationsView{PROP:Filter} = '(Rel:PrimaryTable = ' & '''' & ParamsGrp.RelationPrimaryGuid & '''' & ') xor (Rel:ForeignTable = ' & '''' & ParamsGrp.RelationPrimaryGuid & '''' & ')'
    !MyDct.Trace('<9>RelationsView{{PROP:Filter} = ' &  RelationsView{PROP:Filter})
    Set(RelationsView)
    Loop
        Next(RelationsView)
        If Errorcode(); Break End
        Clear(Tab:Record)
        !  Tab:Guid = Rel:ForeignTable
        !  If Access:Tables.Fetch(Tab:FKTabTableGuidKey) = Level:Benign
        !      !MyDct.Trace(Tab:TableName)
        !  End
        DwrTreeQ.NormalFG = COLOR:Black
        DwrTreeQ.NormalBG = COLOR:White
        DwrTreeQ.SelectedFG = COLOR:Black
        DwrTreeQ.SelectedBG = COLOR:LightGray
        DwrTreeQ.Level = 2           !Closed=-1,Child=2
        DwrTreeQ.ItemType = 'Relation'
        DwrTreeQ.ItemGuid = Rel:PKGuid
        DwrTreeQ.Ndx = Rel:DctxOrder
        If Rel:PrimaryTable = ParamsGrp.RelationPrimaryGuid
            !Clear(Tab:Record)
            Tab:Guid = Rel:ForeignTable
            If Access:Tables.Fetch(Tab:FKTabTableGuidKey) = Level:Benign
                DwrTreeQ.DisplayStr = Tab:TableName !'1 to Many ' 
                DwrTreeQ.Icon = 6
            End
            If Upper(LastTableName[1:Size(LastTableName)]) <> Upper(Tab:TableName[1:Size(Tab:TableName)])
                Add(DwrTreeQ) 
            End

            Key:KeyGuid = Clip(Rel:PrimaryKey)
            If Access:Keys.Fetch(Key:FKKeyKeyGuidKey) = Level:Benign
                !MyDct.Trace('Relation Primary Key - ' & Clip(Key:KeyName) & ' <<<<<<<<----------------------------------------------------')
            End
            
            Key:KeyGuid = Clip(Rel:ForeignKey)
            If Access:Keys.Fetch(Key:FKKeyKeyGuidKey) = Level:Benign
                !MyDct.Trace('Relation Foreign Key - ' & Clip(Key:KeyName) & ' <<<<<<<<----------------------------------------------------')
            End

            !cycle
        ELSIF Rel:ForeignTable = ParamsGrp.RelationPrimaryGuid
            !Clear(Tab:Record)
            Tab:Guid = Rel:PrimaryTable
            If Access:Tables.Fetch(Tab:FKTabTableGuidKey) = Level:Benign
                DwrTreeQ.Icon = 5
                DwrTreeQ.DisplayStr = Tab:TableName !'Many to 1 '
            End
            If Upper(LastTableName[1:Size(LastTableName)]) <> Upper(Tab:TableName[1:Size(Tab:TableName)])
                Add(DwrTreeQ) 
            End

            Key:KeyGuid = Clip(Rel:PrimaryKey)
            If Access:Keys.Fetch(Key:FKKeyKeyGuidKey) = Level:Benign
                !MyDct.Trace('Relation Primary Key - ' & Clip(Key:KeyName) & ' <<<<<<<<----------------------------------------------------')
            End
            
            Key:KeyGuid = Clip(Rel:ForeignKey)
            If Access:Keys.Fetch(Key:FKKeyKeyGuidKey) = Level:Benign
                !MyDct.Trace('Relation Foreign Key - ' & Clip(Key:KeyName) & ' <<<<<<<<----------------------------------------------------')
            End

            !cycle
        End
        LastTableName = Tab:TableName
        Clear(Rel:Record)
        
    End
    Close(RelationsView)



local.PopulateTableTypesTree      Procedure()
    CODE
    !MyDct.Trace('')
    !MyDct.Trace('Main3 - local.PopulateTableTypesTree')
    !MyDct.Trace('')
    YIELD

    Free(DwrTreeQ:1)

    Open(TablePoolsView9)
    TablePoolsView9{PROP:Order} = 'TabPool:TableName'
    TablePoolsView9{PROP:Filter} = '(TabPool:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (TabPool:Usage = ' & '''' & 'Pool' & '''' & ')'
    !MyDct.Trace('<9>TablePoolsView9{{PROP:Filter} = ' &  TablePoolsView9{PROP:Filter})
    Set(TablePoolsView9)
    
    Open(TableGlobalsView9)
    TableGlobalsView9{PROP:Order} = 'TabGlo:TableName'
    TableGlobalsView9{PROP:Filter} = '(TabGlo:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (TabGlo:Usage = ' & '''' & 'Global' & '''' & ')'
    !MyDct.Trace('<9>TableGlobalsView9{{PROP:Filter} = ' &  TableGlobalsView9{PROP:Filter})
    Set(TableGlobalsView9)
    
    Open(TablesView9)
    TablesView9{PROP:Order} = 'Tab:TableName'
    TablesView9{PROP:Filter} = '(Tab:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (Tab:Usage = ' & '''' & '' & '''' & ')'
    !MyDct.Trace('<9>TablesView9{{PROP:Filter} = ' &  TablesView9{PROP:Filter})
    Set(TablesView9)   



    

    DwrTreeQ:1.DisplayStr = 'Pools'
    DwrTreeQ:1.NormalFG = COLOR:Black
    DwrTreeQ:1.NormalBG = COLOR:White
    DwrTreeQ:1.SelectedFG = COLOR:Black
    DwrTreeQ:1.SelectedBG = COLOR:White
    If TablesListContracted
        DwrTreeQ:1.Level = -1           !Closed=-1,Child=2
        DwrTreeQ:1.Icon = 1
    Else
        DwrTreeQ:1.Level = 0           !Closed=-1,Child=2
        DwrTreeQ:1.Icon = 2
    End
    DwrTreeQ:1.Ndx = Records(DwrTreeQ)+1
    Add(DwrTreeQ:1)  

    LOOP
        Next(TablePoolsView9)
        If Errorcode(); Break END
        If ParamsGrp.TableGuid = ''
            ParamsGrp.TableGuid = TabPool:PKGuid
        End
        DwrTreeQ:1.DisplayStr = TabPool:TableName
        DwrTreeQ:1.NormalFG = COLOR:Black
        DwrTreeQ:1.NormalBG = COLOR:White
        DwrTreeQ:1.SelectedFG = COLOR:Black
        DwrTreeQ:1.SelectedBG = COLOR:LightGray
        DwrTreeQ:1.Icon = 3
        DwrTreeQ:1.Level = 2           !Closed=-1,Child=2
        DwrTreeQ:1.ItemType = 'Pool'
        DwrTreeQ:1.ItemGuid = TabPool:PKGuid
        DwrTreeQ:1.Ndx = TabPool:DctxOrder !Records(DwrTreeQ)+1
        DwrTreeQ:1.WinGuid = TabPool:Guid
        DwrTreeQ:1.TablePrefix = TabPool:TablePrefix
        Add(DwrTreeQ:1)  
    End

    DwrTreeQ:1.DisplayStr = 'Globals'
    DwrTreeQ:1.NormalFG = COLOR:Black
    DwrTreeQ:1.NormalBG = COLOR:White
    DwrTreeQ:1.SelectedFG = COLOR:Black
    DwrTreeQ:1.SelectedBG = COLOR:White
    If TablesListContracted
        DwrTreeQ:1.Level = -1           !Closed=-1,Child=2
        DwrTreeQ:1.Icon = 1
    Else
        DwrTreeQ:1.Level = 0           !Closed=-1,Child=2
        DwrTreeQ:1.Icon = 2
    End
    DwrTreeQ:1.Ndx = Records(DwrTreeQ)+1
    Add(DwrTreeQ:1)  

    LOOP
        Next(TableGlobalsView9)
        If Errorcode(); Break END
        DwrTreeQ:1.DisplayStr = TabGlo:TableName
        DwrTreeQ:1.NormalFG = COLOR:Black
        DwrTreeQ:1.NormalBG = COLOR:White
        DwrTreeQ:1.SelectedFG = COLOR:Black
        DwrTreeQ:1.SelectedBG = COLOR:LightGray
        DwrTreeQ:1.Icon = 4
        DwrTreeQ:1.Level = 2           !Closed=-1,Child=2
        DwrTreeQ:1.ItemType = 'Global'
        DwrTreeQ:1.ItemGuid = TabGlo:PKGuid
        DwrTreeQ:1.Ndx = TabGlo:DctxOrder !Records(DwrTreeQ)+1
        DwrTreeQ:1.WinGuid = TabGlo:Guid
        DwrTreeQ:1.TablePrefix = TabGlo:TablePrefix
        Add(DwrTreeQ:1)  
    End

    DwrTreeQ:1.DisplayStr = 'Tables'
    DwrTreeQ:1.NormalFG = COLOR:Black
    DwrTreeQ:1.NormalBG = COLOR:White
    DwrTreeQ:1.SelectedFG = COLOR:Black
    DwrTreeQ:1.SelectedBG = COLOR:White
    If TablesListContracted
        DwrTreeQ:1.Level = -1           !Closed=-1,Child=2
        DwrTreeQ:1.Icon = 1
    Else
        DwrTreeQ:1.Level = 0           !Closed=-1,Child=2
        DwrTreeQ:1.Icon = 2
    End
    DwrTreeQ:1.Ndx = Records(DwrTreeQ)+1
    Add(DwrTreeQ:1)  

    LOOP
        Next(TablesView9)
        If Errorcode(); Break END
        DwrTreeQ:1.DisplayStr = Tab:TableName
        DwrTreeQ:1.NormalFG = COLOR:Black
        DwrTreeQ:1.NormalBG = COLOR:White
        DwrTreeQ:1.SelectedFG = COLOR:Black
        DwrTreeQ:1.SelectedBG = COLOR:LightGray
        DwrTreeQ:1.Icon = 5
        DwrTreeQ:1.Level = 2           !Closed=-1,Child=2
        DwrTreeQ:1.ItemType = 'Table'
        DwrTreeQ:1.ItemGuid = Tab:PKGuid
        DwrTreeQ:1.Ndx = Tab:DctxOrder !Records(DwrTreeQ)+1
        DwrTreeQ:1.WinGuid = Tab:Guid
        DwrTreeQ:1.TablePrefix = Tab:TablePrefix
        Add(DwrTreeQ:1)  

        Open(AliasesView9)
        AliasesView9{PROP:Order} = 'Ali:AliasName'
        AliasesView9{PROP:Filter} = '(Ali:ParentGUID = ' & '''' & Tab:PKGuid & '''' & ')'
        Set(AliasesView9)
        LOOP
            Next(AliasesView9)
            If Errorcode(); Break End
            DwrTreeQ:1.DisplayStr = Ali:AliasName
            DwrTreeQ:1.NormalFG = COLOR:Black
            DwrTreeQ:1.NormalBG = COLOR:White
            DwrTreeQ:1.SelectedFG = COLOR:Black
            DwrTreeQ:1.SelectedBG = COLOR:LightGray
            DwrTreeQ:1.Icon = 5
            DwrTreeQ:1.Level = 3           !Closed=-1,Child=2
            DwrTreeQ:1.ItemType = 'Table'
            DwrTreeQ:1.ItemGuid = Tab:PKGuid
            !DwrTreeQ:1.Ndx = Records(DwrTreeQ)+1 !Tab:DctxOrder !Records(DwrTreeQ)+1
            DwrTreeQ:1.WinGuid = Ali:AliasGuid
            Add(DwrTreeQ:1)
        End
        Close(AliasesView9)

    End
    !MyDct.Trace('')


    Close(TableGlobalsView9)
    Close(TablePoolsView9)
    Close(TablesView9)
    


    !?DwrTreeList:2{PROPLIST:MouseDownRow} = 2
    !Get(DwrTreeQ:1,2)
    !DonutHoleHost:5.TryOpen(START(UpdateDictionary,5000,OverStr,Thread()))

    !Post(EVENT:Accepted,?DwrTreeList:2) 

local.ParseMarked                 Procedure()
x       Long
    CODE
    !Sort(DwrTreeQ,-DwrTreeQ.RealMark)
    Loop x = 1 to Records(DwrTreeQ:1)
        Get(DwrTreeQ:1,x)
        !If DwrTreeQ.RealMark = 0; Break End
        If DwrTreeQ:1.RealMark = 1
            !ListMarker.Trace(x & '<9>' & Clip(DwrTreeQ:1.DisplayStr) & '<9>' & DwrTreeQ:1.RealMark)
        ELSE
            Cycle
        End
    End

local.SetFieldGuids               Procedure(long pMouseDownRow)
    code
    Clear(DwrTreeQ)
    Get(DwrTreeQ,pMouseDownRow-1)
    self.PreviousFieldGuid = DwrTreeQ.ItemGuid

    Clear(DwrTreeQ)
    Get(DwrTreeQ,pMouseDownRow)
    self.SelectedFieldGuid = DwrTreeQ.ItemGuid

    Clear(DwrTreeQ)
    Get(DwrTreeQ,pMouseDownRow+1)
    self.NextFieldGuid = DwrTreeQ.ItemGuid

    !MyDct.Trace('')
    !MyDct.Trace('==================================================================================================================================')
    !MyDct.Trace('local.SetFieldGuids')
    !MyDct.Trace('<9>self.PreviousFieldGuid  = ' & self.PreviousFieldGuid)
    !MyDct.Trace('<9>self.SelectedFieldGuid  = ' & self.SelectedFieldGuid)
    !MyDct.Trace('<9>self.NextFieldGuid      = ' & self.NextFieldGuid)
    !MyDct.Trace('==================================================================================================================================')
    !MyDct.Trace('')

local.SetTaggedCount              Procedure()
    CODE
    !MyDct.Trace('')
    !MyDct.Trace('local.SetTaggedCount - START')
    TaggedCount = 0
    Loop x = 1 to Records(DwrTreeQ:1)
        Get(DwrTreeQ:1,x)
        !If DwrTreeQ.RealMark = 0; Break End
        If DwrTreeQ:1.WinMark = 1
            !MyDct.Trace('local.SetTaggedCount - DwrTreeQ:1.RealMark = 1')
            TaggedCount += 1
        End
    End
    ParamsGrp.TaggedCnt = TaggedCount
    MyDct.SendNotifyJson(NotifyManager,'BulkActionsWindow','Main3','',OverStr)
    !NotifyManager.NotifyProcedure('BulkActionsWindow',)
    If TaggedCount > 0

    End
    !MyDct.Trace('local.SetTaggedCount - TaggedCount = ' & TaggedCount)
    !MyDct.Trace('')
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
!----------------------------------------------------
csResize.Sized   PROCEDURE (Long pLevel=0)
  CODE
  PARENT.Sized (pLevel)
    !MyDct.Trace('csResize.Sized <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csResize.Reset   PROCEDURE ()
  CODE
  PARENT.Reset ()
    !MyDct.Trace('csResize.Reset After PC <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
    !DonutHoleHost:5.RepaintDonutholeWindow()
    !csResize.Update()

    POST(EVENT:Sized,,DonutHoleHost:5.GetThread())
!----------------------------------------------------
csResize.Fetch   PROCEDURE (STRING Sect,STRING Ent,*? Val)
  CODE
  INIMgr.Fetch(Sect,Ent,Val)
  PARENT.Fetch (Sect,Ent,Val)
!----------------------------------------------------
csResize.Update   PROCEDURE (STRING Sect,STRING Ent,STRING Val)
  CODE
  INIMgr.Update(Sect,Ent,Val)
  PARENT.Update (Sect,Ent,Val)
!----------------------------------------------------
csResize.Init   PROCEDURE ()
  CODE
  PARENT.Init ()
  Self.CornerStyle = Ras:CornerDots
  SELF.GrabCornerLines() !
  SELF.SetStrategy(?DwrTreeList,,0,,100)
  SELF.SetStrategy(?DonutHoleHostRegion,0,0,100,100)
  SELF.SetStrategy(?DwrTreeList:2,,0,,100)
  SELF.SetStrategy(?EasyHSplit,,0,,100)
  SELF.SetStrategy(?EasyHSplit:2,,0,,100)
    ?EasyVSplit{prop:cursor} = Cursor:SizeNS
    csVSplit16.Init('EasyVSplit',SELF,?EasyVSplit,'PANEL',)
    ?EasyHSplit{prop:cursor} = Cursor:SizeWE
    csHSplit6.Init('EasyHSplit',SELF,?EasyHSplit,'PANEL',)
    ?EasyHSplit:2{prop:cursor} = Cursor:SizeWE
    csHSplit14.Init('EasyHSplit:2',SELF,?EasyHSplit:2,'PANEL',)
!----------------------------------------------------
csVSplit16.Init   PROCEDURE ()
  CODE
  SELF.ResizedControl(?DictionariesList)
  SELF.ResizedControl(?DwrTreeList:2)
  SELF.MovedControl(?AddNewTableBtn)
  SELF.MovedControl(?SearchEntry)
  SELF.MovedControl(?SearchBtn)
  PARENT.Init ()
!----------------------------------------------------
csHSplit6.Init   PROCEDURE ()
  CODE
  SELF.ResizedControl(?DwrTreeList)
  SELF.ResizedControl(?DonutHoleHostRegion)
  SELF.ResizedControl(?SearchEntry:2)
  SELF.MovedControl(?SearchBtn:2)
  PARENT.Init ()
!----------------------------------------------------
csHSplit6.Resize   PROCEDURE ()
  CODE
  PARENT.Resize ()
    !MyDct.Trace('csHSplit6.Resize <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.Update   PROCEDURE ()
  CODE
  PARENT.Update ()
    !MyDct.Trace('csHSplit6.Update <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.MoveSplitter   PROCEDURE (Long pDeltaX, Long pDeltaY)
  CODE
  PARENT.MoveSplitter (pDeltaX,pDeltaY)
    !MyDct.Trace('csHSplit6.MoveSplitter <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.MovedControl   PROCEDURE (SIGNED Control)
  CODE
  PARENT.MovedControl (Control)
    !MyDct.Trace('csHSplit6.MovedControl <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.ResizedControl   PROCEDURE (SIGNED Control)
  CODE
    !MyDct.Trace('csHSplit6.ResizedControl Before PC <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
  PARENT.ResizedControl (Control)
    !MyDct.Trace('csHSplit6.ResizedControl After PC <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.Moved   PROCEDURE (UNSIGNED phWnd,LONG plParam)
  CODE
  PARENT.Moved (phWnd,plParam)
    !MyDct.Trace('csHSplit6.Moved <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.ChangeControl   PROCEDURE (SIGNED Control,SIGNED NewControl,UNSIGNED hWnd)
  CODE
  PARENT.ChangeControl (Control,NewControl,hWnd)
    !MyDct.Trace('csHSplit6.ChangeControl <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.AlignmentResize   PROCEDURE (LONG Pos,SIGNED Control)
  CODE
  PARENT.AlignmentResize (Pos,Control)
    !MyDct.Trace('csHSplit6.AlignmentResize <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.Alignment   PROCEDURE (LONG Pos,SIGNED Control,QAlignmentERS pQueue)
  CODE
  PARENT.Alignment (Pos,Control,pQueue)
    !MyDct.Trace('csHSplit6.Alignment <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.GetUpPos   PROCEDURE ()
ReturnValue   SIGNED
  CODE
  ReturnValue = PARENT.GetUpPos ()
    !MyDct.Trace('csHSplit6.GetUpPos <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
    Return ReturnValue
!----------------------------------------------------
csHSplit6.Reset   PROCEDURE ()
  CODE
  PARENT.Reset ()
    !MyDct.Trace('csHSplit6.Reset <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit6.GetPosition   PROCEDURE (Long pControl,*ERS_POS Pos)
  CODE
  PARENT.GetPosition (pControl,Pos)
    !MyDct.Trace('csHSplit6.GetPosition After PC <<<<<<<<<<<<<<<<<<<<<<<<------------------------------------------------------------------------')
!----------------------------------------------------
csHSplit14.Init   PROCEDURE ()
  CODE
  SELF.ResizedControl(?DwrTreeList)
  SELF.ResizedControl(?DictionariesList)
  SELF.ResizedControl(?DwrTreeList:2)
  SELF.ResizedControl(?DctSearch)
  SELF.ResizedControl(?SearchEntry)
  SELF.ResizedControl(?SearchEntry:2)
  SELF.ResizedControl(?EasyVSplit)
  SELF.MovedControl(?AddNewFieldBtn)
  SELF.MovedControl(?SearchBtn)
  SELF.MovedControl(?MoveFieldUpBtn)
  SELF.MovedControl(?MoveFieldDownBtn)
  PARENT.Init ()

BrwDct.ResetFromFile PROCEDURE

  CODE
  PARENT.ResetFromFile
    !MyDct.Trace('')
    !MyDct.Trace('BrwDct.ResetFromFile - After PC')
      ParamsGrp.DictionaryGuid = Dct:GUID!Queue:Browse.Dct:GUID
      ParamsGrp.ActiveGuid = Dct:GUID !Queue:Browse.Dct:GUID
      
      Post(DctEvent:PopulateTablesTree) 
      
      !Hide(?DonutHoleHostRegion)
      DonutHoleHost:5.TryOpen(START(UpdateDictionary,50000,OverStr,Thread()))
      !Unhide(?DonutHoleHostRegion)
      !Post(EVENT:Accepted,?DwrTreeList:2) 
      SELECT(?DictionariesList)  


BrwDct.TakeNewSelection PROCEDURE

  CODE
    !ParamsGrp.DictionaryGuid = Dct:GUID  
  	!Post(EVENT:Accepted,?DwrTreeList:2) 
    
  PARENT.TakeNewSelection
    !MyDct.Trace('')
    !MyDct.Trace('BrwDct.TakeNewSelection - After PC')
    ParamsGrp.DictionaryGuid = Dct:GUID  
  !    Open(TablePoolsView9)
  !    TablePoolsView9{PROP:Order} = 'TabPool:TableName'
  !    TablePoolsView9{PROP:Filter} = '(TabPool:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (TabPool:Usage = ' & '''' & 'Pool' & '''' & ')'
  !    Set(TablePoolsView9)
  !  
  !    Open(TableGlobalsView9)
  !    TableGlobalsView9{PROP:Order} = 'TabGlo:TableName'
  !    TableGlobalsView9{PROP:Filter} = '(TabGlo:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (TabGlo:Usage = ' & '''' & 'Global' & '''' & ')'
  !    Set(TableGlobalsView9)
  !  
  !    Open(TablesView9)
  !    TablesView9{PROP:Order} = 'Tab:TableName'
  !    TablesView9{PROP:Filter} = '(Tab:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (Tab:Usage = ' & '''' & '' & '''' & ')'
  !    Set(TablesView9)   
  !  
  !    local.PopulateTableTypesTree() 

!----------------------------------------------------
xml.ValidateRecord   PROCEDURE ()
ReturnValue   Long
  CODE
    !MyDct.Trace('xml.ValidateRecord')
  ReturnValue = PARENT.ValidateRecord ()
    Return ReturnValue
!----------------------------------------------------
xml.AfterReflectionParse   PROCEDURE (String pGroupName,*View pView)
  CODE
    !MyDct.Trace('xml.AfterReflectionParse - pGroupName = ' & pGroupName)
  PARENT.AfterReflectionParse (pGroupName,pView)
!----------------------------------------------------
xml._AddViewNode   PROCEDURE (View pView, *Cstring pGroupName, Long pTableId)
  CODE
    !MyDct.Trace('xml._AddViewNode - pGroupName = ' & pGroupName & ' pTableId = ' & pTableId)
  PARENT._AddViewNode (pView,pGroupName,pTableId)
!----------------------------------------------------
xml._AddViewRecordNested   PROCEDURE (View pView, *Cstring pGroupName)
  CODE
    !MyDct.Trace('xml._AddViewRecordNested - pGroupName = ' & pGroupName)
  PARENT._AddViewRecordNested (pView,pGroupName)
!----------------------------------------------------
xml.AddChild   PROCEDURE (Long pComponentType, String pStr, Long pIndex=0)
ReturnValue   Long
  CODE
    !MyDct.Trace('xml.AddChild - pStr = ' & pStr)
  ReturnValue = PARENT.AddChild (pComponentType,pStr,pIndex)
    Return ReturnValue
!----------------------------------------------------
xml.AddNode   PROCEDURE (String pTag,String pValue, Long pIndex, Long pForce=false)
ReturnValue   &XmlTree
  CODE
    !MyDct.Trace('xml.AddNode - pTag = ' & pTag)
  ReturnValue &= PARENT.AddNode (pTag,pValue,pIndex,pForce)
    Return ReturnValue
!----------------------------------------------------
xml.AddNode   PROCEDURE (String pTag,Long pIndex=0)
ReturnValue   &XmlTree
  CODE
    !MyDct.Trace('xml.AddNode - pTag = ' & pTag)
  ReturnValue &= PARENT.AddNode (pTag,pIndex)
    Return ReturnValue
NotifyHandler.HandleNotify       PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter)
us    UltimateString
TempSt      StringTheory
VersionVal  Long
    CODE    
    NotifyParam = NotifyManager.GetNotifyParm(NotifyParameter)
	If Size(NotifyParam) > 0
		TempSt.SetValue(NotifyParam)
        !MyDct.Trace('')
        !MyDct.Trace('Main3 - NotifyHandler.HandleNotify')
		!MyDct.Trace(TempSt.GetValue())
        !MyDct.Trace('')
		json.Start()
		json.SetTagCase(jF:CaseAsIs)
		json.Load(ParamsGrp,TempSt)
        Case ParamsGrp.Action[1:Size(ParamsGrp.Action)]
        Of 'BulkDone'
            local.ClearMarked()
        Of 'DctLoaded'
            Dct:GUID = ParamsGrp.DictionaryGuid !MyDct.GetDictionaryGuid()
            Access:Dictionaries.Fetch(Dct:PKDctGUIDKey)
            !MyDct.Trace('')
            !MyDct.Trace('============================================================================================================================================')
            !MyDct.Trace('')
            !MyDct.Trace('After Loading DCTX')
            !MyDct.Trace('<9>Dct:GUID = ' & Dct:GUID)
            !MyDct.Trace('<9>Records(Dictionaries) = ' & Records(Dictionaries))
            If Errorcode()
                !MyDct.Trace('<9>Error Fetching Dictionaries Record - Error = ' & ERROR())
            ELSE
                !MyDct.Trace('<9>Dictionaries Record Fetched!')
                BrwDct.ResetFromFile()
                ThisWindow.Reset(True) 
                POST(EVENT:Accepted,?DictionariesList)
                SELECT(?DictionariesList)
            End
        Of 'AddGloPathNames'
            Loop x = 1 to Records(DwrTreeQ:1)
                Get(DwrTreeQ:1,x)
                If DwrTreeQ:1.ItemType = 'Table'
                    !Procedure(*FILE pTable,string pTableGuid,<string pName>,<string pDataType>,<string pSize>)
                    MyDct.InsertField(Tables,ParamsGrp.TableGuid,Clip(DwrTreeQ:1.DisplayStr),'String',ParamsGrp.FieldSize)
                End
            End
        Of 'SettingsChanged'
            MyDct.LoadSettings(SettingsGrp)
        OF 'SaveNeeded'
            ?SaveBtn{PROP:Disable} = False
            Post(DctEvent:PopulateTablesTree)
        Of 'RecordSaved'
            ?SaveBtn{PROP:Disable} = True
            Post(DctEvent:PopulateTablesTree)

        Of 'AddFm3Version'
            !Message('Main3 - AddFm3Version')
            Loop x = 1 to Records(DwrTreeQ:1)
                Get(DwrTreeQ:1,x)
                If DwrTreeQ:1.WinMark = 1
                    Tab:PKGuid = DwrTreeQ:1.ItemGuid
                    If Access:Tables.Fetch(Tab:PKTabGuidKey) = Level:Benign
                        If Not MyDct.GetHasFM3VersionOption(Tables,'OptionsBlob')
                            !MyDct.Trace('Adding Version Option To: ' & Clip(Tab:TableName))
                            MyDct.InsertOptionQRec(Tables,'OptionsBlob','Version','1','1')
                        End
                    ELSE
                        !MyDct.Trace('Main3 - NotifyHandler.HandleNotify - AddFm3Version - Error Fetching Tables Record!')
                    End
                End
            End
!            Open(TablesView9)
!            TablesView9{PROP:Order} = ''
!            TablesView9{PROP:Filter} = '(Tab:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
!            Set(TablesView9)
!            LOOP
!                Next(TablesView9)
!                If Errorcode(); Break END
!                !MyDct.Trace(Clip(Tab:TableName) & '<9>Has Version = ' & MyDct.GetHasFM3VersionOption(Tables,'OptionsBlob'))
!                If Not MyDct.GetHasFM3VersionOption(Tables,'OptionsBlob')
!                    !MyDct.Trace('Adding Version Option To: ' & Clip(Tab:TableName))
!                    MyDct.InsertOptionQRec(Tables,'OptionsBlob','Version','1','1')
!                End
!            END
!            Close(TablesView9)
        Of 'IncrementFm3Version'
            Loop x = 1 to Records(DwrTreeQ:1)
                Get(DwrTreeQ:1,x)
                If DwrTreeQ:1.WinMark = 1
                    Tab:PKGuid = DwrTreeQ:1.ItemGuid
                    If Access:Tables.Fetch(Tab:PKTabGuidKey) = Level:Benign
                        VersionVal = MyDct.GetHasFM3VersionOption(Tables,'OptionsBlob')
                        If VersionVal
                            VersionVal = VersionVal + 1
                            !MyDct.Trace('Incrementing Version Option To: ' & VersionVal)
                            MyDct.PopulateOptionsQ(Tables,'OptionsBlob')
                            If Not Errorcode()
                                MyDct.UpdateOptionQRec(MyDct.OptionsQ,Tables,'OptionsBlob','Version',VersionVal)
                            ELSE
                                !MyDct.Trace('Main3 - NotifyHandler.HandleNotify - Error GETting OptionsQ Record!')
                            End
                            !MyDct.InsertOptionQRec(Tables,'OptionsBlob','Version','1',VersionVal)
                        End
                    ELSE
                        !MyDct.Trace('Main3 - NotifyHandler.HandleNotify - IncrementFm3Version - Error Fetching Tables Record!')
                    End
                End
            End
!            Open(TablesView9)
!            TablesView9{PROP:Order} = ''
!            TablesView9{PROP:Filter} = '(Tab:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
!            Set(TablesView9)
!            LOOP
!                Next(TablesView9)
!                If Errorcode(); Break END
!
!                VersionVal = MyDct.GetHasFM3VersionOption(Tables,'OptionsBlob')
!                !MyDct.Trace(Clip(Tab:TableName) & '<9>Has Version = ' & MyDct.GetHasFM3VersionOption(Tables,'OptionsBlob'))
!                If VersionVal
!                    VersionVal = VersionVal + 1
!                    !MyDct.Trace('Incrementing Version Option To: ' & VersionVal)
!                    MyDct.PopulateOptionsQ(Tables,'OptionsBlob')
!                    If Not Errorcode()
!                        MyDct.UpdateOptionQRec(MyDct.OptionsQ,Tables,'OptionsBlob','Version',VersionVal)
!                    ELSE
!                        !MyDct.Trace('Main3 - NotifyHandler.HandleNotify - Error GETting OptionsQ Record!')
!                    End
!                    !MyDct.InsertOptionQRec(Tables,'OptionsBlob','Version','1',VersionVal)
!                End
!                !Access:Tables.Update()
!            END
!            Close(TablesView9)
        Of 'ChangeDrivers'
            Loop x = 1 to Records(DwrTreeQ:1)
                Get(DwrTreeQ:1,x)
                If DwrTreeQ:1.WinMark = 1
                    Tab:PKGuid = DwrTreeQ:1.ItemGuid
                    If Access:Tables.Fetch(Tab:PKTabGuidKey) = Level:Benign
                        Tab:TableDriver = ParamsGrp.SelectedDriver
                        Access:Tables.Update()
                    ELSE
                        !MyDct.Trace('Main3 - NotifyHandler.HandleNotify - ChangeDrivers - Error Fetching Tables Record!')
                    End
                End
            End
        Of 'TableNameTpsPath'
            Loop x = 1 to Records(DwrTreeQ:1)
                Get(DwrTreeQ:1,x)
                If DwrTreeQ:1.WinMark = 1
                    Tab:PKGuid = DwrTreeQ:1.ItemGuid
                    If Access:Tables.Fetch(Tab:PKTabGuidKey) = Level:Benign
                        Tab:TablePath = Clip(Tab:TableName) & '.tps'
                        Access:Tables.Update()
                    ELSE
                        !MyDct.Trace('Main3 - NotifyHandler.HandleNotify - TableNameTpsPath - Error Fetching Tables Record!')
                    End
                End
            End
        End
        ql17.SetSearchType(SettingsGrp.DefaultSearchType)
        ql11.SetSearchType(SettingsGrp.DefaultSearchType) 
    End
