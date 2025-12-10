

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR009.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('DICTIONARYEDITOR016.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR017.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR018.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! Form Dictionaries
!!! </summary>
UpdateDictionary PROCEDURE (string pParams,string pThread)

ParamsGrp                   Group(ParamsGroupType).
OverStr                     String(Size(ParamsGrp)),Over(ParamsGrp)
SavedBuffer                 USHORT
          
 
json				        JSONClass
st					        StringTheory
MyDct				        dwrDctParser
xml                         xFilesTree
mt                          MyTableClass

Rec                         &GROUP
DictionaryGrp		        Group
GUID                        STRING(16),NAME('PKGuid | Private')
DctName                     STRING(100),NAME('Name | Attribute')
Version                     STRING(5),NAME('Version | Attribute')
StructureChecked            STRING(5),NAME('StructureChecked | Attribute')
DctxFormat                  STRING(5),NAME('DctxFormat | Attribute')
					        End

DctEvent:FetchRecord        Equate(EVENT:User+900)
DctEvent:RefreshData        Equate(EVENT:User+901)
DctEvent:RefreshControls    Equate(EVENT:User+902)
DctEvent:SaveRecord         Equate(EVENT:User+903)

OptionGrp               Group(OptionGroupType).
CommentGrp              Group(CommentGroupType).
DctGrp                  Group(DictionaryGroupType).
Rec                     &GROUP
DctOverStr              String(Size(DctGrp)),Over(DctGrp)

CommentRecsCnt          Long
OptionRecCnt            Long

!CommentGrp                  Group,Name('Comment | RowName(Comment)')
!PKGuid                      STRING(16),NAME('PKGuid | Private')
!text                        STRING(255),Name('Text | attribute')
!Audit                           Group,Name('Audit')
!CreateUser                      STRING(50),Name('CreateUser | attribute')
!CreateDate                      STRING(11),Name('CreateDate | attribute')
!CreateTime                      STRING(10),Name('CreateTime | attribute')
!CreateVersionNumber             STRING(3),Name('CreateVersionNumber | attribute')
!ModifiedUser                    STRING(50),Name('ModifiedUser | attribute')
!ModifiedDate                    STRING(11),Name('ModifiedDate | attribute')
!ModifiedTime                    STRING(10),Name('ModifiedTime | attribute')
!ModifiedVersionNumber           STRING(3),Name('ModifiedVersionNumber | attribute')
!							    End
!                            End

!OptionGrp       			Group,Name('Option')
!Id                          LONG,NAME('Id | Private')
!Property                   	STRING(100),Name('Property | attribute')
!PropertyType               	STRING(5),Name('PropertyType | attribute')
!PropertyValue              	STRING(100),Name('PropertyValue | attribute')
!							End

local                       CLASS
PopulateCommentsQ           Procedure()
PopulateOptionsQ            Procedure()
ResetOptionsBrowse          Procedure()
ResetVersionsBrowse         Procedure()
                            End

MyCrypto                    Cryptonite
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
CurrentTab           STRING(80)                            ! 
CommentPkGuid        STRING(16)                            ! 
OptionQId            LONG                                  ! 
NotifyParam          STRING(1500)                          ! 
OptionsQ             QUEUE,PRE(),NAME('Option')            ! 
Id                   LONG,NAME('Id | Private')             ! 
Property             STRING(100),NAME('Property | attribute') ! 
PropertyType         STRING(5),NAME('PropertyType | attribute') ! 
PropertyValue        STRING(100),NAME('PropertyValue | attribute') ! 
                     END                                   ! 
CommentQ             QUEUE,PRE(CmtQ),NAME('Comment  | RowName(Comment)') ! 
PKGuid               STRING(16),NAME('PKGuid | Private')   ! 
Text                 STRING(255),NAME('Text | attribute')  ! 
Audit                GROUP,PRE(),NAME('Audit')             ! 
CreateUser           STRING(50),NAME('CreateUser | attribute') ! 
CreateDate           STRING(11),NAME('CreateDate | attribute') ! 
CreateTime           STRING(10),NAME('CreateTime | attribute') ! 
CreateVersionNumber  STRING(3),NAME('CreateVersionNumber | attribute') ! 
ModifiedUser         STRING(50),NAME('ModifiedUser | attribute') ! 
ModifiedDate         STRING(11),NAME('ModifiedDate | attribute') ! 
ModifiedTime         STRING(10),NAME('ModifiedTime | attribute') ! 
ModifiedVersionNumber STRING(3),NAME('ModifiedVersionNumber | attribute') ! 
                     END                                   ! 
                     END                                   ! 
ActionMessage        CSTRING(40)                           ! 
VersionRec			&GROUP
OptionsRec			&GROUP
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DonutHoleWindow     Class(NinjaDonutHoleWindowClass)
Repaint               PROCEDURE(),DERIVED
                    end
BRW4::View:Browse    VIEW(DctVersions)
                       PROJECT(Ver:DctVersion)
                       PROJECT(Ver:Description)
                       PROJECT(Ver:GUID)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?VersionsList
Ver:DctVersion         LIKE(Ver:DctVersion)           !List box control field - type derived from field
Ver:Description        LIKE(Ver:Description)          !List box control field - type derived from field
Ver:GUID               LIKE(Ver:GUID)                 !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::Dct:Record  LIKE(Dct:RECORD),THREAD
QuickWindow          WINDOW('Caption'),AT(,,449,183),FONT('Segoe UI',10,,FONT:regular,CHARSET:ANSI),RESIZE,AUTO, |
  CENTER,GRAY,IMM,MAX,HLP('UpdateDictionary'),SYSTEM,WALLPAPER('Background1.png')
                       BUTTON,AT(397,1,23,12),USE(?OK),LEFT,ICON('SaveLarge1.ico'),DEFAULT,DISABLE,FLAT,HIDE,MSG('Accept dat' & |
  'a and close the window'),TIP('Accept data and close the window')
                       BUTTON,AT(423,1,23,12),USE(?Cancel),LEFT,ICON('Close.ico'),FLAT,HIDE,MSG('Cancel operation'), |
  TIP('Cancel operation')
                       SHEET,AT(2,1,445,180),USE(?SHEET1)
                         TAB('General'),USE(?TAB1)
                           ENTRY(@s100),AT(86,17,151,10),USE(Dct:DctName),LEFT(2)
                           PROMPT('Dctx Format:'),AT(7,62),USE(?Dct:DctxFormat:Prompt),TRN
                           ENTRY(@s5),AT(86,47,40,10),USE(Dct:StructureChecked),LEFT(2)
                           PROMPT('Structure Checked:'),AT(7,48),USE(?Dct:StructureChecked:Prompt),TRN
                           ENTRY(@s5),AT(86,32,40,10),USE(Dct:Version),LEFT(2)
                           PROMPT('Version:'),AT(7,32),USE(?Dct:Version:Prompt),TRN
                           PROMPT('Dct Name:'),AT(7,18),USE(?Dct:DctName:Prompt),TRN
                           ENTRY(@s5),AT(86,62,40,10),USE(Dct:DctxFormat),LEFT(2)
                         END
                         TAB('Comments'),USE(?CommentsTab)
                           LIST,AT(10,37,432,113),USE(?CommentsList),HVSCROLL,FORMAT('51L(2)M~Create Date~@s11@#9#' & |
  '83L(2)M~Create User~@s50@#8#1020L(2)M~Text~@s255@#2#')
                           BUTTON,AT(10,17),USE(?AddCommentBtn),ICON('AddNew.ico'),FLAT
                           BUTTON,AT(31,17),USE(?EditCommentBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(53,17),USE(?DeleteCommentBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                         END
                         TAB('Versions'),USE(?VersionsTab)
                           LIST,AT(10,40,432,113),USE(?VersionsList),LEFT(2),VSCROLL,FORMAT('36L(2)M~Version~@s5@1' & |
  '020L(2)M~Description~@s255@'),FROM(Queue:Browse),IMM
                           BUTTON,AT(10,20),USE(?Insert),ICON('AddNew.ico'),FLAT
                           BUTTON,AT(31,20),USE(?Change),ICON('Edit3.ico'),FLAT
                           BUTTON,AT(53,20),USE(?Delete),ICON('GarbageClosed.ico'),FLAT
                         END
                         TAB('Options'),USE(?OptionsTab)
                           BUTTON,AT(10,17),USE(?AddOptionBtn),ICON('AddNew.ico'),FLAT
                           BUTTON,AT(31,17),USE(?EditOptionBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(53,17),USE(?DeleteOptionBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                           LIST,AT(10,37,432,113),USE(?OptionsList),HVSCROLL,FORMAT('102L(2)M~Property~@s100@#2#53' & |
  'L(2)M~Property Type~@s5@#3#400L(2)M~Property Value~@s100@#4#')
                         END
                         TAB('Statistics'),USE(?StatisticsTab)
                         END
                         TAB('Bulk Dictionary Tools'),USE(?BulkToolsTab)
                         END
                       END
                     END

CommentsView        View(DctComments)
                    Project(Dcom:PKGuid)
                    Project(Dcom:DictionaryGuid)
                    Project(Dcom:Comments)
                    End

DctVersionsView     View(DctVersions)
                    Project(Ver:GUID)
                    Project(Ver:ParentGUID)
                    Project(Ver:DictionaryGuid)
                    Project(Ver:DctVersion)
                    Project(Ver:Description)
                    End

OptionsView         View(Options)
                    Project(Opt:PKGuid)
                    Project(Opt:ParentGUID)
                    Project(Opt:DictionaryGuid)
                    Project(Opt:TableGuid)
                    Project(Opt:FieldGuid)
                    Project(Opt:KeyGuid)
                    Project(Opt:RelationGuid)
                    Project(Opt:ParentNode)
                    Project(Opt:Property)
                    Project(Opt:PropertyType)
                    Project(Opt:PropertyValue)
                    End
    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
ChangeAction           PROCEDURE(),BYTE,DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeCloseEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeCompleted          PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
Update                 PROCEDURE(),DERIVED
                     END

Toolbar              ToolbarClass
mhResize             CLASS(MH::ResizeWindowClass)
AfterResize            PROCEDURE(),DERIVED
Init                   PROCEDURE()
InitialResize          PROCEDURE()                         ! New method added to this class instance
                     END

UpdateDictionary:mhResize:WM CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END
BrwVersions          CLASS(BrowseClass)                    ! Browse using ?VersionsList
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
TakeNewSelection       PROCEDURE(),DERIVED
                     END

CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(DctComments:Record)
? DEBUGHOOK(DctVersions:Record)
? DEBUGHOOK(Dictionaries:Record)
? DEBUGHOOK(Options:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  if ~DonutHoleWindow.GetIsConnecting()  
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Record Will Be Added'
  OF ChangeRecord
    ActionMessage = 'Record Will Be Changed'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  End  
  PARENT.Ask


ThisWindow.ChangeAction PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.ChangeAction()
    !MyDct.Trace('UpdateDictionary - ThisWindow.ChangeAction')  
  RETURN ReturnValue


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  OverStr = pParams
  GlobalErrors.SetProcedureName('UpdateDictionary')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?OK
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('Ver:DctVersion',Ver:DctVersion)                    ! Added by: BrowseBox(ABC)
  BIND('Ver:Description',Ver:Description)                  ! Added by: BrowseBox(ABC)
  BIND('Ver:GUID',Ver:GUID)                                ! Added by: BrowseBox(ABC)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('UpdateDictionary',pThread)
    NotifyHandler.AddNotifyCode(1)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(Dct:Record,History::Dct:Record)
  SELF.AddHistoryField(?Dct:DctName,2)
  SELF.AddHistoryField(?Dct:StructureChecked,4)
  SELF.AddHistoryField(?Dct:Version,3)
  SELF.AddHistoryField(?Dct:DctxFormat,5)
  SELF.AddUpdateFile(Access:Dictionaries)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:DctComments.SetOpenRelated()
  Relate:DctComments.Open()                                ! File DctComments used by this procedure, so make sure it's RelationManager is open
  Access:Options.UseFile()                                 ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Dictionaries
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.CancelAction = Cancel:Cancel+Cancel:Query         ! Confirm cancel
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  BrwVersions.Init(?VersionsList,Queue:Browse.ViewPosition,BRW4::View:Browse,Queue:Browse,Relate:DctVersions,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('UpdateDictionary', THREAD(), QuickWindow)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?SHEET1{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
  0{PROP:Buffer} = 1     
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
  IF SELF.Request = ViewRecord                             ! Configure controls for View Only mode
    ?Dct:DctName{PROP:ReadOnly} = True
    ?Dct:StructureChecked{PROP:ReadOnly} = True
    ?Dct:Version{PROP:ReadOnly} = True
    ?Dct:DctxFormat{PROP:ReadOnly} = True
    DISABLE(?AddCommentBtn)
    DISABLE(?EditCommentBtn)
    DISABLE(?DeleteCommentBtn)
    DISABLE(?Insert)
    DISABLE(?Change)
    DISABLE(?Delete)
    DISABLE(?AddOptionBtn)
    DISABLE(?EditOptionBtn)
    DISABLE(?DeleteOptionBtn)
  END
  mhResize.Init
  BrwVersions.Q &= Queue:Browse
  BrwVersions.AddSortOrder(,)                              ! Add the sort order for  for sort order 1
  BrwVersions.AddField(Ver:DctVersion,BrwVersions.Q.Ver:DctVersion) ! Field Ver:DctVersion is a hot field or requires assignment from browse
  BrwVersions.AddField(Ver:Description,BrwVersions.Q.Ver:Description) ! Field Ver:Description is a hot field or requires assignment from browse
  BrwVersions.AddField(Ver:GUID,BrwVersions.Q.Ver:GUID)    ! Field Ver:GUID is a hot field or requires assignment from browse
  INIMgr.Fetch('UpdateDictionary',QuickWindow)             ! Restore window settings from non-volatile store
  BrwVersions.AskProcedure = 1                             ! Will call: UpdateDctVersions(OverStr,Thread())
  BrwVersions.AddToolbarTarget(Toolbar)                    ! Browse accepts toolbar control
  DonutHoleWindow.SetLogPreamble('UpdateDictionary')
  DonutHoleWindow.init(DonutHoleRegistrator.IDonutHoleRegistration,thread(),QuickWindow)
  SELF.AddItem(DonutHoleWindow.WindowComponent)
  !DonutHoleWindow.
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
    SavedBuffer = SELF.Primary.Me.SaveBuffer()
  
    ?CommentsList{PROP:From} = MyDct.CommentsQ
    ?OptionsList{PROP:From} = MyDct.OptionsQ
    Post(DctEvent:FetchRecord)       
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('UpdateDictionary', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  !  if bRepost
  !    POST(event:DonutHoleHide,, thread()) !since we close the window on a hide, this can only be a hide. 
  !  end!If  
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:DctComments.Close()
  END
    NotifyManager.DeleteProcedure('UpdateDictionary',pThread)
  mhResize.Kill
  IF SELF.Opened
    INIMgr.Update('UpdateDictionary',QuickWindow)          ! Save window data to non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.Kill()
  NYS:DockingPane_EventMgr.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UpdateDctVersions(OverStr,Thread())
    ReturnValue = GlobalResponse
  END
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
    CASE ACCEPTED()
    OF ?CommentsList
      Get(MyDct.CommentsQ,Choice(?CommentsList))
      ParamsGrp.CommentJson = MyDct.CommentsQ.Text
      If Keycode() = MouseLeft2
        !MyDct.Trace('UpdateDictionary - Double Clicked ?CommentsList')  
        SetKeycode(0)  
        POST(EVENT:Accepted,?EditCommentBtn)
      End	      
    OF ?AddOptionBtn
      !      
    OF ?DeleteOptionBtn
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteOptionQRec(MyDct.OptionsQ,Dictionaries,'OptionsBlob')
        !MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
        Glo:SaveBtnFeq{PROP:Disable} = False
        Post(DctEvent:RefreshData)
      End
    OF ?OptionsList
      Get(MyDct.OptionsQ,Choice(?OptionsList))      
      !ParamsGrp.PropertyLabel = MyDct.OptionsQ.Property
      !ParamsGrp.PropertyType = MyDct.OptionsQ.PropertyType
      !ParamsGrp.PropertyValue = MyDct.OptionsQ.PropertyValue
      !OptionGrp = MyDct.OptionsQ
      !json.Start()
      !json.SetTagCase(jF:CaseAsIs)
      !json.Save(OptionGrp,st)
      ParamsGrp.OptionJson = MyDct.GetOptionJson(MyDct.OptionsQ) !st.GetValue()
      If Keycode() = MouseLeft2
        !MyDct.Trace('UpdateDictionary - Double Clicked ?OptionsList') 
        SetKeycode(0)  
        POST(EVENT:Accepted,?EditOptionBtn)
      End   
    END
  !
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    OF ?AddCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertComment'
      ParamsGrp.ProcedureToNotify = 'UpdateDictionary'
      !MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset
    OF ?EditCommentBtn
      ThisWindow.Update()
      !MyDct.Trace('OF ?EditCommentBtn - ' & Clip(CommentQ.Text))
      ParamsGrp.Action = 'EditComment'
      ParamsGrp.ProcedureToNotify = 'UpdateDictionary'
      ParamsGrp.CommentJson = Clip(MyDct.CommentsQ.Text)    
      !MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset   
    OF ?DeleteCommentBtn
      ThisWindow.Update()
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteCommentQRec(MyDct.CommentsQ,Dictionaries,'CommentsBlob')
        !MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
        Glo:SaveBtnFeq{PROP:Disable} = False
        Post(DctEvent:RefreshData)
      End
      !      Delete(CommentQ)
      !      xml.Start()
      !      xml.SetTagCase(XF:CaseAsIs)
      !      xml.SetOmitXMLHeader(True)
      !      xml.SetDontSaveBlanks(true)
      !      xml.SetDontSaveBlankGroups(true)
      !      xml.Save(CommentQ,st,'','Comment')
      !      !MyDct.Trace('')  
      !      !MyDct.Trace('UpdateDictionary - EVENT:Accepted ?DeleteCommentBtn - Records(CommentQ) = ' & Records(CommentQ))
      !      !MyDct.Trace('')   
      !      !MyDct.Trace(st.GetValue())   
      !      !MyDct.Trace('')   
      !      st.ToBlob(Dct:CommentsBlob)
      !      Access:Dictionaries.Update()
      !      Post(DctEvent:RefreshData)
    OF ?AddOptionBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertOption'
      ParamsGrp.ProcedureToNotify = 'UpdateDictionary'
      !MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
      START(UpdateOption, 25000, OverStr,Thread())
      ThisWindow.Reset      
    OF ?EditOptionBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'EditOption'
      ParamsGrp.ProcedureToNotify = 'UpdateDictionary'
      !MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
      START(UpdateOption, 25000, OverStr,Thread())
      ThisWindow.Reset            
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeCloseEvent PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.TakeCloseEvent()
  if ReturnValue = Level:Benign AND DonutHoleWindow.GetIsConnected()
    if SELF.Response = RequestCompleted
      !DonutHoleWindow.NotifyHost(not:RecordUpdated, Dct:GUID)  
    end!if
    DonutHoleWindow.NotifyHost(not:closeWindow, thread())
    ReturnValue = Level:Benign
  end!if  
  RETURN ReturnValue


ThisWindow.TakeCompleted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    If MyDct.CheckRecordHash(Dictionaries) = DwrDct:RecordChanged
    !If ~Access:Dictionaries.EqualBuffer(self.saved)
        Access:Dictionaries.Update()
        !self.Saved = Access:Dictionaries.SaveBuffer()
    End   
  ReturnValue = PARENT.TakeCompleted()
    Case ReturnValue  
    Of Level:Benign
        MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','RecordSaved',OverStr)
        MyDct.SetCurrentRecordHash(Dictionaries)
        Post(DctEvent:RefreshData)
        Glo:SaveBtnFeq{PROP:Disable} = True
    Of Level:Notify
    Of Level:Fatal
    End  
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  mhResize.InitialResize
  ReturnValue = mhResize.TakeEvent()
  IF ReturnValue <> LEVEL:Benign
    RETURN ReturnValue
  END
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    Case EVENT()
    Of EVENT:CloseWindow
        MyDct.Trace('UpdateDictionary - EVENT:CloseWindow') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))  
    Of DctEvent:SaveRecord
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - DctEvent:SaveRecord')
        Access:Dictionaries.Update()
        !Access:Dictionaries.RestoreBuffer(self.Saved)
        !Clear(self.Saved)
  	    !Dct:GUID = ParamsGrp.DictionaryGuid  
  	    !Access:Dictionaries.Fetch(Dct:PKDctGUIDKey)
        !self.Saved = Access:Dictionaries.SaveBuffer()
        !Access:Dictionaries.RestoreBuffer(self.Saved)
        !ThisWindow.Reset()
    Of EVENT:Selected
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - EVENT:Selected')
        If MyDct.CheckRecordHash(Dictionaries) = DwrDct:RecordChanged
            If ParamsGrp.GlobalRequest <> 0
                MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
                !Glo:SaveBtnFeq{PROP:Disable} = False  
            End
        End
    Of EVENT:Accepted
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - EVENT:Accepted')
        !MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - EVENT:Selected <<<<<<<<<<<<<--------------------------------------------------------------------------------------')
  
        IF NOT SELF.Primary.Me.EqualBuffer(SavedBuffer) ! did something change?
            MyDct.Trace('-------------- Record Changed ----------------------------')
        END
    !SELF.Primary.Me.RestoreBuffer(SavedBuffer, False)    
  
        !IF ThisWindow.OriginalRequest=2
        !    IF Not Access:Dictionaries.EqualBuffer(self.Saved)
        !        MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
        !        Glo:SaveBtnFeq{PROP:Disable} = False 
        !    ELSE
        !        !POST(EVENT:Accepted,?Cancel)
        !    END
        !END
  
        !Rec &= Dictionaries{PROP:Record}
        !DctGrp = Rec
        !st.SetValue(DctOverStr)
        !MyCrypto.MakeHash(st)
        !MyDct.Trace('')
        !MyDct.Trace(st.GetValue())
        !MyDct.Trace('')
        !If self.Saved
            !If Not Access:Dictionaries.EqualBuffer(self.Saved)  
            !    MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
            !    Glo:SaveBtnFeq{PROP:Disable} = False 
            !End 
        !End
    Of EVENT:LoseFocus
        !MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - EVENT:LoseFocus')
    Of event:DonutHoleConnected
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - event:DonutHoleConnected')
    Of event:DonutHoleDisconnected
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - event:DonutHoleDisconnected')
        !If self.Saved  
  
  
  
        If MyDct.CheckRecordHash(Dictionaries) = DwrDct:RecordChanged
            !If Not Access:Dictionaries.EqualBuffer(self.Saved)
            Case MESSAGE('The item has changed. Do you want to save changes?','Dictionary Editor',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
            Of BUTTON:NO
            Of BUTTON:YES
                Access:Dictionaries.Update()
                !self.Saved = Access:Dictionaries.SaveBuffer()
                MyDct.SetCurrentRecordHash(Dictionaries)
                MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','RecordSaved',OverStr)
                !Post(DctEvent:FetchRecord)
            End       
            !MyDct.Trace('UpdateField - event:DonutHoleDisconnected - Before RestoreBuffer')
            !Access:Dictionaries.RestoreBuffer(self.Saved)
        End  
    Of event:DonutHoleWinRepaint
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - event:DonutHoleWinRepaint')
    Of event:DonutHoleHide
    Of EVENT:Restored
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - EVENT:Restored')
    Of EVENT:NewSelection   !Post(DctEvent:RefreshControls)
    Of EVENT:GainFocus
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - EVENT:GainFocus')
        !Post(DctEvent:RefreshControls)
    Of DctEvent:FetchRecord
  	    Dct:GUID = ParamsGrp.DictionaryGuid  
  	    Access:Dictionaries.Fetch(Dct:PKDctGUIDKey)
        !Rec &= Dictionaries{PROP:Record}
        !self.Saved = Access:Dictionaries.SaveBuffer()
        Rec &= Dictionaries{PROP:Record}
        !DctGrp = Rec
        !st.SetValue(DctOverStr)
        !MyCrypto.MakeHash(st)
        !MyDct.Trace('')
        !MyDct.Trace(st.GetValue())
        !MyDct.Trace('')
        MyDct.SetCurrentRecordHash(Dictionaries)
        !MakeHash (*StringTheory st, long hashType=cs:CALG_SHA1, Long Encode = Crypto:EncHex)
        Post(DctEvent:RefreshData)
    Of DctEvent:RefreshData
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - DctEvent:RefreshData')
        local.PopulateCommentsQ()
        local.PopulateOptionsQ()
        local.ResetVersionsBrowse()
        If Records(MyDct.CommentsQ) <> CommentRecsCnt
            CommentRecsCnt = Records(MyDct.CommentsQ)
            If ParamsGrp.GlobalRequest <> 0
                MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
            End
        END
        If Records(MyDct.OptionsQ) <> OptionRecCnt
            OptionRecCnt = Records(MyDct.OptionsQ)
            If ParamsGrp.GlobalRequest <> 0
                MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
            End
        End
        Post(DctEvent:RefreshControls)
    Of DctEvent:RefreshControls
        MyDct.Trace('UpdateDictionary - ThisWindow.TakeEvent - DctEvent:RefreshControls')
        If Records(MyDct.CommentsQ)
            ?EditCommentBtn{PROP:Disable} = FALSE
            ?DeleteCommentBtn{PROP:Disable} = FALSE
        ELSE
            ?EditCommentBtn{PROP:Disable} = True
            ?DeleteCommentBtn{PROP:Disable} = True
        End
        If Records(MyDct.OptionsQ)
            ?EditOptionBtn{PROP:Disable} = False
            ?DeleteOptionBtn{PROP:Disable} = FALSE
        ELSE
            ?EditOptionBtn{PROP:Disable} = True
            ?DeleteOptionBtn{PROP:Disable} = True
        End
        !Display(?EditCommentBtn)
        !Display(?EditOptionBtn)
        !Display(?DeleteOptionBtn)
        !Display(?DeleteCommentBtn)
        If ParamsGrp.GlobalRequest <> 0
            Post(EVENT:Selected)
        End
    End  
  ReturnValue = PARENT.TakeEvent()
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeEvent(EVENT())
    IF NYS:DockingPane_EventMgr.TakeEventCycleRequired = TRUE THEN CYCLE.
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  If event() = event:VisibleOnDesktop !or event() = event:moved
    ds_VisibleOnDesktop()
  end
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
   ! !MyDct.Trace('UpdateDictionary - ThisWindow.TakeNewSelection')  
    !Post(DctEvent:RefreshControls)  
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
    OF EVENT:Sized
      !      of event:DonutHoleHide
      !      SELF.SetResponse(RequestCancelled)
      !      bRepost = true      
    END
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeWindowEvent(QuickWindow)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
        post(event:visibleondesktop)
    END
    !MyDct.Trace('UpdateDictionary - ThisWindow.TakeWindowEvent')  
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.Update PROCEDURE

  CODE
  PARENT.Update
    MyDct.Trace('UpdateDictionary - ThisWindow.Update')
    !    Post(DctEvent:RefreshControls)  
  

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
local.PopulateCommentsQ       Procedure()
TempSt          StringTheory
LneSt           StringTheory
StartPos        Long
EndPos          Long
xml             xFilesTree
    code
    MyDct.PopulateCommentsQ(Dictionaries,'CommentsBlob')
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateDictionary - local.PopulateCommentsQ')
!    Free(CommentQ)
!    !Dct:GUID = ParamsGrp.DictionaryGuid  
!    !Access:Dictionaries.Fetch(Dct:PKDctGUIDKey)    
!    TempSt.Start()
!    TempSt.FromBlob(Dct:CommentsBlob)
!    !MyDct.Trace('')
!    !MyDct.Trace(TempSt.GetValue())
!    !MyDct.Trace('')
!    !MyDct.Trace('Length = ' & st.Length())
!    !MyDct.Trace('')
!    StartPos = TempSt.FindChars('<Comment<32>')
!    Loop 
!        If StartPos => TempSt.Length(); Break End
!        LneSt.Start()
!        LneSt.SetValue(MyDct.GetDelimitedStr(TempSt,StartPos,EndPos,'</Comment>',False),True)
!        LneSt.Remove('<13,10>')
!        !MyDct.Trace(StartPos & '<9>' & LneSt.GetValue())
!        Clear(CommentGrp)
!        If LneSt.ContainsA('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
!            xml.Start()
!            xml.SetTagCase(XF:CaseAsIs)
!            xml.Load(CommentGrp,LneSt,'Comment')
!            CommentQ = CommentGrp
!            Add(CommentQ)
!        End
!        StartPos = StartPos + 1
!    End
!    If Not CommentRecsCnt
!        CommentRecsCnt = Records(CommentQ)
!    END
!    TempSt.Start()
!    !MyDct.Trace('')

local.PopulateOptionsQ          Procedure()
TempSt          StringTheory
LneSt           StringTheory
StartPos        Long
EndPos          Long
xml             xFilesTree
    code
    MyDct.PopulateOptionsQ(Dictionaries,'OptionsBlob')
!    Free(OptionsQ)
!    !Dct:GUID = ParamsGrp.DictionaryGuid  
!    !Access:Dictionaries.Fetch(Dct:PKDctGUIDKey)    
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateDictionary - local.PopulateOptionsQ')
!    TempSt.Start()
!    TempSt.FromBlob(Dct:OptionsBlob)
!    !MyDct.Trace('')
!    !MyDct.Trace(TempSt.GetValue())
!    !MyDct.Trace('')
!    !MyDct.Trace('Length = ' & TempSt.Length())
!    !MyDct.Trace('')
!    StartPos = TempSt.FindChars('<Option<32>')    
!    Loop 
!        If StartPos => TempSt.Length(); Break End
!        LneSt.Start()
!        LneSt.SetValue(MyDct.GetDelimitedStr(TempSt,StartPos,EndPos,'/>',False),True)
!        LneSt.Remove('<13,10>')
!        !MyDct.Trace(StartPos & '<9>' & LneSt.GetValue())
!        Clear(OptionGrp)
!        If LneSt.ContainsA('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
!            xml.Start()
!            xml.SetTagCase(XF:CaseAsIs)
!            xml.Load(OptionGrp,LneSt,'Option')
!            OptionGrp.Id = Records(OptionsQ) + 1
!            OptionsQ = OptionGrp
!            Add(OptionsQ)
!        End
!    End
!    If Not OptionRecCnt
!        OptionRecCnt = Records(OptionsQ)
!    End
!    TempSt.Start()
!    !MyDct.Trace('')
!    !st.Start()
    

local.ResetOptionsBrowse        Procedure()
    CODE
    !BRW11.SetFilter('(Opt:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')')
    !BRW11.ResetSort(True)

local.ResetVersionsBrowse       Procedure()
    CODE
    BrwVersions.SetFilter('(Ver:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')')
    BrwVersions.ResetSort(True)

mhResize.AfterResize PROCEDURE

  CODE
  PARENT.AfterResize
  !  DonutHoleWindow.Repaint()  


mhResize.Init PROCEDURE

  CODE
  SELF.Init(                                               |
      mhResizeFrame,                                       | !Global Host Object
      QuickWindow,                                         | !Window
      UpdateDictionary:mhResize:WM.MH::ResizeIWindowManager, | !Window Manager
      449,                                                 | !Original Width
      183,                                                 | !Original Height
      1,                                                   | !Resize Width?
      1,                                                   | !Resize Height?
      0,                                                   | !Orig Size is Min?
      0,                                                   | !Border Beyond Min?
      1,                                                   | !Maximize Initially?
      1)                                                     !Do NOT hide+unhide during opening operations?
  SELF.AddControl(?OK,1,0,0,0)
  SELF.AddControl(?Cancel,1,0,0,0)
  SELF.AddControl(?SHEET1,0,0,1,1)
  SELF.AddControl(?CommentsList,0,0,1,1)
  SELF.AddControl(?VersionsList,0,0,1,1)
  SELF.AddControl(?OptionsList,0,0,1,1)
  RETURN
  PARENT.Init

mhResize.InitialResize PROCEDURE()

MH::RestoreFromIni   BYTE,AUTO

  CODE
  IF NOT SELF.IsInitialResizeDone
    MH::RestoreFromIni = CHOOSE(INIMgr.TryFetch('UpdateDictionary','XPos') OR INIMgr.TryFetch('UpdateDictionary','Maximize'))
    SELF.InitialResize(                                    |
        MH::RestoreFromIni,                                |  !Restore from INI file?
        1,                                                 |  !Maximize Initially?
        mhHorzPos:Full,                                    |  !Init Horz Pos
        mhVertPos:Full,                                    |  !Init Vert Pos
        0,                                                 |  !Horizontal Shift
        0                                                  )  !Vertical Shift
    IF SELF.IsInitialResizeDone
    END
  END


UpdateDictionary:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
  CODE
  ThisWindow.Reset(Force)


BrwVersions.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END


BrwVersions.TakeNewSelection PROCEDURE

  CODE
  PARENT.TakeNewSelection
  !  Post(DctEvent:RefreshData)  

NotifyHandler.HandleNotify       PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter)
us    UltimateString
TempSt          StringTheory
XmlSt           StringTheory
    CODE    
    NotifyParam = NotifyManager.GetNotifyParm(NotifyParameter)
	If Size(NotifyParam) > 0
		TempSt.SetValue(NotifyParam)
        !MyDct.Trace('')
        !MyDct.Trace('UpdateDictionary - NotifyHandler.HandleNotify')
		MyDct.Trace(TempSt.GetValue())
        !MyDct.Trace('')
		json.Start()
		json.SetTagCase(jF:CaseAsIs)
		json.Load(ParamsGrp,TempSt)
        If ParamsGrp.Action = ''
            Post(DctEvent:FetchRecord)
        End
        Case ParamsGrp.Action[1:Size(ParamsGrp.Action)]
        OF 'InsertComment'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
            MyDct.InsertCommentQRec(MyDct.CommentsQ,Dictionaries,'CommentsBlob',ParamsGrp.CommentJson)
        OF 'EditComment'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
            MyDct.UpdateCommentQRec(MyDct.CommentsQ,Dictionaries,'CommentsBlob',ParamsGrp.CommentJson)
        Of 'InsertOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
            MyDct.InsertOptionQRec(MyDct.OptionsQ,Dictionaries,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'EditOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
            MyDct.UpdateOptionQRec(MyDct.OptionsQ,Dictionaries,'OptionsBlob',ParamsGrp.OptionJson)
!            XmlSt.SetValue(ParamsGrp.OptionJson,True)
!            json.Start()
!            json.SetTagCase(jF:CaseAsIs)
!            json.Load(OptionGrp,XmlSt)
!            OptionsQ.Id = OptionGrp.Id
!            Get(OptionsQ,OptionsQ.Id)
!            If Not Errorcode()
!                OptionsQ = OptionGrp
!                Put(OptionsQ)
!            ELSE
!                !MyDct.Trace('UpdateDictionary - NotifyHandler.HandleNotify - Error GETing OptionsQ Record!')
!            End
!            xml.Start()
!            xml.SetTagCase(XF:CaseAsIs)
!            xml.SetOmitXMLHeader(True)
!            xml.SetDontSaveBlanks(True)
!            xml.Save(OptionsQ,XmlSt,'','Option')
!            XmlSt.ToBlob(Dct:OptionsBlob)  
!            Display(?OptionsList)
!            Access:Dictionaries.Update()
        Of 'RecordSaved'
            POST(EVENT:Accepted,?OK)
            !Post(DctEvent:SaveRecord)
        END  
        !MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
        !Glo:SaveBtnFeq{PROP:Disable} = False
        Clear(ParamsGrp.Action)
        Display(?CommentsList)
	End
    Post(DctEvent:RefreshData)
DonutHoleWindow.Repaint               PROCEDURE()!,DERIVED
  CODE
  PARENT.Repaint()
