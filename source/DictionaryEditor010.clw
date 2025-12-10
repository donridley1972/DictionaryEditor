

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABDROPS.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR010.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('DICTIONARYEDITOR016.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR017.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! Form Tables
!!! </summary>
UpdateTable PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
json				JSONClass
st					StringTheory
MyDct				dwrDctParser
xml                 xFilesTree
mt                  MyTableClass

DctEvent:FetchRecord        Equate(EVENT:User+900)
DctEvent:RefreshData        Equate(EVENT:User+901)
DctEvent:RefreshControls    Equate(EVENT:User+902)

OptionGrp               Group(OptionGroupType).
CommentGrp              Group(CommentGroupType).

CommentRecsCnt          Long
OptionRecCnt            Long


SavedBuffer             USHORT
RecordChanged           BYTE

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
!
!OptionGrp       			Group,Name('Option')
!Id                          LONG,NAME('Id | Private')
!Property                   	STRING(100),Name('Property | attribute')
!PropertyType               	STRING(5),Name('PropertyType | attribute')
!PropertyValue              	STRING(100),Name('PropertyValue | attribute')
!							End

local                       CLASS
PopulateCommentsQ           Procedure()
PopulateOptionsQ            Procedure()
                            End
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
CurrentTab           STRING(80)                            ! 
NotifyParam          STRING(1500)                          ! 
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
OptionsQ             QUEUE,PRE(),NAME('Option')            ! 
Id                   LONG,NAME('Id | Private')             ! 
Property             STRING(100),NAME('Property | attribute') ! 
PropertyType         STRING(5),NAME('PropertyType | attribute') ! 
PropertyValue        STRING(100),NAME('PropertyValue | attribute') ! 
                     END                                   ! 
ActionMessage        CSTRING(40)                           ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DonutHoleWindow     Class(NinjaDonutHoleWindowClass)
Repaint               PROCEDURE(),DERIVED
                    end
FDB4::View:FileDrop  VIEW(DriversLkUp)
                       PROJECT(Dri:Driver)
                       PROJECT(Dri:GUID)
                     END
Queue:FileDrop       QUEUE                            !
Dri:Driver             LIKE(Dri:Driver)               !List box control field - type derived from field
Dri:GUID               LIKE(Dri:GUID)                 !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::Tab:Record  LIKE(Tab:RECORD),THREAD
QuickWindow          WINDOW('Update Tables'),AT(,,449,206),FONT('Segoe UI',10,,FONT:regular,CHARSET:ANSI),RESIZE, |
  CENTER,ICON('Dictionary.ico'),GRAY,HLP('UpdateTable'),SYSTEM,IMM
                       SHEET,AT(2,1,446,204),USE(?CurrentTab),ABOVE
                         TAB('General'),USE(?GeneralTab)
                           PROMPT('Usage:'),AT(6,19),USE(?Tab:Usage:Prompt),TRN
                           ENTRY(@s100),AT(74,19,205,10),USE(Tab:Usage),LEFT(2)
                           PROMPT('Label:'),AT(6,33),USE(?Tab:TableName:Prompt),TRN
                           ENTRY(@s100),AT(74,33,205,10),USE(Tab:TableName),LEFT(2)
                           PROMPT('Description:'),AT(6,48),USE(?Tab:Description:Prompt),TRN
                           ENTRY(@s255),AT(74,47,205,10),USE(Tab:Description),LEFT(2)
                           PROMPT('Prefix:'),AT(6,62),USE(?Tab:TablePrefix:Prompt),TRN
                           ENTRY(@s20),AT(74,61,84,10),USE(Tab:TablePrefix),LEFT(2)
                           PROMPT('Driver:'),AT(6,76),USE(?Tab:TableDriver:Prompt),TRN
                           ENTRY(@s100),AT(74,104,205,10),USE(Tab:Owner),LEFT(2)
                           ENTRY(@s100),AT(74,89,205,10),USE(Tab:DriverOption),LEFT(2)
                           PROMPT('Owner:'),AT(6,104),USE(?Tab:Owner:Prompt),TRN
                           PROMPT('Driver Options:'),AT(6,90),USE(?Tab:DriverOption:Prompt),TRN
                           PROMPT('Full Path Name:'),AT(6,118),USE(?Tab:TablePath:Prompt),TRN
                           ENTRY(@s100),AT(74,118,205,10),USE(Tab:TablePath),LEFT(2)
                           CHECK(' Create'),AT(74,141),USE(Tab:Create),TRN,VALUE('true','false')
                           CHECK(' Threaded'),AT(161,141),USE(Tab:Thread),TRN,VALUE('true','false')
                           CHECK(' Recliam'),AT(74,152),USE(Tab:Reclaim),TRN,VALUE('true','false')
                           CHECK(' OEM'),AT(161,152),USE(Tab:OEM),TRN,VALUE('true','false')
                           CHECK(' Encrypt'),AT(74,167),USE(Tab:Encrypt),TRN,VALUE('true','false')
                           CHECK(' Bindable'),AT(161,167),USE(Tab:Bindable),TRN,VALUE('true','false')
                           LIST,AT(74,75,116,10),USE(Tab:TableDriver),LEFT(2),DROP(20),FORMAT('80L(2)|M~Driver~@s20@'), |
  FROM(Queue:FileDrop)
                         END
                         TAB('Comments'),USE(?CommentsTab)
                           LIST,AT(10,37,429,113),USE(?CommentsList),FORMAT('44L(2)M~Create Date~@s11@#5#108L(2)M~' & |
  'Create User~@s50@#8#1020L(2)M~Text~@s255@#2#')
                           BUTTON,AT(10,17),USE(?AddCommentBtn),ICON('AddNew.ico'),FLAT
                           BUTTON,AT(31,17),USE(?EditCommentBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(53,17),USE(?DeleteCommentBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                         END
                         TAB('Options'),USE(?OptionsTab)
                           BUTTON,AT(10,17),USE(?AddOptionBtn),ICON('AddNew.ico'),FLAT
                           BUTTON,AT(31,17),USE(?EditOptionBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(53,17),USE(?DeleteOptionBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                           LIST,AT(10,37,429,113),USE(?OptionsList),FORMAT('97L(2)M~Property~@s100@#2#62L(2)M~Prop' & |
  'erty Type~@s5@#3#400L(2)M~Property Value~@s100@#4#')
                         END
                         TAB('Bulk Table Actions'),USE(?BulkToolsTab)
                         END
                       END
                       BUTTON,AT(397,1,23,12),USE(?OK),LEFT,ICON('Save1.ico'),DEFAULT,DISABLE,FLAT,HIDE,MSG('Accept dat' & |
  'a and close the window'),TIP('Accept data and close the window')
                       BUTTON,AT(423,1,23,12),USE(?Cancel),LEFT,ICON('Close.ico'),FLAT,HIDE,MSG('Cancel operation'), |
  TIP('Cancel operation')
                     END

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
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeCloseEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeCompleted          PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
Update                 PROCEDURE(),DERIVED
                     END

Toolbar              ToolbarClass
mhResize             CLASS(MH::ResizeWindowClass)
AfterResize            PROCEDURE(),DERIVED
Init                   PROCEDURE()
InitialResize          PROCEDURE()                         ! New method added to this class instance
                     END

UpdateTable:mhResize:WM CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END
FDB4                 CLASS(FileDropClass)                  ! File drop manager
Q                      &Queue:FileDrop                !Reference to display queue
                     END

CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(DriversLkUp:Record)
? DEBUGHOOK(Tables:Record)
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
    MyDct.Trace('[UpdateTable][ThisWindow.ChangeAction] <<<<<<<<<<<<<<<<<<<<<<<<<<<--------------------------------------------------------')  
  ReturnValue = PARENT.ChangeAction()
  RETURN ReturnValue


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  	OverStr = pParams  
    self.Primary &= Relate:Tables
  GlobalErrors.SetProcedureName('UpdateTable')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Tab:Usage:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
    MyDct.Trace('')
    MyDct.Trace('UpdateTable - SELF.SetAlerts()')
    MyDct.Trace('   ParamsGrp.Action = ' & ParamsGrp.Action) 
    MyDct.Trace('   ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
    MyDct.Trace('   ParamsGrp.TableGuid = ' & ParamsGrp.TableGuid)
    MyDct.Trace('   ParamsGrp.GlobalRequest = ' & ParamsGrp.GlobalRequest) 
    MyDct.Trace('   ParamsGrp.Usage = ' & ParamsGrp.Usage)
    MyDct.Trace('   ParamsGrp.Order = ' & ParamsGrp.Order)
    MyDct.Trace('   SELF.Request = ' & SELF.Request)
    MyDct.Trace('')  
    
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('UpdateTable',pThread)
    NotifyHandler.AddNotifyCode(1)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(Tab:Record,History::Tab:Record)
  SELF.AddHistoryField(?Tab:Usage,8)
  SELF.AddHistoryField(?Tab:TableName,9)
  SELF.AddHistoryField(?Tab:Description,10)
  SELF.AddHistoryField(?Tab:TablePrefix,11)
  SELF.AddHistoryField(?Tab:Owner,14)
  SELF.AddHistoryField(?Tab:DriverOption,13)
  SELF.AddHistoryField(?Tab:TablePath,15)
  SELF.AddHistoryField(?Tab:Create,16)
  SELF.AddHistoryField(?Tab:Thread,20)
  SELF.AddHistoryField(?Tab:Reclaim,17)
  SELF.AddHistoryField(?Tab:OEM,19)
  SELF.AddHistoryField(?Tab:Encrypt,18)
  SELF.AddHistoryField(?Tab:Bindable,21)
  SELF.AddHistoryField(?Tab:TableDriver,12)
  SELF.AddUpdateFile(Access:Tables)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:DriversLkUp.Open()                                ! File DriversLkUp used by this procedure, so make sure it's RelationManager is open
  Relate:Tables.SetOpenRelated()
  Relate:Tables.Open()                                     ! File Tables used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Tables
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
  
  SELF.Open(QuickWindow)                                   ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('UpdateTable', THREAD(), QuickWindow)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?CurrentTab{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
  QuickWindow{PROP:Buffer} = 1       
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
    ?Tab:Usage{PROP:ReadOnly} = True
    ?Tab:TableName{PROP:ReadOnly} = True
    ?Tab:Description{PROP:ReadOnly} = True
    ?Tab:TablePrefix{PROP:ReadOnly} = True
    ?Tab:Owner{PROP:ReadOnly} = True
    ?Tab:DriverOption{PROP:ReadOnly} = True
    ?Tab:TablePath{PROP:ReadOnly} = True
    DISABLE(?Tab:TableDriver)
    DISABLE(?AddCommentBtn)
    DISABLE(?EditCommentBtn)
    DISABLE(?DeleteCommentBtn)
    DISABLE(?AddOptionBtn)
    DISABLE(?EditOptionBtn)
    DISABLE(?DeleteOptionBtn)
  END
  mhResize.Init
  FDB4.Init(?Tab:TableDriver,Queue:FileDrop.ViewPosition,FDB4::View:FileDrop,Queue:FileDrop,Relate:DriversLkUp,ThisWindow)
  FDB4.Q &= Queue:FileDrop
  FDB4.AddSortOrder()
  FDB4.AddField(Dri:Driver,FDB4.Q.Dri:Driver) !List box control field - type derived from field
  FDB4.AddField(Dri:GUID,FDB4.Q.Dri:GUID) !Primary key field - type derived from field
  FDB4.AddUpdateField(Dri:Driver,Tab:TableDriver)
  ThisWindow.AddItem(FDB4.WindowComponent)
  FDB4.DefaultFill = 0
  DonutHoleWindow.SetLogPreamble('UpdateTable')
  DonutHoleWindow.init(DonutHoleRegistrator.IDonutHoleRegistration,thread(),QuickWindow)
  SELF.AddItem(DonutHoleWindow.WindowComponent)
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  
    ?CommentsList{PROP:From} = MyDct.CommentsQ
    ?OptionsList{PROP:From} = MyDct.OptionsQ
    
    If ParamsGrp.GlobalRequest <> InsertRecord
        Post(DctEvent:FetchRecord)
    ELSE
  		!Tab:PKGuid = ParamsGrp.TableGuid  
  		!Access:Tables.Fetch(Tab:PKTabGuidKey) 
        Clear(Tables)
        MyDct.SetCurrentRecordHash(Tables)
    End    
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('UpdateTable', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:DriversLkUp.Close()
    Relate:Tables.Close()
  END
    NotifyManager.DeleteProcedure('UpdateTable',pThread)
  mhResize.Kill
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
    Omit('****')  
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
    ****  
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
    OF ?OptionsList
      Get(MyDct.OptionsQ,Choice(?OptionsList))    
      ParamsGrp.OptionJson = MyDct.GetOptionJson(MyDct.OptionsQ)
      If Keycode() = MouseLeft2
        !MyDct.Trace('UpdateDictionary - Double Clicked ?OptionsList') 
        SetKeycode(0)  
        POST(EVENT:Accepted,?EditOptionBtn)
      End         
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?AddCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertComment'
      ParamsGrp.ProcedureToNotify = 'UpdateTable'
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset      
    OF ?EditCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'EditComment'
      ParamsGrp.ProcedureToNotify = 'UpdateTable'
      ParamsGrp.CommentJson = Clip(MyDct.CommentsQ.Text)
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset       
    OF ?DeleteCommentBtn
      ThisWindow.Update()
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteCommentQRec(MyDct.CommentsQ,Tables,'CommentsBlob')  
        ?OK{PROP:Disable} = False
        Post(DctEvent:RefreshData)
      End
    OF ?AddOptionBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertOption'
      ParamsGrp.ProcedureToNotify = 'UpdateTable'
      START(UpdateOption, 25000, OverStr,Thread())
      ThisWindow.Reset         
    OF ?EditOptionBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'EditOption'
      ParamsGrp.ProcedureToNotify = 'UpdateTable'
      START(UpdateOption, 25000, OverStr,Thread())
      ThisWindow.Reset       
    OF ?DeleteOptionBtn
      ThisWindow.Update()
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteOptionQRec(MyDct.OptionsQ,Tables,'OptionsBlob')    
        ?OK{PROP:Disable} = False  
        Post(DctEvent:RefreshData)
      End
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
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
      !DonutHoleWindow.NotifyHost(not:RecordUpdated, CUS:CustNumber)  
    end!if
    DonutHoleWindow.NotifyHost(not:closeWindow, thread())
    ReturnValue = Level:Benign
  end!if    
  RETURN ReturnValue


ThisWindow.TakeCompleted PROCEDURE

ReturnValue          BYTE,AUTO

st          StringTheory
TableGrp    Group(TableGroupType).
TblOverStr  String(Size(TableGrp)),Over(TableGrp)
Rec         &GROUP
Looped BYTE
  CODE
  LOOP
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    MyDct.Trace('')
    MyDct.Trace('UpdateTable - ThisWindow.TakeCompleted - self.Request = ' & self.Request)
    MyDct.Trace('   ParamsGrp.Action = ' & ParamsGrp.Action) 
    MyDct.Trace('   ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
    MyDct.Trace('   ParamsGrp.TableGuid = ' & ParamsGrp.TableGuid)
    MyDct.Trace('   SELF.Request = ' & SELF.Request)
    MyDct.Trace('   GlobalRequest = ' & GlobalRequest)  
    MyDct.Trace('')
    !If ~Access:Tables.EqualBuffer(self.saved)
    !If MyDct.CheckRecordHash(Tables) = DwrDct:RecordChanged
  
        !self.Saved = Access:Tables.SaveBuffer()
    !End     
  ReturnValue = PARENT.TakeCompleted()
    Case ReturnValue  
    Of Level:Benign
        ?OK{PROP:Disable} = True
        If ParamsGrp.Usage <> 'Table'
            If Tab:Usage = ''
                Tab:Usage = ParamsGrp.Usage
            End
        End
        If Tab:PKGuid = '' !ParamsGrp.Action = InsertRecord
            MyDct.Trace('<9>Insert Record')
            Rec &= Tables{PROP:Record}
            TableGrp                 =  Rec
            TableGrp.PKGuid          =  glo:st.MakeGuid()
            TableGrp.ParentGUID      =  ParamsGrp.DictionaryGuid
            TableGrp.DictionaryGuid  =  ParamsGrp.DictionaryGuid
            TableGrp.Guid            =  MyDct.GetWindowsGuid()
            TableGrp.DctxOrder       =  ParamsGrp.Order + 1 !Records(Tables)+1
            !TableGrp.TableName = Tab:TableName
            
            If ParamsGrp.Usage <> 'Table'
                TableGrp.Usage = ParamsGrp.Usage
            End
            MyDct.InsertTable(Tables,TblOverStr)
            !Tab:PKGuid = glo:st.MakeGuid()
            !Tab:ParentGUID = ParamsGrp.DictionaryGuid
            !Tab:DictionaryGuid = ParamsGrp.DictionaryGuid
            !Tab:Guid = MyDct.GetWindowsGuid()
            !Tab:DctxOrder = Records(Tables)+1
            !st.SetValue(MyDct.GetAuditXml(Today(),Clock(),1))
            !st.ToBlob(Tab:AuditBlob)
            !Access:Tables.Insert()
        Else !ParamsGrp.Action = ChangeRecord
            MyDct.Trace('<9>Update Record')
            Access:Tables.Update()
        End
        MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTable','RecordSaved',OverStr)
        MyDct.SetCurrentRecordHash(Tables)
        !Post(DctEvent:FetchRecord)
        Post(DctEvent:RefreshData)
    Of Level:Notify
    Of Level:Fatal
    End    
  
    !ChangeRecord
    !ViewRecord
    !DeleteRecord
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
        MyDct.Trace('UpdateTable - EVENT:CloseWindow') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))  
    Of event:DonutHoleDisconnected
        MyDct.Trace('UpdateTable - EVENT:DonutHoleDisconnected')
        !If self.Saved
            !If Not Access:Tables.EqualBuffer(self.Saved)
            If MyDct.CheckRecordHash(Tables) = DwrDct:RecordChanged
                Case MESSAGE('The item has changed. Do you want to save changes?','Dictionary Editor',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
                Of BUTTON:NO
                Of BUTTON:YES
                    Access:Tables.Update()
                    !self.Saved = Access:Tables.SaveBuffer()
                End       
            End 
        !    Access:Tables.RestoreBuffer(self.Saved)
        !End  
    Of event:DonutHoleHide
    Of EVENT:Selected
        MyDct.Trace('UpdateTable - EVENT:Selected')
        If MyDct.CheckRecordHash(Tables) = DwrDct:RecordChanged
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTable','SaveNeeded',OverStr)
            Glo:SaveBtnFeq{PROP:Disable} = False  
        End
    Of DctEvent:FetchRecord
        MyDct.Trace('UpdateTable - DctEvent:FetchRecord')
  		Tab:PKGuid = ParamsGrp.TableGuid  
  		Access:Tables.Fetch(Tab:PKTabGuidKey) 
        MyDct.SetCurrentRecordHash(Tables)
        !self.Saved = Access:Tables.SaveBuffer()
        Post(DctEvent:RefreshData)
    Of DctEvent:RefreshData
        MyDct.Trace('UpdateTable - DctEvent:RefreshData')
        local.PopulateCommentsQ()
        local.PopulateOptionsQ()
        If (CommentRecsCnt > 0 And (Records(MyDct.CommentsQ) <> CommentRecsCnt))
            CommentRecsCnt = Records(MyDct.CommentsQ)
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTable','SaveNeeded',OverStr)
        END
        If (OptionRecCnt > 0 And (Records(MyDct.OptionsQ) <> OptionRecCnt))
            OptionRecCnt = Records(MyDct.OptionsQ)
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTable','SaveNeeded',OverStr)
        End
        If Not CommentRecsCnt
            CommentRecsCnt = Records(MyDct.CommentsQ)
        END
        If Not OptionRecCnt
            OptionRecCnt = Records(MyDct.OptionsQ)
        End
        Post(DctEvent:RefreshControls)
    Of DctEvent:RefreshControls
        !MyDct.Trace('UpdateTable - DctEvent:RefreshControls')
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
        Display(?EditCommentBtn)
        Display(?EditOptionBtn)
        Display(?DeleteOptionBtn)
        Display(?DeleteCommentBtn)
        Post(EVENT:Selected)
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
    NYS:DockingPane_EventMgr.TakeWindowEvent(QuickWindow)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
        post(event:visibleondesktop)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.Update PROCEDURE

  CODE
    MyDct.Trace('[UpdateTable][ThisWindow.Update] <<<<<<<<<<<<<<<<<<<<<<<<<<<--------------------------------------------------------')  
  PARENT.Update

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
local.PopulateCommentsQ       Procedure()
TempSt          StringTheory
LneSt           StringTheory
StartPos        Long
EndPos          Long
xml             xFilesTree
    code
    MyDct.PopulateCommentsQ(Tables,'CommentsBlob')
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateDictionary - local.PopulateCommentsQ')
!    Free(CommentQ)
!    TempSt.Start()
!    TempSt.FromBlob(Tab:CommentsBlob)
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
!        Clear(CommentQ)
!        If LneSt.ContainsA('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
!            xml.Start()
!            xml.SetTagCase(XF:CaseAsIs)
!            xml.Load(CommentGrp,LneSt,'Comment')
!            !MyDct.Trace(CommentGrp.Audit.CreateDate & ' ' & WHERE(CommentGrp,CommentGrp.Audit.CreateDate))
!            CommentQ = CommentGrp
!            Add(CommentQ)
!        End
!    End
!    TempSt.Start()
!    !If Not CommentRecsCnt
!    !    CommentRecsCnt = Records(CommentQ)
!    !END
!    !MyDct.Trace('')

local.PopulateOptionsQ          Procedure()
TempSt          StringTheory
LneSt           StringTheory
StartPos        Long
EndPos          Long
xml             xFilesTree
    code
    MyDct.GetHasFM3VersionOption(Tables,'OptionsBlob')
    MyDct.PopulateOptionsQ(Tables,'OptionsBlob')
!    Free(OptionsQ) 
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateDictionary - local.PopulateOptionsQ')
!    TempSt.Start()
!    TempSt.FromBlob(Tab:OptionsBlob)
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
!        Clear(OptionsQ)
!        If LneSt.ContainsA('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
!            xml.Start()
!            xml.SetTagCase(XF:CaseAsIs)
!            xml.Load(OptionGrp,LneSt,'Option')
!            OptionGrp.Id = Records(OptionsQ) + 1
!            OptionsQ = OptionGrp
!            Add(OptionsQ)
!        End
!    End
!    TempSt.Start()
!    !If Not OptionRecCnt
!    !    OptionRecCnt = Records(OptionsQ)
!    !End
!    !MyDct.Trace('')
!    !st.Start()

mhResize.AfterResize PROCEDURE

  CODE
  PARENT.AfterResize
  !  DonutHoleWindow.Repaint() 


mhResize.Init PROCEDURE

  CODE
  SELF.Init(                                               |
      mhResizeFrame,                                       | !Global Host Object
      QuickWindow,                                         | !Window
      UpdateTable:mhResize:WM.MH::ResizeIWindowManager,    | !Window Manager
      449,                                                 | !Original Width
      206,                                                 | !Original Height
      1,                                                   | !Resize Width?
      1,                                                   | !Resize Height?
      0,                                                   | !Orig Size is Min?
      0,                                                   | !Border Beyond Min?
      1,                                                   | !Maximize Initially?
      0)                                                     !Do NOT hide+unhide during opening operations?
  SELF.AddControl(?CurrentTab,0,0,1,1)
  SELF.AddControl(?CommentsList,0,0,1,1)
  SELF.AddControl(?OptionsList,0,0,1,1)
  SELF.AddControl(?OK,1,0,0,0)
  SELF.AddControl(?Cancel,1,0,0,0)
  RETURN
  PARENT.Init

mhResize.InitialResize PROCEDURE()

  CODE
  IF NOT SELF.IsInitialResizeDone
    SELF.InitialResize(                                    |
        0,                                                 |  !Restore from INI file?
        1,                                                 |  !Maximize Initially?
        mhHorzPos:Full,                                    |  !Init Horz Pos
        mhVertPos:Full,                                    |  !Init Vert Pos
        0,                                                 |  !Horizontal Shift
        0                                                  )  !Vertical Shift
    IF SELF.IsInitialResizeDone
    END
  END


UpdateTable:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
  CODE
  ThisWindow.Reset(Force)

NotifyHandler.HandleNotify       PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter)
us    UltimateString
TempSt          StringTheory
XmlSt           StringTheory
    CODE    
    NotifyParam = NotifyManager.GetNotifyParm(NotifyParameter)
	If Size(NotifyParam) > 0
		TempSt.SetValue(NotifyParam)
        !MyDct.Trace('')
        !MyDct.Trace('UpdateTable - NotifyHandler.HandleNotify')
		MyDct.Trace(TempSt.GetValue())
        !MyDct.Trace('')
		json.Start()
		json.SetTagCase(jF:CaseAsIs)
		json.Load(ParamsGrp,TempSt)
  		Dct:GUID = ParamsGrp.DictionaryGuid  
  		Access:Dictionaries.Fetch(Dct:PKDctGUIDKey)
        If ParamsGrp.Action = ''
            Post(DctEvent:FetchRecord)
        End
        Case ParamsGrp.Action[1:Size(ParamsGrp.Action)]
        OF 'InsertComment'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTable','SaveNeeded',OverStr)
            MyDct.InsertCommentQRec(MyDct.CommentsQ,Tables,'CommentsBlob',ParamsGrp.CommentJson)
            !Post(EVENT:Selected)
        OF 'EditComment'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTable','SaveNeeded',OverStr)
            MyDct.UpdateCommentQRec(MyDct.CommentsQ,Tables,'CommentsBlob',ParamsGrp.CommentJson)
        Of 'InsertOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTable','SaveNeeded',OverStr)
            MyDct.InsertOptionQRec(MyDct.OptionsQ,Tables,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'EditOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTable','SaveNeeded',OverStr)
            MyDct.UpdateOptionQRec(MyDct.OptionsQ,Tables,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'RecordSaved'
            POST(EVENT:Accepted,?OK)
        END  
        ?OK{PROP:Disable} = False
        !Clear(ParamsGrp.Action)
        Display(?CommentsList)
	End
    Post(DctEvent:RefreshData)
DonutHoleWindow.Repaint               PROCEDURE()!,DERIVED
  CODE
  PARENT.Repaint()
