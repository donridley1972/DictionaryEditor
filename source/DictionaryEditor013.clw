

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR013.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('DICTIONARYEDITOR016.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR017.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! Form Keys
!!! </summary>
UpdateKey PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
json				JSONClass
st					StringTheory
MyDct				dwrDctParser
xml                 xFilesTree



OptionGrp               Group(OptionGroupType).
CommentGrp              Group(CommentGroupType).

CommentRecsCnt          Long
OptionRecCnt            Long


DctEvent:FetchRecord        Equate(EVENT:User+900)
DctEvent:RefreshData        Equate(EVENT:User+901)
DctEvent:RefreshControls    Equate(EVENT:User+902)

local                       CLASS
PopulateCommentsQ           Procedure()
PopulateOptionsQ            Procedure()
                            End
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
CurrentTab           STRING(80)                            ! 
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
component            QUEUE,PRE(),NAME('Component | RowNam') ! 
guid                 STRING(255),NAME('Guid | attribute')  ! 
fieldid              STRING(255),NAME('FieldId | attribute') ! 
order                STRING(255),NAME('Order | attribute') ! 
ascend               STRING(255),NAME('Ascend | attribute') ! 
audit                GROUP,PRE(),NAME('Audit')             ! 
createuser           STRING(255),NAME('CreateUser | attribute') ! 
createdate           STRING(255),NAME('CreateDate | attribute') ! 
createtime           STRING(255),NAME('CreateTime | attribute') ! 
createversionnumber  STRING(255),NAME('CreateVersionNumber | attribute') ! 
modifieduser         STRING(255),NAME('ModifiedUser | attribute') ! 
modifieddate         STRING(255),NAME('ModifiedDate | attribute') ! 
modifiedtime         STRING(255),NAME('ModifiedTime | attribute') ! 
modifiedversionnumber STRING(255),NAME('ModifiedVersionNumber | attribute') ! 
                     END                                   ! 
                     END                                   ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DonutHoleWindow     Class(NinjaDonutHoleWindowClass)
Repaint               PROCEDURE(),DERIVED
                    end
History::Key:Record  LIKE(Key:RECORD),THREAD
QuickWindow          WINDOW('Form Keys'),AT(,,449,176),FONT('Segoe UI',10,,FONT:regular,CHARSET:ANSI),RESIZE,AUTO, |
  CENTER,GRAY,IMM,MAX,HLP('UpdateKey'),SYSTEM,WALLPAPER('Background1.png')
                       SHEET,AT(2,1,443,174),USE(?CurrentTab),ABOVE
                         TAB('General'),USE(?GeneralTab)
                           PROMPT('Parent GUID:'),AT(6,21),USE(?Key:ParentGUID:Prompt),TRN
                           ENTRY(@s16),AT(70,21,68,10),USE(Key:ParentGUID),LEFT(2)
                           PROMPT('Key Order:'),AT(6,33),USE(?Key:KeyOrder:Prompt),TRN
                           ENTRY(@n10.2),AT(70,33,44,10),USE(Key:KeyOrder),DECIMAL(14)
                           PROMPT('Key Guid:'),AT(6,48),USE(?Key:KeyGuid:Prompt),TRN
                           ENTRY(@s38),AT(70,48,156,10),USE(Key:KeyGuid),LEFT(2)
                           PROMPT('Ident:'),AT(6,64),USE(?Key:Ident:Prompt),TRN
                           ENTRY(@s5),AT(70,64,40,10),USE(Key:Ident),LEFT(2)
                           PROMPT('Order:'),AT(6,78),USE(?Key:Order:Prompt),TRN
                           ENTRY(@s5),AT(70,78,40,10),USE(Key:Order),LEFT(2)
                           PROMPT('Key Name:'),AT(6,90),USE(?Key:KeyName:Prompt),TRN
                           ENTRY(@s100),AT(70,90,156,10),USE(Key:KeyName),LEFT(2)
                           PROMPT('External Name:'),AT(6,104),USE(?Key:ExternalName:Prompt),TRN
                           ENTRY(@s100),AT(70,104,156,10),USE(Key:ExternalName),LEFT(2)
                           PROMPT('Description:'),AT(6,120),USE(?Key:Description:Prompt),TRN
                           ENTRY(@s100),AT(70,120,156,10),USE(Key:Description),LEFT(2)
                           PROMPT('Key Type:'),AT(6,133),USE(?Key:KeyType:Prompt),TRN
                           ENTRY(@s20),AT(70,133,84,10),USE(Key:KeyType),LEFT(2)
                           PROMPT('Unique:'),AT(6,145),USE(?Key:Unique:Prompt),TRN
                           ENTRY(@s5),AT(70,145,40,10),USE(Key:Unique),LEFT(2)
                           ENTRY(@s5),AT(330,72,40,10),USE(Key:AutoNumber),LEFT(2)
                           PROMPT('Case Sensitive:'),AT(266,115),USE(?Key:CaseSensitive:Prompt),TRN
                           ENTRY(@s5),AT(330,101,40,10),USE(Key:Exclude),LEFT(2)
                           PROMPT('Exclude:'),AT(266,101),USE(?Key:Exclude:Prompt),TRN
                           PROMPT('Auto Number:'),AT(266,72),USE(?Key:AutoNumber:Prompt),TRN
                           PROMPT('Primary:'),AT(266,86),USE(?Key:Primary:Prompt),TRN
                           ENTRY(@s5),AT(330,115,40,10),USE(Key:CaseSensitive),LEFT(2)
                           ENTRY(@s5),AT(330,86,40,10),USE(Key:Primary),LEFT(2)
                         END
                         TAB('Components'),USE(?ComponentsTab)
                           LIST,AT(11,18,428,145),USE(?ComponentsList),HVSCROLL,FORMAT('200L(2)M~FieldName~L(2)@s100@')
                         END
                         TAB('Comments'),USE(?CommentsTab)
                           LIST,AT(7,38,432,113),USE(?CommentsList),FORMAT('44L(2)M~Create Date~@s11@#5#108L(2)M~C' & |
  'reate User~@s50@#8#1020L(2)M~Text~@s255@#2#')
                           BUTTON,AT(31,18,18,15),USE(?EditCommentBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(51,18,18,15),USE(?DeleteCommentBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                           BUTTON,AT(10,18,18,15),USE(?AddCommentBtn),ICON('AddNew.ico'),FLAT
                         END
                         TAB('Options'),USE(?OptionsTab)
                           LIST,AT(7,37,432,101),USE(?OptionsList),FORMAT('97L(2)M~Property~@s100@#2#62L(2)M~Prope' & |
  'rty Type~@s5@#3#400L(2)M~Property Value~@s100@#4#')
                           BUTTON,AT(29,19,18,15),USE(?EditOptionBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(50,19,18,15),USE(?DeleteOptionBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                           BUTTON,AT(7,19,18,15),USE(?AddOptionBtn),ICON('AddNew.ico'),FLAT
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
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeCloseEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeCompleted          PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
mhResize             CLASS(MH::ResizeWindowClass)
AfterResize            PROCEDURE(),DERIVED
Init                   PROCEDURE()
InitialResize          PROCEDURE()                         ! New method added to this class instance
                     END

UpdateKey:mhResize:WM CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(Fields:Record)
? DEBUGHOOK(Keys:Record)
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


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  OverStr = pParams
  GlobalErrors.SetProcedureName('UpdateKey')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Key:ParentGUID:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('UpdateKey',pThread)
    NotifyHandler.AddNotifyCode(1)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(Key:Record,History::Key:Record)
  SELF.AddHistoryField(?Key:ParentGUID,3)
  SELF.AddHistoryField(?Key:KeyOrder,5)
  SELF.AddHistoryField(?Key:KeyGuid,6)
  SELF.AddHistoryField(?Key:Ident,7)
  SELF.AddHistoryField(?Key:Order,8)
  SELF.AddHistoryField(?Key:KeyName,9)
  SELF.AddHistoryField(?Key:ExternalName,10)
  SELF.AddHistoryField(?Key:Description,11)
  SELF.AddHistoryField(?Key:KeyType,12)
  SELF.AddHistoryField(?Key:Unique,14)
  SELF.AddHistoryField(?Key:AutoNumber,16)
  SELF.AddHistoryField(?Key:Exclude,17)
  SELF.AddHistoryField(?Key:CaseSensitive,18)
  SELF.AddHistoryField(?Key:Primary,15)
  SELF.AddUpdateFile(Access:Keys)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Fields.Open()                                     ! File Fields used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Keys
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
  TemplateHelper.AddActiveWindow('UpdateKey', THREAD(), QuickWindow)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?CurrentTab{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
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
    ?Key:ParentGUID{PROP:ReadOnly} = True
    ?Key:KeyOrder{PROP:ReadOnly} = True
    ?Key:KeyGuid{PROP:ReadOnly} = True
    ?Key:Ident{PROP:ReadOnly} = True
    ?Key:Order{PROP:ReadOnly} = True
    ?Key:KeyName{PROP:ReadOnly} = True
    ?Key:ExternalName{PROP:ReadOnly} = True
    ?Key:Description{PROP:ReadOnly} = True
    ?Key:KeyType{PROP:ReadOnly} = True
    ?Key:Unique{PROP:ReadOnly} = True
    ?Key:AutoNumber{PROP:ReadOnly} = True
    ?Key:Exclude{PROP:ReadOnly} = True
    ?Key:CaseSensitive{PROP:ReadOnly} = True
    ?Key:Primary{PROP:ReadOnly} = True
    DISABLE(?EditCommentBtn)
    DISABLE(?DeleteCommentBtn)
    DISABLE(?AddCommentBtn)
    DISABLE(?EditOptionBtn)
    DISABLE(?DeleteOptionBtn)
    DISABLE(?AddOptionBtn)
  END
  mhResize.Init
  DonutHoleWindow.SetLogPreamble('UpdateKey')
  DonutHoleWindow.init(DonutHoleRegistrator.IDonutHoleRegistration,thread(),QuickWindow)
  SELF.AddItem(DonutHoleWindow.WindowComponent)
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
    ?CommentsList{PROP:From} = MyDct.CommentsQ
    ?OptionsList{PROP:From} = MyDct.OptionsQ
    ?ComponentsList{PROP:From} = MyDct.ComponentsQ
    Post(DctEvent:FetchRecord)    
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('UpdateKey', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Fields.Close()
  END
    NotifyManager.DeleteProcedure('UpdateKey',pThread)
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
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
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
    OF ?EditCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'EditComment'
      ParamsGrp.ProcedureToNotify = 'UpdateKey'
      ParamsGrp.CommentJson = Clip(MyDct.CommentsQ.Text)
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset        
    OF ?DeleteCommentBtn
      ThisWindow.Update()
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteCommentQRec(MyDct.CommentsQ,Keys,'CommentsBlob') 
        ?OK{PROP:Disable} = False     
        Post(DctEvent:RefreshData)
      End           
    OF ?AddCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertComment'
      ParamsGrp.ProcedureToNotify = 'UpdateKey'
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset         
    OF ?EditOptionBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'EditOption'
      ParamsGrp.ProcedureToNotify = 'UpdateKey'
      START(UpdateOption, 25000, OverStr,Thread())
      ThisWindow.Reset           
    OF ?DeleteOptionBtn
      ThisWindow.Update()
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteOptionQRec(MyDct.OptionsQ,Keys,'OptionsBlob') 
        ?OK{PROP:Disable} = False   
        Post(DctEvent:RefreshData)
      End           
    OF ?AddOptionBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertOption'
      ParamsGrp.ProcedureToNotify = 'UpdateKey'
      START(UpdateOption, 25000, OverStr,Thread())
      ThisWindow.Reset        
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

Looped BYTE
  CODE
  LOOP
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    !If ~Access:Keys.EqualBuffer(self.saved)
    If MyDct.CheckRecordHash(Keys) = DwrDct:RecordChanged
        Access:Keys.Update()
        !self.Saved = Access:Keys.SaveBuffer()
    End    
  ReturnValue = PARENT.TakeCompleted()
    Case ReturnValue  
    Of Level:Benign
        MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateKey','RecordSaved',OverStr)
        MyDct.SetCurrentRecordHash(Keys)
        Post(DctEvent:RefreshData)
        ?OK{PROP:Disable} = True
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
        MyDct.Trace('UpdateKey - EVENT:CloseWindow') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))  
    Of event:DonutHoleDisconnected
        MyDct.Trace('UpdateKey - event:DonutHoleDisconnected') 
        !If self.Saved
            !If Not Access:Keys.EqualBuffer(self.Saved)
            If MyDct.CheckRecordHash(Keys) = DwrDct:RecordChanged
                Case MESSAGE('The item has changed. Do you want to save changes?','Dictionary Editor',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
                Of BUTTON:NO
                Of BUTTON:YES
                    Access:Keys.Update()
                    !self.Saved = Access:Keys.SaveBuffer()
                End       
            End 
            !Access:Keys.RestoreBuffer(self.Saved)
        !End  
    Of event:DonutHoleHide
        !MyDct.Trace('')
        !MyDct.Trace('UpdateRelation - event:DonutHoleHide') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved) & ' PkGUID = ' & Rel:PKGuid)  
        !MyDct.Trace('')
  
    Of EVENT:Selected
        If MyDct.CheckRecordHash(Keys) = DwrDct:RecordChanged
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateKey','SaveNeeded',OverStr)
            Glo:SaveBtnFeq{PROP:Disable} = False  
        End
        !If self.Saved
            !If Not Access:Keys.EqualBuffer(self.Saved)
            !    ?OK{PROP:Disable} = False
                !MyDct.Trace('')
                !MyDct.Trace('Record Changed <<<<<<<<<<<<<<<----------------------------------------------------------')
                !MyDct.Trace('')
                !SaveNeeded = True
                !ParamsGrp.Action = 'SaveNeeded'
                !ParamsGrp.ProcedureToNotify = 'UpdateRelation'
                !json.Start()
                !json.SetTagCase(jF:CaseAsIs)
                !json.Save(ParamsGrp,st)
                !NotifyManager.NotifyProcedure('Main3',st.GetValue())   
            !End 
        !End
    Of EVENT:Accepted
    Of EVENT:NewSelection   !Post(DctEvent:RefreshControls)
    Of EVENT:GainFocus
        !MyDct.Trace('UpdateKey - EVENT:Selected,EVENT:Accepted,EVENT:NewSelection,EVENT:GainFocus')    
        !Access:Keys.Update()    
    Of DctEvent:FetchRecord
        !MyDct.Trace('')
        !MyDct.Trace('UpdateKey - EVENT DctEvent:FetchRecord')
        !MyDct.Trace('   ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
        !MyDct.Trace('   ParamsGrp.TableGuid = ' & ParamsGrp.TableGuid)
        !MyDct.Trace('   ParamsGrp.FieldGuid = ' & ParamsGrp.FieldGuid)
        !MyDct.Trace('   ParamsGrp.KeyGuid = ' & ParamsGrp.KeyGuid)
        !MyDct.Trace('')
  	    Key:PKGuid = ParamsGrp.KeyGuid
  	    Access:Keys.Fetch(Key:PKKeyGuidKey) 
        MyDct.SetCurrentRecordHash(Keys)
        !self.Saved = Access:Keys.SaveBuffer()
  
        st.FromBlob(Key:ComponentsBlob)
        MyDct.ParseComponentsXml(st.GetValue(),Fields)
        !xml.start()
        !xml.SetTagCase(xf:CaseAsIs)
        !xml.Load(component,st,'','Component') ! Load From a StringTheory object
  
        Post(DctEvent:RefreshData)
    Of DctEvent:RefreshData
        local.PopulateCommentsQ()
        local.PopulateOptionsQ()
        If Records(MyDct.CommentsQ) <> CommentRecsCnt
            CommentRecsCnt = Records(MyDct.CommentsQ)
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateKey','SaveNeeded',OverStr)
        END
        If Records(MyDct.OptionsQ) <> OptionRecCnt
            OptionRecCnt = Records(MyDct.OptionsQ)
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateKey','SaveNeeded',OverStr)
        End      
        Post(DctEvent:RefreshControls)
    Of DctEvent:RefreshControls
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

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
local.PopulateCommentsQ       Procedure()
TempSt          StringTheory
LneSt           StringTheory
StartPos        Long
EndPos          Long
xml             xFilesTree
    code
    MyDct.PopulateCommentsQ(Keys,'CommentsBlob')
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateDictionary - local.PopulateCommentsQ')
!    Free(CommentQ)
!    TempSt.Start()
!    TempSt.FromBlob(Key:CommentsBlob)
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
!    If Not CommentRecsCnt
!        CommentRecsCnt = Records(CommentQ)
!    END
!    !MyDct.Trace('')

local.PopulateOptionsQ          Procedure()
TempSt          StringTheory
LneSt           StringTheory
StartPos        Long
EndPos          Long
xml             xFilesTree
    code
    MyDct.PopulateOptionsQ(Keys,'OptionsBlob')
!    Free(OptionsQ) 
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateDictionary - local.PopulateOptionsQ')
!    TempSt.Start()
!    TempSt.FromBlob(Key:OptionsBlob)
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
!    If Not OptionRecCnt
!        OptionRecCnt = Records(OptionsQ)
!    End
!    !MyDct.Trace('')

mhResize.AfterResize PROCEDURE

  CODE
  PARENT.AfterResize
  !  DonutHoleWindow.Repaint()    


mhResize.Init PROCEDURE

  CODE
  SELF.Init(                                               |
      mhResizeFrame,                                       | !Global Host Object
      QuickWindow,                                         | !Window
      UpdateKey:mhResize:WM.MH::ResizeIWindowManager,      | !Window Manager
      449,                                                 | !Original Width
      176,                                                 | !Original Height
      1,                                                   | !Resize Width?
      1,                                                   | !Resize Height?
      0,                                                   | !Orig Size is Min?
      0,                                                   | !Border Beyond Min?
      1,                                                   | !Maximize Initially?
      1)                                                     !Do NOT hide+unhide during opening operations?
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


UpdateKey:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
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
        !MyDct.Trace('UpdateField - NotifyHandler.HandleNotify')
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
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateKey','SaveNeeded',OverStr)
            MyDct.InsertCommentQRec(MyDct.CommentsQ,Keys,'CommentsBlob',ParamsGrp.CommentJson)
        OF 'EditComment'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateKey','SaveNeeded',OverStr)
            MyDct.UpdateCommentQRec(MyDct.CommentsQ,Keys,'CommentsBlob',ParamsGrp.CommentJson)
        Of 'InsertOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateKey','SaveNeeded',OverStr)
            MyDct.InsertOptionQRec(MyDct.OptionsQ,Keys,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'EditOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateKey','SaveNeeded',OverStr)
            MyDct.UpdateOptionQRec(MyDct.OptionsQ,Keys,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'RecordSaved'
            POST(EVENT:Accepted,?OK)
        END  
        ?OK{PROP:Disable} = False
        Clear(ParamsGrp.Action)
        Display(?CommentsList)
	End
    Post(DctEvent:RefreshData)
DonutHoleWindow.Repaint               PROCEDURE()!,DERIVED
  CODE
  PARENT.Repaint()
