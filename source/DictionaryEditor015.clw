

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR015.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('DICTIONARYEDITOR016.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR017.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! Form Relations
!!! </summary>
UpdateRelation PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
json				JSONClass
st					StringTheory
MyDct				dwrDctParser
xml                 xFilesTree

EventDebug          Debuger

SaveNeeded          BYTE

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
    !Omit('****')
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DonutHoleWindow     Class(NinjaDonutHoleWindowClass)
Repaint               PROCEDURE(),DERIVED
                    end
    !****
!DonutHoleWindow                 Class(NinjaDonutHoleWindowClass)
!TakeEvent                       PROCEDURE(),BYTE,DERIVED
!SetAlerts                       PROCEDURE(),DERIVED
!GetThread                       PROCEDURE(),LONG,DERIVED
!GetWindowHandle                 PROCEDURE(),LONG,DERIVED
!GetWindowClientHandle           PROCEDURE(),LONG,DERIVED
!GetIsConnected                  PROCEDURE(),BYTE,DERIVED
!GetIsConnecting                 PROCEDURE(),BYTE,DERIVED
!SetSize                         PROCEDURE(LONG p_X, LONG p_Y,LONG p_Width, LONG p_Height),DERIVED
!SetWindowPosition               PROCEDURE(),DERIVED
!ShowWindow                      PROCEDURE(),DERIVED
!PrepareWindow                   PROCEDURE(),DERIVED
!Repaint                         PROCEDURE(),DERIVED
!                                End
History::Rel:Record  LIKE(Rel:RECORD),THREAD
QuickWindow          WINDOW('Form Relations'),AT(,,449,184),FONT('Segoe UI',10,,FONT:regular,CHARSET:ANSI),RESIZE, |
  AUTO,CENTER,GRAY,MAX,HLP('UpdateRelation'),SYSTEM,WALLPAPER('Background1.png'),IMM
                       SHEET,AT(2,1,446,180),USE(?CurrentTab),ABOVE
                         TAB('General'),USE(?GeneralTab)
                           PROMPT('Parent GUID:'),AT(12,55),USE(?Rel:ParentGUID:Prompt),TRN
                           ENTRY(@s16),AT(73,55,68,10),USE(Rel:ParentGUID),LEFT(2)
                           PROMPT('Dctx Order:'),AT(12,67),USE(?Rel:DctxOrder:Prompt),TRN
                           ENTRY(@n-14),AT(73,67,64,10),USE(Rel:DctxOrder),RIGHT(1)
                           PROMPT('Guid:'),AT(12,82),USE(?Rel:Guid:Prompt),TRN
                           ENTRY(@s38),AT(73,82,156,10),USE(Rel:Guid),LEFT(2)
                           PROMPT('Primary Table:'),AT(12,98),USE(?Rel:PrimaryTable:Prompt),TRN
                           ENTRY(@s38),AT(73,98,156,10),USE(Rel:PrimaryTable),LEFT(2)
                           PROMPT('Foreign Table:'),AT(12,112),USE(?Rel:ForeignTable:Prompt),TRN
                           ENTRY(@s38),AT(73,112,156,10),USE(Rel:ForeignTable),LEFT(2)
                           PROMPT('Primary Key:'),AT(12,124),USE(?Rel:PrimaryKey:Prompt),TRN
                           ENTRY(@s38),AT(73,124,156,10),USE(Rel:PrimaryKey),LEFT(2)
                           PROMPT('Foreign Key:'),AT(12,138),USE(?Rel:ForeignKey:Prompt),TRN
                           ENTRY(@s38),AT(73,138,156,10),USE(Rel:ForeignKey),LEFT(2)
                           PROMPT('Delete:'),AT(12,154),USE(?Rel:Delete:Prompt),TRN
                           ENTRY(@s20),AT(73,154,84,10),USE(Rel:Delete),LEFT(2)
                           PROMPT('Update:'),AT(12,167),USE(?Rel:Update:Prompt),TRN
                           ENTRY(@s20),AT(73,167,84,10),USE(Rel:Update),LEFT(2)
                           GROUP('Relationship for'),AT(9,20,431,32),USE(?GROUP1),BOXED,TRN
                           END
                           ENTRY(@s16),AT(262,56,78,10),USE(Rel:PKGuid),LEFT(2)
                           PROMPT('PKG uid:'),AT(211,56),USE(?Rel:PKGuid:Prompt)
                         END
                         TAB('Comments'),USE(?CommentsTab)
                           LIST,AT(8,39,432,113),USE(?CommentsList),FORMAT('44L(2)M~Create Date~@s11@#5#108L(2)M~C' & |
  'reate User~@s50@#8#1020L(2)M~Text~@s255@#2#')
                           BUTTON,AT(31,19,18,15),USE(?EditCommentBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(53,19,18,15),USE(?DeleteCommentBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                           BUTTON,AT(10,19,18,15),USE(?AddCommentBtn),ICON('AddNew.ico'),FLAT
                         END
                         TAB('Options'),USE(?OptionsTab)
                           LIST,AT(8,37,432,101),USE(?OptionsList),FORMAT('97L(2)M~Property~@s100@#2#62L(2)M~Prope' & |
  'rty Type~@s5@#3#400L(2)M~Property Value~@s100@#4#')
                           BUTTON,AT(30,20,18,15),USE(?EditOptionBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(51,20,18,15),USE(?DeleteOptionBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                           BUTTON,AT(8,20,18,15),USE(?AddOptionBtn),ICON('AddNew.ico'),FLAT
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
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
SaveOnChangeAction     PROCEDURE(),BYTE,DERIVED
SetResponse            PROCEDURE(BYTE Response),DERIVED
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

UpdateRelation:mhResize:WM CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(Relations:Record)
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
    !MyDct.Trace('UpdateRelation - ThisWindow.Ask After PC')    


ThisWindow.ChangeAction PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
    !MyDct.Trace('UpdateRelation - ThisWindow.ChangeAction Before PC')  
  ReturnValue = PARENT.ChangeAction()
    !MyDct.Trace('UpdateRelation - ThisWindow.ChangeAction After PC')
    !if ReturnValue = Level:Benign
    !    !MyDct.Trace('UpdateRelation - ThisWindow.ChangeAction After PC - RetunValue = Level:Benign') 
    !end     
  RETURN ReturnValue


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  	OverStr = pParams  
  GlobalErrors.SetProcedureName('UpdateRelation')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Rel:ParentGUID:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('UpdateRelation',pThread)
    NotifyHandler.AddNotifyCode(1)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(Rel:Record,History::Rel:Record)
  SELF.AddHistoryField(?Rel:ParentGUID,2)
  SELF.AddHistoryField(?Rel:DctxOrder,3)
  SELF.AddHistoryField(?Rel:Guid,4)
  SELF.AddHistoryField(?Rel:PrimaryTable,5)
  SELF.AddHistoryField(?Rel:ForeignTable,6)
  SELF.AddHistoryField(?Rel:PrimaryKey,7)
  SELF.AddHistoryField(?Rel:ForeignKey,8)
  SELF.AddHistoryField(?Rel:Delete,9)
  SELF.AddHistoryField(?Rel:Update,10)
  SELF.AddHistoryField(?Rel:PKGuid,1)
  SELF.AddUpdateFile(Access:Relations)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Relations.SetOpenRelated()
  Relate:Relations.Open()                                  ! File Relations used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Relations
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
    !FLUSH(Relations)
    !If self.Saved
    !    Access:Relations.RestoreBuffer(self.Saved)
    !End
    !SELF.Primary.Me.SaveBuffer()  
    !RecSaved = Access:Relations.SaveBuffer()
  
  SELF.Open(QuickWindow)                                   ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('UpdateRelation', THREAD(), QuickWindow)
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
    ?Rel:ParentGUID{PROP:ReadOnly} = True
    ?Rel:DctxOrder{PROP:ReadOnly} = True
    ?Rel:Guid{PROP:ReadOnly} = True
    ?Rel:PrimaryTable{PROP:ReadOnly} = True
    ?Rel:ForeignTable{PROP:ReadOnly} = True
    ?Rel:PrimaryKey{PROP:ReadOnly} = True
    ?Rel:ForeignKey{PROP:ReadOnly} = True
    ?Rel:Delete{PROP:ReadOnly} = True
    ?Rel:Update{PROP:ReadOnly} = True
    ?Rel:PKGuid{PROP:ReadOnly} = True
    DISABLE(?EditCommentBtn)
    DISABLE(?DeleteCommentBtn)
    DISABLE(?AddCommentBtn)
    DISABLE(?EditOptionBtn)
    DISABLE(?DeleteOptionBtn)
    DISABLE(?AddOptionBtn)
  END
  mhResize.Init
  DonutHoleWindow.SetLogPreamble('UpdateRelation')
  DonutHoleWindow.init(DonutHoleRegistrator.IDonutHoleRegistration,thread(),QuickWindow)
  SELF.AddItem(DonutHoleWindow.WindowComponent)
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
    ?CommentsList{PROP:From} = MyDct.CommentsQ
    ?OptionsList{PROP:From} = MyDct.OptionsQ
    Post(DctEvent:FetchRecord)        
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('UpdateRelation', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Relations.Close()
  END
    NotifyManager.DeleteProcedure('UpdateRelation',pThread)
  mhResize.Kill
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.Kill()
  NYS:DockingPane_EventMgr.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF QuickWindow{Prop:AcceptAll} THEN RETURN.
  PARENT.Reset(Force)
    !MyDct.Trace('UpdateRelation - ThisWindow.Reset After PC')  


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.SaveOnChangeAction PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.SaveOnChangeAction()
    !MyDct.Trace('UpdateRelation - ThisWindow.SaveOnChangeAction After PC')  
  RETURN ReturnValue


ThisWindow.SetResponse PROCEDURE(BYTE Response)

  CODE
  PARENT.SetResponse(Response)
    !MyDct.Trace('UpdateRelation - ThisWindow.SetResponse After PC')  


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
      ParamsGrp.ProcedureToNotify = 'UpdateRelation'
      ParamsGrp.CommentJson = Clip(MyDct.CommentsQ.Text)
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset            
    OF ?DeleteCommentBtn
      ThisWindow.Update()
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteCommentQRec(MyDct.CommentsQ,Relations,'CommentsBlob')      
        ?OK{PROP:Disable} = False
        Post(DctEvent:RefreshData)
      End          
    OF ?AddCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertComment'
      ParamsGrp.ProcedureToNotify = 'UpdateRelation'
      START(UpdateComment, 25000, OverStr,Thread())
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
        MyDct.DeleteOptionQRec(MyDct.OptionsQ,Relations,'OptionsBlob')   
        ?OK{PROP:Disable} = False   
        Post(DctEvent:RefreshData)
      End         
    OF ?AddOptionBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertOption'
      ParamsGrp.ProcedureToNotify = 'UpdateRelation'
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
    !MyDct.Trace('==================================================================================================================')
    !MyDct.Trace('UpdateRelation - ThisWindow.TakeCloseEvent - ReturnValue = Level:Benign AND DonutHoleWindow.GetIsConnected()')
    !MyDct.Trace('==================================================================================================================')
    if SELF.Response = RequestCompleted
      !MyDct.Trace('UpdateRelation - ThisWindow.TakeCloseEvent - SELF.Response = RequestCompleted')
      !DonutHoleWindow.NotifyHost(not:RecordUpdated, CUS:CustNumber)  
    end!if
    !Access:Relations.RestoreBuffer(self.Saved)
    DonutHoleWindow.NotifyHost(not:closeWindow, thread())
    ReturnValue = Level:Benign
  end!if  
    !not:RecordUpdated
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
    !If ~Access:Relations.EqualBuffer(self.saved)
    !If MyDct.CheckRecordHash(Relations) = DwrDct:RecordChanged
        !MyDct.Trace('')
        !MyDct.Trace('UpdateRelation - ThisWindow.TakeCompleted - Record Changed <<<<<<<<<<<<<<<----------------------------------------------------------')
        !MyDct.Trace('')
        Access:Relations.Update()
        !self.Saved = Access:Relations.SaveBuffer()
    !End   
  ReturnValue = PARENT.TakeCompleted()
    Case ReturnValue  
    Of Level:Benign
        !MyDct.Trace('UpdateRelation - ThisWindow.TakeCompleted - Level:Benign')
        MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','RecordSaved',OverStr)
        MyDct.SetCurrentRecordHash(Relations)
        Post(DctEvent:RefreshData)
        ?OK{PROP:Disable} = True
    Of Level:Notify
        !MyDct.Trace('UpdateRelation - ThisWindow.TakeCompleted - Level:Notify')
    Of Level:Fatal
        !MyDct.Trace('UpdateRelation - ThisWindow.TakeCompleted - Level:Fatal')
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
    !MyDct.Trace(EventDebug.GetEventDescr(EVENT()))
    Case EVENT()
    Of event:DonutHoleConnected
        MyDct.Trace('UpdateRelation - event:DonutHoleConnected')  
        !Post(DctEvent:FetchRecord)
    Of event:DonutHoleUnhide
        MyDct.Trace('UpdateRelation - event:DonutHoleUnhide - PkGuid = ' & Rel:PKGuid)  
    Of event:DonutHoleDisconnected
        MyDct.Trace('UpdateRelation - event:DonutHoleDisconnected')
        !If self.Saved
            !MyDct.Trace('UpdateRelation - event:DonutHoleDisconnected - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))    
            !If Not Access:Relations.EqualBuffer(self.Saved)
            If MyDct.CheckRecordHash(Relations) = DwrDct:RecordChanged
                SaveNeeded = False
                Case MESSAGE('The item has changed. Do you want to save changes?','Dictionary Editor',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
                Of BUTTON:NO
                Of BUTTON:YES
                    Access:Relations.Update()
                    !self.Saved = Access:Relations.SaveBuffer()
                End       
            End 
        !    !MyDct.Trace('UpdateRelation - event:DonutHoleDisconnected - Before RestoreBuffer')
        !    Access:Relations.RestoreBuffer(self.Saved)
        !End  
    Of event:DonutHoleHide
        !MyDct.Trace('')
        MyDct.Trace('UpdateRelation - event:DonutHoleHide') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved) & ' PkGUID = ' & Rel:PKGuid)  
        !MyDct.Trace('')
  
    Of EVENT:Selected
        MyDct.Trace('UpdateRelation - EVENT:Selected') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))  
        If MyDct.CheckRecordHash(Relations) = DwrDct:RecordChanged
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateRelation','SaveNeeded',OverStr)
            Glo:SaveBtnFeq{PROP:Disable} = False  
        End
        !MyDct.Trace('   RecStart Size = ' & Size(RecStart))
        !MyDct.Trace('   RecNow Size = ' & Size(RecNow))
        !Access:Relations.RestoreBuffer(RecSaved)
        !If self.Saved
        !    !MyDct.Trace('UpdateRelation - EVENT:Selected - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))  
        !    If Not Access:Relations.EqualBuffer(self.Saved)
        !        ?OK{PROP:Disable} = False
        !        !MyDct.Trace('')
        !        !MyDct.Trace('Record Changed <<<<<<<<<<<<<<<----------------------------------------------------------')
        !        !MyDct.Trace('')
        !        !SaveNeeded = True
        !        !ParamsGrp.Action = 'SaveNeeded'
        !        !ParamsGrp.ProcedureToNotify = 'UpdateRelation'
        !        !json.Start()
        !        !json.SetTagCase(jF:CaseAsIs)
        !        !json.Save(ParamsGrp,st)
        !        !NotifyManager.NotifyProcedure('Main3',st.GetValue())   
        !    End 
        !End
    Of EVENT:CloseWindow
        MyDct.Trace('UpdateRelation - EVENT:CloseWindow') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))  
    Of EVENT:Accepted
        MyDct.Trace('UpdateRelation - EVENT:Accepted')  
    Of EVENT:NewSelection   !Post(DctEvent:RefreshControls)
        MyDct.Trace('UpdateRelation - EVENT:NewSelection')  
    Of EVENT:GainFocus
        MyDct.Trace('UpdateRelation - EVENT:GainFocus')    
    Of DctEvent:FetchRecord
        !MyDct.Trace('')
        MyDct.Trace('UpdateRelation - EVENT DctEvent:FetchRecord')
        !MyDct.Trace('   ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
        !MyDct.Trace('   ParamsGrp.TableGuid = ' & ParamsGrp.TableGuid)
        !MyDct.Trace('   ParamsGrp.FieldGuid = ' & ParamsGrp.FieldGuid)
        !MyDct.Trace('   ParamsGrp.RelationGuid = ' & ParamsGrp.RelationGuid)
  		Rel:PKGuid = ParamsGrp.RelationGuid
  		Access:Relations.Fetch(Rel:PKRelGuidKey) 
        MyDct.SetCurrentRecordHash(Relations)
        !self.Saved = Access:Relations.SaveBuffer()
        !MyDct.Trace('UpdateRelation - DctEvent:FetchRecord - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))  
        !MyDct.Trace('   self.Saved = ' & self.Saved)
        !MyDct.Trace('   EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))
        !MyDct.Trace('')
        Post(DctEvent:RefreshData)
    Of DctEvent:RefreshData
        MyDct.Trace('UpdateRelation - DctEvent:RefreshData')
        local.PopulateCommentsQ()
        local.PopulateOptionsQ()      
        If Records(MyDct.CommentsQ) <> CommentRecsCnt
            CommentRecsCnt = Records(MyDct.CommentsQ)
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateRelation','SaveNeeded',OverStr)
        END
        If Records(MyDct.OptionsQ) <> OptionRecCnt
            OptionRecCnt = Records(MyDct.OptionsQ)
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateRelation','SaveNeeded',OverStr)
        End
        Post(DctEvent:RefreshControls)
    Of DctEvent:RefreshControls
        MyDct.Trace('UpdateRelation - DctEvent:RefreshControls')
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
        !Post(EVENT:Selected)
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
    OF EVENT:Completed
      !If ~Access:Relations.EqualBuffer(self.saved)
      !  !MyDct.Trace('')
      !  !MyDct.Trace('UpdateRelation - EVENT:Completed - Record Changed <<<<<<<<<<<<<<<----------------------------------------------------------')
      !  !MyDct.Trace('')
      !End       
    OF EVENT:OpenWindow
        post(event:visibleondesktop)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.Update PROCEDURE

  CODE
  PARENT.Update
    !SaveNeeded = False
    !self.Saved = Access:Relations.SaveBuffer() 
    !MyDct.Trace('UpdateRelation - ThisWindow.Update After PC - EqualBuffer = ' & Access:Relations.EqualBuffer(ThisWindow.Saved))
    !MyDct.Trace('UpdateRelation - ThisWindow.Update After PC')
  !LOOP
  !    SELF.Response = RequestCancelled
  !    SETCURSOR(Cursor:Wait)
  !    UnChanged = SELF.Primary.Me.EqualBuffer(SELF.Saved)
  !    IF UnChanged
  !      !MyDct.Trace('UpdateRelation - No Change')
  !      Error = 0
  !    ELSE
  !      !MyDct.Trace('UpdateRelation - Record Changed')
  !      Error = SELF.Primary.Update(CHOOSE(SELF.HistoryKey))
  !    END
  !End  

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
local.PopulateCommentsQ       Procedure()
TempSt          StringTheory
LneSt           StringTheory
StartPos        Long
EndPos          Long
xml             xFilesTree
    code
    MyDct.PopulateCommentsQ(Relations,'CommentsBlob')
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateRelation - local.PopulateCommentsQ')
!    Free(CommentQ)
!    TempSt.Start()
!    TempSt.FromBlob(Rel:CommentsBlob)
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
    MyDct.PopulateOptionsQ(Relations,'OptionsBlob')
!    Free(OptionsQ) 
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateRelation - local.PopulateOptionsQ')
!    TempSt.Start()
!    TempSt.FromBlob(Rel:OptionsBlob)
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
      UpdateRelation:mhResize:WM.MH::ResizeIWindowManager, | !Window Manager
      449,                                                 | !Original Width
      184,                                                 | !Original Height
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


UpdateRelation:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
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
        MyDct.Trace('UpdateRelation - NotifyHandler.HandleNotify')
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
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateRelation','SaveNeeded',OverStr)
            MyDct.InsertCommentQRec(MyDct.CommentsQ,Relations,'CommentsBlob',ParamsGrp.CommentJson)
        OF 'EditComment'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateRelation','SaveNeeded',OverStr)
            MyDct.UpdateCommentQRec(MyDct.CommentsQ,Relations,'CommentsBlob',ParamsGrp.CommentJson)
        Of 'InsertOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateRelation','SaveNeeded',OverStr)
            MyDct.InsertOptionQRec(MyDct.OptionsQ,Relations,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'EditOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateRelation','SaveNeeded',OverStr)
            MyDct.UpdateOptionQRec(MyDct.OptionsQ,Relations,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'SaveRecord'
            !DonutHoleWindow.Repaint()
            Access:Relations.Update()
            !ThisWindow.Saved = Access:Relations.SaveBuffer()
            !ThisWindow.Update()
            !self.Saved = Access:Relations.SaveBuffer()
             !ThisWindow.
            !MyDct.Trace('<9>EqualBuffer = ' & Access:Relations.EqualBuffer(ThisWindow.Saved))
            !Post(DctEvent:FetchRecord)
            !RecSaved = Access:Relations.SaveBuffer()
            !Access:Relations.RestoreBuffer(RecSaved)
  		    !Rel:PKGuid = ParamsGrp.RelationGuid
  		    !Access:Relations.Fetch(Rel:PKRelGuidKey) 
            !RecSaved = Access:Relations.SaveBuffer()

            ParamsGrp.Action = 'RecordSaved'
            ParamsGrp.ProcedureToNotify = 'UpdateRelation'
            json.Start()
            json.SetTagCase(jF:CaseAsIs)
            json.Save(ParamsGrp,st)
            NotifyManager.NotifyProcedure('Main3',st.GetValue()) 
            !ThisWindow.Reset()
            Post(DctEvent:FetchRecord)
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
