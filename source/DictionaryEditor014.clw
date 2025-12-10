

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR014.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('DICTIONARYEDITOR016.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! Form Triggers
!!! </summary>
UpdateTrigger PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
json				JSONClass
st					StringTheory
CodeSt              StringTheory
MyDct				dwrDctParser
xml                 xFilesTree

StartPos            Long
EndPos              Long
x                   Long

DctEvent:FetchRecord        Equate(EVENT:User+900)
DctEvent:RefreshData        Equate(EVENT:User+901)
DctEvent:RefreshControls    Equate(EVENT:User+902)

CommentGrp              Group(CommentGroupType).
CommentRecsCnt          Long


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


code_               Queue,Name('Code | RowName(Code)')
line                STRING(255),Name('Line | attribute')
                    End

local                       CLASS
PopulateCommentsQ           Procedure()
                            End
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
CurrentTab           STRING(80)                            ! 
TriggerCode          STRING(2000)                          ! 
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
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DonutHoleWindow     Class(NinjaDonutHoleWindowClass)
Repaint               PROCEDURE(),DERIVED
                    end
History::Tri:Record  LIKE(Tri:RECORD),THREAD
QuickWindow          WINDOW('Form Triggers'),AT(,,449,183),FONT('Segoe UI',10,,FONT:regular,CHARSET:ANSI),RESIZE, |
  AUTO,CENTER,GRAY,IMM,MAX,HLP('UpdateTrigger'),SYSTEM,WALLPAPER('Background1.png')
                       SHEET,AT(2,1,446,180),USE(?CurrentTab),ABOVE
                         TAB('General'),USE(?GeneralTab)
                           PROMPT('Parent GUID:'),AT(7,17),USE(?Tri:ParentGUID:Prompt),TRN
                           ENTRY(@s16),AT(101,17,68,10),USE(Tri:ParentGUID),LEFT(2)
                           PROMPT('Guid:'),AT(7,29),USE(?Tri:Guid:Prompt),TRN
                           ENTRY(@s40),AT(101,29,164,10),USE(Tri:Guid),LEFT(2)
                           PROMPT('Trigger Type:'),AT(7,44),USE(?Tri:TriggerType:Prompt),TRN
                           ENTRY(@s20),AT(101,44,84,10),USE(Tri:TriggerType),LEFT(2)
                           TEXT,AT(7,57,431,116),USE(?CodeText),FONT('Courier New',10),VSCROLL
                         END
                         TAB('Comments'),USE(?CommentsTab)
                           LIST,AT(7,38,432,113),USE(?CommentsList),FORMAT('44L(2)M~Create Date~@s11@#5#108L(2)M~C' & |
  'reate User~@s50@#8#1020L(2)M~Text~@s255@#2#')
                           BUTTON,AT(31,18,18,15),USE(?EditCommentBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(50,18,18,15),USE(?DeleteCommentBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                           BUTTON,AT(10,18,18,15),USE(?AddCommentBtn),ICON('AddNew.ico'),FLAT
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

UpdateTrigger:mhResize:WM CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(Triggers:Record)
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
  GlobalErrors.SetProcedureName('UpdateTrigger')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Tri:ParentGUID:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('UpdateTrigger',pThread)
    NotifyHandler.AddNotifyCode(1)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(Tri:Record,History::Tri:Record)
  SELF.AddHistoryField(?Tri:ParentGUID,2)
  SELF.AddHistoryField(?Tri:Guid,5)
  SELF.AddHistoryField(?Tri:TriggerType,6)
  SELF.AddUpdateFile(Access:Triggers)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Triggers.Open()                                   ! File Triggers used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Triggers
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
  !  	Tri:PKGuid = ParamsGrp.TriggerGuid
  !  	Access:Triggers.Fetch(Tri:PKTriGuidKey)  
  !  	st.FromBlob(Tri:Code_)
  !  	MyDct.Trace('')
  !  	MyDct.Trace(st.GetValue())
  !  	MyDct.Trace('')
  !  	xml.start()
  !  	xml.SetTagCase(xf:CaseAsIs)
  !  	xml.Load(code_,st,'Code','Line')
  !  	MyDct.Trace('')
  !  	MyDct.Trace('Records(code_) = ' & Records(code_))
  !  	MyDct.Trace('')
  !  	
  
   
  SELF.Open(QuickWindow)                                   ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('UpdateTrigger', THREAD(), QuickWindow)
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
    ?Tri:ParentGUID{PROP:ReadOnly} = True
    ?Tri:Guid{PROP:ReadOnly} = True
    ?Tri:TriggerType{PROP:ReadOnly} = True
    DISABLE(?EditCommentBtn)
    DISABLE(?DeleteCommentBtn)
    DISABLE(?AddCommentBtn)
  END
  mhResize.Init
  DonutHoleWindow.SetLogPreamble('UpdateTrigger')
  DonutHoleWindow.init(DonutHoleRegistrator.IDonutHoleRegistration,thread(),QuickWindow)
  SELF.AddItem(DonutHoleWindow.WindowComponent)
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
    ?CommentsList{PROP:From} = MyDct.CommentsQ   
    Post(DctEvent:FetchRecord)  
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('UpdateTrigger', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Triggers.Close()
  END
    NotifyManager.DeleteProcedure('UpdateTrigger',pThread)
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
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?EditCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'EditComment'
      ParamsGrp.ProcedureToNotify = 'UpdateTrigger'
      ParamsGrp.CommentJson = Clip(MyDct.CommentsQ.Text)
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset        
    OF ?DeleteCommentBtn
      ThisWindow.Update()
      !      MyDct.DeleteCommentQRec(MyDct.CommentsQ,Triggers,'CommentsBlob')  
      !      ?OK{PROP:Disable} = False
      !      Post(DctEvent:RefreshData)  
      
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteCommentQRec(MyDct.CommentsQ,Triggers,'CommentsBlob')  
        ?OK{PROP:Disable} = False
        Post(DctEvent:RefreshData)
      End    
    OF ?AddCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertComment'
      ParamsGrp.ProcedureToNotify = 'UpdateTrigger'
      START(UpdateComment, 25000, OverStr,Thread())
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
    !If ~Access:Triggers.EqualBuffer(self.saved)
    !If MyDct.CheckRecordHash(Triggers) = DwrDct:RecordChanged
        Access:Triggers.Update()
        !self.Saved = Access:Triggers.SaveBuffer()
    !End      
  ReturnValue = PARENT.TakeCompleted()
    Case ReturnValue  
    Of Level:Benign
        ?OK{PROP:Disable} = True
        MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTrigger','RecordSaved',OverStr)
        MyDct.SetCurrentRecordHash(Triggers)
        Post(DctEvent:RefreshData)
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
        MyDct.Trace('UpdateTrigger - EVENT:CloseWindow') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))  
    Of event:DonutHoleDisconnected
        MyDct.Trace('UpdateTrigger - event:DonutHoleDisconnected')
        !If self.Saved
            !If Not Access:Triggers.EqualBuffer(self.Saved)
            If MyDct.CheckRecordHash(Triggers) = DwrDct:RecordChanged
                Case MESSAGE('The item has changed. Do you want to save changes?','Dictionary Editor',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
                Of BUTTON:NO
                Of BUTTON:YES
                    Access:Triggers.Update()
                   ! self.Saved = Access:Triggers.SaveBuffer()
                End       
            End 
            !Access:Triggers.RestoreBuffer(self.Saved)
        !End  
    Of event:DonutHoleHide
        !MyDct.Trace('')
        !MyDct.Trace('UpdateRelation - event:DonutHoleHide') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved) & ' PkGUID = ' & Rel:PKGuid)  
        !MyDct.Trace('')
    Of EVENT:Selected
        If MyDct.CheckRecordHash(Triggers) = DwrDct:RecordChanged
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTrigger','SaveNeeded',OverStr)
            Glo:SaveBtnFeq{PROP:Disable} = False  
        End
        !If self.Saved
        !    If Not Access:Triggers.EqualBuffer(self.Saved)
        !        ?OK{PROP:Disable} = False
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
        !    End 
        !End
    Of DctEvent:FetchRecord
  		Tri:PKGuid = ParamsGrp.TriggerGuid
  		Access:Triggers.Fetch(Tri:PKTriGuidKey) 
        !self.Saved = Access:Triggers.SaveBuffer()
        MyDct.SetCurrentRecordHash(Triggers)
        st.FromBlob(Tri:Code_)
        !MyDct.Trace('')
        !MyDct.Trace('st.Length() = ' & st.Length())
        !MyDct.Trace('')
        StartPos = 1 !st.FindChars('Line="',StartPos)+6
        LOOP 50 TIMES
            If StartPos => st.Length(); Break End
            StartPos = st.FindChars('Line="',StartPos)+6
            !CodeSt.AppendA(StartPos & '<9>' & MyDct.GetDelimitedStr(st,StartPos,EndPos,'"/>',True),'<13,10>')
            CodeSt.AddLine(Records(CodeSt.lines)+1,MyDct.GetDelimitedStr(st,StartPos,EndPos,'"/>',True))
            StartPos = StartPos + 3
        END
        CodeSt.Join('<13,10>')
        !TriggerCode = CodeSt.GetValue()
        ?CodeText{PROP:Text} = CodeSt.GetValue()  
        Post(DctEvent:RefreshData)
    Of DctEvent:RefreshData
        !MyDct.Trace('UpdateRelation - DctEvent:RefreshData')
        local.PopulateCommentsQ()
        !local.PopulateOptionsQ()      
        If Records(MyDct.CommentsQ) <> CommentRecsCnt
            CommentRecsCnt = Records(MyDct.CommentsQ)
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTrigger','SaveNeeded',OverStr)
        END
        Post(DctEvent:RefreshControls)
    Of DctEvent:RefreshControls
        !MyDct.Trace('UpdateRelation - DctEvent:RefreshControls')
        If Records(MyDct.CommentsQ)
            ?EditCommentBtn{PROP:Disable} = FALSE
            ?DeleteCommentBtn{PROP:Disable} = FALSE
        ELSE
            ?EditCommentBtn{PROP:Disable} = True
            ?DeleteCommentBtn{PROP:Disable} = True
        End
        !If Records(CommentQ)
        !    ?EditOptionBtn{PROP:Disable} = False
        !    ?DeleteOptionBtn{PROP:Disable} = FALSE
        !ELSE
        !    ?EditOptionBtn{PROP:Disable} = True
        !    ?DeleteOptionBtn{PROP:Disable} = True
        !End
        Display(?EditCommentBtn)
        !Display(?EditOptionBtn)
        !Display(?DeleteOptionBtn)
        !Display(?DeleteCommentBtn)
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
    MyDct.PopulateCommentsQ(Triggers,'CommentsBlob')
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateDictionary - local.PopulateCommentsQ')
!    Free(CommentQ)
!    TempSt.Start()
!    TempSt.FromBlob(Tri:CommentsBlob)
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

mhResize.AfterResize PROCEDURE

  CODE
  PARENT.AfterResize
  !  DonutHoleWindow.Repaint()    


mhResize.Init PROCEDURE

  CODE
  SELF.Init(                                               |
      mhResizeFrame,                                       | !Global Host Object
      QuickWindow,                                         | !Window
      UpdateTrigger:mhResize:WM.MH::ResizeIWindowManager,  | !Window Manager
      449,                                                 | !Original Width
      183,                                                 | !Original Height
      1,                                                   | !Resize Width?
      1,                                                   | !Resize Height?
      0,                                                   | !Orig Size is Min?
      0,                                                   | !Border Beyond Min?
      1,                                                   | !Maximize Initially?
      1)                                                     !Do NOT hide+unhide during opening operations?
  SELF.AddControl(?CurrentTab,0,0,1,1)
  SELF.AddControl(?CodeText,0,0,1,1)
  SELF.AddControl(?CommentsList,0,0,1,1)
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


UpdateTrigger:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
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
        !MyDct.Trace('UpdateTrigger - NotifyHandler.HandleNotify')
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
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTrigger','SaveNeeded',OverStr)
            MyDct.InsertCommentQRec(MyDct.CommentsQ,Triggers,'CommentsBlob',ParamsGrp.CommentJson)
        OF 'EditComment'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateTrigger','SaveNeeded',OverStr)
            MyDct.UpdateCommentQRec(MyDct.CommentsQ,Triggers,'CommentsBlob',ParamsGrp.CommentJson)
        Of 'InsertOption'
            !MyDct.InsertOptionQRec(OptionsQ,Triggers,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'EditOption'
            !MyDct.UpdateOptionQRec(OptionsQ,Triggers,'OptionsBlob',ParamsGrp.OptionJson)
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
