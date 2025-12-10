

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR003.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
TreeWindow PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
MyDct               dwrDctParser
json                JSONClass
st                  StringTheory
x                   Long
DwrTreeQ               QUEUE,PRE(Tree8)
DisplayStr      STRING(200)
NormalFG        LONG
NormalBG        LONG
SelectedFG      LONG
SelectedBG      LONG
Icon            SHORT
Level           LONG !Closed=-1,Child=2
Loaded          SHORT
Position        STRING(1024)
RecordId        String(16)
DataType        String(20)
FieldsSize      String(20)
                END
NotifyParam          STRING(1000)                          ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_WndExt        CLASS(DockingPaneWndExtClass)
InitComplete                    PROCEDURE(),DERIVED
InitPrepare                     PROCEDURE(SIGNED paramOCXCtrl),DERIVED
InitResize                      PROCEDURE(),DERIVED
InitSecurity                    PROCEDURE(),DERIVED
InitTemplateSettings            PROCEDURE(),DERIVED
Keystroke                       PROCEDURE(UNSIGNED paramKeycode),DERIVED
Kill                            PROCEDURE(),DERIVED
KillComplete                    PROCEDURE(),DERIVED
ParametersReceived              PROCEDURE(<LONG paramSessionID>),DERIVED
ProcessClones                   PROCEDURE(),DERIVED
ProcessMimics                   PROCEDURE(),DERIVED
ProcessShortcutKey              PROCEDURE(UNSIGNED pKeyCode),DERIVED
TakeEvent                       PROCEDURE(SIGNED paramEvent),DERIVED
TakeNotify                      PROCEDURE(UNSIGNED paramNotifyCode, SIGNED paramThread, LONG paramParameter),DERIVED
TakeSubClassEvent               PROCEDURE(UNSIGNED paramWndHndl, UNSIGNED paramMsg, UNSIGNED paramWParam, LONG paramLParam),DERIVED
TakeTimer                       PROCEDURE(),DERIVED
TakeWindowEvent                 PROCEDURE(*WINDOW paramWindow),DERIVED
Init                            PROCEDURE(DockingPaneWndMgrClass pWMC),DERIVED
WindowDocked                    PROCEDURE(),DERIVED
WindowOpened                    PROCEDURE(),DERIVED
WindowPrepare                   PROCEDURE(STRING paramProcName),DERIVED
WindowRedraw                    PROCEDURE(),DERIVED
                              END
DockableContent_Ctrl    CSTRING(20)
DockableContent_Result  BYTE
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
Window               WINDOW('Caption'),AT(,,166,264),FONT('Segoe UI',10),RESIZE,AUTO,GRAY,SYSTEM
                       BUTTON,AT(2,1,18,15),USE(?ExpandAllBtn),ICON('DataSplitLarge.ico')
                       BUTTON,AT(24,1,18,15),USE(?ContractAllBtn),ICON('DataMergeLarge.ico')
                       LIST,AT(3,19,161,243),USE(?DwrTreeList),HVSCROLL,FORMAT('100LM*IT~Display Str~@s200@#1#'), |
  FROM(DwrTreeQ)
                     END

FieldsView                  View(Fields)
                            END
KeysView                    View(Keys)
                            END
TriggersView                View(Triggers)
                            Project(Tri:PKGuid)
                            Project(Tri:ParentGUID)
                            Project(Tri:TableGuid)
                            Project(Tri:Guid)
                            Project(Tri:TriggerType)
                            Project(Tri:CommentsBlob)
                            Project(Tri:Code_)
                            End
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
? DEBUGHOOK(Fields:Record)
? DEBUGHOOK(Keys:Record)
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
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitHelper(TemplateHelper)
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    OverStr = pParams  
  GlobalErrors.SetProcedureName('TreeWindow')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?ExpandAllBtn
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  Relate:Fields.Open()                                     ! File Fields used by this procedure, so make sure it's RelationManager is open
  Access:Triggers.UseFile()                                ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Keys.UseFile()                                    ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
    DwrTreeQ.DisplayStr = 'Fields'
    DwrTreeQ.NormalFG  = COLOR:Black  
    DwrTreeQ.NormalBG = COLOR:White
    DwrTreeQ.SelectedFG = COLOR:Blue
    DwrTreeQ.SelectedBG = COLOR:White !COLOR:Gray
    DwrTreeQ.Icon = 1
    DwrTreeQ.Level = -1
    !DwrTreeQ.Loaded = 
    !DwrTreeQ.Position
    Add(DwrTreeQ)  
  
    DwrTreeQ.DisplayStr = 'Keys'
    DwrTreeQ.NormalFG  = COLOR:Black  
    DwrTreeQ.NormalBG = COLOR:White
    DwrTreeQ.SelectedFG = COLOR:Blue
    DwrTreeQ.SelectedBG = COLOR:White !COLOR:Gray
    DwrTreeQ.Icon = 2
    DwrTreeQ.Level = -1
    !DwrTreeQ.Loaded = 
    !DwrTreeQ.Position
    Add(DwrTreeQ)
  
  !        DwrTreeQ.DisplayStr = 'Keys'
  !        DwrTreeQ.NormalFG  = COLOR:Black  
  !        DwrTreeQ.NormalBG = COLOR:White
  !        DwrTreeQ.SelectedFG = COLOR:Blue
  !        DwrTreeQ.SelectedBG = COLOR:White !COLOR:Gray
  !        DwrTreeQ.Icon = 3
  !        DwrTreeQ.Level = -2
  !        !DwrTreeQ.Loaded = 
  !        !DwrTreeQ.Position
  !        Add(DwrTreeQ)
  
  !        Open(BRW5::View:Browse)
  !        BRW5::View:Browse{PROP:Filter} = '(Key:ParentGUID = ' & '''' & Tab:PKGuid & '''' & ')'
  !        Set(BRW5::View:Browse)
  !        LOOP
  !            Next(BRW5::View:Browse)
  !            If Errorcode(); Break END
  !            DwrTreeQ.DisplayStr = Clip(Key:KeyName)
  !            DwrTreeQ.NormalFG  = COLOR:Black  
  !            DwrTreeQ.NormalBG = COLOR:White
  !            DwrTreeQ.SelectedFG = COLOR:Blue
  !            DwrTreeQ.SelectedBG = COLOR:White !COLOR:Gray
  !            DwrTreeQ.Icon = 3
  !            DwrTreeQ.Level = 3
  !            DwrTreeQ.RecordId = Key:PKGuid
  !            !DwrTreeQ.Loaded = 
  !            !DwrTreeQ.Position
  !            Add(DwrTreeQ)
  !        End      
  
  
  
      
  !        DwrTreeQ.DisplayStr = 'Triggers'
  !        DwrTreeQ.NormalFG  = COLOR:Black  
  !        DwrTreeQ.NormalBG = COLOR:White
  !        DwrTreeQ.SelectedFG = COLOR:Blue
  !        DwrTreeQ.SelectedBG = COLOR:White !COLOR:Gray
  !        DwrTreeQ.Icon = 1
  !        DwrTreeQ.Level = 2
  !        !DwrTreeQ.Loaded = 
  !        !DwrTreeQ.Position
  !        Add(DwrTreeQ)
      
  !        DwrTreeQ.DisplayStr = 'Relations'
  !        DwrTreeQ.NormalFG  = COLOR:Black  
  !        DwrTreeQ.NormalBG = COLOR:White
  !        DwrTreeQ.SelectedFG = COLOR:Blue
  !        DwrTreeQ.SelectedBG = COLOR:White !COLOR:Gray
  !        DwrTreeQ.Icon = 1
  !        DwrTreeQ.Level = 2
  !        !DwrTreeQ.Loaded = 
  !        !DwrTreeQ.Position
  !        Add(DwrTreeQ)
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('TreeWindow', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitPrepare(nysInit_PrepareNoOCXCtrl)
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?DwrTreeList{prop:lineheight} = 10
  ?DwrTreeList{PROP:iconlist,1} = 'DialogLarge.ico'  
  ?DwrTreeList{PROP:iconlist,2} = 'Login2Large.ico'  
  ?DwrTreeList{PROP:iconlist,3} = 'Folder5Large.ico'  
  ?DwrTreeList{PROP:iconlist,4} = 'FolderOpenLarge.ico'  
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.Init(DockingPaneWndMgr)
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitTemplateSettings()
  NYS:DockingPane_WndExt.InitComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.WindowPrepare('TreeWindow')
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  Do DefineListboxStyle
  Resizer.Init(AppStrategy:Resize)                         ! Controls will change size as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitResize()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  IF NYS:DockingPane_WndExt.Active <> TRUE
  INIMgr.Fetch('TreeWindow',Window)                        ! Restore window settings from non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  Resizer.Resize                                           ! Reset required after window size altered by INI manager
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.WindowInit()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('TreeWindow', THREAD())
  !---- Noyantis : Template Helper - End ----
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Fields.Close()
  END
  IF SELF.Opened
    INIMgr.Update('TreeWindow',Window)                     ! Save window data to non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.Kill()
  NYS:DockingPane_EventMgr.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.WindowClosed()
  NYS:DockingPane_WndExt.Kill()
  NYS:DockingPane_WndExt.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  GlobalErrors.SetProcedureName
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
    OF ?ExpandAllBtn
      Loop x = 1 to Records(DwrTreeQ) 
        Get(DwrTreeQ,x)
        If DwrTreeQ.Level = -1
            DwrTreeQ.Level = 0
        End
        Put(DwrTreeQ)
      End
    OF ?ContractAllBtn
      Loop x = 1 to Records(DwrTreeQ) 
        Get(DwrTreeQ,x)
        If DwrTreeQ.Level = 0
            DwrTreeQ.Level = -1
        End
        Put(DwrTreeQ)
      End
    END
  ReturnValue = PARENT.TakeAccepted()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeEvent()
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeEvent(EVENT())
    IF NYS:DockingPane_EventMgr.TakeEventCycleRequired = TRUE THEN CYCLE.
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_WndExt.TakeEvent(EVENT())
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
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
    OF EVENT:OpenWindow
      !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
      NYS:DockingPane_WndExt.WindowOpened()
      !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    END
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeWindowEvent(Window)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_WndExt.TakeWindowEvent(Window)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_WndExt.InitComplete      PROCEDURE()
  CODE
  PARENT.InitComplete()
  RETURN

NYS:DockingPane_WndExt.InitPrepare       PROCEDURE(SIGNED paramOCXCtrl)
  CODE
  SELF.DisableAtRuntime = FALSE
  PARENT.InitPrepare(paramOCXCtrl)
  RETURN

NYS:DockingPane_WndExt.InitResize        PROCEDURE()
  CODE
  PARENT.InitResize()
  SELF.claResize.Active = TRUE
  IF SELF.DragDropCtrl <> 0 THEN Resizer.SetParentControl(SELF.DragDropCtrl, SELF.OCXCtrl).
  IF SELF.ParentGroup  <> 0 THEN Resizer.SetParentControl(SELF.ParentGroup,  SELF.OCXCtrl).
  IF SELF.ParentRegion <> 0 THEN Resizer.SetParentControl(SELF.ParentRegion, SELF.OCXCtrl).
  IF SELF.ResizeBox    <> 0 THEN Resizer.SetParentControl(SELF.ResizeBox,    SELF.OCXCtrl).
  RETURN

NYS:DockingPane_WndExt.InitSecurity      PROCEDURE()
  CODE
  PARENT.InitSecurity()
  RETURN

NYS:DockingPane_WndExt.InitTemplateSettings PROCEDURE()
  CODE
  PARENT.InitTemplateSettings()
  SELF.AllowedAsContent       = TRUE
  SELF.DisableClose           = 1
  SELF.HideTitle              = 1
  SELF.HideWindow             = 1
  SELF.UpdParentTitle         = nysDockingPane_ParentTitleNoAction
  SELF.UpdParentTitleMinLevel = 1
  SELF.ParentPaneID           = ''
  SELF.ParentThreadNo         = 0
  RETURN

NYS:DockingPane_WndExt.Keystroke         PROCEDURE(UNSIGNED paramKeycode)
  CODE
  PARENT.Keystroke(paramKeycode)
  RETURN

NYS:DockingPane_WndExt.Kill              PROCEDURE()
  CODE
  PARENT.Kill()
  RETURN

NYS:DockingPane_WndExt.KillComplete      PROCEDURE()
  CODE
  PARENT.KillComplete()
  RETURN

NYS:DockingPane_WndExt.ParametersReceived PROCEDURE(<LONG paramSessionID>)
  CODE
  PARENT.ParametersReceived(paramSessionID)
  RETURN

NYS:DockingPane_WndExt.ProcessClones     PROCEDURE()
  CODE
  PARENT.ProcessClones()
  RETURN

NYS:DockingPane_WndExt.ProcessMimics     PROCEDURE()
  CODE
  PARENT.ProcessMimics()
  RETURN

NYS:DockingPane_WndExt.ProcessShortcutKey PROCEDURE(UNSIGNED pKeyCode)
  CODE
  PARENT.ProcessShortcutKey(pKeyCode)
  RETURN

NYS:DockingPane_WndExt.TakeEvent         PROCEDURE(SIGNED paramEvent)
  CODE
  PARENT.TakeEvent(paramEvent)
  RETURN

NYS:DockingPane_WndExt.TakeNotify        PROCEDURE(UNSIGNED paramNotifyCode, SIGNED paramThread, LONG paramParameter)
  CODE
  PARENT.TakeNotify(paramNotifyCode, paramThread, paramParameter)
  RETURN

NYS:DockingPane_WndExt.TakeSubClassEvent PROCEDURE(UNSIGNED paramWndHndl, UNSIGNED paramMsg, UNSIGNED paramWParam, LONG paramLParam)
  CODE
  PARENT.TakeSubClassEvent(paramWndHndl, paramMsg, paramWParam, paramLParam)
  RETURN

NYS:DockingPane_WndExt.TakeTimer         PROCEDURE()
  CODE
  PARENT.TakeTimer()
  RETURN

NYS:DockingPane_WndExt.TakeWindowEvent   PROCEDURE(*WINDOW paramWindow)
  CODE
  PARENT.TakeWindowEvent(paramWindow)
  RETURN

NYS:DockingPane_WndExt.Init              PROCEDURE(DockingPaneWndMgrClass pWMC)
  CODE
  SELF.MakeTarget = UPPER('DictionaryEditor.EXE')
  PARENT.Init(pWMC)
  RETURN

NYS:DockingPane_WndExt.WindowDocked      PROCEDURE()
  CODE
  PARENT.WindowDocked()
  RETURN

NYS:DockingPane_WndExt.WindowOpened      PROCEDURE()
  CODE
  PARENT.WindowOpened()
  RETURN

NYS:DockingPane_WndExt.WindowPrepare     PROCEDURE(STRING paramProcName)
  CODE
  PARENT.WindowPrepare(paramProcName)
  RETURN

NYS:DockingPane_WndExt.WindowRedraw      PROCEDURE()
  CODE
  PARENT.WindowRedraw()
  RETURN

!---- Noyantis : Codejock Docking Pane Wrapper - End ----

Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window
  SELF.SetStrategy(?ExpandAllBtn, Resize:LockXPos+Resize:LockYPos, Resize:LockSize) ! Override strategy for ?ExpandAllBtn
  SELF.SetStrategy(?ContractAllBtn, Resize:LockXPos+Resize:LockYPos, Resize:LockSize) ! Override strategy for ?ContractAllBtn

