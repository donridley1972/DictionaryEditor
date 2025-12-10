

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR004.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Tables file
!!! </summary>
BrowseTables PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
json                JSONClass
st                  StringTheory
MyDct               dwrDctParser
CurrentTab           STRING(80)                            ! 
NotifyParam          STRING(1000)                          ! 
DictionaryGuid       STRING(16)                            ! 
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
BRW1::View:Browse    VIEW(Tables)
                       PROJECT(Tab:PKGuid)
                       PROJECT(Tab:ParentGUID)
                       PROJECT(Tab:DictionaryGuid)
                       JOIN(Fld:FKFldParentGuidKey,Tab:PKGuid),INNER
                         PROJECT(Fld:ParentGUID)
                       END
                       JOIN(Key:FKKeyParentGuidKey,Tab:PKGuid),INNER
                         PROJECT(Key:ParentGUID)
                       END
                       JOIN(Ali:FKAliParentGuidKey,Tab:PKGuid)
                       END
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
Tab:PKGuid             LIKE(Tab:PKGuid)               !List box control field - type derived from field
Tab:ParentGUID         LIKE(Tab:ParentGUID)           !List box control field - type derived from field
Tab:DictionaryGuid     LIKE(Tab:DictionaryGuid)       !List box control field - type derived from field
Fld:ParentGUID         LIKE(Fld:ParentGUID)           !List box control field - type derived from field
Key:ParentGUID         LIKE(Key:ParentGUID)           !List box control field - type derived from field
DictionaryGuid         LIKE(DictionaryGuid)           !Browse hot field - type derived from local data
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Browse the Tables file'),AT(,,449,264),FONT('Segoe UI',10,COLOR:Black,FONT:regular, |
  CHARSET:DEFAULT),RESIZE,AUTO,CENTER,GRAY,IMM,HLP('BrowseTables'),SYSTEM
                       LIST,AT(2,2,444,253),USE(?Browse:1),HVSCROLL,FORMAT('64L(2)|M~PKG uid~@s16@64L(2)|M~Tab' & |
  'le Parent GUID~@s16@64L(2)|M~Dictionary Guid~@s16@64L(2)|M~Field Parent GUID~@s16@64' & |
  'L(2)|M~Key Parent GUID~@s16@'),FROM(Queue:Browse:1),IMM,MSG('Browsing the Tables file')
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
ApplyFilter            PROCEDURE(),DERIVED
TakeNewSelection       PROCEDURE(),DERIVED
                     END

BRW1::Sort0:StepClass StepClass                            ! Default Step Manager
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
? DEBUGHOOK(Tables:Record)
? DEBUGHOOK(TreeMemTable:Record)
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
  GlobalErrors.SetProcedureName('BrowseTables')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('Tab:PKGuid',Tab:PKGuid)                            ! Added by: BrowseBox(ABC)
  BIND('Tab:ParentGUID',Tab:ParentGUID)                    ! Added by: BrowseBox(ABC)
  BIND('Tab:DictionaryGuid',Tab:DictionaryGuid)            ! Added by: BrowseBox(ABC)
  BIND('Fld:ParentGUID',Fld:ParentGUID)                    ! Added by: BrowseBox(ABC)
  BIND('Key:ParentGUID',Key:ParentGUID)                    ! Added by: BrowseBox(ABC)
  BIND('DictionaryGuid',DictionaryGuid)                    ! Added by: BrowseBox(ABC)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  Relate:Tables.SetOpenRelated()
  Relate:Tables.Open()                                     ! File Tables used by this procedure, so make sure it's RelationManager is open
  Access:TreeMemTable.UseFile()                            ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Tables,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('BrowseTables', THREAD(), QuickWindow)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitPrepare(nysInit_PrepareNoOCXCtrl)
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
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
  NYS:DockingPane_WndExt.WindowPrepare('BrowseTables')
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1.AddSortOrder(,)                                     ! Add the sort order for  for sort order 1
  !  BRW1.SetFilter('Tab:DictionaryGuid = ' & '''' & DictionaryGuid & '''')  
  BRW1.AddField(Tab:PKGuid,BRW1.Q.Tab:PKGuid)              ! Field Tab:PKGuid is a hot field or requires assignment from browse
  BRW1.AddField(Tab:ParentGUID,BRW1.Q.Tab:ParentGUID)      ! Field Tab:ParentGUID is a hot field or requires assignment from browse
  BRW1.AddField(Tab:DictionaryGuid,BRW1.Q.Tab:DictionaryGuid) ! Field Tab:DictionaryGuid is a hot field or requires assignment from browse
  BRW1.AddField(Fld:ParentGUID,BRW1.Q.Fld:ParentGUID)      ! Field Fld:ParentGUID is a hot field or requires assignment from browse
  BRW1.AddField(Key:ParentGUID,BRW1.Q.Key:ParentGUID)      ! Field Key:ParentGUID is a hot field or requires assignment from browse
  BRW1.AddField(DictionaryGuid,BRW1.Q.DictionaryGuid)      ! Field DictionaryGuid is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Surface,Resize:SetMinSize)      ! Controls like list boxes will resize, whilst controls like buttons will move
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitResize()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  IF NYS:DockingPane_WndExt.Active <> TRUE
  INIMgr.Fetch('BrowseTables',QuickWindow)                 ! Restore window settings from non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  Resizer.Resize                                           ! Reset required after window size altered by INI manager
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  !    GlobalThreadsQueue.ThreadNo = Thread()
  !    GlobalThreadsQueue.ProcedureName = GlobalErrors.GetProcedureName()
  !    Add(GlobalThreadsQueue)    
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.WindowInit()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('BrowseTables', THREAD())
  !---- Noyantis : Template Helper - End ----
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Tables.Close()
  END
  IF SELF.Opened
    INIMgr.Update('BrowseTables',QuickWindow)              ! Save window data to non-volatile store
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
  !    GlobalThreadsQueue.ThreadNo = Thread()
  !    Get(GlobalThreadsQueue,GlobalThreadsQueue.ThreadNo)
  !    If Not Errorcode()
  !        Delete(GlobalThreadsQueue)
  !    End   
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
    OF ?Browse:1
      get(Queue:Browse:1,choice(?Browse:1))    
      ParamsGrp.ParentGuid = Queue:Browse:1.Tab:PKGuid
      ParamsGrp.TableGuid = Queue:Browse:1.Tab:PKGuid
      json.Start()
      json.SetTagCase(jF:CaseAsIs)
      json.Save(ParamsGrp,st)
      NotifyManager.NotifyProcedure('BrowseFields',st.GetValue()) 
      NotifyManager.NotifyProcedure('BrowseKeys',st.GetValue())   
      NotifyManager.NotifyProcedure('Main3',st.GetValue())  
      !Set(TreeMemTable)
      !Next(TreeMemTable)
      !Tree:TableGuid = ParamsGrp.TableGuid
      !Access:TreeMemTable.Update()
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
    NYS:DockingPane_EventMgr.TakeWindowEvent(QuickWindow)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_WndExt.TakeWindowEvent(QuickWindow)
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

BRW1.ApplyFilter PROCEDURE

  CODE
  PARENT.ApplyFilter
    !MyDct.Trace('BrowseTables - Filter = ' & BRW1::View:Browse{PROP:Filter})


BRW1.TakeNewSelection PROCEDURE

  CODE
  PARENT.TakeNewSelection
    ParamsGrp.Action = 'Refresh'
    ParamsGrp.TableGuid = Tab:PKGuid
    ParamsGrp.ParentGuid = Tab:PKGuid
    json.Start()
    json.SetTagCase(jF:CaseAsIs)
    json.Save(ParamsGrp,st)
    NotifyManager.NotifyProcedure('TreeWindow',st.GetValue())
    NotifyManager.NotifyProcedure('BrowseFields',st.GetValue()) 
    NotifyManager.NotifyProcedure('BrowseKeys',st.GetValue())     
  
    !Next(TreeMemTable)
    !Tree:TableGuid = ParamsGrp.TableGuid
    !Access:TreeMemTable.Update()


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

    
