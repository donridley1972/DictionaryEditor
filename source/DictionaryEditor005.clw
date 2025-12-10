

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR005.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Dictionaries file
!!! </summary>
BrowseDictionaries PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
json                JSONClass
st                  StringTheory
CurrentTab           STRING(80)                            ! 
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
BRW1::View:Browse    VIEW(Dictionaries)
                       PROJECT(Dct:DctName)
                       PROJECT(Dct:GUID)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
Dct:DctName            LIKE(Dct:DctName)              !List box control field - type derived from field
Dct:GUID               LIKE(Dct:GUID)                 !Browse hot field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Browse the Dictionaries file'),AT(,,137,264),FONT('Segoe UI',10,COLOR:Black,FONT:regular, |
  CHARSET:DEFAULT),RESIZE,AUTO,CENTER,GRAY,IMM,HLP('BrowseDictionaries'),SYSTEM
                       LIST,AT(2,2,131,260),USE(?Browse:1),HVSCROLL,FORMAT('80L(2)|M~Dct Name~@s100@'),FROM(Queue:Browse:1), |
  IMM,MSG('Browsing the Dictionaries file')
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
TakeNewSelection       PROCEDURE(),DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepStringClass                      ! Default Step Manager
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
? DEBUGHOOK(Dictionaries:Record)
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
  GlobalErrors.SetProcedureName('BrowseDictionaries')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('Dct:DctName',Dct:DctName)                          ! Added by: BrowseBox(ABC)
  BIND('Dct:GUID',Dct:GUID)                                ! Added by: BrowseBox(ABC)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  Relate:Dictionaries.SetOpenRelated()
  Relate:Dictionaries.Open()                               ! File Dictionaries used by this procedure, so make sure it's RelationManager is open
  Access:TreeMemTable.UseFile()                            ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Dictionaries,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('BrowseDictionaries', THREAD(), QuickWindow)
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
  NYS:DockingPane_WndExt.WindowPrepare('BrowseDictionaries')
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Runtime) ! Moveable thumb based upon Dct:GUID for sort order 1
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,Dct:PKDctGUIDKey) ! Add the sort order for Dct:PKDctGUIDKey for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,Dct:GUID,1,BRW1)               ! Initialize the browse locator using  using key: Dct:PKDctGUIDKey , Dct:GUID
  BRW1.AddField(Dct:DctName,BRW1.Q.Dct:DctName)            ! Field Dct:DctName is a hot field or requires assignment from browse
  BRW1.AddField(Dct:GUID,BRW1.Q.Dct:GUID)                  ! Field Dct:GUID is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Surface,Resize:SetMinSize)      ! Controls like list boxes will resize, whilst controls like buttons will move
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitResize()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  IF NYS:DockingPane_WndExt.Active <> TRUE
  INIMgr.Fetch('BrowseDictionaries',QuickWindow)           ! Restore window settings from non-volatile store
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
  TemplateHelper.DeleteActiveWindow('BrowseDictionaries', THREAD())
  !---- Noyantis : Template Helper - End ----
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Dictionaries.Close()
  END
  IF SELF.Opened
    INIMgr.Update('BrowseDictionaries',QuickWindow)        ! Save window data to non-volatile store
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
      ParamsGrp.Action = 'Refresh'
      ParamsGrp.DictionaryGuid = Queue:Browse:1.Dct:GUID
      json.Start()
      json.SetTagCase(jF:CaseAsIs)
      json.Save(ParamsGrp,st)
      NotifyManager.NotifyProcedure('BrowseTables',st.GetValue())   
      NotifyManager.NotifyProcedure('Main3',st.GetValue()) 
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

BRW1.TakeNewSelection PROCEDURE

st          StringTheory
  CODE
  PARENT.TakeNewSelection
    ParamsGrp.Action = 'Refresh'
    ParamsGrp.DictionaryGuid = Dct:GUID
    json.Start()
    json.SetTagCase(jF:CaseAsIs)
    json.Save(ParamsGrp,st)
    NotifyManager.NotifyProcedure('BrowseTables',st.GetValue())     
    NotifyManager.NotifyProcedure('Main3',st.GetValue())      
  !    CREATE(TreeMemTable)
  !    Tree:PKGuid = glo:st.MakeGuid()
  !    Tree:DictionaryGuid = Dct:GUID
  !    Access:TreeMemTable.Insert()    


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

