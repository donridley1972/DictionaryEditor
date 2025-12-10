

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR028.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('DICTIONARYEDITOR029.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR030.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
DuplicateKeysWindow PROCEDURE (string pParams,string pThread)

ParamsGrp                   Group(ParamsGroupType).
OverStr                     String(Size(ParamsGrp)),Over(ParamsGrp) 
MyDct                       dwrDctParser
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
NotifyParam          STRING(1500)                          ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DonutHoleWindow     Class(NinjaDonutHoleWindowClass)
Repaint               PROCEDURE(),DERIVED
                    end
BRW6::View:Browse    VIEW(DuplicateKeys)
                       PROJECT(DupK:KeyName)
                       PROJECT(DupK:DictionaryGuid)
                       PROJECT(DupK:PKGuid)
                       PROJECT(DupK:TableGuid)
                       JOIN(Tab:PKTabGuidKey,DupK:TableGuid),INNER
                         PROJECT(Tab:TableName)
                       END
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
DupK:KeyName           LIKE(DupK:KeyName)             !List box control field - type derived from field
Tab:TableName          LIKE(Tab:TableName)            !List box control field - type derived from field
DupK:DictionaryGuid    LIKE(DupK:DictionaryGuid)      !Browse hot field - type derived from field
DupK:PKGuid            LIKE(DupK:PKGuid)              !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
Window               WINDOW('Caption'),AT(,,454,269),FONT('Segoe UI',10),RESIZE,AUTO,GRAY,SYSTEM,WALLPAPER('Background1.png')
                       LIST,AT(2,27,450,240),USE(?List),LEFT(2),VSCROLL,FORMAT('116L(2)M~Key Name~@s100@400L(2' & |
  ')M~Table Name~@s100@'),FROM(Queue:Browse),IMM
                       BUTTON('Prepend With Table Prefix'),AT(2,1,77,23),USE(?AddTablePrefixBtn),LEFT,ICON('Prepend1.ico'), |
  FLAT
                       BUTTON('Recheck'),AT(83,1,77,23),USE(?RecheckForDuplicatesBtn),LEFT,ICON('DataRefresh.ico'), |
  FLAT
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
mhResize             CLASS(MH::ResizeWindowClass)
Init                   PROCEDURE()
InitialResize          PROCEDURE()                         ! New method added to this class instance
                     END

DuplicateKeysWindow:mhResize:WM CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END
BRW6                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
                     END

! ----- MyObject7 --------------------------------------------------------------------------
MyObject7            Class(dwrDctParser)
                     End  ! MyObject7
! ----- end MyObject7 -----------------------------------------------------------------------

  CODE
? DEBUGHOOK(DuplicateKeys:Record)
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
    OverStr = pParams  
  GlobalErrors.SetProcedureName('DuplicateKeysWindow')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('Tab:TableName',Tab:TableName)                      ! Added by: BrowseBox(ABC)
  BIND('DupK:PKGuid',DupK:PKGuid)                          ! Added by: BrowseBox(ABC)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('DuplicateKeysWindow',pThread)
    NotifyHandler.AddNotifyCode(1)
  MyObject7.UpdateFilesQueue('DuplicateKeys',DuplicateKeys)
  MyObject7.UpdateFilesQueue('Keys',Keys)
  MyObject7.UpdateFilesQueue('Tables',Tables)
  Relate:DuplicateKeys.Open()                              ! File DuplicateKeys used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW6.Init(?List,Queue:Browse.ViewPosition,BRW6::View:Browse,Queue:Browse,Relate:DuplicateKeys,SELF) ! Initialize the browse manager
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('DuplicateKeysWindow', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
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
  mhResize.Init
  BRW6.Q &= Queue:Browse
  BRW6.AddSortOrder(,)                                     ! Add the sort order for  for sort order 1
  BRW6.AddField(DupK:KeyName,BRW6.Q.DupK:KeyName)          ! Field DupK:KeyName is a hot field or requires assignment from browse
  BRW6.AddField(Tab:TableName,BRW6.Q.Tab:TableName)        ! Field Tab:TableName is a hot field or requires assignment from browse
  BRW6.AddField(DupK:DictionaryGuid,BRW6.Q.DupK:DictionaryGuid) ! Field DupK:DictionaryGuid is a hot field or requires assignment from browse
  BRW6.AddField(DupK:PKGuid,BRW6.Q.DupK:PKGuid)            ! Field DupK:PKGuid is a hot field or requires assignment from browse
  INIMgr.Fetch('DuplicateKeysWindow',Window)               ! Restore window settings from non-volatile store
  BRW6.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW6.SetFilter('(DupK:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')')
  DonutHoleWindow.SetLogPreamble('DuplicateKeysWindow')
  DonutHoleWindow.init(DonutHoleRegistrator.IDonutHoleRegistration,thread(),Window)
  SELF.AddItem(DonutHoleWindow.WindowComponent)
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('DuplicateKeysWindow', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:DuplicateKeys.Close()
  END
    NotifyManager.DeleteProcedure('DuplicateKeysWindow',pThread)
  mhResize.Kill
  IF SELF.Opened
    INIMgr.Update('DuplicateKeysWindow',Window)            ! Save window data to non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.Kill()
  NYS:DockingPane_EventMgr.KillComplete()
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
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?AddTablePrefixBtn
      ThisWindow.Update()
      Resume(START(FixDuplicateKeys, 25000, OverStr,Thread()))
      ThisWindow.Reset      
    OF ?RecheckForDuplicatesBtn
      ThisWindow.Update()
      Resume(START(CheckForDuplicateKeys, 25000, OverStr,Thread()))
      ThisWindow.Reset      
    END
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
    NYS:DockingPane_EventMgr.TakeWindowEvent(Window)
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

mhResize.Init PROCEDURE

  CODE
  SELF.Init(                                               |
      mhResizeFrame,                                       | !Global Host Object
      Window,                                              | !Window
      DuplicateKeysWindow:mhResize:WM.MH::ResizeIWindowManager, | !Window Manager
      454,                                                 | !Original Width
      269,                                                 | !Original Height
      1,                                                   | !Resize Width?
      1,                                                   | !Resize Height?
      0,                                                   | !Orig Size is Min?
      0,                                                   | !Border Beyond Min?
      0,                                                   | !Maximize Initially?
      0)                                                     !Do NOT hide+unhide during opening operations?
  SELF.AddControl(?List,0,0,1,1)
  RETURN
  PARENT.Init

mhResize.InitialResize PROCEDURE()

MH::RestoreFromIni   BYTE,AUTO

  CODE
  IF NOT SELF.IsInitialResizeDone
    MH::RestoreFromIni = CHOOSE(INIMgr.TryFetch('DuplicateKeysWindow','XPos') OR INIMgr.TryFetch('DuplicateKeysWindow','Maximize'))
    SELF.InitialResize(                                    |
        MH::RestoreFromIni,                                |  !Restore from INI file?
        0,                                                 |  !Maximize Initially?
        mhHorzPos:Full,                                    |  !Init Horz Pos
        mhVertPos:Full,                                    |  !Init Vert Pos
        0,                                                 |  !Horizontal Shift
        0                                                  )  !Vertical Shift
    IF SELF.IsInitialResizeDone
    END
  END


DuplicateKeysWindow:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
  CODE
  ThisWindow.Reset(Force)

NotifyHandler.HandleNotify       PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter)
us    UltimateString
TempSt      StringTheory
VersionVal  Long
json        JSONClass
    CODE    
    NotifyParam = NotifyManager.GetNotifyParm(NotifyParameter)
	If Size(NotifyParam) > 0
		TempSt.SetValue(NotifyParam)
        MyDct.Trace('')
        MyDct.Trace('DuplicateKeysWindow - NotifyHandler.HandleNotify')
		MyDct.Trace(TempSt.GetValue())
        MyDct.Trace('')
		json.Start()
		json.SetTagCase(jF:CaseAsIs)
		json.Load(ParamsGrp,TempSt)
        Case ParamsGrp.Action[1:Size(ParamsGrp.Action)]
        Of 'DuplicatesChecked'
            BRW6.ResetSort(True)
        End
    End
DonutHoleWindow.Repaint               PROCEDURE()!,DERIVED
  CODE
  PARENT.Repaint()
