

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR025.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
LoadDctx PROCEDURE (string pDctxFile,string pParams,string pThread)

AmReady             Long
ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
NotifyParam          STRING(1500)                          ! 
DisplayStr           STRING(50)                            ! 
ProgressStr          STRING(10)                            ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
Window               WINDOW('Load DCTX'),AT(,,260,62),FONT('Segoe UI',9),AUTO,ICON('Dictionary.ico'),GRAY,SYSTEM, |
  TIMER(1),WALLPAPER('Background1.png'),IMM
                       BUTTON('Close'),AT(226,47),USE(?Close)
                       PROGRESS,AT(1,20,257,10),USE(?PROGRESS1),HIDE,RANGE(0,100)
                       STRING(@s50),AT(2,7,256),USE(DisplayStr),CENTER(2),TRN
                       STRING(@s10),AT(2,34,256),USE(ProgressStr),CENTER(2),TRN
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
! ----- MyDct --------------------------------------------------------------------------
MyDct                Class(dwrDctParser)
    ! derived method declarations
SetProgress            PROCEDURE (long pCurrentCnt,long pTotal,<string pDisplayStr>),Virtual,Proc
                     End  ! MyDct
! ----- end MyDct -----------------------------------------------------------------------

  CODE
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
  GlobalErrors.SetProcedureName('LoadDctx')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Close
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('LoadDctx',pThread)
    NotifyHandler.AddNotifyCode(1)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  MyDct.UpdateFilesQueue('Dictionaries',Dictionaries)
  MyDct.UpdateFilesQueue('Tables',Tables)
  MyDct.UpdateFilesQueue('DctVersions',DctVersions)
  MyDct.UpdateFilesQueue('Fields',Fields)
  MyDct.UpdateFilesQueue('Keys',Keys)
  MyDct.UpdateFilesQueue('Relations',Relations)
  MyDct.UpdateFilesQueue('Aliases',Aliases)
  MyDct.UpdateFilesQueue('TableTypes',TableTypes)
  MyDct.UpdateFilesQueue('Triggers',Triggers)
  MyDct.UpdateFilesQueue('DuplicateKeys',DuplicateKeys)
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('LoadDctx', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
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
  INIMgr.Fetch('LoadDctx',Window)                          ! Restore window settings from non-volatile store
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
    AmReady = 1  
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('LoadDctx', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
    NotifyManager.DeleteProcedure('LoadDctx',pThread)
  IF SELF.Opened
    INIMgr.Update('LoadDctx',Window)                       ! Save window data to non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.Kill()
  NYS:DockingPane_EventMgr.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  GlobalErrors.SetProcedureName
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
    Case EVENT()
    Of EVENT:Timer
        If AmReady
            0{PROP:Timer} = 0
            UNHIDE(?PROGRESS1)
            MyDct.LoadFile(pDctxFile)
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
!----------------------------------------------------
MyDct.SetProgress   PROCEDURE (long pCurrentCnt,long pTotal,<string pDisplayStr>)
  CODE
  PARENT.SetProgress (pCurrentCnt,pTotal,pDisplayStr)
    If self.Progress > 100
        self.Progress = 100
    End
    ?PROGRESS1{PROP:Progress} = self.Progress
    ProgressStr = self.Progress & ' %'
    DisplayStr = pDisplayStr
    Display(?ProgressStr)
    Display(?DisplayStr)
    self.Trace(pDisplayStr)
    If self.Progress = 100
        ParamsGrp.DictionaryGuid = self.GetDictionaryGuid()
        MyDct.SendNotifyJson(NotifyManager,'Main3','LoadDctx','DctLoaded',OverStr)
        Post(EVENT:CloseWindow)
    End
NotifyHandler.HandleNotify       PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter)
us    UltimateString
    CODE    
    NotifyParam = NotifyManager.GetNotifyParm(NotifyParameter)
