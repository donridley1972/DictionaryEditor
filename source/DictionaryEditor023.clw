

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR023.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
SettingsWindow PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
MyDct               dwrDctParser
SettingsGrp          GROUP(SettingsGroupType),PRE()        ! 
                     END                                   ! 
NotifyParam          STRING(1500)                          ! 
SearchTypesQ         QUEUE,PRE(Srch)                       ! 
Type                 STRING(20)                            ! 
TypeVal              LONG                                  ! 
RowNum               LONG                                  ! 
                     END                                   ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DonutHoleWindow     Class(NinjaDonutHoleWindowClass)
Repaint               PROCEDURE(),DERIVED
                    end
Window               WINDOW('Caption'),AT(,,363,242),FONT('Segoe UI',9),RESIZE,AUTO,GRAY,SYSTEM,WALLPAPER('Background1.png')
                       SHEET,AT(2,2,359,238),USE(?SettingsSheet)
                         TAB('Settings'),USE(?SettingsTab),ICON('Settings9.ico')
                           LIST,AT(77,24,103,10),USE(?SearchTypesList),DROP(2),FORMAT('80L(2)|M@s20@'),FROM(SearchTypesQ)
                           PROMPT('Default Search Type:'),AT(7,25),USE(?DefaultSearchTypePrompt),TRN
                           CHECK('  Automatically Set New Field External Name Same as Field Name'),AT(7,44,221,10),USE(?AutoExternalNameCheck), |
  TRN
                         END
                       END
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
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
mhResize             CLASS(MH::ResizeWindowClass)
Init                   PROCEDURE()
InitialResize          PROCEDURE()                         ! New method added to this class instance
                     END

SettingsWindow:mhResize:WM CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END

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
  GlobalErrors.SetProcedureName('SettingsWindow')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?SearchTypesList
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  
  
    SearchTypesQ.Type = 'Contains'
    SearchTypesQ.TypeVal = 1
    SearchTypesQ.RowNum = 1
    Add(SearchTypesQ)  
  
    SearchTypesQ.Type = 'StartsWith'
    SearchTypesQ.TypeVal = 2
    SearchTypesQ.RowNum = 2
    Add(SearchTypesQ) 
  
    Clear(SearchTypesQ)
  
    MyDct.LoadSettings(SettingsGrp)
  
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('SettingsWindow', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?SettingsSheet{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
    ?AutoExternalNameCheck{PROP:Use} = SettingsGrp.AutoSetExternalNameAsLabel  
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
  WinAlertMouseZoom()
  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
  mhResize.Init
  INIMgr.Fetch('SettingsWindow',Window)                    ! Restore window settings from non-volatile store
  DonutHoleWindow.SetLogPreamble('SettingsWindow')
  DonutHoleWindow.init(DonutHoleRegistrator.IDonutHoleRegistration,thread(),Window)
  SELF.AddItem(DonutHoleWindow.WindowComponent)
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
    MyDct.Trace('')
    MyDct.Trace('SettingsWindow')
    MyDct.Trace('')
    MyDct.Trace('   SettingsGrp.DefaultSearchType = ' & SettingsGrp.DefaultSearchType)
  
    SearchTypesQ.TypeVal = SettingsGrp.DefaultSearchType
    Get(SearchTypesQ,SearchTypesQ.TypeVal)
    MyDct.Trace('   Position(SearchTypesQ) = ' & Position(SearchTypesQ))
    MyDct.Trace('   SearchTypesQ.RowNum = ' & SearchTypesQ.RowNum)
    SELECT(?SearchTypesList,SearchTypesQ.RowNum)
    Post(EVENT:NewSelection,?SearchTypesList)  
    MyDct.Trace('')
    Clear(SearchTypesQ)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('SettingsWindow', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  mhResize.Kill
  IF SELF.Opened
    INIMgr.Update('SettingsWindow',Window)                 ! Save window data to non-volatile store
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
    CASE ACCEPTED()
    OF ?SearchTypesList
      Get(SearchTypesQ,Choice(?SearchTypesList))      
      MyDct.Trace('')
      MyDct.Trace('?SearchTypesList - SearchTypesQ.TypeVal = ' & SearchTypesQ.TypeVal)
      MyDct.Trace('')
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?SearchTypesList
      ! Get(SearchTypesQ,Choice(?SearchTypesList))
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
    Case EVENT()
    Of event:DonutHoleDisconnected
    Of EVENT:Accepted
        MyDct.Trace('')
        MyDct.Trace('SettingsWindow - EVENT:Accepted - SearchTypesQ.TypeVal = ' & SearchTypesQ.TypeVal)
        MyDct.Trace('')
        SettingsGrp.DefaultSearchType = SearchTypesQ.TypeVal     
        MyDct.SaveSettings(SettingsGrp)
        MyDct.SendNotifyJson(NotifyManager,'Main3','SettingsWindow','SettingsChanged',OverStr)
    End    
  If event() = event:VisibleOnDesktop !or event() = event:moved
    ds_VisibleOnDesktop()
  end
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?SearchTypesList
      !      Get(SearchTypesQ,Choice(?SearchTypesList))      
      !      MyDct.Trace('')
      !      MyDct.Trace('?SearchTypesList - SearchTypesQ.TypeVal = ' & SearchTypesQ.TypeVal)
      !      MyDct.Trace('')
    END
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
      SettingsWindow:mhResize:WM.MH::ResizeIWindowManager, | !Window Manager
      363,                                                 | !Original Width
      242,                                                 | !Original Height
      1,                                                   | !Resize Width?
      1,                                                   | !Resize Height?
      0,                                                   | !Orig Size is Min?
      0,                                                   | !Border Beyond Min?
      0,                                                   | !Maximize Initially?
      0)                                                     !Do NOT hide+unhide during opening operations?
  SELF.AddControl(?SettingsSheet,0,0,1,1)
  RETURN
  PARENT.Init

mhResize.InitialResize PROCEDURE()

MH::RestoreFromIni   BYTE,AUTO

  CODE
  IF NOT SELF.IsInitialResizeDone
    MH::RestoreFromIni = CHOOSE(INIMgr.TryFetch('SettingsWindow','XPos') OR INIMgr.TryFetch('SettingsWindow','Maximize'))
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


SettingsWindow:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
  CODE
  ThisWindow.Reset(Force)

DonutHoleWindow.Repaint               PROCEDURE()!,DERIVED
  CODE
  PARENT.Repaint()
