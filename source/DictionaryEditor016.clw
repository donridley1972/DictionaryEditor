

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR016.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
UpdateComment PROCEDURE (string pParams,string pThread)

ParamsGrp               Group(ParamsGroupType).
OverStr                 String(Size(ParamsGrp)),Over(ParamsGrp)
json				    JSONClass
st					    StringTheory
MyDct				    dwrDctParser


CommentStartValue       String(255)
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
ActionMessage        CSTRING(40)                           ! 
NotifyParam          STRING(1000)                          ! 
CommentStr           STRING(255)                           ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
Window               WINDOW('Caption'),AT(,,280,177),FONT('Segoe UI',10),AUTO,ICON('Dictionary.ico'),GRAY,SYSTEM, |
  WALLPAPER('Background1.png'),IMM
                       BUTTON('Cancel'),AT(235,161),USE(?Cancel),FLAT
                       BUTTON('OK'),AT(194,161,38),USE(?SaveBtn),FLAT
                       SHEET,AT(2,1,277,157),USE(?SHEET1)
                         TAB('General'),USE(?GeneralTab)
                           PROMPT('Detail:'),AT(9,17),USE(?DetailPrompt),TRN
                           TEXT,AT(10,30,262,120),USE(CommentStr),LEFT
                         END
                         TAB('Comments'),USE(?CommentsTab)
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
  GlobalErrors.SetProcedureName('UpdateComment')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Cancel
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('UpdateComment',pThread)
    NotifyHandler.AddNotifyCode(1)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('UpdateComment', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?SHEET1{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
  SELECT(?CommentStr)
    json.Start()
    json.SetTagCase(jF:CaseAsIs)
    json.Save(ParamsGrp,st)  
    MyDct.Trace('')
    MyDct.Trace('UpdateComment - SELF.Open(Window)')
    MyDct.Trace('')
    MyDct.Trace(st.GetValue())
    MyDct.Trace('')
  
  
  
    Case ParamsGrp.Action[1:Size(ParamsGrp.Action)]
    OF 'InsertComment'
    OF 'EditComment'
        CommentStr = ParamsGrp.CommentJson
        CommentStartValue = CommentStr
    END  
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
  INIMgr.Fetch('UpdateComment',Window)                     ! Restore window settings from non-volatile store
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('UpdateComment', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
    NotifyManager.DeleteProcedure('UpdateComment',pThread)
  IF SELF.Opened
    INIMgr.Update('UpdateComment',Window)                  ! Save window data to non-volatile store
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
    OF ?SaveBtn
      !Case ParamsGrp.Action[1:Size(ParamsGrp.Action)]
      !OF 'InsertComment'
      !OF 'EditComment'
      !    ParamsGrp.CommentJson = CommentStr
      !END  
      
      ParamsGrp.CommentJson = CommentStr[1:Size(CommentStr)]
      If Upper(CommentStartValue[1:Size(CommentStartValue)]) <> Upper(ParamsGrp.CommentJson[1:Size(ParamsGrp.CommentJson)])
        json.Start()
        json.SetTagCase(jF:CaseAsIs)
        json.Save(ParamsGrp,st)
        NotifyManager.NotifyProcedure(ParamsGrp.ProcedureToNotify[1:Size(ParamsGrp.ProcedureToNotify)],st.GetValue())  
      End
      Post(EVENT:CloseWindow)
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
    OF ?CommentStr
      ?CommentStr{PROP:Touched} = 1      
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
NotifyHandler.HandleNotify       PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter)
us    UltimateString
    CODE    
    NotifyParam = NotifyManager.GetNotifyParm(NotifyParameter)
	If Size(NotifyParam) > 0
		st.SetValue(NotifyParam)
        MyDct.Trace('UpdateComment - NotifyHandler.HandleNotify')
		MyDct.Trace(st.GetValue())
		!json.Start()
		!json.SetTagCase(jF:CaseAsIs)
		!json.Load(ParamsGrp,st)
	End
