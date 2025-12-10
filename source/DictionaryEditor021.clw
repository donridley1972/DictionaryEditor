

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR021.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
ds_Message PROCEDURE (STRING MessageTxt,<STRING HeadingTxt>,<STRING IconSent>,<STRING ButtonsPar>,UNSIGNED Defaults=0,BOOL StylePar=FALSE)

FilesOpened          BYTE                                  ! 
Loc:ButtonPressed    UNSIGNED                              ! 
EmailLink            CSTRING(4096)                         ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DontShowThisAgain       byte(0)         !CapeSoft MessageBox Data
LocalMessageBoxdata     group,pre(LMBD) !CapeSoft MessageBox Data
MessageText               cstring(4096)
HeadingText               cstring(1024)
UseIcon                   cstring(1024)
Buttons                   cstring(1024)
Defaults                  unsigned
                        end
window               WINDOW('Caption'),AT(,,413,108),FONT('Segoe UI',10,,FONT:regular),DOUBLE,GRAY,WALLPAPER('Background1.png')
                       IMAGE,AT(3,4),USE(?Image1),HIDE
                       PROMPT(''''),AT(63,14),USE(?MainTextPrompt),COLOR(00ECEFF0h),TRN
                       STRING('HyperActive Link'),AT(63,26),USE(?HALink),COLOR(00ECEFF0h),HIDE,TRN
                       STRING('Time Out:'),AT(63,37),USE(?TimerCounter),COLOR(00ECEFF0h),HIDE,TRN
                       BUTTON('Button 1'),AT(9,81,45,14),USE(?Button1),HIDE
                       BUTTON('Button 2'),AT(59,81,45,14),USE(?Button2),HIDE
                       BUTTON('Button 3'),AT(109,81,45,14),USE(?Button3),HIDE
                       BUTTON('Button 4'),AT(159,81,45,14),USE(?Button4),HIDE
                       BUTTON('Button 5'),AT(209,81,45,14),USE(?Button5),HIDE
                       BUTTON('Button 6'),AT(259,81,45,14),USE(?Button6),HIDE
                       BUTTON('Button 7'),AT(309,81,45,14),USE(?Button7),HIDE
                       BUTTON('Button 8'),AT(359,81,45,14),USE(?Button8),HIDE
                       CHECK('&Don''t Show This Again'),AT(63,63),USE(DontShowThisAgain),COLOR(00EEEEEEh),HIDE
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop
  RETURN(Loc:ButtonPressed)

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
ThisMessageBoxLogInit          routine         !CapeSoft MessageBox Initialize properties routine
    if not omitted(2)
      LMBD:HeadingText = HeadingTxt
    end
    if ~omitted(3) then LMBD:UseIcon = IconSent .
    if ~omitted(4) then LMBD:Buttons = ButtonsPar .
    if ~omitted(5) then LMBD:Defaults = Defaults .
    LMBD:MessageText = MessageTxt
    ThisMessageBox.FromExe = command(0)
    if instring('\',ThisMessageBox.FromExe,1,1)
      ThisMessageBox.FromExe = ThisMessageBox.FromExe[ (instring('\',ThisMessageBox.FromExe,-1,len(ThisMessageBox.FromExe)) + 1) : len(ThisMessageBox.FromExe) ]
    end
    ThisMessageBox.TrnStrings = 1
    ThisMessageBox.PromptControl = ?MainTextPrompt
    ThisMessageBox.IconControl = ?Image1
         !End of CapeSoft MessageBox Initialize properties routine

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
    ThisMessageBox.SavedResponse = GlobalResponse        !CapeSoft MessageBox Code - Preserves the GlobalResponse
  GlobalErrors.SetProcedureName('ds_Message')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Image1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
        GlobalRequest = ThisWindow.Request         !Keep GlobalRequest the correct value.
        if ThisMessageBox.MessagesUsed then
          return level:fatal
        end
        ThisMessageBox.MessagesUsed += 1
        do ThisMessageBoxLogInit                               !CapeSoft MessageBox Code
        ThisMessageBox.ReturnValue = ThisMessageBox.PreOpen(LMBD:MessageText,LMBD:HeadingText,LMBD:Defaults)
        if ThisMessageBox.ReturnValue then
          self.Response = RequestCompleted
          return level:notify
        end
        ThisMessageBox.DontShowThisAgain = ?DontShowThisAgain
        ThisMessageBox.TimeOutControl = ?TimerCounter
        ThisMessageBox.HAControl = ?HALink
  SELF.Open(window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('ds_Message', THREAD(), window)
  !---- Noyantis : Template Helper - End ----
  target{prop:width} = 100
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ThisMessageBox.TimeOutText = ?TimerCounter{prop:text}
  ThisMessageBox.MinWinWidth = 100
  ThisMessageBox.open(LMBD:MessageText,LMBD:HeadingText,LMBD:UseIcon,LMBD:Buttons,LMBD:Defaults,StylePar)         !CapeSoft MessageBox Code
      Alert(EscKey)
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
  WinAlertMouseZoom()
  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
              !CapeSoft MessageBox EndCode
    Loc:ButtonPressed = ThisMessageBox.ReturnValue
    ThisMessageBox.Close()
    if ThisMessageBox.MessagesUsed > 0 then ThisMessageBox.MessagesUsed -= 1 .
              !End of CapeSoft MessageBox EndCode
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('ds_Message', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
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
    ReturnValue = ThisMessageBox.SavedResponse           !CapeSoft MessageBox Code - Preserves the GlobalResponse
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
  ThisMessageBox.TakeEvent()                             !CapeSoft MessageBox Code
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
    NYS:DockingPane_EventMgr.TakeWindowEvent(window)
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
