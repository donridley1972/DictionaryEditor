

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR017.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
UpdateOption PROCEDURE (string pParams,string pThread)

ParamsGrp               Group(ParamsGroupType).
OverStr                 String(Size(ParamsGrp)),Over(ParamsGrp)
json				    JSONClass
st					    StringTheory
MyDct				    dwrDctParser

OptionStartValue        String(255)
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
NotifyParam          STRING(1000)                          ! 
PropertyType         STRING(1)                             ! 
OptionLabel          STRING(100)                           ! 
OptionValue          STRING(100)                           ! 
OptionGrp            GROUP,PRE(),NAME('Option')            ! 
Id                   LONG,NAME('Id | Private')             ! 
Property             STRING(100),NAME('Property | attribute') ! 
PropertyType         STRING(5),NAME('PropertyType | attribute') ! 
PropertyValue        STRING(100),NAME('PropertyValue | attribute') ! 
                     END                                   ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
Window               WINDOW('Option Properties'),AT(,,211,142),FONT('Segoe UI',10),AUTO,ICON('Dictionary.ico'), |
  GRAY,SYSTEM,WALLPAPER('Background1.png'),IMM
                       SHEET,AT(2,2,206,119),USE(?SHEET1)
                         TAB('General'),USE(?GeneralTab)
                           OPTION('Type'),AT(9,24,194,43),USE(PropertyType),BOXED,TRN
                             RADIO(' String'),AT(20,39),USE(?OPTION1:RADIO1),TRN,VALUE('1')
                             RADIO(' Boolean'),AT(135,39),USE(?PROPERTYTYPE:RADIO1),TRN,VALUE('2')
                             RADIO(' Number'),AT(20,51),USE(?PROPERTYTYPE:RADIO2),TRN,VALUE('3')
                             RADIO(' Predefined'),AT(135,51),USE(?PROPERTYTYPE:RADIO3),TRN,VALUE('4')
                           END
                           PROMPT('Label:'),AT(9,76),USE(?OptionLabel:Prompt),TRN
                           ENTRY(@s100),AT(33,75,170,10),USE(OptionLabel),LEFT(2)
                           OPTION('option2'),AT(33,88,122,27),USE(OptionValue,,?OptionValue:2),HIDE,TRN
                             RADIO(' True'),AT(43,96),USE(?OPTION1:RADIO2),TRN,VALUE('1')
                             RADIO(' False'),AT(110,96),USE(?OPTIONVALUE),TRN,VALUE('0')
                           END
                           PROMPT('Value:'),AT(9,96),USE(?OptionValue:Prompt),TRN
                           ENTRY(@s100),AT(34,96,170,10),USE(OptionValue,,?OptionValue:3),LEFT(2)
                         END
                         TAB('Comments'),USE(?CommentsTab)
                         END
                       END
                       BUTTON('Cancel'),AT(176,124,32),USE(?Cancel),FLAT
                       BUTTON('OK'),AT(141,124),USE(?OkBtn),FLAT
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

  CODE
? DEBUGHOOK(Dictionaries:Record)
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
  GlobalErrors.SetProcedureName('UpdateOption')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?PropertyType
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('UpdateOption',pThread)
    NotifyHandler.AddNotifyCode(1)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Dictionaries.SetOpenRelated()
  Relate:Dictionaries.Open()                               ! File Dictionaries used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('UpdateOption', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?SHEET1{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
    json.Start()
    json.SetTagCase(jF:CaseAsIs)
    json.Save(ParamsGrp,st)  
    MyDct.Trace('')
    MyDct.Trace('UpdateOption - SELF.Open(Window)')
    MyDct.Trace('')
    MyDct.Trace(st.GetValue())
    MyDct.Trace('')
  
  
  
    Case ParamsGrp.Action[1:Size(ParamsGrp.Action)]
    OF 'InsertOption'
    OF 'EditOption'
        st.SetValue(ParamsGrp.OptionJson)
        json.Start()
        json.SetTagCase(jF:CaseAsIs)
        json.Load(OptionGrp,st)
        OptionLabel = OptionGrp.Property
        PropertyType = OptionGrp.PropertyType
        OptionValue = OptionGrp.PropertyValue
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
  INIMgr.Fetch('UpdateOption',Window)                      ! Restore window settings from non-volatile store
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('UpdateOption', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Dictionaries.Close()
  END
    NotifyManager.DeleteProcedure('UpdateOption',pThread)
  IF SELF.Opened
    INIMgr.Update('UpdateOption',Window)                   ! Save window data to non-volatile store
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
    OF ?PropertyType
      Case PropertyType
      Of 1
        ?OptionValue:3{PROP:Hide} = False
        ?OptionValue:2{PROP:Hide} = True
      Of 2
        ?OptionValue:3{PROP:Hide} = True
        ?OptionValue:2{PROP:Hide} = False
      Of 3
        ?OptionValue:3{PROP:Hide} = False
        ?OptionValue:2{PROP:Hide} = True
      Of 4
        ?OptionValue:3{PROP:Hide} = False
        ?OptionValue:2{PROP:Hide} = True
      End
    OF ?OkBtn
      st.Start()
      OptionGrp.Property = OptionLabel
      OptionGrp.PropertyType = PropertyType
      OptionGrp.PropertyValue = OptionValue
      json.Start()
      json.SetTagCase(jF:CaseAsIs)
      json.Save(OptionGrp,st)
      MyDct.Trace('')
      MyDct.Trace('UpdateOption - OF ?OkBtn')
      MyDct.Trace('')
      MyDct.Trace(st.GetValue())
      MyDct.Trace('')
      ParamsGrp.OptionJson = st.GetValue()
      
      st.Start()
      json.Start()
      json.SetTagCase(jF:CaseAsIs)
      json.Save(ParamsGrp,st)
      ParamsGrp.Action = 'EditOption'
      NotifyManager.NotifyProcedure(ParamsGrp.ProcedureToNotify[1:Size(ParamsGrp.ProcedureToNotify)],st.GetValue())  
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
