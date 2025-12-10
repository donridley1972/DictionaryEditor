

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR022.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
TableTypesTree PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
MyDct               dwrDctParser
st                  StringTheory

ListMarker          JS_ListMark
MarkCount           LONG



local                           CLASS
PopulateDwrTree                 Procedure()
ParseMarked                     Procedure()
                                End
DwrTreeQ               QUEUE,PRE(Tree5)
DisplayStr      STRING(200)
NormalFG        LONG
NormalBG        LONG
SelectedFG      LONG
SelectedBG      LONG
Icon            SHORT
Level           LONG !Closed=-1,Child=2
Loaded          SHORT
Position        STRING(1024)
WinMark         BYTE
RealMark        BYTE
ItemType        String(20)
ItemGuid        String(16)
Ndx             Long
                END
TablePoolsView5    View(TablePools)
                    Project(TabPool:PKGuid)
                    Project(TabPool:TableTypeGuid)
                    Project(TabPool:ParentGUID)
                    Project(TabPool:DictionaryGuid)
                    Project(TabPool:DctxOrder)
                    Project(TabPool:Guid)
                    Project(TabPool:Ident)
                    Project(TabPool:Usage)
                    Project(TabPool:TableName)
                    Project(TabPool:Description)
                    Project(TabPool:TablePrefix)
                    Project(TabPool:TableDriver)
                    Project(TabPool:DriverOption)
                    Project(TabPool:Owner)
                    Project(TabPool:TablePath)
                    Project(TabPool:Create)
                    Project(TabPool:Reclaim)
                    Project(TabPool:Encrypt)
                    Project(TabPool:OEM)
                    Project(TabPool:Thread)
                    Project(TabPool:Bindable)
                    Project(TabPool:AuditBlob)
                    Project(TabPool:CommentsBlob)
                    Project(TabPool:OptionsBlob)
                    Project(TabPool:SqlCreateBlob)
                End ! TablePoolsView5
TableGlobalsView5    View(TableGlobals)
                    Project(TabGlo:PKGuid)
                    Project(TabGlo:TableTypeGuid)
                    Project(TabGlo:ParentGUID)
                    Project(TabGlo:DictionaryGuid)
                    Project(TabGlo:DctxOrder)
                    Project(TabGlo:Guid)
                    Project(TabGlo:Ident)
                    Project(TabGlo:Usage)
                    Project(TabGlo:TableName)
                    Project(TabGlo:Description)
                    Project(TabGlo:TablePrefix)
                    Project(TabGlo:TableDriver)
                    Project(TabGlo:DriverOption)
                    Project(TabGlo:Owner)
                    Project(TabGlo:TablePath)
                    Project(TabGlo:Create)
                    Project(TabGlo:Reclaim)
                    Project(TabGlo:Encrypt)
                    Project(TabGlo:OEM)
                    Project(TabGlo:Thread)
                    Project(TabGlo:Bindable)
                    Project(TabGlo:AuditBlob)
                    Project(TabGlo:CommentsBlob)
                    Project(TabGlo:OptionsBlob)
                    Project(TabGlo:SqlCreateBlob)
                End ! TableGlobalsView5
TablesView5    View(Tables)
                    Project(Tab:PKGuid)
                    Project(Tab:TableTypeGuid)
                    Project(Tab:ParentGUID)
                    Project(Tab:DictionaryGuid)
                    Project(Tab:DctxOrder)
                    Project(Tab:Guid)
                    Project(Tab:Ident)
                    Project(Tab:Usage)
                    Project(Tab:TableName)
                    Project(Tab:Description)
                    Project(Tab:TablePrefix)
                    Project(Tab:TableDriver)
                    Project(Tab:DriverOption)
                    Project(Tab:Owner)
                    Project(Tab:TablePath)
                    Project(Tab:Create)
                    Project(Tab:Reclaim)
                    Project(Tab:Encrypt)
                    Project(Tab:OEM)
                    Project(Tab:Thread)
                    Project(Tab:Bindable)
                    Project(Tab:AuditBlob)
                    Project(Tab:CommentsBlob)
                    Project(Tab:OptionsBlob)
                    Project(Tab:SqlCreateBlob)
                End ! TablesView5
    
DisplayString        STRING(255)                           ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
Window               WINDOW('Caption'),AT(,,455,302),FONT('Segoe UI',9),AUTO,GRAY,SYSTEM
                       BUTTON('Close'),AT(63,250),USE(?Close)
                       LIST,AT(2,2,158,237),USE(?DwrTreeList),HVSCROLL,FORMAT('100LM*IT~Display Str~@s200@#1#'),FROM(DwrTreeQ), |
  MARK(DwrTreeQ.RealMark)
                       BUTTON('Button1'),AT(11,250),USE(?BUTTON1)
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
? DEBUGHOOK(TableGlobals:Record)
? DEBUGHOOK(TablePools:Record)
? DEBUGHOOK(TableTypes:Record)
? DEBUGHOOK(Tables:Record)
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
  
    MyDct.Trace('')
    MyDct.Trace('TableTypesTree')
    MyDct.Trace('   ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
    MyDct.Trace('')
    MyDct.Trace('')
  
    
  GlobalErrors.SetProcedureName('TableTypesTree')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Close
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:TableGlobals.Open()                               ! File TableGlobals used by this procedure, so make sure it's RelationManager is open
  Relate:TablePools.Open()                                 ! File TablePools used by this procedure, so make sure it's RelationManager is open
  Relate:TableTypes.Open()                                 ! File TableTypes used by this procedure, so make sure it's RelationManager is open
  Relate:Tables.SetOpenRelated()
  Relate:Tables.Open()                                     ! File Tables used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('TableTypesTree', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  ?DwrTreeList{prop:lineheight} = 10
  ?DwrTreeList{PROP:iconlist,1} = 'Folder5Large.ico'  
  ?DwrTreeList{PROP:iconlist,2} = 'FolderOpenLarge.ico'  
  ?DwrTreeList{PROP:iconlist,3} = 'Pools.ico'  
  ?DwrTreeList{PROP:iconlist,4} = 'Globals.ico'  
  ?DwrTreeList{PROP:iconlist,5} = 'Tables.ico'  
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    ListMarker.Init(?DwrTreeList,DwrTreeQ,DwrTreeQ.RealMark)
    ListMarker.Behavior = 0  
    MarkCount =  ListMarker.GetMarkCount()  
  
    Open(TablePoolsView5)
    TablePoolsView5{PROP:Order} = 'TabPool:TableName'
    TablePoolsView5{PROP:Filter} = '(TabPool:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (TabPool:Usage = ' & '''' & 'Pool' & '''' & ')'
    Set(TablePoolsView5)
  
    Open(TableGlobalsView5)
    TableGlobalsView5{PROP:Order} = 'TabGlo:TableName'
    TableGlobalsView5{PROP:Filter} = '(TabGlo:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (TabGlo:Usage = ' & '''' & 'Global' & '''' & ')'
    Set(TableGlobalsView5)
  
    Open(TablesView5)
    TablesView5{PROP:Order} = 'Tab:TableName'
    TablesView5{PROP:Filter} = '(Tab:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (Tab:Usage = ' & '''' & '' & '''' & ')'
    Set(TablesView5)    
  
    local.PopulateDwrTree()
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
  WinAlertMouseZoom()
  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
  INIMgr.Fetch('TableTypesTree',Window)                    ! Restore window settings from non-volatile store
  !
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('TableTypesTree', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:TableGlobals.Close()
    Relate:TablePools.Close()
    Relate:TableTypes.Close()
    Relate:Tables.Close()
  END
  IF SELF.Opened
    INIMgr.Update('TableTypesTree',Window)                 ! Save window data to non-volatile store
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
    OF ?BUTTON1
      local.ParseMarked()      
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
    OF EVENT:GainFocus
      !MyDct.Trace(REL5::Position)      
    OF EVENT:OpenWindow
        post(event:visibleondesktop)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
local.PopulateDwrTree                 Procedure()
    CODE
    Free(DwrTreeQ)

    DwrTreeQ.DisplayStr = 'Pools'
    DwrTreeQ.NormalFG = COLOR:Black
    DwrTreeQ.NormalBG = COLOR:White
    DwrTreeQ.SelectedFG = COLOR:Black
    DwrTreeQ.SelectedBG = COLOR:White
    DwrTreeQ.Icon = 2
    DwrTreeQ.Level = 0           !Closed=-1,Child=2
    DwrTreeQ.Ndx = Records(DwrTreeQ)+1
    Add(DwrTreeQ)  

    LOOP
        Next(TablePoolsView5)
        If Errorcode(); Break END
        DwrTreeQ.DisplayStr = TabPool:TableName
        DwrTreeQ.NormalFG = COLOR:Black
        DwrTreeQ.NormalBG = COLOR:White
        DwrTreeQ.SelectedFG = COLOR:White
        DwrTreeQ.SelectedBG = COLOR:Blue
        DwrTreeQ.Icon = 3
        DwrTreeQ.Level = 2           !Closed=-1,Child=2
        DwrTreeQ.ItemType = 'Pool'
        DwrTreeQ.ItemGuid = TabPool:PKGuid
        DwrTreeQ.Ndx = Records(DwrTreeQ)+1
        Add(DwrTreeQ)  
    End

    DwrTreeQ.DisplayStr = 'Globals'
    DwrTreeQ.NormalFG = COLOR:Black
    DwrTreeQ.NormalBG = COLOR:White
    DwrTreeQ.SelectedFG = COLOR:Black
    DwrTreeQ.SelectedBG = COLOR:White
    DwrTreeQ.Icon = 2
    DwrTreeQ.Level = 0           !Closed=-1,Child=2
    DwrTreeQ.Ndx = Records(DwrTreeQ)+1
    Add(DwrTreeQ)  

    LOOP
        Next(TableGlobalsView5)
        If Errorcode(); Break END
        DwrTreeQ.DisplayStr = TabGlo:TableName
        DwrTreeQ.NormalFG = COLOR:Black
        DwrTreeQ.NormalBG = COLOR:White
        DwrTreeQ.SelectedFG = COLOR:White
        DwrTreeQ.SelectedBG = COLOR:Blue
        DwrTreeQ.Icon = 4
        DwrTreeQ.Level = 2           !Closed=-1,Child=2
        DwrTreeQ.ItemType = 'Global'
        DwrTreeQ.ItemGuid = TabGlo:PKGuid
        DwrTreeQ.Ndx = Records(DwrTreeQ)+1
        Add(DwrTreeQ)  
    End

    DwrTreeQ.DisplayStr = 'Tables'
    DwrTreeQ.NormalFG = COLOR:Black
    DwrTreeQ.NormalBG = COLOR:White
    DwrTreeQ.SelectedFG = COLOR:Black
    DwrTreeQ.SelectedBG = COLOR:White
    DwrTreeQ.Icon = 2
    DwrTreeQ.Level = 0           !Closed=-1,Child=2
    DwrTreeQ.Ndx = Records(DwrTreeQ)+1
    Add(DwrTreeQ)  

    LOOP
        Next(TablesView5)
        If Errorcode(); Break END
        DwrTreeQ.DisplayStr = Tab:TableName
        DwrTreeQ.NormalFG = COLOR:Black
        DwrTreeQ.NormalBG = COLOR:White
        DwrTreeQ.SelectedFG = COLOR:White
        DwrTreeQ.SelectedBG = COLOR:Blue
        DwrTreeQ.Icon = 5
        DwrTreeQ.Level = 2           !Closed=-1,Child=2
        DwrTreeQ.ItemType = 'Table'
        DwrTreeQ.ItemGuid = Tab:PKGuid
        DwrTreeQ.Ndx = Records(DwrTreeQ)+1
        Add(DwrTreeQ)  
    End

local.ParseMarked                     Procedure()   
x       Long
    CODE
    !Sort(DwrTreeQ,-DwrTreeQ.RealMark)
    Loop x = 1 to Records(DwrTreeQ)
        Get(DwrTreeQ,x)
        !If DwrTreeQ.RealMark = 0; Break End
        If DwrTreeQ.RealMark = 1
            ListMarker.Trace(x & '<9>' & Clip(DwrTreeQ.DisplayStr) & '<9>' & DwrTreeQ.RealMark)
        ELSE
            Cycle
        End
    End
    !Sort(DwrTreeQ,DwrTreeQ.Ndx) 
