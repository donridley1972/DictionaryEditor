

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR011.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
BulkActionsWindow PROCEDURE (string pParams,string pThread)

ParamsGrp                   Group(ParamsGroupType).
OverStr                     String(Size(ParamsGrp)),Over(ParamsGrp)
!MyDct				        dwrDctParser
json                        JSONClass
QNdx                        Long
MyQ                         dwrMyQueue
FldNum                      Long
Fld                         ANY

local                       CLASS
AddGlobalPathFields         Procedure()
AddGuidToTables             Procedure()
                            End

TablesView                  View(Tables)
                            End
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
NotifyParam          STRING(1500)                          ! 
UseExistingGlobals   LONG                                  ! 
NewFieldLength       STRING(5)                             ! 
GlobalsQ             QUEUE,PRE(GloQ)                       ! 
TableName            STRING(50)                            ! 
TableGuid            STRING(16)                            ! 
                     END                                   ! 
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
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DonutHoleWindow     Class(NinjaDonutHoleWindowClass)
Repaint               PROCEDURE(),DERIVED
                    end
BRW7::View:Browse    VIEW(DriversLkUp)
                       PROJECT(Dri:Driver)
                       PROJECT(Dri:GUID)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
Dri:Driver             LIKE(Dri:Driver)               !List box control field - type derived from field
Dri:GUID               LIKE(Dri:GUID)                 !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
Window               WINDOW('Caption'),AT(,,449,265),FONT('Segoe UI',10),RESIZE,AUTO,GRAY,MAX,SYSTEM,WALLPAPER('Background1.png'),IMM
                       SHEET,AT(2,2,445,262),USE(?SHEET1),ABOVE
                         TAB('General'),USE(?GeneralTab),ICON('Settings9.ico')
                           LIST,AT(9,21,150,10),USE(?List),LEFT(2),DROP(20),FORMAT('80L(2)|M~Driver~@s20@'),FROM(Queue:Browse), |
  IMM
                           BUTTON('Change File Drivers'),AT(167,21,,11),USE(?ChangeDriversBtn),LEFT,ICON('DataFiles.ico'), |
  FLAT
                           BUTTON('Add Global Path Name Field for Each Table'),AT(9,38),USE(?AddGloPathNameVarBtn),FLAT
                           LIST,AT(60,56,111,10),USE(?LIST1),DROP(10),FORMAT('100L(2)M@s50@'),FROM(GlobalsQ),HIDE
                           OPTION('Create New Globals Table or Use Existing?'),AT(14,140,163,32),USE(UseExistingGlobals), |
  BOXED,HIDE,TRN
                             RADIO('New'),AT(47,154),USE(?OPTION1:RADIO1),TRN,VALUE('1')
                             RADIO('Existing'),AT(97,154),USE(?USEEXISTINGGLOBALS:RADIO1),TRN,VALUE('2')
                           END
                           ENTRY(@s5),AT(141,70,31,10),USE(NewFieldLength),LEFT(2),HIDE
                           PROMPT('New Field Length:'),AT(9,70),USE(?NewFieldLength:Prompt),HIDE,TRN
                           PROMPT('Add Fields To:'),AT(9,56),USE(?AddFieldsToPrompt),HIDE,TRN
                           BUTTON('Add Fields'),AT(107,84,,11),USE(?AddGloPathNamesBtn),LEFT,ICON('AddNew.ico'),FLAT, |
  HIDE
                           BUTTON('Add GUID to Tables'),AT(195,38,163),USE(?AddGuidToTables),FLAT
                           BUTTON('Set Path to "Table Name" + .tps'),AT(195,55,163),USE(?TableNamePlusTps),FLAT
                         END
                         TAB('FM3'),USE(?Fm3Tab),ICON('fm3.ico')
                           BUTTON('Add FM3 Version Option'),AT(14,26,124,26),USE(?AddFm3VersionBtn),LEFT,ICON('DataSetVersion.ico'), |
  DISABLE,FLAT
                           BUTTON('Increment FM3 Version'),AT(14,55,124,26),USE(?IncrementFm3VersionBtn),LEFT,ICON('DataVersio' & |
  'nIncrement.ico'),DISABLE,FLAT
                         END
                         TAB('SQL'),USE(?SqlTab),ICON('DataFiles.ico')
                         END
                       END
                     END

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

BulkActionsWindow:mhResize:WM CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END
BRW7                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
TakeNewSelection       PROCEDURE(),DERIVED
                     END

! ----- MyDct --------------------------------------------------------------------------
MyDct                Class(dwrDctParser)
                     End  ! MyDct
! ----- end MyDct -----------------------------------------------------------------------

  CODE
? DEBUGHOOK(DriversLkUp:Record)
? DEBUGHOOK(Fields:Record)
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
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitHelper(TemplateHelper)
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    OverStr = pParams  
  GlobalErrors.SetProcedureName('BulkActionsWindow')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('BulkActionsWindow',pThread)
    NotifyHandler.AddNotifyCode(1)
  MyDct.UpdateFilesQueue('Tables',Tables)
  MyDct.UpdateFilesQueue('Fields',Fields)
  Relate:DriversLkUp.Open()                                ! File DriversLkUp used by this procedure, so make sure it's RelationManager is open
  Relate:Fields.SetOpenRelated()
  Relate:Fields.Open()                                     ! File Fields used by this procedure, so make sure it's RelationManager is open
  Access:Tables.UseFile()                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  BRW7.Init(?List,Queue:Browse.ViewPosition,BRW7::View:Browse,Queue:Browse,Relate:DriversLkUp,SELF) ! Initialize the browse manager
  
    Select(?GeneralTab)
  
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('BulkActionsWindow', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitPrepare(nysInit_PrepareNoOCXCtrl)
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.Init(DockingPaneWndMgr)
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitTemplateSettings()
  NYS:DockingPane_WndExt.InitComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.WindowPrepare('BulkActionsWindow')
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?SHEET1{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
  Do DefineListboxStyle
    NotifyHandler.InitWindow()
  mhResize.Init
  BRW7.Q &= Queue:Browse
  BRW7.AddSortOrder(,)                                     ! Add the sort order for  for sort order 1
  BRW7.AddField(Dri:Driver,BRW7.Q.Dri:Driver)              ! Field Dri:Driver is a hot field or requires assignment from browse
  BRW7.AddField(Dri:GUID,BRW7.Q.Dri:GUID)                  ! Field Dri:GUID is a hot field or requires assignment from browse
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.InitResize()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  IF NYS:DockingPane_WndExt.Active <> TRUE
  INIMgr.Fetch('BulkActionsWindow',Window)                 ! Restore window settings from non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  BRW7.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  DonutHoleWindow.SetLogPreamble('BulkActionsWindow')
  DonutHoleWindow.init(DonutHoleRegistrator.IDonutHoleRegistration,thread(),Window)
  SELF.AddItem(DonutHoleWindow.WindowComponent)
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
    MyDct.Trace('')
    MyDct.Trace('BulkActionsWindow')
    MyDct.Trace('<9>ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
    MyDct.Trace('<9>ParamsGrp.TableGuid = ' & ParamsGrp.TableGuid)
    MyDct.Trace('<9>ParamsGrp.Order = ' & ParamsGrp.Order)
    MyDct.Trace('')
    MyDct.Trace('')
    
    
  
    !FldNum = MyQ.GetQueueFldNum(ParamsGrp.TablesQRef,'ItemType',mq:label)
    Loop QNdx = 1 to Records(ParamsGrp.TablesQRef)
        Get(ParamsGrp.TablesQRef,QNdx)
        If MyQ.GetValue(ParamsGrp.TablesQRef,'ItemType',mq:label) = 'Global' And MyQ.GetValue(ParamsGrp.TablesQRef,'Level',mq:label) > 0
            GlobalsQ.TableGuid = MyQ.GetValue(ParamsGrp.TablesQRef,'ItemGuid',mq:label)
            GlobalsQ.TableName = MyQ.GetValue(ParamsGrp.TablesQRef,'DisplayStr',mq:label)
            Add(GlobalsQ)
        End
        MyDct.Trace(MyQ.GetValue(ParamsGrp.TablesQRef,'ItemType',mq:label))
        !Fld &= WHAT(ParamsGrp.TablesQRef,FldNum)
        !MyDct.Trace(Fld)
        !If Fld = 'Global' and MyQ.GetQueueFieldValue(ParamsGrp.TablesQRef,'ItemGuid','Global','ItemGuid',mq:label)
        !    GlobalsQ.TableGuid = MyQ.GetQueueFieldValue(ParamsGrp.TablesQRef,'ItemGuid','Global','ItemGuid',mq:label)
        !    GlobalsQ.TableName = MyQ.GetQueueFieldValue(ParamsGrp.TablesQRef,'ItemGuid','Global','DisplayStr',mq:label)
        !    Add(GlobalsQ)
        !End
  
        !MyDct.Trace(MyQ.GetQueueFieldValue(ParamsGrp.TablesQRef,'ItemGuid','Globals','ItemType',mq:label))
        !If ParamsGrp.TablesQRef.ItemType = 'Global'
  
        !End
    End
  
  !?LIST1{PROP:From} = ParamsGrp.TablesQRef  
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.WindowInit()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('BulkActionsWindow', THREAD())
  !---- Noyantis : Template Helper - End ----
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:DriversLkUp.Close()
    Relate:Fields.Close()
  END
    NotifyManager.DeleteProcedure('BulkActionsWindow',pThread)
  mhResize.Kill
  IF SELF.Opened
    INIMgr.Update('BulkActionsWindow',Window)              ! Save window data to non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_WndExt.WindowClosed()
  NYS:DockingPane_WndExt.Kill()
  NYS:DockingPane_WndExt.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
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
    OF ?ChangeDriversBtn
      ParamsGrp.SelectedDriver = Dri:Driver
      MyDct.SendNotifyJson(NotifyManager,'Main3','','ChangeDrivers',OverStr) 
      !?ChangeDriversBtn{PROP:Disable} = True        
    OF ?AddGloPathNameVarBtn
      UNHIDE(?AddFieldsToPrompt)
      UNHIDE(?LIST1)
      !UNHIDE(?NewFieldLength:Prompt)
      !UNHIDE(?NewFieldLength)
      !MyDct.SendNotifyJson(NotifyManager,'Main3','','AddGloPathNames',OverStr)      
    OF ?AddFm3VersionBtn
      !ParamsGrp.Action = 'AddFm3Version'
      MyDct.SendNotifyJson(NotifyManager,'Main3','','AddFm3Version',OverStr)      
    OF ?IncrementFm3VersionBtn
      MyDct.SendNotifyJson(NotifyManager,'Main3','','IncrementFm3Version',OverStr)       
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?LIST1
      Get(GlobalsQ,Choice(?LIST1))
      ParamsGrp.TableGuid = GlobalsQ.TableGuid  
      
      UNHIDE(?NewFieldLength:Prompt)
      UNHIDE(?NewFieldLength)
      ?AddGloPathNamesBtn{PROP:Hide} = FALSE    
      Display(?AddGloPathNamesBtn) 
    OF ?NewFieldLength
      !UNHIDE(?AddGloPathNamesBtn) 
      !?AddGloPathNamesBtn{PROP:Hide} = FALSE
      !Display(?AddGloPathNamesBtn)     
    OF ?AddGloPathNamesBtn
      ThisWindow.Update()
        ParamsGrp.FieldSize = NewFieldLength
        !MyDct.SendNotifyJson(NotifyManager,'Main3','','AddGloPathNames',OverStr)   
      local.AddGlobalPathFields()   
    OF ?AddGuidToTables
      ThisWindow.Update()
      local.AddGuidToTables()      
    OF ?TableNamePlusTps
      ThisWindow.Update()
      MyDct.SendNotifyJson(NotifyManager,'Main3','','TableNameTpsPath',OverStr)      
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
    CASE EVENT()
    Of event:DonutHoleDisconnected
        MyDct.SendNotifyJson(NotifyManager,'Main3','BulkActionsWindow','BulkDone',OverStr) 
    End  
  ReturnValue = PARENT.TakeEvent()
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_WndExt.TakeEvent(EVENT())
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeEvent(EVENT())
    IF NYS:DockingPane_EventMgr.TakeEventCycleRequired = TRUE THEN CYCLE.
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
    NYS:DockingPane_WndExt.TakeWindowEvent(Window)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeWindowEvent(Window)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

local.AddGlobalPathFields         Procedure()
st          StringTheory
    CODE
    Open(TablesView)
    TablesView{PROP:Filter} = '(Tab:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (Tab:Usage = <39><39>)'
    Set(TablesView)
    LOOP
        Next(TablesView)
        If Errorcode(); Break END
        MyDct.Trace(Clip(Tab:TableName))
        st.SetValue(Tab:TableName,True)
        st.Append('FilePath')
        MyDct.InsertField(Fields,GlobalsQ.TableGuid,st.GetValue(),'STRING',NewFieldLength)
        
    End
    Close(TablesView)
    MyDct.ResetOrder(Fields,'FKFldParentGuidAndOrderKey','ParentGUID',GlobalsQ.TableGuid,False)
    MyDct.SendNotifyJson(NotifyManager,'Main3','BulkActionsWindow','BulkDone',OverStr) 
    !MyDct.InsertField(Fields,GlobalsQ.TableGuid,'','STRING',NewFieldLength)
    !Procedure(*FILE pTable,string pTableGuid,string pName,<string pDataType>,<string pSize>)

local.AddGuidToTables             Procedure()
TableGuid       String(16)
    CODE
    Open(TablesView)
    TablesView{PROP:Filter} = '(Tab:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ') and (Tab:Usage = <39><39>)'
    Set(TablesView)
    LOOP
        Next(TablesView)
        If Errorcode(); Break END
        If Not TableGuid
            TableGuid = Tab:PKGuid
        End
        MyDct.Trace(Clip(Tab:TableName))
        MyDct.InsertGuidField(Fields,Tab:PKGuid,'GUID','STRING',16)
        MyDct.ResetOrder(Fields,'FKFldParentGuidAndOrderKey','ParentGUID',Tab:PKGuid,False)
    End
    Close(TablesView)
    
    MyDct.SendNotifyJson(NotifyManager,'Main3','BulkActionsWindow','BulkDone',OverStr)
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
  IF SELF.DragDropCtrl <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.DragDropCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  IF SELF.OCXBox       <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.OCXBox,       SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  IF SELF.ParentGroup  <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.ParentGroup,  SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  IF SELF.ParentRegion <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.ParentRegion, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  IF SELF.ResizeBox    <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.ResizeBox,    SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  CLEAR(SELF.mhResize)
  SELF.mhResize.Active = TRUE
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
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----

mhResize.Init PROCEDURE

  CODE
  SELF.Init(                                               |
      mhResizeFrame,                                       | !Global Host Object
      Window,                                              | !Window
      BulkActionsWindow:mhResize:WM.MH::ResizeIWindowManager, | !Window Manager
      449,                                                 | !Original Width
      265,                                                 | !Original Height
      1,                                                   | !Resize Width?
      1,                                                   | !Resize Height?
      1,                                                   | !Orig Size is Min?
      0,                                                   | !Border Beyond Min?
      0,                                                   | !Maximize Initially?
      1)                                                     !Do NOT hide+unhide during opening operations?
  SELF.AddControl(?SHEET1,0,0,1,1)
  RETURN
  PARENT.Init

mhResize.InitialResize PROCEDURE()

MH::RestoreFromIni   BYTE,AUTO

  CODE
  IF NOT SELF.IsInitialResizeDone
    MH::RestoreFromIni = CHOOSE(INIMgr.TryFetch('BulkActionsWindow','XPos') OR INIMgr.TryFetch('BulkActionsWindow','Maximize'))
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


BulkActionsWindow:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
  CODE
  ThisWindow.Reset(Force)


BRW7.TakeNewSelection PROCEDURE

  CODE
  PARENT.TakeNewSelection
  !  ?ChangeDriversBtn{PROP:Disable} = False  

NotifyHandler.HandleNotify       PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter)
us    UltimateString
TempSt          StringTheory
XmlSt           StringTheory
    CODE    
    NotifyParam = NotifyManager.GetNotifyParm(NotifyParameter)
    MyDct.Trace('')
    MyDct.Trace('BulkActionsWindow - NotifyHandler.HandleNotify')
	If Size(NotifyParam) > 0
		TempSt.SetValue(NotifyParam)
        MyDct.Trace('')
		MyDct.Trace(TempSt.GetValue())
        MyDct.Trace('')
		json.Start()
		json.SetTagCase(jF:CaseAsIs)
		json.Load(ParamsGrp,TempSt)

        If ParamsGrp.TaggedCnt > 0
            ?AddFm3VersionBtn{PROP:Disable} = False
            ?IncrementFm3VersionBtn{PROP:Disable} = False
        ELSE
            ?AddFm3VersionBtn{PROP:Disable} = True
            ?IncrementFm3VersionBtn{PROP:Disable} = True
        End

        If ParamsGrp.Action = ''

        End
        Case ParamsGrp.Action[1:Size(ParamsGrp.Action)]
        Of ''
        END
    End
DonutHoleWindow.Repaint               PROCEDURE()!,DERIVED
  CODE
  PARENT.Repaint()
