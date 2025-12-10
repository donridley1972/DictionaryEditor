

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR001.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('DICTIONARYEDITOR024.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
BuildDctx2 PROCEDURE (string pParams,string pThread)

ParamsGrp               Group(ParamsGroupType).
OverStr                 String(Size(ParamsGrp)),Over(ParamsGrp)

xml                     xFilesTree
mt                      MyTableClass
MyDct                   dwrDctParser
xml                     xFilesTree

!TotalRecs               Long
!RecCnt                  Long
!ThisProgress            Long

!TableChanged            Long
!TableCnt                Long
FieldCnt				Long

VersionRec              &Group
!OptionsRec              &Group
TableRec                &Group
FieldRec                &Group
KeyRec                  &Group
AliasRec                &Group
TriggerRec              &Group
RelationRec             &Group

st                      StringTheory

TableSt                 StringTheory
TableRowSt				StringTheory
FieldRowSt				StringTheory
FieldSt                 StringTheory
KeyRowSt				StringTheory
KeySt                   StringTheory
TriggerRowSt			StringTheory
TriggerSt				StringTheory
AliasRowSt				StringTheory
AliasSt					StringTheory

TempSt					StringTheory	! Delete when done
CommentSt				StringTheory


NodeSt                  StringTheory
MasterSt                StringTheory
EndTagSt                StringTheory

!TableGrp				Group(TableGroupType).
FieldsGrp               Group(FieldGroupType).
KeyGrp					Group(KeyGroupType).
!TriggerGrp				Group(TriggerGroupType).
RelationGrp				Group(RelationGroupType).

MyState					Long
MyState:Ready			Equate(1)

!saveTableGuid           Like(Tab:PKGuid)
!saveFieldParentGuid     Like(Fld:ParentGUID)
!saveFieldGuid           Like(Fld:PKGuid)
!saveKeyGuid             Like(Key:PKGuid)
!saveTriggerGuid         Like(Tri:PKGuid)
!saveAliasGuid			Like(Ali:PKGuid)

table             &File
!rec               &Group
!grp               &Group
str               StringTheory
!TablesInView      long
!Level             Long
!Children          Byte,dim(100)
!x                 long
!y                 long
!ParentId          long
!TableFieldNumber  long
!ViewFieldNumber   long
!WasLevel          long

!TableQueue          Queue(xFilesTableQueueType).

!BeenUsedQ			Queue
!Guid				String(16)
!					End


local                       CLASS       
BuildDctx					Procedure()
AddToFields                 Procedure(string pVal,string pDataType,*byte pLastInGroup)
AddToKeys					Procedure(string pVal)
AddToTriggers				Procedure(string pVal)
AddToAliases				Procedure(string pVal)
InsertUpdateQ				Procedure(string pGuid,string pStructName,string pStructType)
InQueue						Procedure(string pGuid),Long
CreateTableNode				Procedure()
                            End
RecordCnt            LONG                                  ! 
DisplayStr           STRING(50)                            ! 
TotalRecs            LONG                                  ! 
RecCnt               LONG                                  ! 
ThisProgress         LONG                                  ! 
LoopStartTime        LONG                                  ! 
LoopEndTime          LONG                                  ! 
LoopElapsedTime      LONG                                  ! 
BeenUsedQ            QUEUE,PRE()                           ! 
Guid                 STRING(16)                            ! 
StructureName        STRING(100)                           ! 
StructureType        STRING(20)                            ! 
                     END                                   ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
Window               WINDOW('Build DCTX'),AT(,,226,65),FONT('Segoe UI',9),ICON('Dictionary.ico'),GRAY,SYSTEM,TIMER(1), |
  WALLPAPER('Background1.png'),IMM
                       BUTTON('Close'),AT(191,47),USE(?Close)
                       PROGRESS,AT(2,20,221,11),USE(?PROGRESS1),HIDE,RANGE(0,100),SMOOTH
                       STRING(@n_3),AT(2,34,221,10),USE(ThisProgress),CENTER(1),HIDE,TRN
                       STRING(@s50),AT(2,6,221),USE(DisplayStr),CENTER(2),HIDE,TRN
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
!DictionariesView    View(Dictionaries)
!                    Project(Dct:GUID)
!                    Project(Dct:DctName)
!                    Project(Dct:Version)
!                    Project(Dct:StructureChecked)
!                    Project(Dct:DctxFormat)
!                    End

CommentsView        View(DctComments)
                    Project(Dcom:PKGuid)
                    Project(Dcom:DictionaryGuid)
                    Project(Dcom:Comments)
                    End

OptionsView         View(Options)
                    Project(Opt:PKGuid)
                    Project(Opt:ParentGUID)
                    Project(Opt:DictionaryGuid)
                    Project(Opt:TableGuid)
                    Project(Opt:FieldGuid)
                    Project(Opt:KeyGuid)
                    Project(Opt:RelationGuid)
                    Project(Opt:ParentNode)
                    Project(Opt:Property)
                    Project(Opt:PropertyType)
                    Project(Opt:PropertyValue)
                    End

TablesView          View(Tables)
                    !Project(Tab:PKGuid)
                    !Project(Tab:ParentGUID)
                    !Project(Tab:DictionaryGuid)
                    !Project(Tab:DctxOrder)
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
                    Project(Tab:Encrypt)
                    Project(Tab:Reclaim)
                    Project(Tab:Thread)
                    Project(Tab:Bindable)
                    Project(Tab:AuditBlob)
                    Project(Tab:CommentsBlob)
                    Project(Tab:OptionsBlob)
                    Join(Fld:FKFldParentGuidKey,Tab:PKGuid)	!,INNER
                    !Project(Fld:PKGuid)
                    !Project(Fld:ParentGUID)
                    !Project(Fld:TableGuid)
                    !Project(Fld:FieldOrder)
                    !Project(Fld:LastInGroup)
                    Project(Fld:Guid)
                    Project(Fld:Ident)
                    Project(Fld:FieldName)
                    Project(Fld:DerivedFrom)
                    Project(Fld:Description)
                    Project(Fld:BaseType)
                    Project(Fld:DataType)
                    Project(Fld:Reference)
                    Project(Fld:Thread)
                    Project(Fld:Binary)
                    Project(Fld:FieldSize)
                    Project(Fld:Places)
                    Project(Fld:ScreenPicture)
                    Project(Fld:ScreenPrompt)
                    Project(Fld:ReportHeading)
                    Project(Fld:InitialValue)
                    Project(Fld:Case_)
                    Project(Fld:Over)
                    Project(Fld:Justification)
                    Project(Fld:ReadOnly)
                    Project(Fld:Offset)
                    Project(Fld:ExternalName)
                    Project(Fld:NoPopulate)
                    Project(Fld:VerticalSpace)
                    Project(Fld:HelpId)
                    Project(Fld:Message)
                    Project(Fld:Tooltip)
                    Project(Fld:WindowControl)
                    Project(Fld:ReportControl)
                    Project(Fld:WindowControlBlob)
                    Project(Fld:ReportControlBlob)
                    Project(Fld:AuditBlob)
                    Project(Fld:ValidityBlob)
                    Project(Fld:CommentsBlob)
                    Project(Fld:OptionsBlob)
                    End
                    Join(Key:FKKeyParentGuidKey,Tab:PKGuid)!,INNER
                    !Project(Key:PKGuid)
                    !Project(Key:DictionaryGuid)
                    !Project(Key:ParentGUID)
                    !Project(Key:TableGuid)
                    Project(Key:KeyGuid)
                    Project(Key:Ident)
                    Project(Key:Order)
                    Project(Key:KeyName)
                    Project(Key:ExternalName)
                    Project(Key:Description)
                    Project(Key:KeyType)
                    Project(Key:Unique)
                    Project(Key:AutoNumber)
                    Project(Key:Primary)
                    Project(Key:Exclude)
                    Project(Key:CaseSensitive)
                    Project(Key:AuditBlob)
                    Project(Key:CommentsBlob)
                    Project(Key:OptionsBlob)
                    Project(Key:ComponentsBlob)
                    End
                    Join(Tri:FKTriParentGuidKey,Tab:PKGuid)!,INNER
                    !Project(Tri:PKGuid)
                    !Project(Tri:ParentGUID)
                    !Project(Tri:TableGuid)
                    !Project(Tri:Guid)
                    !Project(Tri:TriggerType)
                    !Project(Tri:Audit)
                    !Project(Tri:createuser)
                    !Project(Tri:createdate)
                    !Project(Tri:createtime)
                    !Project(Tri:createversionnumber)
                    !Project(Tri:modifieduser)
                    !Project(Tri:modifieddate)
                    !Project(Tri:modifiedtime)
                    !Project(Tri:modifiedversionnumber)
                    Project(Tri:CommentsBlob)
                    Project(Tri:Code_)
                    End
                    Join(Ali:FKAliParentGuidKey,Tab:PKGuid)!,INNER
                    !Project(Ali:PKGuid)
                    !Project(Ali:ParentGUID)
                    !Project(Ali:TableGuid)
                    !Project(Ali:AliasGuid)
                    !Project(Ali:AliasName)
                    !Project(Ali:Prefix)
                    !Project(Ali:audit)
                    !Project(Ali:createuser)
                    !Project(Ali:createdate)
                    !Project(Ali:createtime)
                    !Project(Ali:createversionnumber)
                    !Project(Ali:modifieduser)
                    !Project(Ali:modifieddate)
                    !Project(Ali:modifiedtime)
                    !Project(Ali:modifiedversionnumber)
                    End
                    End

DctVersionsView     View(DctVersions)
                    Project(Ver:GUID)
                    Project(Ver:ParentGUID)
                    Project(Ver:DictionaryGuid)
                    Project(Ver:DctVersion)
                    Project(Ver:Description)
                    End

RelationsView       View(Relations)
                    Project(Rel:PKGuid)
                    Project(Rel:ParentGUID)
                    Project(Rel:DctxOrder)
                    Project(Rel:Guid)
                    Project(Rel:PrimaryTable)
                    Project(Rel:ForeignTable)
                    Project(Rel:PrimaryKey)
                    Project(Rel:ForeignKey)
                    Project(Rel:Delete)
                    Project(Rel:Update)
					Project(Rel:AuditBlob)
                    Project(Rel:MappingsBlob)
                    End

!TriggersView        View(Triggers)
!                    Project(Tri:PKGuid)
!                    Project(Tri:ParentGUID)
!                    Project(Tri:TableGuid)
!                    Project(Tri:Guid)
!                    Project(Tri:TriggerType)
!                    Project(Tri:Audit)
!                    Project(Tri:createuser)
!                    Project(Tri:createdate)
!                    Project(Tri:createtime)
!                    Project(Tri:createversionnumber)
!                    Project(Tri:modifieduser)
!                    Project(Tri:modifieddate)
!                    Project(Tri:modifiedtime)
!                    Project(Tri:modifiedversionnumber)
!                    Project(Tri:CommentsBlob)
!                    Project(Tri:Code_)
!                    End

!AliasesView         View(Aliases)
!!                    Project(Ali:PKGuid)
!!                    Project(Ali:ParentGUID)
!!                    Project(Ali:TableGuid)
!!                    Project(Ali:AliasGuid)
!!                    Project(Ali:AliasName)
!!                    Project(Ali:Prefix)
!!                    Project(Ali:audit)
!!                    Project(Ali:createuser)
!!                    Project(Ali:createdate)
!!                    Project(Ali:createtime)
!!                    Project(Ali:createversionnumber)
!!                    Project(Ali:modifieduser)
!!                    Project(Ali:modifieddate)
!!                    Project(Ali:modifiedtime)
!!                    Project(Ali:modifiedversionnumber)
!                    End

  CODE
? DEBUGHOOK(Aliases:Record)
? DEBUGHOOK(DctComments:Record)
? DEBUGHOOK(DctVersions:Record)
? DEBUGHOOK(Dictionaries:Record)
? DEBUGHOOK(Fields:Record)
? DEBUGHOOK(Keys:Record)
? DEBUGHOOK(Options:Record)
? DEBUGHOOK(Relations:Record)
? DEBUGHOOK(Tables:Record)
? DEBUGHOOK(Triggers:Record)
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
  GlobalErrors.SetProcedureName('BuildDctx2')
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
  Relate:Aliases.SetOpenRelated()
  Relate:Aliases.Open()                                    ! File Aliases used by this procedure, so make sure it's RelationManager is open
  Access:Options.UseFile()                                 ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:DctComments.UseFile()                             ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:DctVersions.UseFile()                             ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Dictionaries.UseFile()                            ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Triggers.UseFile()                                ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Relations.UseFile()                               ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Keys.UseFile()                                    ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Fields.UseFile()                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Tables.UseFile()                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('BuildDctx2', THREAD(), Window)
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
  INIMgr.Fetch('BuildDctx2',Window)                        ! Restore window settings from non-volatile store
    TotalRecs = TotalRecordsCount(OverStr,Thread())    
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  	MyState = MyState:Ready   
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('BuildDctx2', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Aliases.Close()
  END
  IF SELF.Opened
    INIMgr.Update('BuildDctx2',Window)                     ! Save window data to non-volatile store
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
  		Case MyState
  		Of MyState:Ready 
  			LoopStartTime = Clock()
  			local.BuildDctx()
  			!0{PROP:Timer} = 0
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
local.BuildDctx						Procedure()
TabSt       StringTheory
TabCnt      Long
	CODE
    0{PROP:Timer} = 0
    ?PROGRESS1{PROP:Progress} = 0

    UNHIDE(?DisplayStr)
    UNHIDE(?PROGRESS1)
    UNHIDE(?ThisProgress)

    !MyDct.Trace('')
    !MyDct.Trace('BuildDctx2 - TotalRecs = ' & TotalRecs)
    !MyDct.Trace('')

	MasterSt.AddLine(Records(MasterSt.lines)+1,'<?xml version="1.0" encoding="UTF-8"?>')
	!MasterSt.SetValue('<?xml version="1.0" encoding="UTF-8"?>')  
    Dct:GUID = ParamsGrp.DictionaryGuid
	
    If Access:Dictionaries.Fetch(Dct:PKDctGUIDKey) = Level:Benign
        st.SetValue(MyDct.GetXmlString(Dictionaries,'Dictionary',False,0))
		MasterSt.AddLine(Records(MasterSt.lines)+1,st.GetValue())
    End

    Open(DctVersionsView)
    DctVersionsView{PROP:Order} = ''
    DctVersionsView{PROP:Filter} = '(Ver:ParentGUID = ' & '''' & Clip(ParamsGrp.DictionaryGuid) & '''' & ')'
    Set(DctVersionsView)
    LOOP
        Next(DctVersionsView)
        If Errorcode(); Break END
        RecCnt+=1
        ThisProgress = ((RecCnt / TotalRecs) * 100)
        !MyDct.Trace('<9>RecCnt = ' & RecCnt & '<9>ThisProgress = ' & ThisProgress)
        ?PROGRESS1{PROP:Progress} = ThisProgress
        Display(?ThisProgress)
        VersionRec &= DctVersions{PROP:Record}
		MasterSt.AddLine(Records(MasterSt.lines)+1,MyDct.XmlGetNode(VersionRec,'DictionaryVersion',False))
    End    
    Close(DctVersionsView)
  
    MasterSt.AddLine(Records(MasterSt.lines)+1,MyDct.XmlGetBlob(Dictionaries,'CommentsBlob',1,'Comment'))
    MasterSt.AddLine(Records(MasterSt.lines)+1,MyDct.XmlGetBlob(Dictionaries,'OptionsBlob',1,'Option'))

	Open(Tables)
	Tab:DictionaryGuid = Clip(ParamsGrp.DictionaryGuid)
	Tab:DctxOrder = 1
	Set(Tab:FKTabDctGuidAndOrderKey,Tab:FKTabDctGuidAndOrderKey)  
	Loop
		Next(Tables)
		If Errorcode(); Break End
        RecCnt+=1
        DisplayStr = 'Working on Table ' & Clip(Tab:TableName)
        ThisProgress = (RecCnt / TotalRecs) * 100
        !MyDct.Trace('<9>RecCnt = ' & RecCnt & '<9>ThisProgress = ' & ThisProgress)
        ?PROGRESS1{PROP:Progress} = ThisProgress
        Display(?DisplayStr)
        Display(?ThisProgress)
		!TableSt.Start()
		FieldSt.Start()
		If Tab:DictionaryGuid <> Clip(ParamsGrp.DictionaryGuid); Cycle End

		TableRec &= Tables{PROP:Record}

		!MyDct.Trace('Tab:TableName = ' & Tab:TableName)

		TableSt.Append(MyDct.XmlGetNode(TableRec,'Table',True)) !,'<32>')
		TableSt.Append(MyDct.XmlGetBlob(Tables,'AuditBlob',2,'Audit')) !,'<32>')
		TableSt.Append(MyDct.XmlGetBlob(Tables,'CommentsBlob',2,'Comment')) !,'<32>')
		TableSt.Append(MyDct.XmlGetBlob(Tables,'OptionsBlob',2,'Option')) !,'<32>')
  		
		CLEAR(Fld:Record)
		FieldCnt = 0
		Fld:ParentGUID = Tab:PKGuid
		Fld:FieldOrder = 1
		SET(Fld:FKFldDctGuidAndOrderKey,Fld:FKFldDctGuidAndOrderKey)
		Loop
			Next(Fields)
			If Errorcode() or Fld:ParentGUID <> Tab:PKGuid 
                Break 
            End
			FieldCnt += 1
            If Fld:DataType = 'UNSIGNED'
                Fld:DataType = ''
            END

			FieldRec &= Fields{PROP:Record}
			FieldsGrp = FieldRec
			FieldSt.Append(MyDct.XmlGetNode(FieldRec,'Field',True)) !,'<13,10>')
			FieldSt.Append(MyDct.XmlGetBlob(Fields,'WindowControlBlob',3,'WindowControl')) !,'<13,10>')
			FieldSt.Append(MyDct.XmlGetBlob(Fields,'ReportControlBlob',3,'ReportControl'))
			FieldSt.Append(MyDct.XmlGetBlob(Fields,'AuditBlob',3,'Audit')) !,'<13,10>')
			FieldSt.Append(MyDct.XmlGetBlob(Fields,'ValidityBlob',3,'Validity')) !,'<13,10>')

			FieldSt.Append(MyDct.XmlGetBlob(Fields,'CommentsBlob',3,'Comment')) !,'<13,10>')
			FieldSt.Append(MyDct.XmlGetBlob(Fields,'OptionsBlob',3,'Option')) !,'<13,10>')
 
  
            If FieldsGrp.NumEndTags
                Loop TabCnt = (FieldsGrp.NumEndTags) to 1 By -1  !TabCnt = 1 to FieldsGrp.NumEndTags
                    FieldSt.Append(MyDct.GetXmlEndTag('Field'))
                End
            End
			TableSt.Append(FieldSt.GetValue()) !,'<13,10>')
			FieldSt.Start()
		End ! Fields Loop
  
		Clear(Key:Record)
		RecordCnt = 0
		Key:ParentGUID = Tab:PKGuid
		Set(Key:FKKeyParentGuidKey,Key:FKKeyParentGuidKey)
		Loop
			Next(Keys)
			If Errorcode() Or Key:ParentGUID <> Tab:PKGuid; Break End
			KeyRec &= Keys{PROP:Record}
			KeyGrp = KeyRec
			RecordCnt += 1
			!MyDct.Trace('BuildDctx - Keys - RecordCnt = ' & RecordCnt)
			KeySt.Append(MyDct.XmlGetNode(KeyGrp,'Key',True)) !,'<13,10>')
			KeySt.Append(MyDct.XmlGetBlob(Keys,'AuditBlob',3,'Audit')) !,'<13,10>')
			KeySt.Append(MyDct.XmlGetBlob(Keys,'CommentsBlob',3,'Comment')) !,'<13,10>')
			KeySt.Append(MyDct.XmlGetBlob(Keys,'OptionsBlob',3,'Option')) !,'<13,10>')

  			If KeyGrp.KeyType <> 'DYNAMIC'
				KeySt.Append(MyDct.XmlGetBlob(Keys,'ComponentsBlob',3,'Component')) !,'<13,10>')  
			End
			KeySt.Append(MyDct.GetXmlEndTag('Key')) !,'<13,10>') 
  			TableSt.Append(KeySt.GetValue()) !,'<13,10>')
			Clear(KeyRec)
			Clear(KeyGrp)
			KeySt.Start()
		End ! Keys Loop

		Clear(Tri:Record)
		Tri:ParentGUID = Tab:PKGuid
		Set(Tri:FKTriParentGuidKey,Tri:FKTriParentGuidKey)
		LOOP
			Next(Triggers)
			If Errorcode() Or Tri:ParentGUID <> Tab:PKGuid; Break END

			TriggerRec &= Triggers{PROP:Record}
			TriggerSt.Append(MyDct.XmlGetNode(TriggerRec,'Trigger',True)) !,'<13,10>')
			TriggerSt.Append(MyDct.XmlGetBlob(Triggers,'CommentsBlob',2,'Comment')) !,'<13,10>')
			TriggerSt.Append(MyDct.XmlGetBlob(Triggers,'Code_',2,'Code')) !,'<13,10>')
			TriggerSt.Append(MyDct.GetXmlEndTag('Trigger')) !,'<13,10>')
  			TableSt.Append(TriggerSt.GetValue()) !,'<13,10>')
			TriggerSt.Start()
			Clear(TriggerRec)
		End ! Triggers Loop
	
		Clear(Ali:Record)
		Ali:ParentGUID = Tab:PKGuid
		Set(Ali:FKAliParentGuidKey,Ali:FKAliParentGuidKey)
		LOOP
			Next(Aliases)
			If Errorcode() Or Ali:ParentGUID <> Tab:PKGuid; Break End
			AliasRec &= Aliases{PROP:Record}
			AliasSt.Append(MyDct.XmlGetNode(AliasRec,'Alias',True)) !,'<13,10>')
            AliasSt.Append(MyDct.XmlGetBlob(Aliases,'AuditBlob',2,'Audit'))
			AliasSt.Append(MyDct.XmlGetBlob(Aliases,'CommentsBlob',2,'Alias')) !,'<13,10>')
			AliasSt.Append(MyDct.GetXmlEndTag('Alias')) !,'<13,10>')
  			TableSt.Append(AliasSt.GetValue()) !,'<13,10>')
			AliasSt.Start()
			Clear(AliasRec)
		End ! Alias Loop
		TableSt.Append(MyDct.GetXmlEndTag('Table')) !,'<13,10>')
		TableSt.Split('<13,10>')
		TableSt.RemoveLines()
		TableSt.Join('<13,10>')
		MasterSt.AddLine(Records(MasterSt.lines)+1,TableSt.GetValue())
		TableSt.Start()
	    Clear(TableRec)
	End
    DisplayStr = 'Working on Relations'
    Display(?DisplayStr)

    MyDct.Trace('')
  	Open(RelationsView)
  	!BUFFER(RelationsView,100,0,20,300)
  	RelationsView{PROP:Order} = ''
  	RelationsView{PROP:Filter} = '(Rel:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
  	!Stream(Relations)
  	Set(RelationsView)
  	Loop
  		Next(RelationsView)
  		If Errorcode(); Break End
        RecCnt+=1
        ThisProgress = (RecCnt / TotalRecs) * 100
        !MyDct.Trace('<9>RecCnt = ' & RecCnt & '<9>ThisProgress = ' & ThisProgress)
        ?PROGRESS1{PROP:Progress} = ThisProgress
        Display(?ThisProgress)

  		RelationRec &= Relations{PROP:Record}
		RelationGrp = RelationRec

        MyDct.Trace(MyDct.XmlGetNode(RelationGrp,'Relation',True))
        MyDct.Trace(MyDct.XmlGetBlob(Relations,'AuditBlob',2,'Audit'))
        MyDct.Trace(MyDct.XmlGetBlob(Relations,'CommentsBlob',2,'Comment'))
        MyDct.Trace(MyDct.XmlGetBlob(Relations,'OptionsBlob',2,'Option'))
        MyDct.Trace(MyDct.XmlGetBlob(Relations,'MappingsBlob',2,'Relation'))
        MyDct.Trace(MyDct.GetXmlEndTag('Relation'))

		MasterSt.AddLine(Records(MasterSt.lines)+1,MyDct.XmlGetNode(RelationGrp,'Relation',True))
		MasterSt.AddLine(Records(MasterSt.lines)+1,MyDct.XmlGetBlob(Relations,'AuditBlob',2,'Audit'))
		MasterSt.AddLine(Records(MasterSt.lines)+1,MyDct.XmlGetBlob(Relations,'CommentsBlob',2,'Comment'))
		MasterSt.AddLine(Records(MasterSt.lines)+1,MyDct.XmlGetBlob(Relations,'OptionsBlob',2,'Option'))
		MasterSt.AddLine(Records(MasterSt.lines)+1,MyDct.XmlGetBlob(Relations,'MappingsBlob',2,'Relation'))
		MasterSt.AddLine(Records(MasterSt.lines)+1,MyDct.GetXmlEndTag('Relation'))
		Clear(RelationRec)
		Clear(RelationGrp)
  	End
  	Close(RelationsView)  

    DisplayStr = 'Done'
    Display(?DisplayStr)
    ?PROGRESS1{PROP:Progress} = 100
    HIDE(?PROGRESS1)
    HIDE(?ThisProgress)

	MasterSt.AddLine(Records(MasterSt.lines)+1,'</Dictionary>')
  	MasterSt.RemoveLines()
  	MasterSt.Join('<13,10>')
  	MasterSt.Split('<13,10>')
  	MasterSt.RemoveLines()
  	MasterSt.Join('<13,10>')
  	MasterSt.Split('<13,10>')
  	MasterSt.RemoveLines()
  	MasterSt.Join('<13,10>')
	!TempSt.RemoveLines()
	!TempSt.Join('<13,10>')
	!TempSt.SaveFile('FieldComments.dctx')
    
    MasterSt.RemoveChars('<13><10><9>')
    !MasterSt.ReplaceBetween('&apos;','&apos;','"','&quot;')
    MasterSt.FormatHTML()
  	MasterSt.SaveFile('XmlOut.dctx')
    LoopEndTime = Clock()
  	LoopElapsedTime = (LoopEndTime - LoopStartTime) + 1
    Post(EVENT:CloseWindow)

local.AddToFields                 	Procedure(string pVal,string pDataType,*byte pLastInGroup)
TempSt			StringTheory
    CODE
	!MyDct.Trace('')
	!MyDct.Trace('<9>local.AddToFields')
	!MyDct.Trace('	pLastInGroup = ' & pLastInGroup)
	TempSt.SetValue(pVal,True)
	TempSt.ReplaceBetween('<','>','<13,10>','<0>')
	TempSt.Remove('<0>')
	FieldSt.AddLine(Records(FieldSt.lines)+1,TempSt.GetValue())
	If Upper(Clip(pDataType)) <> 'GROUP'
		FieldSt.AddLine(Records(FieldSt.lines)+1,MyDct.GetXmlEndTag('Field'))
	End
	If pLastInGroup = TRUE
		FieldSt.AddLine(Records(FieldSt.lines)+1,MyDct.GetXmlEndTag('Field'))
	End
	FieldRowSt.Start()
	If pLastInGroup
		pLastInGroup = 0
    END
	Return

local.AddToKeys						Procedure(string pVal)
TempSt			StringTheory
    CODE
	!MyDct.Trace('<9>local.AddToKeys')
	TempSt.SetValue(pVal,True)
	If TempSt.Length() > 0 and TempSt.Instring('<Key<32>')
		KeySt.AppendA(TempSt.GetValue(),'<13,10>')
		KeySt.AppendA(MyDct.GetXmlEndTag('Key'),'<13,10>')
	End
	Return

local.AddToTriggers				Procedure(string pVal)
TempSt			StringTheory
    CODE
	TempSt.SetValue(pVal,True)
	If TempSt.Length() > 0 and TempSt.Instring('<Trigger<32>')
		TriggerSt.AppendA(TempSt.GetValue(),'<13,10>')
		TriggerSt.AppendA(MyDct.GetXmlEndTag('Trigger'),'<13,10>')
	End
	
	Return

local.AddToAliases				Procedure(string pVal)
TempSt			StringTheory
    CODE
	TempSt.SetValue(pVal,True)
	If TempSt.Length() > 0
		AliasSt.AppendA(TempSt.GetValue(),'<13,10>')
		AliasSt.AppendA(MyDct.GetXmlEndTag('Alias'),'<13,10>')
	End
	Return

local.InsertUpdateQ				Procedure(string pGuid,string pStructName,string pStructType)	
	CODE
	BeenUsedQ = pGuid[1:Size(pguid)]
	Get(BeenUsedQ,BeenUsedQ.Guid)
	If Errorcode()
		BeenUsedQ.StructureName = pStructName
		BeenUsedQ.StructureType = pStructType
		Add(BeenUsedQ)
	End
	RETURN

local.InQueue						Procedure(string pGuid)
	CODE
	BeenUsedQ = pGuid[1:Size(pguid)]
	Get(BeenUsedQ,BeenUsedQ.Guid)
	If Errorcode()
		Return 0
	ELSE
		Return 1
	End

local.CreateTableNode				Procedure()
	CODE
	!MyDct.Trace('')
	!MyDct.Trace('<9>local.CreateTableNode')
	!MyDct.Trace('')
	If KeySt.Length() > 0
		TableRowSt.AppendA(KeySt.GetValue(),'<13,10>')
	End
	TableRowSt.AppendA(TriggerSt.GetValue(),'<13,10>')
	TableRowSt.AppendA(AliasSt.GetValue(),'<13,10>')
	TableRowSt.Replace('<Field','<13,10><Field')
	TableRowSt.Replace('<WindowControl>','<13,10><WindowControl>')
	TableRowSt.Replace('</WindowControl>','<13,10></WindowControl>')
	TableRowSt.Replace('<Validity','<13,10><Validity')
	TableRowSt.Replace('<Line','<13,10><Line')
	TableRowSt.Replace('<Key','<13,10><Key')
	TableRowSt.Replace('</Key>','<13,10></Key>')
	TableRowSt.Replace('<Component','<13,10><Component')
	TableRowSt.Replace('</Component>','<13,10></Component>')
	TableRowSt.Replace('<Trigger','<13,10><Trigger')
	TableRowSt.Replace('<Code','<13,10><Code')
	TableRowSt.Replace('<Comment','<13,10><Comment')
	TableRowSt.Replace('<Audit','<13,10><Audit')
	TableRowSt.Replace('<Option','<13,10><Option')
	TableRowSt.Replace('<Alias','<13,10><Alias')
	TableRowSt.Replace('</Alias>','<13,10></Alias>')

  	!TableRowSt.ReplaceBetween('"','"','<13,10>','<32>')

  	TableRowSt.ReplaceBetween('"','"','&','&amp;')
  	!TableRowSt.ReplaceBetween('(',')','<39>','&apos;')
  	!TableRowSt.ReplaceBetween('(',')','<','&lt;')
  	!TableRowSt.ReplaceBetween('(',')','>','&gt;')
  	!TableRowSt.ReplaceBetween('"','"','<','&lt;')
  	!TableRowSt.ReplaceBetween('"','"','>','&gt;')

  	!TableRowSt.ReplaceBetween('"','"','<60>13,10<62>','')

	If Not TableRowSt.Instring('</Table>')
		TableRowSt.AppendA('<13,10>' & MyDct.GetXmlEndTag('Table'),'<13,10>')
	End

	!MyDct.Trace('')
	!MyDct.Trace(TableRowSt.GetValue())
	!MyDct.Trace('')

	MasterSt.AppendA(TableRowSt.GetValue(),'<13,10>')
	TableRowSt.Start()
	KeySt.Start()
	TriggerSt.Start()
	AliasSt.Start()
	!Clear(FieldRec)
	!Clear(KeyRec)
	!Clear(TableRec)
	Return
