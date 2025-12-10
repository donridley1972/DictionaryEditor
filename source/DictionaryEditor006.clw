

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABREPORT.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR006.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Process
!!! Process the Tables File
!!! </summary>
BuildDctx PROCEDURE (string pParams,string pThread)

ParamsGrp               Group(ParamsGroupType).
OverStr                 String(Size(ParamsGrp)),Over(ParamsGrp)

MyDct                   dwrDctParser
st                      StringTheory
NodeSt                  StringTheory
MasterSt                StringTheory
TableSt                 StringTheory
FieldSt                 StringTheory
FieldRowSt				StringTheory
KeySt                   StringTheory
KeyRowSt				StringTheory
TriggerRowSt			StringTheory
mt                      MyTableClass

VersionRec              &Group
OptionsRec              &Group
TableRec                &Group
FieldRec                &Group
KeyRec                  &Group
!AliasRec                &Group
!TriggerRec              &Group
!RelationRec             &Group
FieldsGrp               Group(FieldGroupType).

saveTableGuid           Like(Tab:PKGuid)
!saveFieldParentGuid     Like(Fld:ParentGUID)
saveFieldGuid           Like(Fld:PKGuid)
saveKeyGuid             Like(Key:PKGuid)
!saveTriggerGuid         Like(Tri:PKGuid)

x						Long
TableCnt                Long
Progress:Thermometer BYTE                                  ! 
Loc:DctGud           STRING(16)                            ! 
ParamsGrp            GROUP(ParamsGroupType),PRE()          ! 
                     END                                   ! 
BeenUsedQ            QUEUE,PRE()                           ! 
Guid                 STRING(16)                            ! 
StructureName        STRING(100)                           ! 
StructureType        STRING(20)                            ! 
                     END                                   ! 
Process:View         VIEW(Tables)
                       PROJECT(Tab:PKGuid)
                       JOIN(Fld:FKFldParentGuidKey,Tab:PKGuid)
                       END
                       JOIN(Key:FKKeyParentGuidKey,Tab:PKGuid)
                       END
                       JOIN(Tri:FKTriParentGuidKey,Tab:PKGuid)
                       END
                     END
DctVersionsView     View(DctVersions)
                    Project(Ver:GUID)
                    Project(Ver:ParentGUID)
                    Project(Ver:DictionaryGuid)
                    Project(Ver:DctVersion)
                    Project(Ver:Description)
                    End

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
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
ProgressWindow       WINDOW('Process Tables'),AT(,,142,59),FONT('Microsoft Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(46,42,49,15),USE(?Progress:Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Process'), |
  TIP('Cancel Process')
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
local                       CLASS       
AddToFields                 Procedure(string pVal,string pDataType,*byte pLastInGroup)
AddToKeys					Procedure(string pVal)
InsertUpdateQ				Procedure(string pGuid,string pStructName,string pStructType)
InQueue						Procedure(string pGuid),Long
CreateTableNode				Procedure()
                            End
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Init                   PROCEDURE(ProcessClass PC,<REPORT R>,<PrintPreviewClass PV>)
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisProcess          CLASS(ProcessClass)                   ! Process Manager
Close                  PROCEDURE(BYTE Force),DERIVED
SetOrder               PROCEDURE(STRING Order),DERIVED
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepClass                             ! Progress Manager

  CODE
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
  	Loc:DctGud = ParamsGrp.DictionaryGuid
  GlobalErrors.SetProcedureName('BuildDctx')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('ParamsGrp.DictionaryGuid',ParamsGrp.DictionaryGuid)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Tables.SetOpenRelated()
  Relate:Tables.Open()                                     ! File Tables used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  	LOGOUT(1,Tables,Fields,Keys,Triggers,Relations)
  
    MasterSt.AddLine(Records(MasterSt.lines)+1,'<?xml version="1.0" encoding="UTF-8"?>')
  
    Dct:GUID = ParamsGrp.DictionaryGuid
    If Access:Dictionaries.Fetch(Dct:PKDctGUIDKey) = Level:Benign
        st.SetValue(MyDct.GetXmlString(Dictionaries,'Dictionary',False,0))
        !MyDct.Trace(st.GetValue())
        st.Remove('<13,10>')
        MasterSt.AddLine(Records(MasterSt.lines)+1,st.GetValue())
    End
  
  
    Open(DctVersionsView)
    DctVersionsView{PROP:Order} = ''
    DctVersionsView{PROP:Filter} = '(Ver:ParentGUID = ' & '''' & Clip(ParamsGrp.DictionaryGuid) & '''' & ')'
    Set(DctVersionsView)
    LOOP
        Next(DctVersionsView)
        If Errorcode(); Break END
        VersionRec &= DctVersions{PROP:Record}
        st.SetValue(MyDct.XmlGetNode(VersionRec,'DictionaryVersion',False))
        !MyDct.Trace(st.GetValue())
        st.Remove('<13,10>')
        MasterSt.AddLine(Records(MasterSt.lines)+1,st.GetValue())
    End    
    Close(DctVersionsView)
  
    Open(CommentsView)
    CommentsView{PROP:Order} = ''
    CommentsView{PROP:Filter} = '(Dcom:DictionaryGuid = ' & '''' & Clip(ParamsGrp.DictionaryGuid) & '''' & ')'
    Set(CommentsView)
    Loop
        Next(CommentsView)
        If Errorcode(); Break End
        mt.GetBlob(DctComments,'Comments',st,mt:label)
        MasterSt.AddLine(Records(MasterSt.lines)+1,st.GetValue()) 
    End
    Close(CommentsView)
  
    Open(OptionsView)
    OptionsView{PROP:Order} = ''
    OptionsView{PROP:Filter} = '(Opt:DictionaryGuid = ' & '''' & Clip(ParamsGrp.DictionaryGuid) & '''' & ')'
    Set(OptionsView)
    LOOP
        Next(OptionsView)
        If Errorcode(); Break END
        OptionsRec &= Options{PROP:Record}
        st.SetValue(MyDct.XmlGetNode(OptionsRec,'Option',False))
        !MyDct.Trace(st.GetValue())
        st.Remove('<13,10>')
        MasterSt.AddLine(Records(MasterSt.lines)+1,st.GetValue())
    End    
    Close(DctVersionsView)
  SELF.Open(ProgressWindow)                                ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('BuildDctx', THREAD(), ProgressWindow)
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
  INIMgr.Fetch('BuildDctx',ProgressWindow)                 ! Restore window settings from non-volatile store
  	Process:View{PROP:Filter} = '(Tab:DictionaryGuid = ' & '''' & Clip(ParamsGrp.DictionaryGuid) & '''' & ')'  
  ProgressWindow{Prop:Timer} = 10                          ! Assign timer interval
  ThisProcess.Init(Process:View, Relate:Tables, ?Progress:PctText, Progress:Thermometer)
  ThisProcess.AddSortOrder()
  ProgressWindow{Prop:Text} = 'Processing Records'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  SELF.Init(ThisProcess)
  ?Progress:UserString{Prop:Text}=''
  SELF.AddItem(?Progress:Cancel, RequestCancelled)
  !ThisProcess.SetFilter('(Tab:DictionaryGuid = ' & '''' & Clip(ParamsGrp.DictionaryGuid) & '''' & ')')
  	MyDct.Trace('')	
  	MyDct.Trace('Process:View{{PROP:Filter} = ' & Process:View{PROP:Filter} )	
  	MyDct.Trace('')	
  !BUFFER(Process:View,100,0,100,300)
  SEND(Tables,'QUICKSCAN=on')
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Init PROCEDURE(ProcessClass PC,<REPORT R>,<PrintPreviewClass PV>)

  CODE
  PARENT.Init(PC,R,PV)
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
  WinAlertMouseZoom()
  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('BuildDctx', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Tables.Close()
  END
  IF SELF.Opened
    INIMgr.Update('BuildDctx',ProgressWindow)              ! Save window data to non-volatile store
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
    OF EVENT:OpenWindow
        WE::CantCloseNow += 1
        WE::CantCloseNowSetHere = 1
    END
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeWindowEvent(ProgressWindow)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:CloseWindow
      if WE::CantCloseNow > 0 and ReturnValue = Level:Benign and WE::CantCloseNowSetHere
        WE::CantCloseNow -= 1
        WE::CantCloseNowSetHere = 0
      end
    OF EVENT:OpenWindow
        post(event:visibleondesktop)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
local.AddToFields                 	Procedure(string pVal,string pDataType,*byte pLastInGroup)
TempSt			StringTheory
    CODE
	!MyDct.Trace('')
	!MyDct.Trace('<9>local.AddToFields')
	!MyDct.Trace('	pLastInGroup = ' & pLastInGroup)
	TempSt.SetValue(pVal,True)
	TempSt.ReplaceBetween('<','>','<13,10>','<0>')
	TempSt.Remove('<0>')
	!MyDct.Trace('	pField = ') 
	!MyDct.Trace(clip(pField))
	!MyDct.Trace('')
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
	If TempSt.Length() > 0
		KeySt.AddLine(Records(KeySt.lines)+1,TempSt.GetValue())
		KeySt.AddLine(Records(KeySt.lines)+1,MyDct.GetXmlEndTag('Key'))
	End
	KeyRowSt.Start()
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
	!MyDct.Trace('<9>local.CreateTableNode')
	FieldSt.RemoveLines()
	FieldSt.Join('<13,10>')
	KeySt.RemoveLines()
	KeySt.Join('<13,10>')
	!TriggerRowSt.RemoveLines()
	!TriggerRowSt.Join('<13,10>')	

	TableSt.AddLine(Records(TableSt.lines)+1,FieldSt.GetValue())
	TableSt.AddLine(Records(TableSt.lines)+1,KeySt.GetValue())
	!TableSt.AddLine(Records(TableSt.lines)+1,TriggerRowSt.GetValue())

	TableSt.AddLine(Records(TableSt.lines)+1,MyDct.GetXmlEndTag('Table'))
	!TableSt.AppendA(FieldSt.GetValue(),'<13,10>')
	!TableSt.AppendA(KeySt.GetValue(),'<13,10>')
	!TableSt.AppendA(MyDct.GetXmlEndTag('Table'),'<13,10>')
	TableSt.RemoveLines()
	TableSt.Join('<13,10>')
	TableSt.Split('<13,10>')
	TableSt.RemoveLines()
	TableSt.Join('<13,10>')
	TableSt.ReplaceBetween('<','>','<13,10>','')

	MasterSt.AddLine(Records(MasterSt.lines)+1,TableSt.GetValue())
	TableSt.Start()
	!MyDct.Trace('')
	!MyDct.Trace(TableSt.GetValue())
	!MyDct.Trace('')
	!TableSt.SaveFile('TempDctx.dctx')
	FieldRowSt.Start()
	FieldSt.Start()
	KeySt.Start()
	TriggerRowSt.Start()
	Return

ThisProcess.Close PROCEDURE(BYTE Force)

  CODE
  	COMMIT  
  PARENT.Close(Force)


ThisProcess.SetOrder PROCEDURE(STRING Order)

  CODE
  PARENT.SetOrder(Order)
  !ThisProcess.SetFilter('(Tab:DictionaryGuid = ' & '''' & Clip(ParamsGrp.DictionaryGuid) & '''' & ')')  


ThisProcess.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  		x+=1
  		If saveTableGuid <> Tab:PKGuid
  			TableCnt += 1
  			MyDct.Trace(x & '<9>TableGuid Changed - ' & Tab:PKGuid & '<9>' & Tab:TableName) 
  			If x > 1
  				local.CreateTableNode()
  			End
            TableRec &= Tables{PROP:Record}
            NodeSt.SetValue(MyDct.XmlGetNode(TableRec,'Table',True))
            NodeSt.Squeeze(ST:NOPUNCTUATION)
            TableSt.AddLine(Records(TableSt.lines)+1,NodeSt.GetValue())
            mt.GetBlob(Tables,'CommentsBlob',st,mt:label)
            st.Squeeze(ST:NOPUNCTUATION)
            TableSt.AddLine(Records(TableSt.lines)+1,st.GetValue())  
            mt.GetBlob(Tables,'OptionsBlob',st,mt:label)
            st.Squeeze(ST:NOPUNCTUATION)
            TableSt.AddLine(Records(TableSt.lines)+1,st.GetValue()) 
    		local.InsertUpdateQ(Tab:PKGuid,Tab:TableName,'Table')
  			saveTableGuid = Tab:PKGuid
  		End
  
        If saveFieldGuid <> Fld:PKGuid
            FieldRec &= Fields{PROP:Record}
            FieldsGrp = FieldRec
            st.SetValue(MyDct.XmlGetNode(FieldRec,'Field',True))
            st.Squeeze(ST:NOPUNCTUATION)
            FieldRowSt.AddLine(Records(FieldRowSt.lines)+1,st.GetValue())
            mt.GetBlob(Fields,'WindowControlBlob',st,mt:label)
            FieldRowSt.AddLine(Records(FieldRowSt.lines)+1,st.GetValue())
            mt.GetBlob(Fields,'AuditBlob',st,mt:label)
            FieldRowSt.AddLine(Records(FieldRowSt.lines)+1,st.GetValue())
            mt.GetBlob(Fields,'ValidityBlob',st,mt:label)
            FieldRowSt.AddLine(Records(FieldRowSt.lines)+1,st.GetValue())  
            mt.GetBlob(Fields,'CommentsBlob',st,mt:label)
            FieldRowSt.AddLine(Records(FieldRowSt.lines)+1,st.GetValue())  
            mt.GetBlob(Fields,'OptionsBlob',st,mt:label)
            FieldRowSt.AddLine(Records(FieldRowSt.lines)+1,st.GetValue())  
  			FieldRowSt.RemoveLines()
  			FieldRowSt.Join('<13,10>')
  			local.InsertUpdateQ(Fld:PKGuid,Fld:FieldName,'Field')
  			if local.InQueue(Tab:PKGuid) = 1
  				local.AddToFields(FieldRowSt.GetValue(),Upper(Clip(FieldsGrp.DataType)),FieldsGrp.LastInGroup)
  			End
            Clear(FieldsGrp)
            saveFieldGuid = Fld:PKGuid
        End
  		If saveKeyGuid <> Key:PKGuid
  			KeyRec &= Keys{PROP:Record}
  			!RecordCnt += 1
  			st.SetValue(MyDct.XmlGetNode(KeyRec,'Key',True),True)
  			KeyRowSt.AddLine(Records(KeyRowSt.lines)+1,st.GetValue())
  			mt.GetBlob(Keys,'AuditBlob',st,mt:label)
  			KeyRowSt.AddLine(Records(KeyRowSt.lines)+1,st.GetValue())
  			mt.GetBlob(Keys,'CommentsBlob',st,mt:label)
  			KeyRowSt.AddLine(Records(KeyRowSt.lines)+1,st.GetValue())  
  			mt.GetBlob(Keys,'OptionsBlob',st,mt:label)
  			KeyRowSt.AddLine(Records(KeyRowSt.lines)+1,st.GetValue())  
  			mt.GetBlob(Keys,'ComponentsBlob',st,mt:label)
  			KeyRowSt.AddLine(Records(KeyRowSt.lines)+1,st.GetValue())
  			KeyRowSt.RemoveLines()
  			KeyRowSt.Join('<13,10>')
  			KeyRowSt.Split('<13,10>')
  			KeyRowSt.RemoveLines()
  			KeyRowSt.Join('<13,10>')
  			if local.InQueue(Key:PKGuid) = 0
  				local.AddToKeys(KeyRowSt.GetValue())
  			End
  			local.InsertUpdateQ(Key:PKGuid,Key:KeyName,'Key')
  			KeyRowSt.Start()
  			saveKeyGuid = Key:PKGuid
  		End  
  ReturnValue = PARENT.TakeRecord()
  RETURN ReturnValue

