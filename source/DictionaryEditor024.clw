

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module

                     MAP
                       INCLUDE('DICTIONARYEDITOR024.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
TotalRecordsCount    PROCEDURE  (string pParams,string pThread) ! Declare Procedure
ParamsGrp                   Group(ParamsGroupType).
OverStr                     String(Size(ParamsGrp)),Over(ParamsGrp)
RecCnt                      Long

MyDct               dwrDctParser

DctVersionsView     View(DctVersions)
                    Project(Ver:GUID)
                    End
					
TablesView          VIEW(Tables)
                    !PROJECT(Tab:PKGuid)
                    !PROJECT(Tab:DictionaryGuid)
                    !JOIN(Fld:FKFldParentGuidKey,Tab:PKGuid),Inner
                    !END
                    !JOIN(Key:FKKeyParentGuidKey,Tab:PKGuid),Inner
                    !END
                    !JOIN(Ali:FKAliParentGuidKey,Tab:PKGuid),Inner
                    !END
                    !JOIN(Tri:FKTriParentGuidKey,Tab:PKGuid),Inner
                    !END
                    !JOIN(Rel:FKRelPrimaryTableKey,Tab:Guid)
                    !END
                    END

FieldsView          View(Fields)
                    End

KeysView            View(Keys)
                    End

AliasesView         View(Aliases)
                    End

RelationsView       View(Relations)
                    End

TriggersView        View(Triggers)
                    End
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----

  CODE
? DEBUGHOOK(Aliases:Record)
? DEBUGHOOK(DctVersions:Record)
? DEBUGHOOK(Fields:Record)
? DEBUGHOOK(Keys:Record)
? DEBUGHOOK(Relations:Record)
? DEBUGHOOK(Tables:Record)
? DEBUGHOOK(Triggers:Record)



    !Logout(1,DctVersions,Tables,Fields,Keys,Aliases,Triggers,Relations)
    OverStr = pParams

    MyDct.Trace('')
    MyDct.Trace('TotalRecordsCount')
    MyDct.Trace('<9>ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
    MyDct.Trace('')
    MyDct.Trace('')

    Open(DctVersionsView)
    !BUFFER(DctVersionsView,100,0,20,300)
    DctVersionsView{PROP:Order} = ''
    DctVersionsView{PROP:Filter} = '(Ver:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
    Set(DctVersionsView)
    LOOP
        Next(DctVersionsView)
        If Errorcode(); Break END
        RecCnt += 1
    END 
    Close(DctVersionsView)

    Open(TablesView)
    TablesView{PROP:Order} = '' !'Tab:DctxOrder' !'Tab:PKGuid' !,Fld:ParentGUID,Key:ParentGUID,Ali:ParentGUID,Rel:ParentGUID,Tri:ParentGUID'
    TablesView{PROP:Filter} = '(Tab:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
    !MyDct.Trace('<9>TablesView{{PROP:Filter} = ' & TablesView{PROP:Filter})
    Set(TablesView)
    LOOP
        Next(TablesView)
        If Errorcode(); Break END
        RecCnt += 1
    End
    Close(TablesView)

!    Open(TablesView)
!    Open(FieldsView)
!    Open(KeysView)
!    Open(AliasesView)
!    Open(RelationsView)
!    Open(TriggersView)
!    !BUFFER(TablesView,100,0,20,300)
!    TablesView{PROP:Order} = '' !'Tab:DctxOrder' !'Tab:PKGuid' !,Fld:ParentGUID,Key:ParentGUID,Ali:ParentGUID,Rel:ParentGUID,Tri:ParentGUID'
!    TablesView{PROP:Filter} = '(Tab:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
!    !MyDct.Trace('<9>TablesView{{PROP:Filter} = ' & TablesView{PROP:Filter})
!    Set(TablesView)
!    LOOP
!        Next(TablesView)
!        If Errorcode(); Break END
!        RecCnt += 1
!        !MyDct.Trace('TablesView - RecCnt = ' & RecCnt & ' Tab:TableName = ' & Clip(Tab:TableName))
!
!        !If Not FieldsView{PROP:Filter}
!            FieldsView{PROP:Filter} = '(Fld:ParentGUID = ' & '''' & Tab:PKGuid & '''' & ')'
!            !MyDct.Trace('<9>FieldsView{{PROP:Filter} = ' & FieldsView{PROP:Filter})
!            Set(FieldsView)
!        !End
!        LOOP
!            Next(FieldsView)
!            If Errorcode(); Break END
!            RecCnt += 1
!            !MyDct.Trace('<9>FieldsView - RecCnt = ' & RecCnt)
!        END
!
!        !If Not KeysView{PROP:Filter}
!            KeysView{PROP:Filter} = '(Key:ParentGUID = ' & '''' & Tab:PKGuid & '''' & ')'
!            !MyDct.Trace('<9><9>KeysView{{PROP:Filter} = ' & KeysView{PROP:Filter})
!            Set(KeysView)
!        !End
!        LOOP
!            Next(KeysView)
!            If Errorcode(); Break END
!            RecCnt += 1
!            !MyDct.Trace('<9><9>KeysView - RecCnt = ' & RecCnt)
!        END
!        
!        !If Not AliasesView{PROP:Filter}
!            AliasesView{PROP:Filter} = '(Ali:ParentGUID = ' & '''' & Tab:PKGuid & '''' & ')'
!            !MyDct.Trace('<9><9><9><9>AliasesView{{PROP:Filter} = ' & AliasesView{PROP:Filter})
!            Set(AliasesView)
!        !End
!        LOOP
!            Next(AliasesView)
!            If Errorcode(); Break END
!            RecCnt += 1
!            !MyDct.Trace('<9><9><9>AliasesView - RecCnt = ' & RecCnt)
!        END
!
!!        !If Not RelationsView{PROP:Filter}
!!            RelationsView{PROP:Filter} = '(Rel:ParentGUID = ' & '''' & Tab:PKGuid & '''' & ')'
!!            !MyDct.Trace('<9><9><9><9><9><9>RelationsView{{PROP:Filter} = ' & RelationsView{PROP:Filter})
!!            Set(RelationsView)
!!        !End
!!        LOOP
!!            Next(RelationsView)
!!            If Errorcode(); Break END
!!            RecCnt += 1
!!            !MyDct.Trace('<9><9><9><9>RelationsView - RecCnt = ' & RecCnt)
!!        END
!
!        !If Not TriggersView{PROP:Filter}
!            TriggersView{PROP:Filter} = '(Tri:ParentGUID = ' & '''' & Tab:PKGuid & '''' & ')'
!            !MyDct.Trace('<9>TriggersView{{PROP:Filter} = ' & TriggersView{PROP:Filter})
!            Set(TriggersView)
!        !End
!        LOOP
!            Next(TriggersView)
!            If Errorcode(); Break END
!            RecCnt += 1
!            !MyDct.Trace('<9><9><9><9><9>TriggersView - RecCnt = ' & RecCnt)
!        END
!
!    END
!    Close(TablesView)
!    Close(FieldsView)
!    Close(KeysView)
!    Close(AliasesView)
!    Close(RelationsView)
!    Close(TriggersView)

    !BUFFER(TablesView,100,0,20,300)
!    FieldsView{PROP:Order} = '' !'Tab:DctxOrder' !'Tab:PKGuid' !,Fld:ParentGUID,Key:ParentGUID,Ali:ParentGUID,Rel:ParentGUID,Tri:ParentGUID'
!    FieldsView{PROP:Filter} = '(Tab:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
!    MyDct.Trace('<9>FieldsView{{PROP:Filter} = ' & FieldsView{PROP:Filter})
!    Set(FieldsView)
!    LOOP
!        Next(FieldsView)
!        If Errorcode(); Break END
!        RecCnt += 1
!    END
!    Close(FieldsView)

!    Open(KeysView)
!    BUFFER(TablesView,100,0,20,300)
!    KeysView{PROP:Order} = '' !'Tab:DctxOrder' !'Tab:PKGuid' !,Fld:ParentGUID,Key:ParentGUID,Ali:ParentGUID,Rel:ParentGUID,Tri:ParentGUID'
!    KeysView{PROP:Filter} = '(Tab:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
!    MyDct.Trace('<9>KeysView{{PROP:Filter} = ' & KeysView{PROP:Filter})
!    Set(KeysView)
!    LOOP
!        Next(KeysView)
!        If Errorcode(); Break END
!        RecCnt += 1
!    END
!    Close(KeysView)

!    Open(AliasesView)
!    !BUFFER(TablesView,100,0,20,300)
!    AliasesView{PROP:Order} = '' !'Tab:DctxOrder' !'Tab:PKGuid' !,Fld:ParentGUID,Key:ParentGUID,Ali:ParentGUID,Rel:ParentGUID,Tri:ParentGUID'
!    AliasesView{PROP:Filter} = '(Tab:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
!    MyDct.Trace('<9>AliasesView{{PROP:Filter} = ' & AliasesView{PROP:Filter})
!    Set(AliasesView)
!    LOOP
!        Next(AliasesView)
!        If Errorcode(); Break END
!        RecCnt += 1
!    END
!    Close(AliasesView)

    Open(RelationsView)
    !BUFFER(TablesView,100,0,20,300)
    RelationsView{PROP:Order} = '' !'Tab:DctxOrder' !'Tab:PKGuid' !,Fld:ParentGUID,Key:ParentGUID,Ali:ParentGUID,Rel:ParentGUID,Tri:ParentGUID'
    RelationsView{PROP:Filter} = '(Rel:ParentGUID = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
    !MyDct.Trace('<9>RelationsView{{PROP:Filter} = ' & RelationsView{PROP:Filter})
    Set(RelationsView)
    LOOP
        Next(RelationsView)
        If Errorcode(); Break END
        RecCnt += 1
    END
    Close(RelationsView)

!    Open(TriggersView)
!    !BUFFER(TablesView,100,0,20,300)
!    TriggersView{PROP:Order} = '' !'Tab:DctxOrder' !'Tab:PKGuid' !,Fld:ParentGUID,Key:ParentGUID,Ali:ParentGUID,Rel:ParentGUID,Tri:ParentGUID'
!    TriggersView{PROP:Filter} = '(Tab:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
!    MyDct.Trace('<9>TriggersView{{PROP:Filter} = ' & TriggersView{PROP:Filter})
!    Set(TriggersView)
!    LOOP
!        Next(TriggersView)
!        If Errorcode(); Break END
!        RecCnt += 1
!    END
!    Close(TriggersView)
 


    Return RecCnt
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
