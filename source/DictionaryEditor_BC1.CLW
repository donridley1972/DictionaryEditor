  MEMBER('DictionaryEditor.clw')

  INCLUDE('ABFILE.INC'),ONCE

  MAP
DictionaryEditor_BC1:DctInit    PROCEDURE()
DictionaryEditor_BC1:DctKill    PROCEDURE()
DictionaryEditor_BC1:FilesInit  PROCEDURE()
  END

Hide:Access:Aliases  CLASS(FileManager),TYPE               ! FileManager for Aliases
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Aliases  CLASS(RelationManager),TYPE           ! RelationManager for Aliases
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:DuplicateKeys CLASS(FileManager),TYPE          ! FileManager for DuplicateKeys
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:DuplicateKeys CLASS(RelationManager),TYPE      ! RelationManager for DuplicateKeys
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:TreeMemTable CLASS(FileManager),TYPE           ! FileManager for TreeMemTable
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:TreeMemTable CLASS(RelationManager),TYPE       ! RelationManager for TreeMemTable
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:DctComments CLASS(FileManager),TYPE            ! FileManager for DctComments
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:DctComments CLASS(RelationManager),TYPE        ! RelationManager for DctComments
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:TableTypes CLASS(FileManager),TYPE             ! FileManager for TableTypes
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:TableTypes CLASS(RelationManager),TYPE         ! RelationManager for TableTypes
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:FieldDefaultsLkUp CLASS(FileManager),TYPE      ! FileManager for FieldDefaultsLkUp
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:FieldDefaultsLkUp CLASS(RelationManager),TYPE  ! RelationManager for FieldDefaultsLkUp
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:TablePools CLASS(FileManager),TYPE             ! FileManager for TablePools
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:TablePools CLASS(RelationManager),TYPE         ! RelationManager for TablePools
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:TableGlobals CLASS(FileManager),TYPE           ! FileManager for TableGlobals
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:TableGlobals CLASS(RelationManager),TYPE       ! RelationManager for TableGlobals
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END


_Hide:Access:Aliases &Hide:Access:Aliases,AUTO,THREAD
_Hide:Relate:Aliases &Hide:Relate:Aliases,AUTO,THREAD
_Hide:Access:DuplicateKeys &Hide:Access:DuplicateKeys,AUTO,THREAD
_Hide:Relate:DuplicateKeys &Hide:Relate:DuplicateKeys,AUTO,THREAD
_Hide:Access:TreeMemTable &Hide:Access:TreeMemTable,AUTO,THREAD
_Hide:Relate:TreeMemTable &Hide:Relate:TreeMemTable,AUTO,THREAD
_Hide:Access:DctComments &Hide:Access:DctComments,AUTO,THREAD
_Hide:Relate:DctComments &Hide:Relate:DctComments,AUTO,THREAD
_Hide:Access:TableTypes &Hide:Access:TableTypes,AUTO,THREAD
_Hide:Relate:TableTypes &Hide:Relate:TableTypes,AUTO,THREAD
_Hide:Access:FieldDefaultsLkUp &Hide:Access:FieldDefaultsLkUp,AUTO,THREAD
_Hide:Relate:FieldDefaultsLkUp &Hide:Relate:FieldDefaultsLkUp,AUTO,THREAD
_Hide:Access:TablePools &Hide:Access:TablePools,AUTO,THREAD
_Hide:Relate:TablePools &Hide:Relate:TablePools,AUTO,THREAD
_Hide:Access:TableGlobals &Hide:Access:TableGlobals,AUTO,THREAD
_Hide:Relate:TableGlobals &Hide:Relate:TableGlobals,AUTO,THREAD


DictionaryEditor_BC1:DctInit PROCEDURE()
  CODE
  _Hide:Access:Aliases &= NEW(Hide:Access:Aliases)
  _Hide:Relate:Aliases &= NEW(Hide:Relate:Aliases)
  _Hide:Access:DuplicateKeys &= NEW(Hide:Access:DuplicateKeys)
  _Hide:Relate:DuplicateKeys &= NEW(Hide:Relate:DuplicateKeys)
  _Hide:Access:TreeMemTable &= NEW(Hide:Access:TreeMemTable)
  _Hide:Relate:TreeMemTable &= NEW(Hide:Relate:TreeMemTable)
  _Hide:Access:DctComments &= NEW(Hide:Access:DctComments)
  _Hide:Relate:DctComments &= NEW(Hide:Relate:DctComments)
  _Hide:Access:TableTypes &= NEW(Hide:Access:TableTypes)
  _Hide:Relate:TableTypes &= NEW(Hide:Relate:TableTypes)
  _Hide:Access:FieldDefaultsLkUp &= NEW(Hide:Access:FieldDefaultsLkUp)
  _Hide:Relate:FieldDefaultsLkUp &= NEW(Hide:Relate:FieldDefaultsLkUp)
  _Hide:Access:TablePools &= NEW(Hide:Access:TablePools)
  _Hide:Relate:TablePools &= NEW(Hide:Relate:TablePools)
  _Hide:Access:TableGlobals &= NEW(Hide:Access:TableGlobals)
  _Hide:Relate:TableGlobals &= NEW(Hide:Relate:TableGlobals)
  Relate:Aliases &= _Hide:Relate:Aliases
  Relate:DuplicateKeys &= _Hide:Relate:DuplicateKeys
  Relate:TreeMemTable &= _Hide:Relate:TreeMemTable
  Relate:DctComments &= _Hide:Relate:DctComments
  Relate:TableTypes &= _Hide:Relate:TableTypes
  Relate:FieldDefaultsLkUp &= _Hide:Relate:FieldDefaultsLkUp
  Relate:TablePools &= _Hide:Relate:TablePools
  Relate:TableGlobals &= _Hide:Relate:TableGlobals


DictionaryEditor_BC1:FilesInit PROCEDURE()
  CODE
  _Hide:Relate:Aliases.Init
  _Hide:Relate:DuplicateKeys.Init
  _Hide:Relate:TreeMemTable.Init
  _Hide:Relate:DctComments.Init
  _Hide:Relate:TableTypes.Init
  _Hide:Relate:FieldDefaultsLkUp.Init
  _Hide:Relate:TablePools.Init
  _Hide:Relate:TableGlobals.Init


DictionaryEditor_BC1:DctKill PROCEDURE()
  CODE
  _Hide:Relate:Aliases.Kill
  DISPOSE(_Hide:Relate:Aliases)
  _Hide:Relate:DuplicateKeys.Kill
  DISPOSE(_Hide:Relate:DuplicateKeys)
  _Hide:Relate:TreeMemTable.Kill
  DISPOSE(_Hide:Relate:TreeMemTable)
  _Hide:Relate:DctComments.Kill
  DISPOSE(_Hide:Relate:DctComments)
  _Hide:Relate:TableTypes.Kill
  DISPOSE(_Hide:Relate:TableTypes)
  _Hide:Relate:FieldDefaultsLkUp.Kill
  DISPOSE(_Hide:Relate:FieldDefaultsLkUp)
  _Hide:Relate:TablePools.Kill
  DISPOSE(_Hide:Relate:TablePools)
  _Hide:Relate:TableGlobals.Kill
  DISPOSE(_Hide:Relate:TableGlobals)


Hide:Relate:Aliases.Init PROCEDURE()
  CODE
  _Hide:Access:Aliases.Init
  SELF.Init(Access:Aliases,1)


Hide:Access:Aliases.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Ali:Record
  SELF.FileNameValue = 'Aliases'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Aliases
  PARENT.Init
  Access:Aliases &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:Aliases.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Aliases &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Aliases.PrimeFields PROCEDURE

  CODE
  Ali:PKGuid = Glo:st.MakeGuid()                           ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:Aliases.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Aliases,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Ali:PKAliGuidKey,'Ali:PKAliGuidKey',0)
    SELF.AddKey(Ali:FKAliTableGuid,'Ali:FKAliTableGuid',0)
    SELF.AddKey(Ali:FKAliParentGuidKey,'Ali:FKAliParentGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Aliases.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Tables)


Hide:Relate:Aliases.Kill PROCEDURE

  CODE
  _Hide:Access:Aliases.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:Aliases &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Aliases)                            ! destroy the file manager


Hide:Relate:DuplicateKeys.Init PROCEDURE()
  CODE
  _Hide:Access:DuplicateKeys.Init
  SELF.Init(Access:DuplicateKeys,1)


Hide:Access:DuplicateKeys.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= DupK:Record
  SELF.FileNameValue = 'DuplicateKeys'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= DuplicateKeys
  PARENT.Init
  Access:DuplicateKeys &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:DuplicateKeys.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:DuplicateKeys &= NULL                             ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:DuplicateKeys.PrimeFields PROCEDURE

  CODE
  DupK:PKGuid = Glo:st.MakeGuid()                          ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:DuplicateKeys.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(DuplicateKeys,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(DupK:PKDupKGuidKey,'DupK:PKDupKGuidKey',0)
    SELF.AddKey(DupK:FKDupKDictionaryGuid,'DupK:FKDupKDictionaryGuid',0)
    SELF.AddKey(DupK:FKDupKTableGuid,'DupK:FKDupKTableGuid',0)
    SELF.AddKey(DupK:FKDupKPKGuidKey,'DupK:FKDupKPKGuidKey',0)
    SELF.AddKey(DupK:FKDupKKeyGuidKey,'DupK:FKDupKKeyGuidKey',0)
    SELF.AddKey(DupK:FLDupKKeyNameKey,'DupK:FLDupKKeyNameKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:DuplicateKeys.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Dictionaries)

  SELF.AddRelation(Relate:Tables,RI:CASCADE,RI:None,Tab:PKTabGuidKey)
  SELF.AddRelationLink(DupK:TableGuid,Tab:PKGuid)


Hide:Relate:DuplicateKeys.Kill PROCEDURE

  CODE
  _Hide:Access:DuplicateKeys.Kill                          ! Kill the file manager
  PARENT.Kill
  Relate:DuplicateKeys &= NULL                             ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:DuplicateKeys)                      ! destroy the file manager


Hide:Relate:TreeMemTable.Init PROCEDURE()
  CODE
  _Hide:Access:TreeMemTable.Init
  SELF.Init(Access:TreeMemTable,1)


Hide:Access:TreeMemTable.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Tree:Record
  SELF.FileNameValue = 'TreeMemTable'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= TreeMemTable
  PARENT.Init
  Access:TreeMemTable &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:TreeMemTable.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:TreeMemTable &= NULL                              ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:TreeMemTable.PrimeFields PROCEDURE

  CODE
  Tree:PKGuid = Glo:st.MakeGuid()                          ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:TreeMemTable.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(TreeMemTable,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Tree:PKTreeGuidKey,'Tree:PKTreeGuidKey',0)
    SELF.AddKey(Tree:FKTreeDictionaryGuidKey,'Tree:FKTreeDictionaryGuidKey',0)
    SELF.AddKey(Tree:FKTreeTableGuidKey,'Tree:FKTreeTableGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:TreeMemTable.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Fields,RI:CASCADE,RI:None,Fld:FKFldParentGuidKey)
  SELF.AddRelationLink(Tree:TableGuid,Fld:ParentGUID)

  SELF.AddRelation(Relate:Keys,RI:CASCADE,RI:None,Key:FKKeyParentGuidKey)
  SELF.AddRelationLink(Tree:TableGuid,Key:ParentGUID)

  SELF.AddRelation(Relate:Triggers,RI:CASCADE,RI:None,Tri:FKTriParentGuidKey)
  SELF.AddRelationLink(Tree:TableGuid,Tri:ParentGUID)


Hide:Relate:TreeMemTable.Kill PROCEDURE

  CODE
  _Hide:Access:TreeMemTable.Kill                           ! Kill the file manager
  PARENT.Kill
  Relate:TreeMemTable &= NULL                              ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:TreeMemTable)                       ! destroy the file manager


Hide:Relate:DctComments.Init PROCEDURE()
  CODE
  _Hide:Access:DctComments.Init
  SELF.Init(Access:DctComments,1)


Hide:Access:DctComments.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Dcom:Record
  SELF.FileNameValue = 'DctComments'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= DctComments
  PARENT.Init
  Access:DctComments &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:DctComments.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:DctComments &= NULL                               ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:DctComments.PrimeFields PROCEDURE

  CODE
  Dcom:PKGuid = Glo:st.MakeGuid()                          ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:DctComments.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(DctComments,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Dcom:PKDcomGuidKey,'Dcom:PKDcomGuidKey',0)
    SELF.AddKey(Dcom:FKDcomDictionaryGuidKey,'Dcom:FKDcomDictionaryGuidKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:DctComments.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Dictionaries)


Hide:Relate:DctComments.Kill PROCEDURE

  CODE
  _Hide:Access:DctComments.Kill                            ! Kill the file manager
  PARENT.Kill
  Relate:DctComments &= NULL                               ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:DctComments)                        ! destroy the file manager


Hide:Relate:TableTypes.Init PROCEDURE()
  CODE
  _Hide:Access:TableTypes.Init
  SELF.Init(Access:TableTypes,1)


Hide:Access:TableTypes.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= TblTypes:Record
  SELF.FileNameValue = 'TableTypes'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= TableTypes
  PARENT.Init
  Access:TableTypes &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:TableTypes.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:TableTypes &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:TableTypes.PrimeFields PROCEDURE

  CODE
  TblTypes:PKGuid = Glo:st.MakeGuid()                      ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:TableTypes.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(TableTypes,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(TblTypes:TbleTypesPKGuidKey,'TblTypes:TbleTypesPKGuidKey',0)
    SELF.AddKey(TblTypes:TblTypesDescriptionKey,'TblTypes:TblTypesDescriptionKey',0)
    SELF.AddKey(TblTypes:TblTypesTypeOrderKey,'TblTypes:TblTypesTypeOrderKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:TableTypes.Kill PROCEDURE

  CODE
  _Hide:Access:TableTypes.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:TableTypes &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:TableTypes)                         ! destroy the file manager


Hide:Relate:FieldDefaultsLkUp.Init PROCEDURE()
  CODE
  _Hide:Access:FieldDefaultsLkUp.Init
  SELF.Init(Access:FieldDefaultsLkUp,1)


Hide:Access:FieldDefaultsLkUp.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= Fie:Record
  SELF.FileNameValue = 'FieldDefaultsLkUp'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= FieldDefaultsLkUp
  PARENT.Init
  Access:FieldDefaultsLkUp &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:FieldDefaultsLkUp.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:FieldDefaultsLkUp &= NULL                         ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:FieldDefaultsLkUp.PrimeFields PROCEDURE

  CODE
  Fie:PKGuid = Glo:st.MakeGuid()                           ! Assign initial field value
  PARENT.PrimeFields


Hide:Access:FieldDefaultsLkUp.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(FieldDefaultsLkUp,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(Fie:PKGuidKey,'Fie:PKGuidKey',0)
    SELF.AddKey(Fie:TypeNameKey,'Fie:TypeNameKey',0)
    SELF.AddKey(Fie:BaseTypeKey,'Fie:BaseTypeKey',0)
    SELF.AddKey(Fie:DataTypeKey,'Fie:DataTypeKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:FieldDefaultsLkUp.Kill PROCEDURE

  CODE
  _Hide:Access:FieldDefaultsLkUp.Kill                      ! Kill the file manager
  PARENT.Kill
  Relate:FieldDefaultsLkUp &= NULL                         ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:FieldDefaultsLkUp)                  ! destroy the file manager


Hide:Relate:TablePools.Init PROCEDURE()
  CODE
  _Hide:Access:TablePools.Init
  SELF.Init(Access:TablePools,1)
  SELF.SetAlias(Relate:Tables)


Hide:Access:TablePools.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= TabPool:Record
  SELF.AliasedFile &= Access:Tables                        !This is a File Alias, so assign aliased file manager
  SELF.FileNameValue = 'TablePools'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= TablePools
  PARENT.Init
  Access:TablePools &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:TablePools.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:TablePools &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:TablePools.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(TablePools,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(TabPool:PKTabGuidKey,'TabPool:PKTabGuidKey',0)
    SELF.AddKey(TabPool:TabTableTypeGuidKey,'TabPool:TabTableTypeGuidKey',0)
    SELF.AddKey(TabPool:FKTabParentGuidKey,'TabPool:FKTabParentGuidKey',0)
    SELF.AddKey(TabPool:FKTabDictionaryGuidKey,'TabPool:FKTabDictionaryGuidKey',0)
    SELF.AddKey(TabPool:FKTabTableGuidKey,'TabPool:FKTabTableGuidKey',0)
    SELF.AddKey(TabPool:FKTabGuidKey,'TabPool:FKTabGuidKey',0)
    SELF.AddKey(TabPool:FKTabDctxOrderKey,'TabPool:FKTabDctxOrderKey',0)
    SELF.AddKey(TabPool:FKTabTableNameKey,'TabPool:FKTabTableNameKey',0)
    SELF.AddKey(TabPool:FKTabDctGuidAndOrderKey,'Sort table by DCT Guid and DctxOrder value',0)
    SELF.AddKey(TabPool:FKTabParentGuidAndOrderKey,'TabPool:FKTabParentGuidAndOrderKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:TablePools.Kill PROCEDURE

  CODE
  _Hide:Access:TablePools.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:TablePools &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:TablePools)                         ! destroy the file manager


Hide:Relate:TableGlobals.Init PROCEDURE()
  CODE
  _Hide:Access:TableGlobals.Init
  SELF.Init(Access:TableGlobals,1)
  SELF.SetAlias(Relate:Tables)


Hide:Access:TableGlobals.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= TabGlo:Record
  SELF.AliasedFile &= Access:Tables                        !This is a File Alias, so assign aliased file manager
  SELF.FileNameValue = 'TableGlobals'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= TableGlobals
  PARENT.Init
  Access:TableGlobals &= SELF
  SELF.DoNotAssertOnCloseMisuse = 1  ! ABCDefaults
   IF SELF.File{prop:sqldriver} THEN SELF.create = 0.         ! ABCDefaults


Hide:Access:TableGlobals.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:TableGlobals &= NULL                              ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:TableGlobals.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(TableGlobals,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(TabGlo:PKTabGuidKey,'TabGlo:PKTabGuidKey',0)
    SELF.AddKey(TabGlo:TabTableTypeGuidKey,'TabGlo:TabTableTypeGuidKey',0)
    SELF.AddKey(TabGlo:FKTabParentGuidKey,'TabGlo:FKTabParentGuidKey',0)
    SELF.AddKey(TabGlo:FKTabDictionaryGuidKey,'TabGlo:FKTabDictionaryGuidKey',0)
    SELF.AddKey(TabGlo:FKTabTableGuidKey,'TabGlo:FKTabTableGuidKey',0)
    SELF.AddKey(TabGlo:FKTabGuidKey,'TabGlo:FKTabGuidKey',0)
    SELF.AddKey(TabGlo:FKTabDctxOrderKey,'TabGlo:FKTabDctxOrderKey',0)
    SELF.AddKey(TabGlo:FKTabTableNameKey,'TabGlo:FKTabTableNameKey',0)
    SELF.AddKey(TabGlo:FKTabDctGuidAndOrderKey,'Sort table by DCT Guid and DctxOrder value',0)
    SELF.AddKey(TabGlo:FKTabParentGuidAndOrderKey,'TabGlo:FKTabParentGuidAndOrderKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  IF SELF.File{prop:sqldriver} THEN SELF.create = 0.          ! ABCDefaults
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:TableGlobals.Kill PROCEDURE

  CODE
  _Hide:Access:TableGlobals.Kill                           ! Kill the file manager
  PARENT.Kill
  Relate:TableGlobals &= NULL                              ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:TableGlobals)                       ! destroy the file manager

